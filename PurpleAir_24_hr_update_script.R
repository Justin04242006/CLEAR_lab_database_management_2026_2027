###Writing the code for the 24 hour trigger to update sensor_infomation and air_history tables daily (adapted from the code for a previous 24 hour trigger)

library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(lubridate)
library(PurpleAirAPI)
library(DBI)
library(odbc)
library(PurpleAir)

# 1. Connect to server
Purpleair_api_key <- Sys.getenv("PURPLE_AIR_API_KEY") #The environment variable needs to be exactly PURPLE_AIR_API_KEY to match the arguments of the get_sensors_data() function used in Step 3. The code will most assuredly fail if this is not the case!
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script began executing.\n")

con = dbConnect(
  odbc(),
  Driver = "/opt/homebrew/lib/libmsodbcsql.18.dylib",
  Server = "resdbsprdhs02.adms.luc.edu",
  Database = "clearlab_purpleair",
  UID = Sys.getenv("DB_USERNAME"), #Database username and password were saved as environment variables
  PWD = Sys.getenv("DB_PASSWORD"),
  Encrypt = "yes",
  TrustServerCertificate = "yes"
)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Connected to database\n"
)

### 2. Fetch Database Active Sensors
active_sensors <- dbGetQuery(con, "SELECT sensor_index, last_seen FROM Purpleair_sensor_information WHERE sensor_activity_status='Active'")
sensoridsvec <- active_sensors$sensor_index

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Retrieved active sensors\n"
)


### 3.Fetch Live API Timestamps

api_sensors_all <- get_sensors_data(x=as.integer(sensoridsvec), fields = c("last_seen"))

updated_sensors <- c() 
sensors_pulled_log<-c()

for (i in sensoridsvec) {
  query1 <- dbGetQuery(con, "SELECT last_seen FROM Purpleair_sensor_information WHERE sensor_index = ?", params = list(i))
  date_threshold <- dbGetQuery(con, "SELECT DATEADD(day, -30, CAST(GETDATE() AS DATE))")
  
  db_last_seen <- if(nrow(query1) > 0) query1$last_seen else NA
  
  api_row <- api_sensors_all %>% 
    filter(sensor_index == i)
  
  if (nrow(api_row) > 0) {
    api_last_seen <- api_row$last_seen[1]
    
    if (is.na(db_last_seen) || db_last_seen < api_last_seen) {
      
      query2 <- "UPDATE Purpleair_sensor_information SET last_seen = ? WHERE sensor_index = ?"
      
      dbExecute(con, query2, params = list(as.character(api_last_seen), i))}
      
    if (db_last_seen >= date_threshold){
      
      query3 <- "UPDATE Purpleair_sensor_information SET sensor_activity_status= 'Active' WHERE sensor_index = ?"
      
      dbExecute(con, query3, params=list(i))
      
      updated_sensors <- c(updated_sensors, i) 
      sensors_pulled_log <- c(sensors_pulled_log, as.character(i))}

  }
}


cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Updated last_seen where necessary\n"
)

### 4. Fetch and Process Historical Data

#Wrap historical steps in an IF block to prevent errors if 0 sensors need updates

if (length(updated_sensors) > 0) { 
  
  current_time <- Sys.time()
  time_query <- dbGetQuery(con, "SELECT MAX(time_stamp) AS time_stamp FROM Purpleair_air_history")
  initial_time<-time_query$time_stamp
  
  start_date_str <- format(initial_time,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end_date_str   <- format(current_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  update_raw<-NULL
  
  tryCatch({update_raw = getSensorHistory(
    sensorIndex = updated_sensors, 
    apiReadKey=Purpleair_api_key,
    startDate=start_date_str,
    endDate=end_date_str,
    average='0', 
    fields=c('humidity', 'temperature', 'pressure', 'pm2.5_cf_1_a', 'pm2.5_cf_1_b', 'rssi')
  )
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Pulled previous 24 hours of historical data\n"
  )}, error= function(e) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- ERROR: Purpleair API history pull failed:", conditionMessage(e), "\n")
  })
  
  if (!is.null(update_raw) && nrow(update_raw) > 0) {
  
  update_clean = update_raw %>% 
    mutate(
      pm2p5_cf_1_a = `pm2.5_cf_1_a`,
      pm2p5_cf_1_b = `pm2.5_cf_1_b`,
      pm2p5_diff   = pm2p5_cf_1_a - pm2p5_cf_1_b)%>%
    select(sensor_index, time_stamp, humidity, temperature, pressure,
           pm2p5_cf_1_a = `pm2.5_cf_1_a`, pm2p5_cf_1_b = `pm2.5_cf_1_b`, rssi, pm2p5_diff)
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Processed and cleaned historical data\n"
  )
  
  
  dbWriteTable(conn = con,
               name = 'Purpleair_air_history',
               value = update_clean,
               append = TRUE,
               row.names = FALSE)
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Appended historical data to air_history\n"
  )
  
  sensor_list_string <- paste(sensors_pulled_log, collapse = ", ")
  
###5. Add metadata about the new historical pulls to the trigger_pulls table
  
  trigger_log_df <- data.frame(
    pull_timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    rows_inserted     = as.integer(nrow(update_clean)),
    sensors_processed = sensor_list_string,
    stringsAsFactors  = FALSE
  )
  
  dbWriteTable(conn=con, name='Purpleair_trigger_pulls', value=trigger_log_df, append=TRUE, row.names=FALSE)
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Appended historical pull metadata to trigger_pulls table\n"
  )
  
} else { cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- No sensors required historical updates.\n") }
}

###6. Disconnect cleanly
dbDisconnect(con)
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script finished executing and disconnected cleanly.\n")




