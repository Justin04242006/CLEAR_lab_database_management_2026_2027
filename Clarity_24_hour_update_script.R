##Unlike the PurpleAir and AirGradient APIs, the Clarity API does not require an API key, only the corresponding URL.

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

cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script began executing.\n")

con = dbConnect(
  odbc(),
  Driver = "/opt/homebrew/lib/libmsodbcsql.18.dylib",
  Server = "resdbsprdhs02.adms.luc.edu",
  Database = "clearlab_purpleair",
  UID = Sys.getenv("DB_USERNAME"),
  PWD = Sys.getenv("DB_PASSWORD"),
  Encrypt = "yes",
  TrustServerCertificate = "yes"
)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Connected to database\n"
)

Sys.getenv("DB_USERNAME")
Sys.getenv("DB_PASSWORD")

### 2. Fetch Database Active Sensors
Clarity_active_sensors <- dbGetQuery(con, "SELECT datasourceid, last_seen FROM Clarity_sensor_information WHERE sensor_activity_status= 'Active'")
sensoridsvec <- Clarity_active_sensors$datasourceid

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Retrieved active sensors\n"
)


### 3.Fetch Live API Timestamps

#I cannot do this step with Clarity sensors because the last_seen variable does not natively exist in the Clarity API like it does in the PurpleAir API.


### 4. Fetch and Process Historical Data from the most recent time in the database until the current system time


if (length(sensoridsvec) > 0) { 
  
  latest_time <- DBI::dbGetQuery(
    con,
   "SELECT MAX(time) AS latest_time
   FROM Clarity_air_history"
  )$latest_time
  
  latest_time <- format(
    as.POSIXct(latest_time, tz = "UTC"),
    "%Y-%m-%dT%H:%M:%S"
  )
  
  current_time <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )
  
  sensor_filter <- paste0(
    "datasourceid IN ('",
    paste(sensoridsvec, collapse = "', '"),
    "')"
  )
  
  where <- URLencode(
    paste0(
      "time > '", latest_time,
      "' AND time <= '", current_time,
      "' AND ", 
      sensor_filter
    ),
    reserved = TRUE
  )
  
  limit <- 50000
  offset <- 0
  all_data <- list()
  
  repeat {
    
    url <- paste0(
      "https://data.cityofchicago.org/resource/xfya-dxtq.json?",
      "$where=", where,
      "&$order=time%20ASC",
      "&$limit=", limit,
      "&$offset=", as.integer(offset)
    )
    
    response <- GET(url, timeout(3600))
    
    txt <- content(response, as = "text", encoding = "UTF-8")
    
    if (status_code(response) != 200) {
      cat(txt)
      break
    }
    
    chunk <- jsonlite::fromJSON(txt, flatten = TRUE)
    
    if (nrow(chunk) == 0)
      break
    
    all_data[[length(all_data) + 1]] <- chunk
    
    message("Downloaded ", offset + nrow(chunk), " rows")
    
    if (nrow(chunk) < limit)
      break
    
    offset <- offset + limit
  }
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Downloaded new historical data\n"
  )
  
  New_historical_data <- bind_rows(all_data)
  status_code(response)
  txt <- content(response, as = "text", encoding = "UTF-8")
  cat(txt)
  
  New_historical_data$time <- as.POSIXct(
    New_historical_data$time,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  ) }

New_historical_data<-New_historical_data%>%
select(c(datasourceid, sensor_name, time, record_id, relhumidinternalindividual, no2concindividual_raw, no2concindividual_value, pm2_5concmassindividual_raw, pm2_5concmassindividual_value, pm2_5concnumindividual_raw, pm2_5concnumindividual_value, temperatureinternalindividual))

sensors_pulled_log<-c()
for (i in sensoridsvec) {
  sensors_pulled_log <- c(sensors_pulled_log, as.character(i)) 
  sensor_list_string <- paste(sensors_pulled_log, collapse = ", ")}


#5. Updating sensor metadata with new last_seen timestamps based on the updated air_history table

clarity_sensor_information<-dbGetQuery(con, "SELECT * FROM Clarity_sensor_information")
clarity_air_history<-dbGetQuery(con, "SELECT datasourceid, time FROM Clarity_air_history")
max(clarity_air_history$time)

current_last_seen_times<-clarity_air_history%>%
group_by(datasourceid)%>%
summarize(most_recent_timestamp=max(time))
  
  for (i in clarity_sensor_information$datasourceid) {
    query1 <- dbGetQuery(con, "SELECT last_seen FROM Clarity_sensor_information WHERE datasourceid = ?", params = list(i)) #extracts the currently documented last_seen timestamp for each sensor and assigns it as db_last_seen
    db_last_seen <- if(nrow(query1) > 0) query1$last_seen else NA
    
    api_row <- current_last_seen_times %>% 
      filter(datasourceid == i)%>%
      select(most_recent_timestamp)
    
    if (nrow(api_row) > 0) {
      api_last_seen <- api_row$most_recent_timestamp} #assigns the current last_seen times for each sensor based on the updated air_history table
      
      if (is.na(db_last_seen) || db_last_seen < api_last_seen) {
        
        position=which(clarity_sensor_information$datasourceid == i) 
        j=current_last_seen_times$datasourceid[position]
        
        query2 <- "UPDATE Clarity_sensor_information SET last_seen = ? WHERE datasourceid = ?"
        
        dbExecute(con, query2, params = list(as.character(api_last_seen), j))
        

      }
  }


#6. Update trigger_pulls table 

trigger_log_df <- data.frame(
  pull_timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  rows_inserted     = as.integer(nrow(New_historical_data)),
  sensors_processed = sensor_list_string,
  stringsAsFactors  = FALSE
)

dbWriteTable(conn=con, name='Clarity_trigger_pulls', value=trigger_log_df, append=TRUE, row.names=FALSE)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Appended historical pull metadata to trigger_pulls table\n"
)

New_pull_id<-dbGetQuery(con, "SELECT TOP 1 pull_id FROM Clarity_trigger_pulls ORDER BY pull_id DESC")

New_pull_id_integer<-as.numeric(New_pull_id$pull_id[1])

update_clean_with_pull_id<-New_historical_data%>%
  mutate(pull_id=New_pull_id_integer)%>%
  distinct(datasourceid, time, .keep_all=TRUE)

if(nrow(update_clean_with_pull_id) > 0){
  dbWriteTable(
  conn      = con,
  name      = "Clarity_air_history",
  value     = update_clean_with_pull_id,
  append    = TRUE,         
  row.names = FALSE
)
  
 cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Successfully appended",
    nrow(update_clean_with_pull_id),
    "new historical observations\n"
  )
  
} else {
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- No new observations to append.\n"
  )
  
} 

#7. Disconnect cleanly

dbDisconnect(con)
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script finished executing and disconnected cleanly.\n")


