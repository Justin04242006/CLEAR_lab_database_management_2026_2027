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

### 2. Fetch Database Active Sensors
Airgradient_active_sensors <- dbGetQuery(con, "SELECT locationID, last_seen FROM Airgradient_sensor_information WHERE sensor_activity_status= 'Active'")
sensoridsvec <- Airgradient_active_sensors$locationID
sensoridsvec
cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Retrieved active sensors\n"
)

###3. Fetch Live API Timestamps
#I cannot do this step with AirGradient sensors because the last_seen variable does not natively exist in the Clarity API like it does in the PurpleAir API.


### 4. Fetch and Process Historical Data from the most recent time in the database until the current system time

#Wrap historical steps in an IF block to prevent errors if 0 sensors need updates

latest_time <- DBI::dbGetQuery(
  con,
  "SELECT MAX(timestamp) AS latest_time
   FROM Airgradient_air_history"
)$latest_time

latest_time <- format(
  as.POSIXct(latest_time, tz = "UTC") +1,
  "%Y-%m-%dT%H:%M:%S"
)

current_time <- format(
  Sys.time(),
  "%Y-%m-%dT%H:%M:%S",
  tz = "UTC"
)

all_data <- list()

for (i in sensoridsvec){
  url <- paste0("https://api.airgradient.com/public/api/v1/locations/", i, "/measures/past")
  
  response <- GET(
    url,
    query = list(
      token = Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY"),
      from = latest_time,
      to   = current_time)
  )
  
  if (status_code(response) != 200) {
    warning("Request failed for sensor ", i, " with status ", status_code(response))
    next
  }
  
  json_text <- content(response, as = "text", encoding = "UTF-8")
  df <- fromJSON(json_text, flatten = TRUE)
  
  df$location_id <- i
  
  all_data[[i]] <- df
}

final_df <- bind_rows(all_data)

all_data <- list()

for (i in sensoridsvec) {
  
  # Start at the latest timestamp currently in the database
  chunk_start <- as.POSIXct(latest_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # End at the current system time
  final_end <- as.POSIXct(current_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  while (chunk_start < final_end) {
    
    # Request one week at a time
    chunk_end <- min(
      chunk_start + 7 * 24 * 60 * 60,
      final_end
    )
    
    from_time <- format(
      chunk_start,
      "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )
    
    to_time <- format(
      chunk_end,
      "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )
    
    url <- paste0(
      "https://api.airgradient.com/public/api/v1/locations/",
      i,
      "/measures/past"
    )
    
    response <- GET(
      url,
      query = list(
        token = Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY"),
        from = from_time,
        to = to_time
      )
    )
    
    if (status_code(response) != 200) {
      warning(
        "Request failed for sensor ", i,
        " from ", from_time,
        " to ", to_time,
        " with status ", status_code(response)
      )
      
      # Move to the next chunk
      chunk_start <- chunk_end
      next
    }
    
    json_text <- content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
    
    df <- fromJSON(
      json_text,
      flatten = TRUE
    )
    
    # Add the sensor ID to the API data
    if (length(df) > 0) {
      df$locationID <- i
      all_data[[length(all_data) + 1]] <- df
    }
    
    # Move to the next week
    chunk_start <- chunk_end
  }
}
  final_df <- bind_rows(all_data)

New_airgradient_historical_data<-final_df%>%
select(locationID, locationName, timestamp, pm01, pm01_corrected, pm02, pm02_corrected, pm10, pm10_corrected, atmp, atmp_corrected, rhum, rhum_corrected, rco2, rco2_corrected, tvoc)

New_airgradient_historical_data$timestamp <- as.POSIXct(
  New_airgradient_historical_data$timestamp,
  format = "%Y-%m-%dT%H:%M:%OS",
  tz = "UTC")

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Cleaned historical data\n"
)


sensors_pulled_log<-c()
for (i in sensoridsvec) {
  sensors_pulled_log <- c(sensors_pulled_log, as.character(i)) 
  sensor_list_string <- paste(sensors_pulled_log, collapse = ", ")}

#5. Updating sensor metadata with new last_seen timestamps based on the updated air_history table

Airgradient_sensor_information<-dbGetQuery(con, "SELECT * FROM Airgradient_sensor_information")
Airgradient_air_history<-dbGetQuery(con, "SELECT locationID, timestamp FROM Airgradient_air_history")

current_last_seen_times<-Airgradient_air_history%>%
  group_by(locationID)%>%
  summarize(most_recent_timestamp=max(timestamp))

for (i in Airgradient_sensor_information$locationID) {
  query1 <- dbGetQuery(con, "SELECT last_seen FROM Airgradient_sensor_information WHERE locationID = ?", params = list(i)) #extracts the currently documented last_seen timestamp for each sensor and assigns it as db_last_seen
  db_last_seen <- if(nrow(query1) > 0) query1$last_seen else NA
  api_row <- current_last_seen_times %>% 
    filter(locationID == i)%>%
    select(most_recent_timestamp)
  
  if (nrow(api_row) > 0) {
    api_last_seen <- api_row$most_recent_timestamp} #assigns the current last_seen times for each sensor based on the updated air_history table
  
  if (is.na(db_last_seen) || db_last_seen < api_last_seen) {
    
    position=which(Airgradient_sensor_information$locationID == i) 
    j=current_last_seen_times$locationID[position]
    
    query2 <- "UPDATE Airgradient_sensor_information SET last_seen = ? WHERE locationID = ?"
    
    dbExecute(con, query2, params = list(as.character(api_last_seen), j))
    
  }
}

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Updated sensor metadata\n"
)

#6. Update trigger_pulls table 

trigger_log_df <- data.frame(
  pull_timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  rows_inserted     = as.integer(nrow(New_airgradient_historical_data)),
  sensors_processed = sensor_list_string,
  stringsAsFactors  = FALSE
)

dbWriteTable(conn=con, name='Airgradient_trigger_pulls', value=trigger_log_df, append=TRUE, row.names=FALSE)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Appended historical pull metadata to trigger_pulls table\n"
)

New_pull_id<-dbGetQuery(con, "SELECT TOP 1 pull_id FROM Airgradient_trigger_pulls ORDER BY pull_id DESC")

New_pull_id_integer<-as.numeric(New_pull_id$pull_id[1])

update_clean_with_pull_id<-New_airgradient_historical_data%>%
  mutate(pull_id=New_pull_id_integer)%>%
  distinct(locationID, timestamp, .keep_all=TRUE)


if (nrow(update_clean_with_pull_id) > 0){
  dbWriteTable(conn=con,
               name='Airgradient_air_history',
               value=update_clean_with_pull_id,
               append=TRUE,
               overwrite=FALSE,
               row.names=FALSE)

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

Sys.setenv("PURPLE_AIR_API_KEY"="F38F115A-78CA-11EE-A8AF-42010A80000A")
Sys.setenv("LUC_CLEAR_AIRGRADIENT_API_KEY"="3dee03db-d2a7-443c-b8ba-85d2e7ebd8b1")
