This file contains all the code I used to query and update the CLEAR lab
air quality database during my role as CLEAR lab Database Manager during
the 2026-2027 school year. The role commenced on September 21, 2026.

    library(tidyverse)
    library(readxl)
    library(httr)
    library(jsonlite)
    library(lubridate)
    library(PurpleAirAPI)
    library(DBI)
    library(odbc)
    library(PurpleAir)


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

\#September 2026

### 9/21/26:

After orienting a fellow lab member with the updated database schema and
the high level workflow of the R scripts used to update the Purpleair,
Clarity, and Airgradient tables, the very first change I made was to
update the sensor\_activity\_status column for each sensor company, so
that it determined whether a sensor was active based on a period of time
relative to the current date instead of relative to a fixed date.

At the request of my supervisor, who was my mentor during my Summer 2026
USRE project, I investigated reported sparseness in Purpleair sensors’
historical data during 2026. I did this by seeing which sensors pulled
data during 2025 and which sensors pulled data during 2026, and then
comparing those two sets of sensors.

### 9/24/26:

My supervisor then gave me a list of 4 Loyola owned Purpleair sensors
that were not in the database, for which I downloaded metadata and
historical data and updated the respective tables.

    fields=c("date_created", "last_seen", "name", "location_type", "model", "hardware", "latitude", "longitude", "private")
    newdata <- getPurpleairSensors_debugged(apiReadKey=api_key, fields=fields) 

    #This was a version of the getPurpleairSensors function, which I had previously edited to pull additional metadata fields in order to match the schema of the Purpleair_sensor_information table (see the Week 2 section of my Summer_2026_Air_Quality_Research Github repository for details about this).

    newdata_new_sensors<-newdata%>%
    filter(sensor_index %in% c("309628", "309644", "304778", "304768"))
    newdata_new_sensors

Metadata for only 3 of the desired sensor indices was pulled from the
API (the index 304778 was not). Regardless, I appended the metadata I
could retrieve to the Purpleair\_sensor\_information table.

    dbWriteTable(
      con,
      name="Purpleair_sensor_information",
      value=newdata_new_sensors, 
      append=TRUE,
      row.names=FALSE)

    #I then attempted to download historical data for all four sensors in my supervisor's list.

    api_key<-Sys.getenv("API_KEY")
    sensor_index <- c("309628", "309644", "304778", "304768")

    start_date <- "2024-01-01"
    end_date   <- "2026-09-24"
    average <- "60"  # hourly
    fields <- c(
      "humidity",
      "temperature",
      "pressure",
      "pm2.5_cf_1_a",
      "pm2.5_cf_1_b",
      "rssi"
    )

    historical_data <- getSensorHistory(
      sensorIndex = sensor_index,
      apiReadKey  = api_key,
      startDate   = start_date,
      endDate     = end_date,
      average     = average,
      fields      = fields
    )

    table(historical_data$sensor_index)

Not only did the API’s historical data not contain readings for all 4
sensors, but the 3 sensors that did have historical readings (304768,
304778, and 309644) were a different set of 3 than the sensors that had
metadata (304768, 309628,and 309644). This means that only 2 of the 4
sensors had both historical data and metadata, and of those 2, one had
only 30 historical observations. Regardless, I updated the database with
all the historical data available at the time.

    dbWriteTable(con,
                 name="Purpleair_air_history",
                 value=historical_data, 
                 append=TRUE,
                 row.names=FALSE)

### 9/25/26:

I downloaded historical Airgradient sensor data for 7/5/26-7/20/26 (the
week with the worst air quality of the summer in the Chicago area and
the week prior to that), in order to provide the data to two graduate
students working in the lab, so they could use it for a co-location
study.

    Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY")

    Airgradient_sensor_information<-dbGetQuery(con, "SELECT * FROM Airgradient_sensor_information")

    library(httr)
    library(jsonlite)
    library(dplyr)
    token=Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY")
    location_ids <- c("190848", "190849", "190850", "190851", "190852", "190853", "190854", "190855", "190856", "190857", "190858", "190859", "190860", "190861", "190862", "190863", "190864")

    all_data <- list()

    for (id in location_ids){
      url <- paste0("https://api.airgradient.com/public/api/v1/locations/", id, "/measures/past")
      
      response <- GET(
        url,
        query = list(
          token = token,
          from = "2026-07-05",
          to   = "2026-07-12")
      )
      
      if (status_code(response) != 200) {
        warning("Request failed for sensor ", id, " with status ", status_code(response))
        next
      }
      
      json_text <- content(response, as = "text", encoding = "UTF-8")
      df <- fromJSON(json_text, flatten = TRUE)
      
      df$location_id <- id
      
      all_data[[id]] <- df
    }

    final_df_1 <- bind_rows(all_data)

    #Owing to the fact that Airgradient's API only allows a date range up to a week long for historical data requests, I had to perform two requests and then combine the resulting dataframes together.

    all_data_2 <- list()

    for (id in location_ids){
      url <- paste0("https://api.airgradient.com/public/api/v1/locations/", id, "/measures/past")
      
      response <- GET(
        url,
        query = list(
          token = token,
          from = "2026-07-13",
          to   = "2026-07-20")
      )
      
      if (status_code(response) != 200) {
        warning("Request failed for sensor ", id, " with status ", status_code(response))
        next
      }
      
      json_text <- content(response, as = "text", encoding = "UTF-8")
      df <- fromJSON(json_text, flatten = TRUE)
      
      df$location_id <- id
      
      all_data_2[[id]] <- df
    }

    final_df_2 <- bind_rows(all_data_2)

    July_5_thru_20_Airgradient_data<-bind_rows(final_df_1, final_df_2)[ ,-27] #The 27th column is simply a repeat of the locationID field

    July_5_thru_20_Airgradient_Data<-write.csv(July_5_thru_20_Airgradient_data, file="July_5_thru_20_Airgradient_Data", row.names=FALSE)

### 9/30/26: downloading all Airgradient data between 7pm on 9/28 and 7pm on 9/30 for the co-location study

    library(httr)
    library(jsonlite)
    library(dplyr)
    token=Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY")
    location_ids <- c("190848", "190849", "190850", "190851", "190852", "190853", "190854", "190855", "190856", "190857", "190858", "190859", "190860", "190861", "190862", "190863", "190864")

    all_data_9_28_thru_30 <- list()

    for (id in location_ids){
      url <- paste0("https://api.airgradient.com/public/api/v1/locations/", id, "/measures/past")
      
      response <- GET(
        url,
        query = list(
          token = token,
          from = "2026-09-27",
          to   = "2026-10-01")
      )
      
      if (status_code(response) != 200) {
        warning("Request failed for sensor ", id, " with status ", status_code(response))
        next
      }
      
      json_text <- content(response, as = "text", encoding = "UTF-8")
      df <- fromJSON(json_text, flatten = TRUE)
      
      df$location_id <- id
      
      all_data_9_28_thru_30[[id]] <- df
    }

    final_df <- bind_rows(all_data_9_28_thru_30)

    nrow(final_df)



    New_airgradient_historical_data<-final_df%>%
      select(location_id, locationName, timestamp, pm01, pm01_corrected, pm02, pm02_corrected, pm10, pm10_corrected, atmp, atmp_corrected, rhum, rhum_corrected, rco2, rco2_corrected, tvoc)

    New_airgradient_historical_data$timestamp <- as.POSIXct(
      New_airgradient_historical_data$timestamp,
      format = "%Y-%m-%dT%H:%M:%OS",
      tz = "UTC")

    max(New_airgradient_historical_data$timestamp)
    head(New_airgradient_historical_data$timestamp)
    sum(is.na(New_airgradient_historical_data$timestamp))

    New_airgradient_historical_data_filtered<-New_airgradient_historical_data%>%
    filter(timestamp <= as.POSIXct("2026-09-30 19:00:00"))

    max(New_airgradient_historical_data_filtered$timestamp)
    class(New_airgradient_historical_data_filtered$timestamp)

    cutoff_1 <- as.POSIXct(
      "2026-09-30 19:00:00",
      tz = "UTC"
    )

    cutoff_2 <- as.POSIXct(
      "2026-09-28 17:00:00",
      tz = "UTC"
    )

    New_airgradient_historical_data_filtered <-
      New_airgradient_historical_data %>%
      filter(timestamp <= cutoff_1 & timestamp >= cutoff_2)

    max(New_airgradient_historical_data_filtered$timestamp)
    min(New_airgradient_historical_data_filtered$timestamp)

    Airgradient_calibration_data<-write.csv(New_airgradient_historical_data_filtered, file="Airgradient_sensor_calibration_data.csv", row.names=FALSE)

    Airgradient_Calibration_data<-read_csv("~/Desktop/CLEAR lab database mangement 26-27/CLEAR_lab_database_work/Airgradient_sensor_calibration_data.csv")

    nrow(Airgradient_Calibration_data)

### 10/1/26: downloading historical data for four Purpleair sensors between 7pm on 9/28 and 7pm on 9/30 for the co-location study (to co locate with the Airgradient sensors)

    api_key<-Sys.getenv("PURPLE_AIR_API_KEY")

    sensor_index<-c("304768", "309628", "309644", "310423")


    start_date <- "2026-09-28"
    end_date   <- "2026-09-30"
    average <- "0" 
    fields <- c(
      "humidity",
      "temperature",
      "pressure",
      "pm2.5_cf_1_a",
      "pm2.5_cf_1_b",
      "rssi"
    )

    historical_data <- getSensorHistory(
      sensorIndex = sensor_index,
      apiReadKey  = api_key,
      startDate   = start_date,
      endDate     = end_date,
      fields      = fields,
      average     = average
    )

    nrow(historical_data)

    table(historical_data$sensor_index)

    max(historical_data$time_stamp)

    historical_data<-historical_data%>%
    filter(time_stamp >= "2026-09-28 18:58:00" & time_stamp <= "2026-09-30 19:02:00")

    Purpleair_calibration_data<-write.csv(historical_data, file="Purple_air_sensor_calibration_data.csv", row.names=FALSE)

    Purpleair_Calibration_data<-read_csv("~/Desktop/CLEAR lab database mangement 26-27/CLEAR_lab_database_work/Purple_air_sensor_calibration_data.csv")

    nrow(Purpleair_Calibration_data)

\#10/5/26: adding additional Airgradient sensors to the database based
on a file sent by a former student CLEAR lab employee several months ago

    Airgradient_sensor_information<-dbGetQuery(con, "SELECT * FROM Airgradient_sensor_information")

    library(httr)
    library(jsonlite)
    library(dplyr)
    token <- Sys.getenv("LUC_CLEAR_AIRGRADIENT_API_KEY")
    location_ids <- c("59983", "59297", "163250", "75775", "171390", "163298", "58933", "77131")

    from_time = format(Sys.time() - 864000, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    to_time   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    all_data <- list()

    for (id in location_ids){
      url <- paste0("https://api.airgradient.com/public/api/v1/locations/", id, "/measures/past")
      
      response <- GET(
        url,
        query = list(
          token = token,
          from = from_time,
          to   = to_time)
      )
      
      if (status_code(response) != 200) {
        warning("Request failed for sensor ", id, " with status ", status_code(response))
        next
      }
      
      json_text <- content(response, as = "text", encoding = "UTF-8")
      df <- fromJSON(json_text, flatten = TRUE)
      
      df$location_id <- id
      
      all_data[[id]] <- df
    }

    Airgradient_chunk_1 <- bind_rows(all_data)



    Airgradient_chunk_1

None of these sensors contained any sensor metadata or historical data
on the API. My supervisor told me to revisit this at a later point after
she had done some investigating as to why this is the case.

### 10/12/26: downloading PurpleAir data based on a list of sensor indices Dr. Whalen sent me (after updating all three databases with their respective update scripts). Some of these sensors are in the city of Chicago proper, while others are outside the city but within Cook County.

### Dr. Whalen wanted me to add all the sensors to the metadata table, but to only add historical data from the Chicago city sensors (this means marking the Cook County sensors as inactive/not pulling in Chicago in the metadata table).

    library(tidyverse)
    New_sensors_to_add<-read_csv("~/Desktop/CLEAR lab database mangement 26-27/CLEAR_lab_database_work/purple air sensors.csv")
    sensor_indices<-c()
    New_sensors_to_add<-New_sensors_to_add%>%
    filter(sensor_index != 'NA')
    for (i in New_sensors_to_add$sensor_index){
      sensor_indices<-c(sensor_indices, i)}


    fields=c("date_created", "last_seen", "name", "location_type", "model", "hardware", "latitude", "longitude", "private")
    api_key<-Sys.getenv("PURPLE_AIR_API_KEY")
    new_metadata<-getPurpleairSensors_debugged(apiReadKey = api_key, fields=fields)
    new_metadata<-new_metadata%>%
    filter(sensor_index %in% sensor_indices)
    Unknown_sensor<-New_sensors_to_add%>%
    filter(sensor_index !in c(new_metadata$sensor_index))

    new_sensor_indices_retrieved<-new_metadata$sensor_index
    print(New_sensors_to_add$sensor_index[which(!(New_sensors_to_add$sensor_index %in% new_sensor_indices_retrieved))])
    #The index 171443 was not in the downloaded metadata. I will try downloading this sensor's metadata separately.

    sensor_171443_metadata<-new_metadata%>%
    filter(sensor_index==171443)
    nrow(sensor_171443_metadata)
    #This did not return any metadata for the sensor. I will add the other 41 sensors to the database and discuss this pesky individual with Dr. Whalen.

    colnames(new_metadata)

    #Moving the 'private' column after the 'longitude' column to match the structure of the database (AI helped me here; I had not seen this method before)

    library(dplyr)
    new_metadata <- new_metadata %>% relocate(private, .after = longitude)

    new_metadata<-new_metadata%>%
    mutate(temp_date_column=as.Date(last_seen))%>%
    mutate(sensor_activity_status=case_when(
      temp_date_column>=Sys.Date()-30 ~ "Active",
      temp_date_column < Sys.Date()-30 ~ "Inactive"
    ))

    #We want the metadata for all these sensors, but we don't want the historical data for the non Chicago sensors.
    Non_Chicago_sensors<-New_sensors_to_add%>%
    filter(Location != "Chicago")%>%
    select(sensor_index)
    Non_Chicago_sensor_indices<-Non_Chicago_sensors$sensor_index

    #This means I need to set the sensor_activity_status column to Inactive for the sensor indices from the Cook County Suburbs (all of the sensors are currently Active based on their last_seen date).

    new_metadata<-new_metadata%>%
    left_join(New_sensors_to_add, by=c("sensor_index"="sensor_index"))

    new_metadata<-new_metadata%>%
    mutate(sensor_activity_status=case_when(
      Location=="Chicago" ~ "Active",
      Location=="Cook County Suburbs" ~ "Inactive (not pulling in Chicago)"
    ))

    New_metadata<-new_metadata%>%
    select(sensor_index, date_created, last_seen, name.y, location_type, model, hardware, latitude.y, longitude.y, private, sensor_activity_status)
    colnames(New_metadata)[4]="name"
    colnames(New_metadata)[8]="latitude"
    colnames(New_metadata)[9]="longitude"

    dbWriteTable(
      con,
      name="Purpleair_sensor_information",
      value=New_metadata, 
      append=TRUE,
      row.names=FALSE)

    c<-dbGetQuery(con, "SELECT * FROM Purpleair_sensor_information")

    #Now, I need to download and append the historical data for the Chicago sensors.

    Chicago_Sensors<-New_sensors_to_add%>%
    filter(Location=="Chicago")
    Chicago_sensor_indices<-Chicago_Sensors$sensor_index #There are 23 such sensors

    sensor_index=Chicago_sensor_indices

    start_date <- "2018-01-01"
    end_date   <- "2026-10-12"
    average <- "0" 
    fields <- c(
      "humidity",
      "temperature",
      "pressure",
      "pm2.5_cf_1_a",
      "pm2.5_cf_1_b",
      "rssi"
    )

    historical_data <- getSensorHistory(
      sensorIndex = sensor_index,
      apiReadKey  = api_key,
      startDate   = start_date,
      endDate     = end_date,
      fields      = fields,
      average     = average
    )

    nrow(historical_data)

    table(historical_data$sensor_index)

    max(historical_data$time_stamp)

    dbWriteTable(con,
                 name="Purpleair_air_history",
                 value=historical_data, 
                 append=TRUE,
                 row.names=FALSE)

This appended 3,025,729 rows to Purpleair\_air\_history.

### 10/18/26: investigating the number of sensors reporting throughout 2026

I investigating the trend in the number of sensors reporting data on
each day throughout the year 2026 as a time series.

    Purpleair_2026_historical_data<-dbGetQuery(con, "SELECT * FROM Purpleair_air_history WHERE time_stamp >= '2026-01-01 00:00:00'")

    Purpleair_2026_investigation_data<-Purpleair_2026_historical_data%>%
    mutate(Date=as.Date(time_stamp))%>%
    arrange(Date)

    Date_list<-as.Date(character())
    Reporting_sensors_list<-c()

    for(i in unique(Purpleair_2026_investigation_data$Date)){
      new_data<-Purpleair_2026_investigation_data%>%
      filter(Date==i)
      Date_list<-c(Date_list, i)
      print(i)
      Num_distinct_reporting_sensors<-length(unique(new_data$sensor_index))
      Reporting_sensors_list<-c(Reporting_sensors_list, Num_distinct_reporting_sensors)}

    dates_targeted_inquiry<-as.Date(character())
    for (i in Date_list){
      index=which(Date_list==i)
      if (Reporting_sensors_list[index] <= 5){
        dates_targeted_inquiry<-c(dates_targeted_inquiry, Date_list[index])}
    }

    ts_data_frame<-data.frame(Date=Date_list, Num_sensors_reporting=Reporting_sensors_list)

    #Creating the time series

    library(plotly)
    Purpleair_sensors_reporting<-plot_ly(data=ts_data_frame, x=~Date, y=~Num_sensors_reporting, type="scatter", mode="lines", col="blue")%>%
    layout(yaxis=list(range=c(0,80)))
    Purpleair_sensors_reporting

When I initially generated this plot, the line was at 1-3 sensors for
the entire period from January 1 to June 1. I suspect this is because I
initially set the sensor\_activity\_status column based on a static 30
day threshold and only recently (on 9/21/26) changed the column
definition to a moving 30 day threshold relative to the current date.

I then backfilled the sensor data that was unaccounted for prior to me
changing the column definition (going up to the current date just to be
safe).

    vec<-unique(Purpleair_2026_historical_data$sensor_index)

    sensor_index <- vec

    start_date <- "2026-01-01"
    end_date   <- "2026-10-18"
    average <- "60"  
    fields <- c(
      "humidity",
      "temperature",
      "pressure",
      "pm2.5_cf_1_a",
      "pm2.5_cf_1_b",
      "rssi"
    )

    historical_data <- getSensorHistory(
      sensorIndex = sensor_index,
      apiReadKey  = api_key,
      startDate   = start_date,
      endDate     = end_date,
      average     = average,
      fields      = fields
    )

    nrow(historical_data)

    table(historical_data$sensor_index)
    colnames(historical_data)
    historical_data<-historical_data%>%
    mutate(pm_2p5_diff=pm2.5_cf_1_a-pm2.5_cf_1_b)%>%
    mutate(pm2p5_cf_1_a=pm2.5_cf_1_a, pm2p5_cf_1_b=pm2.5_cf_1_b)

    library(dplyr)
    historical_data<-historical_data %>% 
    relocate(sensor_index, .before = time_stamp)%>%
    relocate(humidity, .before = rssi)%>%
    relocate(pm_2p5_diff, .after=pm2p5_cf_1_b)%>%
    relocate(rssi, .before=pm_2p5_diff)%>%
      
    historical_data<-historical_data%>%
    select(-pm2.5_cf_1_a, -pm2.5_cf_1_b)
    colnames(historical_data)[9]="pm2p5_diff"

I then cleaned the duplicates out of Purpleair\_air\_history.

    duplicate_historical_records<- dbGetQuery(con, 
                                    "SELECT
        sensor_index, time_stamp, COUNT(*) AS n
    FROM Purpleair_air_history
    GROUP BY sensor_index, time_stamp
    HAVING COUNT(*) > 1")

    nrow(duplicate_historical_records)

    dbExecute(con, "
    WITH duplicates AS (
        SELECT *,
               ROW_NUMBER() OVER (
                   PARTITION BY sensor_index, time_stamp
                   ORDER BY sensor_index, time_stamp) 
                   AS rn
        FROM Purpleair_air_history)
    DELETE FROM duplicates
    WHERE rn > 1")

This removed 240,242 duplicate records, which is far fewer than the
number of records added to the database.

11/9/26: Revising the air\_history table of each sensor company to
include a pull\_id column, like the trigger\_pulls tables. When creating
an ER diagram of the database using dBeaver, I realized that I could
only make connections between the air\_history tables and the
sensor\_information tables because these tables share a sensor\_index
field. The pull\_id field in the trigger\_pulls tables is not shared
with any other table, preventing me from making connections to the
trigger\_pulls tables.

    dbExecute(con, "ALTER TABLE Purpleair_air_history ADD pull_id INT") #This ensures that all current rows in the table recieve a NULL value for the pull_id column, but subsequent rows appended later will have valid integer values (guidance from Microsoft Learn)

    a<-dbGetQuery(con, "SELECT * FROM Purpleair_trigger_pulls")

    b<-dbGetQuery(con, "SELECT pull_id FROM Purpleair_air_history")
    head(b, 10)

To reflect these changes when I run the PurpleAir\_24\_hour update
script, I will modify the update script so that it does not append the
new historical data to the database until the pull\_id value from the
new row of trigger\_pulls is supplied to all the values of the new
historical dataframe. Specifically, I changed the historical
data/metadata about historical pull part of the update script from this:

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

to this:

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
      
     #Notice that I do not append the new historical data to Purpleair_air_history yet
      
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

      Max_pull_timestamp<-dbGetQuery(con, "SELECT MAX(pull_timestamp) FROM Purpleair_trigger_pulls")
      Max_pull_time_stamp<-ymd_hms(Max_pull_timestamp[1,1])
      New_pull_id<-dbGetQuery(con, "SELECT pull_id FROM Purpleair_trigger_pulls WHERE pull_timestamp=?", params=list(Max_pull_time_stamp))
    New_pull_id_integer=as.numeric(New_pull_id[1,1])
      
      ###The new part: Querying the most recent value of pull_id from PurpleAir_trigger_pulls (the value corresponding to the most recent row of historical update metadata)
      
      update_clean_with_pull_id<-update_clean%>%
        mutate(pull_id=rep(New_pull_id_integer, nrow(update_clean))) #Populating a pull_id column for the new historical dataframe containing the new value of pull_id (a one to many relationship exists between the pull_id in trigger_pulls and the pull_id in air_history)
      
        dbWriteTable(conn = con,
                   name = 'Purpleair_air_history',
                   value = update_clean_with_pull_id,
                   append = TRUE,
                   row.names = FALSE) 
        
        #I wait to append the new historical data until AFTER I append the historical pull metadata and add the pull_id column with the corresponding value of pull_id to the new historical data
        
      
    } else { cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- No sensors required historical updates.\n") }
    }

The old workflow for this part of the script:

Retrieve new historical data –&gt; Clean up historical data column names
to match database schema –&gt; Append new historical data to
Purpleair\_air\_history –&gt; obtain metadata about historical pull
–&gt; Append historical pull metadata to Purpleair\_trigger\_pulls

The new workflow:

Retrieve new historical data –&gt; Clean up historical data column names
to match database schema –&gt; obtain metadata about historical pull
–&gt; Append historical pull metadata to Purpleair\_trigger\_pulls –&gt;
query newest pull\_id from Purpleair\_trigger\_pulls –&gt; Append new
pull\_id to new historical data –&gt; Append new historical data to
Purpleair\_air\_history

I then repeated these steps for the Clarity and AirGradient sensors,
along with corresponding changes to the update scripts.

    dbExecute(con, "ALTER TABLE Clarity_air_history ADD pull_id INT")

    a<-dbGetQuery(con, "SELECT * FROM Clarity_trigger_pulls")

    b<-dbGetQuery(con, "SELECT pull_id FROM Clarity_air_history")
    head(b, 10)

    a

    dbExecute(con, "ALTER TABLE Airgradient_air_history ADD pull_id INT")

11/10/26: Downloading historical measurements from EPA sensors using the
AirNow R package, which enables data retrieval from the AirNow API

    install.packages("airnow")

First, I must instantiate the API key that was randomly generated by the
AirNow API Portal when I created a personal account.

    library(airnow)

    ## Warning: package 'airnow' was built under R version 4.4.3

    set_airnow_token(token="786FD9B7-737A-43E8-8E1D-CDFA43083F67", ask=is_interactive())

    ## ℹ AirNow API token is already set. Overriding for this session only.
    ## To use this token permanently, update `AIRNOW_API_KEY` it in your '~/.Renviron' file.

    get_airnow_token(ask=is_interactive())

    ## [1] "786FD9B7-737A-43E8-8E1D-CDFA43083F67"

Test query: retrieving 1 day of historical measurements for EPA sensors
in Chicago

    box=c(-87.94009, 41.64458, -87.52509, 42.02295)
    test_historical_data<-get_airnow_monitors(box=c(-87.94009, 41.64458, -87.52509, 42.02295), 
                                          parameters="pm25",
                                          start_time="2026-11-10 00:00:00",
                                          end_time="2026-11-11 00:00:00",
                                          monitor_type="both",
                                          data_type="both",
                                          verbose=FALSE,
                                          raw_concentrations=TRUE,
                                          clean_names=TRUE,
                                          api_key = get_airnow_token())

    packageVersion("airnow")
