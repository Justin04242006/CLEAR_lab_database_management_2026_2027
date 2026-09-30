This file contains all the code I used to query and update the CLEAR lab
air quality database during my role as CLEAR lab Database Manager during
the 2026-2027 school year. The role commenced on September 21, 2026.

    library(tidyverse)

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'tibble' was built under R version 4.4.3

    ## Warning: package 'tidyr' was built under R version 4.4.3

    ## Warning: package 'readr' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

    ## Warning: package 'dplyr' was built under R version 4.4.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(readxl)
    library(httr)
    library(jsonlite)

    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

    library(lubridate)
    library(PurpleAirAPI)
    library(DBI)

    ## Warning: package 'DBI' was built under R version 4.4.3

    library(odbc)

    ## Warning: package 'odbc' was built under R version 4.4.3

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

# September 2026

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

    ## [1] "3dee03db-d2a7-443c-b8ba-85d2e7ebd8b1"

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

    head(July_5_thru_20_Airgradient_data)

    ##   locationId locationName pm01 pm02 pm10 pm01_corrected pm02_corrected
    ## 1     190848  AG Indoor 1  4.5  9.5 11.3            4.5            9.1
    ## 2     190848  AG Indoor 1  4.9  8.1  9.4            4.9            9.3
    ## 3     190848  AG Indoor 1  5.3  9.3 11.4            5.3            9.4
    ## 4     190848  AG Indoor 1  5.1  9.5 10.9            5.1            9.3
    ## 5     190848  AG Indoor 1  4.7  9.1 10.1            4.7            9.3
    ## 6     190848  AG Indoor 1  4.8  8.3 10.7            4.8            9.2
    ##   pm10_corrected pm003Count atmp rhum rco2 atmp_corrected rhum_corrected
    ## 1           11.3        569 26.6   68  424           26.6             68
    ## 2            9.4        584 26.6   68  422           26.6             68
    ## 3           11.4        590 26.6   68  423           26.6             68
    ## 4           10.9        578 26.6   68  423           26.6             68
    ## 5           10.1        580 26.6   69  423           26.6             69
    ## 6           10.7        577 26.6   69  423           26.6             69
    ##   rco2_corrected tvoc wifi                timestamp     serialno     model
    ## 1            424 86.4  -84 2026-07-05T00:00:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ## 2            422 83.8  -84 2026-07-05T00:05:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ## 3            423 82.0  -83 2026-07-05T00:10:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ## 4            423 79.8  -84 2026-07-05T00:15:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ## 5            423 78.5  -84 2026-07-05T00:20:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ## 6            423 77.9  -84 2026-07-05T00:25:00.000Z 3cdc75bc1e88 I-9PSL-DE
    ##   firmwareVersion tvocIndex noxIndex batteryVoltage panelVoltage datapoints
    ## 1           3.7.0        92        1             NA           NA          5
    ## 2           3.7.0        89        1             NA           NA          5
    ## 3           3.7.0        87        1             NA           NA          5
    ## 4           3.7.0        85        1             NA           NA          5
    ## 5           3.7.0        83        1             NA           NA          5
    ## 6           3.7.0        83        1             NA           NA          5

### 9/30/26: downloading all Airgradient data between 7pm on 9/28 and 7pm on 9/30 for the co-location study
