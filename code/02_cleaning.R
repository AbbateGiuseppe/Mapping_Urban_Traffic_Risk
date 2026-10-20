#'######################################'#
#'########### CLEANING ############'#
#'######################################'#



source("01_preprocessing.R")

source("utils/packages.R")



# 1) INITIAL DATA -----------------------------------------------------------

# Check the dataset size before cleaning.
cat("Initial rows = ", nrow(data_sf_df), "\n")
cat("Initial columns = ", ncol(data_sf_df), "\n")



# 2) CLEANING (across all years)  ------------------------------------------------------------

# Remove rows missing spatial information (geometry variable)
# Remove rows missing temporal information (datehour, year, month, day)
# For missing values in other variables, replace with Unknown (do not drop the whole row).



## Spatial data cleaning ---------------------------------------------------
data_clean_sf <- data_sf_df[!st_is_empty(data_sf_df$geometry), ]
cat("Number of rows after removing invalid coordinates = ", nrow(data_clean_sf), "\n")

# All rows appear to be populated



## Temporal data cleaning -----------------------------------------------

temporal_cols <- c("DateHour", "Year", "Month", "Day", "Hour")
valid_temporal_rows <- complete.cases(as.data.frame(data_clean_sf)[, temporal_cols])
data_clean_sf <- data_clean_sf[valid_temporal_rows, ]
cat("Rows after removing missing temporal data = ", nrow(data_clean_sf), "\n\n")


## Handling missing values ----------------------------

# Check for missing values in other variables
# Since these are not core variables like position and date (already checked),
# do not drop the whole row, replace with Unknown instead.

colSums(sapply(st_drop_geometry(data_clean_sf), as.character) == "", na.rm = TRUE)

for (col in names(data_clean_sf)) {
  
  # Only modify text or categorical (factor) columns.
  # Numeric variables (date, hour...) and geometry are left untouched.
  
  if (is.character(data_clean_sf[[col]]) || is.factor(data_clean_sf[[col]])) 
  {
    # Replace NA with "Unknown"
    data_clean_sf[[col]][is.na(data_clean_sf[[col]])] <- "Unknown"
    
    # Replace empty strings or " " with "Unknown"
    data_clean_sf[[col]][data_clean_sf[[col]] %in% c("", " ")] <- "Unknown"
  }
}


# Final check: there are no more blanks.
colSums(sapply(st_drop_geometry(data_clean_sf), as.character) == "", na.rm = TRUE)



## Anomalous value correction -----------------------------------------------

# Weather contains an anomalous value ("Orizzontale"), which makes no sense as a weather condition.
# Treat it as unknown.

data_clean_sf <- data_clean_sf %>%
  mutate(
    Weather = ifelse(Weather == "Orizzontale", "Unknown", Weather)
  )



# 3) MACRO CATEGORIES -----------------

# Create a few simpler variables to use in the analyses:
# - time of day
# - accident category
# - injury count category
# - weather


## Time of day ----------------------------------------------------------

# Thresholds change depending on the season. 

data_clean_sf <- data_clean_sf %>%
  mutate(
    hour_num = as.numeric(substr(Hour, 1, 2)),
    DayPeriod = case_when(
      Month %in% c("Nov", "Dec", "Jan", "Feb") ~ case_when(
        hour_num >= 8  & hour_num < 12 ~ "Morning",
        hour_num >= 12 & hour_num < 16 ~ "Afternoon",
        hour_num >= 16 & hour_num < 20 ~ "Evening",
        TRUE                           ~ "Night"
      ),
      Month %in% c("Jun", "Jul", "Aug") ~ case_when(
        hour_num >= 6  & hour_num < 13 ~ "Morning",
        hour_num >= 13 & hour_num < 20 ~ "Afternoon",
        hour_num >= 20 & hour_num < 23 ~ "Evening",
        TRUE                           ~ "Night"
      ),
      TRUE ~ case_when(
        hour_num >= 7  & hour_num < 13 ~ "Morning",
        hour_num >= 13 & hour_num < 18 ~ "Afternoon",
        hour_num >= 18 & hour_num < 21 ~ "Evening",
        TRUE                           ~ "Night"
      )
    )
  ) %>% select(-hour_num)




## Accident type ----------------------------------------------------------
# Groupings:
# - rear-end and multi-vehicle rear-end 
# - run-off-road and rollover without impact 

data_clean_sf <- data_clean_sf %>%
  mutate(
    AccidentType = stringr::str_trim(AccidentType),
    AccidentCategory = case_when(
      AccidentType == "Scontro laterale fra veicoli in marcia" ~ "Side Collision",
      AccidentType == "Scontro frontale/laterale DX fra veicoli in marcia" ~ "Front/Right-Side",
      AccidentType == "Scontro frontale/laterale SX fra veicoli in marcia" ~ "Front/Left-Side",
      AccidentType == "Scontro frontale fra veicoli in marcia" ~ "Frontal Collision",
      AccidentType %in% c("Tamponamento", "Tamponamento Multiplo") ~ "Rear-End",
      AccidentType == "Investimento di pedone" ~ "Pedestrian Hit",
      stringr::str_detect(AccidentType, "veicolo fermo|veicoli fermi|in sosta|in arresto|arresto") ~ "Hit Parked Vehicle",
      stringr::str_detect(AccidentType, "ostacolo fisso|ostacolo accidentale|buche|animale") ~ "Hit Obstacle/Animal",
      stringr::str_detect(AccidentType, "frenata improvvisa|caduta del veicolo") ~ "Sudden Braking/Fall",
      AccidentType %in% c("Fuoriuscita dalla sede stradale", "Ribaltamento senza urto contro ostacolo fisso") ~ "Run-off/Rollover",
      TRUE ~ "Other"
    )
  )



## Accident severity -------------------------------------------------------

data_clean_sf <- data_clean_sf %>%
  mutate(
    injuries_num = as.numeric(as.character(Injuries)),
    InjuriesCategory = case_when(
      injuries_num == 0 ~ "0",
      injuries_num == 1 ~ "1",
      injuries_num == 2 ~ "2",
      injuries_num >= 3 & injuries_num <= 5 ~ "3-5",
      injuries_num > 5 ~ "6 or more",
      TRUE ~ "Unknown"
    ),
    InjuriesCategory = factor(InjuriesCategory, levels = c("0", "1", "2", "3-5", "6 or more", "Unknown"))
  ) %>% select(-injuries_num)




## Weather -------------------------------------------------------------------

data_clean_sf <- data_clean_sf %>%
  dplyr::mutate(
    Weather_simple = dplyr::case_when(
      Weather %in% c("Sereno", "Nuvoloso") ~ "Clear",
      Weather == "Pioggia in atto" ~ "Rain",
      Weather %in% c("Nebbia", "Foschia") ~ "Fog",
      Weather %in% c("Grandine in atto", "Nevicata in atto") ~ "Snow/Hail",
      Weather %in% c("Vento forte", "Sole radente") ~ "Extreme conditions",
      Weather == "Unknown" ~ "Unknown",
      TRUE ~ "Other"
    )
  )



# 4) CHOICE OF YEARS OF INTEREST  --------------------------------------------------------


# Check how much data is available for each year.
cat("\nAccidents by year:\n")
print(table(data_clean_sf$Year))

# Change here to use different target years. 
# Note: spatial data is sparse before 2014
# Note: 2022 is incomplete

target_years <- c(2010, 2022)


# Trim the cleaned dataset to the chosen years

data_target_sf <- data_clean_sf %>%
  filter(Year >= target_years[1] & Year <= target_years[2])
cat("\nRows in target years dataset =", nrow(data_target_sf), "\n")


# 5) SUBCATEGORY CREATION  ----------------------------

# Build a few groups useful for specific analyses:
# - accidents involving pedestrians
# - accidents not involving pedestrians
# - accidents on public holidays
# - accidents on working days
# from the CLEANED DATASET at the TARGET YEARS.


## Pedestrians and vehicles --------------------------------------------------------

# "pedestrian" includes both cases flagged in the Pedestrians variable
# and cases where the accident type is explicitly a pedestrian hit.

data_pedestrians_sf <- data_target_sf[which(data_target_sf$Pedestrians == "true" |
                                              data_target_sf$AccidentType == "Investimento di pedone"),]


data_vehicles_sf <- data_target_sf[which(data_target_sf$Pedestrians == "false" &
                                           data_target_sf$AccidentType != "Investimento di pedone"),]




## Workdays and holidays ------------------------------------------------

# 3B. Workdays vs Holidays

idx_holidays <- as.numeric(filterHolidays(data = data_target_sf,
                                          from = target_years[1],
                                          to = target_years[2]))

idx_workdays <- as.numeric(filterWorkdays(data = data_target_sf,
                                          from = target_years[1],
                                          to = target_years[2]))

data_holidays_sf <- data_target_sf[idx_holidays,]
data_workdays_sf <- data_target_sf[idx_workdays,]



# 6) PROJECTION OF POINTS ONTO THE NETWORK ------------------------------------------
# Taken from the original preprocessing.R.


# Locations of Road Accidents (Projection onto the Road Network)

# Holidays
locations_holidays <- st_coordinates(data_holidays_sf)
locations_holidays <- fdaPDE::projection.points.1.5D(mesh, locations_holidays)
locations_holidays <- data.frame(longitude = locations_holidays[,1], latitude = locations_holidays[,2])


# Workdays
locations_workdays <- st_coordinates(data_workdays_sf)
locations_workdays <- fdaPDE::projection.points.1.5D(mesh, locations_workdays)
locations_workdays <- data.frame(longitude = locations_workdays[,1], latitude = locations_workdays[,2])



# 7) CONVERTING TIME OF DAY TO NORMALIZED TIME ----------------------------
# Taken from the original preprocessing.R.

# Convert the time of day to a number between 0 and 1:
# 0 = start of day, 1 = end of day.

# Times of Road Accidents

# Holidays
hours <- as.numeric(format(strptime(data_holidays_sf$Hour, format = "%H:%M"), "%H")) * 60
minutes <- as.numeric(format(strptime(data_holidays_sf$Hour, format = "%H:%M"), "%M"))
times_holidays <- (hours + minutes) / (24 * 60)

data_holidays <- data.frame(lon = locations_holidays[,1],
                            lat = locations_holidays[,2],
                            times = times_holidays)

# Workdays
hours <- as.numeric(format(strptime(data_workdays_sf$Hour, format = "%H:%M"), "%H")) * 60
minutes <- as.numeric(format(strptime(data_workdays_sf$Hour, format = "%H:%M"), "%M"))
times_workdays <- (hours + minutes) / (24 * 60)

data_workdays <- data.frame(lon = locations_workdays[,1],
                            lat = locations_workdays[,2],
                            times = times_workdays)

# Note:
# - data_holidays_sf and data_workdays_sf contain all the original variables
# - data_holidays and data_workdays contain only coordinates + normalized time



# 8) FINAL SAVE ------------------------------------------------------
# Save all useful objects into a single file,
# so downstream scripts don't need to redo the whole cleaning step (saves time).

file_name <- "data/Rome/Rome_dataset_ready.RData"

save(data_clean_sf, 
     data_target_sf, 
     data_pedestrians_sf, 
     data_vehicles_sf, 
     data_holidays_sf, 
     data_workdays_sf,
     data_holidays,      
     data_workdays,      
     roads,
     mesh, mesh_sf, adm_bdy,
     file = file_name)
