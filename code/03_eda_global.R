#'######################################'#
#'########### EXPLORATORY DATA ANALYSIS ############'#
#'######################################'#

# Global exploratory analysis.


# 1) SETUP -------------------------------------------------------------------

graphics.off()
rm(list = ls())

# Load the file created in the cleaning step. Avoids re-running the whole 02_cleaning.R file.
load("data/Rome/Rome_dataset_ready.RData")

if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi")

setwd(dirname(getActiveDocumentContext()$path))

# source("utils/packages.R")

## Variable preparation  ------------------------------------------------------


# Use the correct (non-alphabetical) order for plotting
data_target_sf <- data_target_sf %>%
  mutate(
    Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    DayPeriod = factor(DayPeriod, levels = c("Morning", "Afternoon", "Evening", "Night")),
    Weather_simple = factor(Weather_simple,
      levels = c("Clear", "Rain", "Fog", "Snow/Hail", "Extreme conditions", "Other", "Unknown")
    )
  ) 

# Get the minimum and maximum years in the target dataset.
# Only used for plot titles.

min_year <- min(data_target_sf$Year, na.rm = TRUE)
max_year <- max(data_target_sf$Year, na.rm = TRUE)
year_title <- paste0("(", min_year, "-", max_year, ")")


# 2) TEMPORAL ANALYSIS --------------------------------------------
# Look at how accidents are distributed:
# - by year
# - by month
# - by time of day


## Yearly trend ------------------------------------------------------------

cat("\n--- Yearly Accident Frequencies ---\n")
print(table(data_target_sf$Year))

yearly_counts <- data_target_sf %>%
  st_drop_geometry() %>%
  count(Year)

ggplot(yearly_counts, aes(x = Year, y = n)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(title = paste("Yearly Trend of Traffic Accidents", year_title),
       x = "Year", 
       y = "Total Accidents")



## Monthly trend -------------------------------------------------

cat("\n--- Monthly Accident Frequencies ---\n")
print(table(data_target_sf$Month))

ggplot(data_target_sf, aes(x = Month)) +
  geom_bar(fill = "coral", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = paste("Monthly Distribution of Accidents",year_title),
       x = "Month", 
       y = "Accident Count")




## Trend by time of day --------------------------------------------------------

cat("\n--- Time of Day Frequencies ---\n")
print(table(data_target_sf$DayPeriod))

data_target_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(x = DayPeriod)) +
  geom_bar(fill = "seagreen", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = paste("Accidents by Time of Day", year_title),
       x = "Time Period", 
       y = "Accident Count")


# ACCIDENT CHARACTERISTICS -------------------------------------
# Look at a few key descriptive variables:
# - accident category
# - simplified weather
# - severity

# (stop including the year in the title from here on)

## Accident category -------------------------------------------------------

cat("\n--- Accident Category Frequencies ---\n")
print(table(data_target_sf$AccidentCategory))

data_target_sf %>%
  st_drop_geometry() %>%
  count(AccidentCategory) %>% 
  ggplot(aes(x = reorder(AccidentCategory, n), y = n)) + 
  geom_col(fill = "orange", color = "black", alpha = 0.8) + 
  coord_flip() + 
  theme_minimal() +
  labs(title = "Most Frequent Accident Types",
       x = "Accident Category", 
       y = "Count")


## Weather -----------------------------------------------------------------

# Use the simplified weather variable created during cleaning.

cat("\n--- Simplified Weather Condition Frequencies ---\n")
print(table(data_target_sf$Weather))

data_target_sf %>%
  st_drop_geometry() %>%
  count(Weather_simple) %>% 
  ggplot(aes(x = reorder(Weather_simple, n), y = n)) +
  geom_col(fill = "lightblue", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Accidents by Weather Conditions",
       x = "Weather", 
       y = "Count")


# Normalizing weather conditions (INCOMPLETE VERSION)

# Issue: ideally we'd have a proper Rome weather dataset.
# For now, average values were taken from Wikipedia (Rome Fiumicino weather station, 1991-2020).

# With this data we can only use two broader categories:
# - Good weather (no rain? 292 days)
# - Bad weather (rain, 73 days)

# Using fixed years, 8 years (2014-2022)


cat("\n--- Normalizing Weather by Macro-Categories ---\n")

# Group into just Good Weather and Adverse Weather
weather_macro_df <- data_target_sf %>%
  st_drop_geometry() %>%
  filter(Weather_simple != "Unknown") %>%
  mutate(
    Weather_Macro = case_when(
      Weather_simple== "Clear" ~ "Good Weather",
      TRUE ~ "Adverse Weather" 
    )
  )

weather_exposure_macro <- data.frame(
  Weather_Macro = c("Good Weather", "Adverse Weather"),
  estimated_days = c(292, 73) 
)


weather_macro_normalized <- weather_macro_df %>%
  count(Weather_Macro) %>%
  left_join(weather_exposure_macro, by = "Weather_Macro") %>%
  mutate(
    risk_index = n / (estimated_days * 8) 
  )


ggplot(weather_macro_normalized, aes(x = Weather_Macro, y = risk_index, fill = Weather_Macro)) +
  geom_col(color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Good Weather" = "gold", "Adverse Weather" = "steelblue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = paste("Normalized accidents by weather condition"),
       x = "Weather Macro-Category", 
       y = "Average Accidents per Day")



## Accident severity ----------------------------------------------------------------

cat("\n--- Accident Severity (Injuries) Frequencies ---\n")
print(table(data_target_sf$InjuriesCategory))

data_target_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(x = InjuriesCategory)) +
  geom_bar(fill = "firebrick", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Accident Severity",
       x = "Number of Injuries (Category)", 
       y = "Count")
