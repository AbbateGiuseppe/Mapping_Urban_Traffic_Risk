load("data/Rome/Base_Esplorazione.RData")

# ============================================================
#  WEATHER: rainy days for the "Rain" dimension of the GLMM
#  SINGLE source: meteo_roma_completo.csv (daily, Rome)
#  Used to:
#    (a) flag every ACCIDENT as rain/dry via a join on DATE
#        -> numerator (accidents per condition)
#    (b) count the rainy/dry DAYS per year-month
#        -> denominator (exposure of the weather condition)
#  Using a SINGLE source guarantees that the numerator and denominator
#  share the same definition of "rain".
# ============================================================

library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(readxl)

# ------------------------------------------------------------
# 1. Read the weather data and flag every day as rain / dry
#    "rain" = the FENOMENI column contains the word "piogg"
#    (captures "pioggia", "pioggia temporale", "pioggia nebbia", etc.)
# ------------------------------------------------------------
meteo <- read_excel("meteo_roma_completo.csv.xlsx")

# what the DATA column actually contains
head(meteo$DATA, 20)
class(meteo$DATA)
str(meteo$DATA)
# adapt the names if different: DATA (dd/mm/yyyy), FENOMENI
meteo <- meteo %>%
  mutate(
    date    = as_date(DATA),                   # DATA is already POSIXct -> Date
    rain_day = if_else(
      str_detect(str_to_lower(coalesce(FENOMENI, "")), "piogg"),
      "Rain", "Dry"
    )
  )

# quick check
cat("Total days:", nrow(meteo),
    "| rainy:", sum(meteo$rain_day == "Rain"),
    "| dry:", sum(meteo$rain_day == "Dry"),
    "| NA dates:", sum(is.na(meteo$date)), "\n")

# ------------------------------------------------------------
# 2. DENOMINATOR: number of rainy/dry days per year-month
#    (this goes into the model's exposure/offset)
# ------------------------------------------------------------
giorni_meteo <- meteo %>%
  filter(!is.na(date)) %>%
  mutate(Year = year(date), month_num = month(date)) %>%
  count(Year, month_num, rain_day, name = "n_giorni_cond")

# in wide form: rainy and dry days side by side per year-month
giorni_wide <- giorni_meteo %>%
  tidyr::pivot_wider(names_from = rain_day, values_from = n_giorni_cond,
                     values_fill = 0) %>%
  rename(giorni_rain = Rain, giorni_dry = Dry)
print(head(giorni_wide))

# ------------------------------------------------------------
# 3. NUMERATOR: flag every ACCIDENT as rain/dry
#    join by DATE with the weather source (single, consistent source)
# ------------------------------------------------------------
# adapt the name of the accident date column (e.g. DateHour or DATA).
# Here it is assumed that dataset_quartieri_sf has a parseable date.
acc <- dataset_quartieri_sf %>%
  sf::st_drop_geometry() %>%
  mutate(date = as_date(DateHour)) %>%       # <-- adapt: DateHour / DATA
  left_join(meteo %>% select(date, rain_day), by = "date")

# agreement with the pre-existing flag (if you want to check it):
# table(acc_meteo$rain_day, acc_meteo$Weather_simple, useNA = "ifany")

cat("\nAccidents flagged - rain:",
    sum(acc$rain_day == "Rain", na.rm = TRUE),
    "| dry:", sum(acc$rain_day == "Dry", na.rm = TRUE),
    "| no weather data:", sum(is.na(acc$rain_day)), "\n")


acc <- acc %>% filter(!is.na(rain_day))


save(acc, roads, mappa_quartieri, file= "Base_Esplorazione_offset.Rdata")

# ------------------------------------------------------------
#  Output to use in the GLMM:
#   - giorni_wide   : giorni_rain / giorni_dry per (Year, month_num)
#                     -> exposure of the weather dimension
#   - acc_meteo     : accidents with the rain_day column (Rain/Dry)
#                     -> for the count per condition
#
#  NB: the weather dimension is added to the grid as Rain/Dry, and
#  each row's offset uses the days of ITS OWN condition
#  (giorni_rain for Rain rows, giorni_dry for Dry rows),
#  multiplied by the other exposure factors already present
#  (road_len, traffic, ...). This way the "Rain" coefficient measures the
#  risk for a rainy day vs a dry day.
# ------------------------------------------------------------
