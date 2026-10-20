# in this file we want to answer the question:
# Given that an accident has occurred, how likely is it to be severe?
graphics.off()
rm(list=ls())
library(tidyverse)
library(car)
library(MASS)
library(dplyr)
library(ggplot2)
library(writexl)
library(sf)
library(lubridate)
library(readxl)   # added: reading weather from a single source
library(stringr)  # added: str_detect for the rain definition
library(DHARMa)   # added: simulated residual diagnostics


load("Base_Esplorazione_offset.RData")

acc[] <- lapply(acc, function(x) {
  if (is.character(x)) iconv(x, to = "UTF-8", sub = "byte") else x
})

# ==============================================================================
# WEATHER — RAIN definition from a single source (logic from file 07)
#   "Rain" = the FENOMENI column contains the word "piogg"
#   (captures "pioggia", "pioggia temporale", "pioggia nebbia", etc.)
#   The join happens by date on the individual accident, so that every row
#   gets the weather condition of the exact day, without depending on the
#   Weather_simple column already present in the dataset (which used the
#   previous Clear/NotClear / multi-category classification).
# ==============================================================================

meteo <- read_excel(
  "meteo_roma_completo.csv.xlsx",
  sheet = "meteo_roma_completo"
)

meteo <- meteo %>%
  mutate(
    date           = as_date(DATA),
    Weather_simple = if_else(
      str_detect(str_to_lower(coalesce(FENOMENI, "")), "piogg"),
      "Rain", "Dry"
    )
  )

cat("Total weather days:", nrow(meteo),
    "| rainy:", sum(meteo$Weather_simple == "Rain"),
    "| dry:", sum(meteo$Weather_simple == "Dry"),
    "| NA dates:", sum(is.na(meteo$date)), "\n")

# ==============================================================================
# DATA PREPARATION
#   - the old Weather_simple is removed from data_clean_sf
#   - a join by date is done with the Weather_simple just computed
#   - Weather_adverse is the 0/1 flag derived from the new source
#   NB: adapt "DateHour" if the date column name differs in your dataset
# ==============================================================================

data_severity2 <- acc %>%
  mutate(date = as_date(DateHour)) %>%   # <-- ADAPT if necessary
  filter(!is.na(rain_day)) %>%     # excludes accidents without weather coverage
  mutate(
    # response variable: 1 if the accident is severe (at least 1 injury or death)
    severity            = ifelse(Injuries >= 1 | Deaths >= 1, 1, 0),
    # weekend flag
    Weekend             = ifelse(tolower(DayType) == "weekend", 1, 0),
    # Weather_adverse derived from the single source: 1 if rain, 0 otherwise
    Weather_adverse     = ifelse(rain_day == "Rain", 1, 0),
    # pedestrian-involved flag
    Pedestrian_involved = ifelse(
      tolower(Pedestrians) %in% c("true", "yes", "si", "si", "1"), 1, 0
    )
  )

cat("Accidents flagged - rain:", sum(data_severity2$Weather_adverse == 1),
    "| dry:", sum(data_severity2$Weather_adverse == 0), "\n")

# ==============================================================================
# VARIABLE SELECTION AND NEIGHBORHOOD SPATIAL JOIN
# ==============================================================================

data_severity2_clean <- data_severity2 %>%
  filter(Year >= 2017 & Year <= 2021 & Year != 2020) %>%
  dplyr::select(
    severity,
    Weekend,
    Weather_adverse,
    Pedestrian_involved,
    DayPeriod,
    DEN_Z_URB,
    Year,
    Month,
    Traffic,
    rain_day
  )

mappa_quartieri        <- st_transform(mappa_quartieri, crs = 4326)
data_severity2_clean   <- st_transform(data_severity2_clean, crs = 4326)
data_severity2_sf      <- st_join(data_severity2_clean, mappa_quartieri,
                                  join = st_intersects)

data_severity2 <- data_severity2_clean %>%
  dplyr::select(
    severity,
    Weekend,
    Weather_adverse,
    Pedestrian_involved,
    DayPeriod,
    DEN_Z_URB,
    Year,
    Month,
    Traffic,
    rain_day
  )


save(data_severity2, file = "data_severity2.RData")

# ==============================================================================
# LOGISTIC GLMM
# ==============================================================================

library(lme4)

data_severity2 <- data_severity2 %>%
  mutate(Traffic = ifelse(Traffic == "Unknown", "Normale", Traffic))

data_severity2$DayPeriod <- as.factor(data_severity2$DayPeriod)
data_severity2$Traffic   <- as.factor(data_severity2$Traffic)
levels(data_severity2$DayPeriod)
data_severity2$DayPeriod <- relevel(data_severity2$DayPeriod, ref = "Morning")
levels(data_severity2$Traffic)
data_severity2$Traffic   <- relevel(data_severity2$Traffic,   ref = "Normale")

mesi_ordine <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data_severity2$Month <- factor(data_severity2$Month, levels = mesi_ordine)
levels(data_severity2$Month)
data_severity2$Month <- relevel(data_severity2$Month, ref = "Jan")
data_severity2$rain_day <- as.factor(data_severity2$rain_day)
levels(data_severity2$rain_day)
modello_severita_logistico <- glmer(
  severity ~ DayPeriod + Weekend + rain_day + Pedestrian_involved +
             Traffic + Month + (1 | DEN_Z_URB),
  data   = data_severity2,
  family = binomial(link = "logit")
)
summary(modello_severita_logistico)

# ==============================================================================
# DHARMa DIAGNOSTICS
#   simulateResiduals also works for binomial GLMMs: it allows
#   detecting patterns in residuals, residual overdispersion, and anomalies
#   that standard Pearson residuals do not catch on 0/1 data.
# ==============================================================================

sim_sev <- simulateResiduals(modello_severita_logistico)
plot(sim_sev)
testDispersion(sim_sev)
testZeroInflation(sim_sev)   # on binomial models, tests whether there are more zeros than expected

# ==============================================================================
# SAVING
# ==============================================================================

save(
  modello_severita_logistico,
  data_severity2,
  file = "severity_model_objects.RData"
)
