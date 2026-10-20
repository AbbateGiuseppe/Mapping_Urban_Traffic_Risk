
# centro must already be in memory (you built it at the start of the script)
# if it isn't, rebuild it:
# roads_m <- st_transform(roads, 32633)
# centro <- roads_m %>%
#   mutate(road_id = as.integer(road_id)) %>%
#   { bind_cols(road_id = .$road_id,
#               as.data.frame(st_coordinates(st_centroid(st_geometry(.))))) }
library(sf)
library(dplyr)
library(DHARMa)
roads_m <- st_transform(roads, 32633)

centro <- roads_m %>%
  mutate(road_id = as.integer(road_id)) %>%
  { bind_cols(
    road_id = .$road_id,
    as.data.frame(st_coordinates(st_centroid(st_geometry(.))))
  )} %>%
  rename(X = X, Y = Y)

# then resume from here
rid_order <- as.integer(as.character(unique(df$road_id)))
cm <- centro[match(rid_order, centro$road_id), ]

# Moran's I test on the residuals aggregated per road
testSpatialAutocorrelation(sim_road_dharma, x = cm$X, y = cm$Y)




library(glmmTMB)

# add the UTM coordinates to df
df <- df %>%
  left_join(
    centro %>% dplyr::select(road_id, X, Y) %>%
      mutate(road_id = factor(road_id)),
    by = "road_id"
  )

# define the spatial term (numFactor = road_id as position)
# glmmTMB uses pos as a coordinate matrix
df$pos <- numFactor(df$X, df$Y)
df$group <- factor(1)   # a single spatial group

m_spatial <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) +
    exp(pos + 0 | group) +          # spatial exponential covariance
    offset(log(esposizione)),
  family = nbinom2, data = df
)

print(AIC(m_spatial, m_glmm_dp_no_road_len_no_Z_URB))
save(m_spatial, file = "modello_spatial.RData")
