#'###########################################'#
#'######## NEIGHBORHOODS ANALYSIS ###########'#
#'###########################################'#

# Territorial exploratory analysis.
# Assign each accident to a Rome neighborhood (quartiere)
# and build a summary table per neighborhood.

setwd(dirname(getActiveDocumentContext()$path))


# 1) SETUP ---------------------------------------------------------
graphics.off()
rm(list = ls())

load("data/Rome/Rome_dataset_ready.RData") 

if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi")

setwd(dirname(getActiveDocumentContext()$path))


source("utils/packages.R")



# 2) NEIGHBORHOOD MAP ---------------------------------------------------------
# Color definitions (used to be in preprocessing)
col_bdy <- rgb(128, 128, 128, max = 255)
col_nodes <- rgb(0, 0, 0, max = 255)
col_edges <- rgb(60, 60, 60, max = 255)
col_accidents <- rgb(190, 0, 0, max = 255)

## Loading the neighborhood map -----------------------------------------

# Load the shapefile of Rome's neighborhoods (quartieri).
# Transform it to the same coordinate system as the dataset
# and fix any invalid geometries.

mappa_quartieri <- sf::st_read(
  "data/Rome/areeUrbanisticheRoma/I_QUARTIERI_SENSIBILI_-_Roma.shp",
  quiet = TRUE
) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_make_valid() 

## Assigning accidents to neighborhoods --------------------------------


# Disable S2 to avoid geometric errors in the spatial join.
sf::sf_use_s2(FALSE)

# Assign each accident to the neighborhood it falls into.
dataset_con_quartiere <- sf::st_join(
  data_target_sf,
  mappa_quartieri,
  join = sf::st_intersects, 
  left = FALSE
)


## Handling points outside the boundary -------------------------------------


# Some points may fall just outside a neighborhood polygon
# due to small geometric or coordinate issues.
# In these cases, assign the nearest neighborhood.


punti_ok <- dataset_con_quartiere %>% dplyr::filter(!is.na(DEN_Z_URB))
punti_na <- dataset_con_quartiere %>% dplyr::filter(is.na(DEN_Z_URB))


# Combine the directly-assigned points with the corrected ones
dataset_quartieri_sf <- punti_ok


##### MAPPING
# 1. Extract only the numeric coordinates for the projection
coords_to_project <- st_coordinates(dataset_quartieri_sf)

# 2. Project onto the road network (mesh)
# This function returns the new coordinates "snapped" onto the road 
locations_projected <- fdaPDE::projection.points.1.5D(mesh, coords_to_project)

# 3. Build the new, updated dataset
dataset_quartieri_sf <- dataset_quartieri_sf %>%
  # Add the new coordinates as numeric columns
  mutate(longitude = locations_projected[,1], 
         latitude  = locations_projected[,2]) %>%
  # Update the geometry based on the newly projected coordinates
  st_set_geometry(NULL) %>% # Drop the old, outdated geometry
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) 
# 'remove = FALSE' keeps the longitude/latitude columns visible alongside geometry


# Also create a tabular version without geometry.
dataset_quartieri_tab <- dataset_quartieri_sf %>% sf::st_drop_geometry()


## Text cleanup -----------------------------------------------------------

# Strip accents and special characters to avoid issues in pivots
# and in automatic column-name generation.
dataset_quartieri_tab <- as.data.frame(
  lapply(dataset_quartieri_tab, function(x) {
    if (is.character(x) || is.factor(x)) {
      x <- iconv(as.character(x), to = "ASCII//TRANSLIT", sub = "")
    }
    return(x)
  })
)





### neighborhood plot

library(ggplot2)

# 1. Spatial filter (to remove points outside Rome)
# Use st_intersection to make sure everything is clipped precisely
accidents_roma <- st_intersection(data_target_sf, adm_bdy)

# 2. High-quality static plot
ggplot() +
  # Draw the neighborhoods (outlines only)
  geom_sf(data = mappa_quartieri, fill = "white", color = "gray80", size = 0.2) +
  # Draw Rome's boundary (thicker black border)
  geom_sf(data = adm_bdy, fill = NA, color = "black", size = 0.8) +
  # Draw the accident points
  geom_sf(data = accidents_roma, color = col_accidents, size = 0.2, alpha = 0.5) +
  # Make the rest of the canvas white
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))   # "Canvas" or NULL for a white background



## Selecting useful variables ------------------------------------------
# Keep only the categorical variables to summarize per neighborhood.

data_selezione <- dataset_quartieri_tab %>%
  dplyr::select(
    DEN_Z_URB, Year, Month, DayType, RoadSurface, Weather_simple,
    Traffic, Visibility, DayPeriod, AccidentCategory, InjuriesCategory
  )

# 3) PIVOT TABLE BY NEIGHBORHOOD ----------------------------

tabella_pivot_quartieri <- data_selezione %>%
  tidyr::pivot_longer(cols = -c(DEN_Z_URB, Year, Month)) %>%
  dplyr::filter(
    !is.na(value),
    value != "Unknown" 
  ) %>%
  dplyr::mutate(
      name_value = dplyr::case_when(
        name == "InjuriesCategory" ~ paste0("Injuries: ", value),
        name == "Weather_simple"   ~ paste0("Weather: ", value),
        TRUE ~ paste0(name, ": ", value)
      )
  ) %>%
  dplyr::count(DEN_Z_URB, Year, Month, name_value) %>%
  tidyr::pivot_wider(names_from = name_value, values_from = n, values_fill = 0)


# Compute the total number of accidents in each neighborhood.
totali_quartieri <- dataset_quartieri_tab %>%
  dplyr::group_by(DEN_Z_URB, Year, Month) %>%
  dplyr::summarise(Totale_Incidenti = dplyr::n(), .groups = "drop")


tabella_finale_quartieri <- totali_quartieri %>%
  dplyr::left_join(tabella_pivot_quartieri, by = c("DEN_Z_URB", "Year", "Month")) %>%
  dplyr::arrange(dplyr::desc(Totale_Incidenti))


# Print the first rows of the final table.
cat("\n--- Final neighborhood summary table ---\n")
print(utils::head(tabella_finale_quartieri))




# 4) EXPLORATORY ANALYSIS BY NEIGHBORHOOD ---------------------------------


## Accident distribution by neighborhood -----------------------------------

cat("\n--- Total accidents by neighborhood ---\n")
print(totali_quartieri)


# Chart of neighborhoods ordered by total accident count.
ggplot2::ggplot(totali_quartieri, 
                ggplot2::aes(x = reorder(DEN_Z_URB, Totale_Incidenti), y = Totale_Incidenti)) +
  ggplot2::geom_col(fill = "steelblue", color = "black", alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Total Number of Accidents by Neighborhood", x = "Neighborhood", y = "Total accidents")

### 10B) SELECTING THE MAIN NEIGHBORHOODS
top_quartieri <- totali_quartieri %>%
  dplyr::slice_max(order_by = Totale_Incidenti, n = 10) %>%
  dplyr::pull(DEN_Z_URB)

cat("\n--- Top neighborhoods by total accidents ---\n")
print(top_quartieri)



file_name <- "data/Rome/Base_Esplorazione.RData"

save(data_clean_sf, 
     data_target_sf, 
     dataset_con_quartiere,
     dataset_quartieri_sf,
     dataset_quartieri_tab,
     mesh, mesh_sf, adm_bdy,
     mappa_quartieri,
     data_selezione,
     tabella_finale_quartieri,
     tabella_pivot_quartieri,
     totali_quartieri,
     roads,
     file = file_name)
