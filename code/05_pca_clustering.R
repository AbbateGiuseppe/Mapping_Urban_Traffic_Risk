#'######################################'#
#'########### PCA AND CLUSTERING ############'#
#'######################################'#


# SETUP -------------------------------------------------------------------
graphics.off()
rm(list = ls())

if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

source("utils/packages.R")

load("data/Rome/Base_Esplorazione.RData") 



# DATA PREPARATION -----------------------------------------------------------

# Aggregated table per neighborhood 
aggre_tabella_quartieri <- tabella_finale_quartieri %>%
  group_by(DEN_Z_URB) %>%
  # for each group, build a summary row that sums the values, except for
  # variables like year 
  summarise(
    across(
      .cols = where(is.numeric) & !any_of("Year"),
      .fns = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Add social vulnerability and area 

# Table with just the indicator, area (converted to km2) and perimeter 
# (converted to km)
quartieri_social <- dataset_quartieri_tab %>%
  group_by(DEN_Z_URB) %>%
  summarise(
    social_vulnerability = if_else(
      all(is.na(INDICATORE)),
      NA_real_,
      first(INDICATORE[!is.na(INDICATORE)])
    ),
    neigh_area_km2 = if_else(
      all(is.na(Shape__Are)),
      NA_real_,
      first(Shape__Are[!is.na(Shape__Are)]) / 1e6
    ),
    perimeter_length_km = if_else(
      all(is.na(Shape__Len)),
      NA_real_,
      first(Shape__Len[!is.na(Shape__Len)]) / 1000
    ),
    .groups = "drop"
  )

# Merge the aggregated accident table with vulnerability and area, and add
# a column for accident density
tabella_quartieri_social <- aggre_tabella_quartieri %>%
  left_join(
    quartieri_social,
    by = "DEN_Z_URB"
  ) %>%
  mutate(
    accident_density_km2 = Totale_Incidenti / neigh_area_km2
  )


# Convert the counts to percentages + select variables  

# Helper function: if a column exists, use it; if not, return 0.
# This avoids errors when some categories are not present in the dataset.

get_col <- function(data, colname) {
  if (colname %in% names(data)) {
    return(data[[colname]])
  } else {
    return(rep(0, nrow(data)))
  }
}


# Build a table with interpretable percentage variables.
# Percentages are computed over the total accidents in the neighborhood.

feature_quartieri <- tabella_quartieri_social %>%
  mutate(
    # Time of day
    pct_night =
      100 * get_col(., "DayPeriod: Night") / Totale_Incidenti,
    
    pct_morning      = 100 * get_col(., "DayPeriod: Morning")      / Totale_Incidenti,
    
    pct_weekend =
      100 * get_col(., "DayType: weekend") / Totale_Incidenti,
    
    # Severity: accidents with at least 2 injuries
    pct_injury_2plus =
      100 * (
        get_col(., "Injuries: 2") +
          get_col(., "Injuries: 3-5") +
          get_col(., "Injuries: 6 or more")
      ) / Totale_Incidenti,
    
    # Vulnerable road users
    pct_pedestrian_hit =
      100 * get_col(., "AccidentCategory: Pedestrian Hit") / Totale_Incidenti,
    
    # Accident dynamics
    pct_rear_end =
      100 * get_col(., "AccidentCategory: Rear-End") / Totale_Incidenti,
    
    pct_side_collision =
      100 * get_col(., "AccidentCategory: Side Collision") / Totale_Incidenti,
    
    pct_frontal_collision =
      100 * get_col(., "AccidentCategory: Frontal Collision") / Totale_Incidenti,
    
    # Road/environmental conditions
    pct_wet_road =
      100 * (
        get_col(., "RoadSurface: Bagnato (pioggia)") +
          get_col(., "RoadSurface: Bagnato (umidit in atto)") +
          get_col(., "RoadSurface: Bagnato (brina)") +
          get_col(., "RoadSurface: Ghiacciato") +
          get_col(., "RoadSurface: Con neve")
      ) / Totale_Incidenti,
    
    pct_hit_parked   = 100 * get_col(., "AccidentCategory: Hit Parked Vehicle") / Totale_Incidenti,
    pct_intense_traffic =
      100 * get_col(., "Traffic: Intenso") / Totale_Incidenti,
    
    pct_insufficient_visibility =
      100 * get_col(., "Visibility: Insufficiente") / Totale_Incidenti
  )

# Percentage-only matrix
candidate_pct <- feature_quartieri %>%
  select(starts_with("pct_")) %>%
  as.data.frame()

# Correlation among the percentages
matrice_cor_pct <- cor(candidate_pct, use = "complete.obs")
round(matrice_cor_pct, 2)

# Correlation plot
corrplot::corrplot(matrice_cor_pct,
                   method  = "color",
                   type    = "upper",
                   tl.cex  = 0.7,
                   addCoef.col = "black",
                   number.cex  = 0.5,
                   tl.col  = "black")

# PCA -------------------------------------------------------------------------

# Select variables for the PCA so that there is no redundant information 

active_features <- c(
  "pct_night",
  "pct_weekend",
  "pct_injury_2plus",
  "pct_pedestrian_hit",
  "pct_rear_end",
  "pct_side_collision",
  "pct_frontal_collision",
  "pct_wet_road",
  "pct_intense_traffic",
  "pct_insufficient_visibility"
)


active_features <- active_features[
  active_features %in% names(feature_quartieri)
]

# Build the numeric matrix for the PCA using only the active variables.
# Rows represent neighborhoods; columns represent percentage features
# of the accident profile.

matrice_pca <- feature_quartieri %>%
  select(all_of(active_features)) %>%
  as.data.frame()

# Assign neighborhood names as row names.
# The names do not enter the PCA, they are only used to recover the
# neighborhoods afterwards.

rownames(matrice_pca) <- feature_quartieri$DEN_Z_URB

# Check matrix dimensions
dim(matrice_pca)

# Check that all columns are numeric
sapply(matrice_pca, is.numeric)

# Check NAs per column
colSums(is.na(matrice_pca))

# Check variable variances
varianze_pca <- sapply(matrice_pca, var, na.rm = TRUE)

varianze_pca

# Use prcomp(), which relies on the SVD decomposition and is numerically stable.

pca <- prcomp(
  matrice_pca,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

# Loadings 

loadings_pca <- as.data.frame(pca$rotation) %>%
  tibble::rownames_to_column("variable")

# Loadings PC1
loadings_pca %>%
  arrange(desc(abs(PC1))) %>%
  select(variable, PC1) %>%
  head(10)
# A more unfavorable/severe profile, associated with worse environmental
# conditions and greater severity.

# Loadings PC2
loadings_pca %>%
  arrange(desc(abs(PC2))) %>%
  select(variable, PC2) %>%
  head(10)
# PC2 separates a traffic-intensive/rear-end profile from one more oriented
# towards vulnerable road users/pedestrians

# Bar plot
# Each bar is one variable. Height indicates how much it contributes to that component. 
# A tall positive bar means that variable grows in the positive PC direction. Near zero means no contribution.
# Chart 1: PC1, PC2, PC3
x11(width = 10, height = 10)
par(mar = c(6, 3, 2, 1), mfrow = c(3, 1))
for(i in 1:3) barplot(loadings_pca[, i+1],
                      names.arg = loadings_pca$variable,
                      ylim = c(-1, 1),
                      las = 2,
                      cex.names = 0.55,
                      main = paste('Loadings PC', i))

# Chart 2: PC4, PC5, PC6
x11(width = 10, height = 10)
par(mar = c(6, 3, 2, 1), mfrow = c(3, 1))
for(i in 4:6) barplot(loadings_pca[, i+1],
                      names.arg = loadings_pca$variable,
                      ylim = c(-1, 1),
                      las = 2,
                      cex.names = 0.55,
                      main = paste('Loadings PC', i))


# Scores: neighborhood coordinates in the principal component space.
scores_pca <- as.data.frame(pca$x) %>%
  tibble::rownames_to_column("DEN_Z_URB") %>%
  left_join(
    feature_quartieri %>%
      select(
        DEN_Z_URB,
        Totale_Incidenti,
        accident_density_km2,
        social_vulnerability,
        all_of(active_features)
      ),
    by = "DEN_Z_URB"
  )

head(scores_pca)

# Main plot: the two most important components
par(mfrow = c(1,1))
plot(scores_pca[, c("PC1", "PC2")], 
     pch = 19, 
     col = "darkblue",
     main = "PC1 vs PC2")
text(scores_pca$PC1, scores_pca$PC2, 
     labels = scores_pca$DEN_Z_URB, 
     pos = 3, cex = 0.6)
abline(h = 0, v = 0, lty = 2, col = "grey")

# PC1 axis is the dense-urban-context vs adverse-conditions axis


# Secondary plot: exploring the third dimension
par(mfrow = c(1,1))
plot(scores_pca[, c("PC1", "PC3")], 
     pch = 19, 
     col = "darkblue",
     main = "PC1 vs PC3")
text(scores_pca$PC1, scores_pca$PC3, 
     labels = scores_pca$DEN_Z_URB, 
     pos = 3, cex = 0.6)
abline(h = 0, v = 0, lty = 2, col = "grey")

library(RColorBrewer)

# Build a color scale based on social_vulnerability
vuln <- scores_pca$social_vulnerability
col_vuln <- colorRampPalette(c("blue", "yellow", "red"))(100)
col_idx <- cut(vuln, breaks = 100, labels = FALSE)
colori <- col_vuln[col_idx]

# Plot PC1 vs PC2 colored by vulnerability
par(mfrow = c(1,1))
plot(scores_pca[, c("PC1", "PC2")],
     pch = 19,
     col = colori,
     main = "PC1 vs PC2 — colored by social vulnerability")
text(scores_pca$PC1, scores_pca$PC2,
     labels = scores_pca$DEN_Z_URB,
     pos = 3, cex = 0.5)
abline(h = 0, v = 0, lty = 2, col = "grey")

# Legend
legend("bottomright",
       legend = c("Low vulnerability", "Medium", "High vulnerability"),
       fill = c("blue", "yellow", "red"),
       bty = "n")

# CLUSTERING ------------------------------------------------------------------

# Base clustering

# Use PC1-PC4 as the clustering space.
# These components summarize the accident profile of the neighborhoods.
cluster_pcs <- c("PC1", "PC2", "PC3", "PC4")

scores_cluster <- scores_pca %>%
  dplyr::select(dplyr::all_of(cluster_pcs)) %>%
  as.data.frame()



rownames(scores_cluster) <- scores_pca$DEN_Z_URB

scores_cluster_scaled <- scale(scores_cluster)


dist_scores <- dist(scores_cluster_scaled)

# Checks
dim(scores_cluster)
colSums(is.na(scores_cluster))




## K-MEANS --------------------------------------------------------------------- 


### FIRST METHOD 

# Choosing k: elbow method
w <- NULL
b <- NULL

for(k in 1:10) {
  km <- kmeans(scores_cluster, centers = k, nstart = 25)
  w  <- c(w, sum(km$withinss))
  b  <- c(b, km$betweenss)
}

par(mfrow = c(1,1))
matplot(1:10, w/(w+b), pch = '',
        xlab = 'Number of clusters', ylab = 'Within/Tot',
        main = 'Elbow method — choosing k', ylim = c(0,1))
lines(1:10, w/(w+b), type = 'b', lwd = 2)

set.seed(123)
k_scelto <- 4

km_result <- kmeans(scores_cluster, centers = k_scelto, nstart = 25)

scores_pca$cluster_kmeans <- as.factor(km_result$cluster)

# Plot PC1 vs PC2 colored by cluster
par(mfrow = c(1,1))
plot(scores_pca[, c("PC1", "PC2")],
     col = km_result$cluster,
     pch = 19,
     main = paste("K-means —", k_scelto, "clusters"))
text(scores_pca$PC1, scores_pca$PC2,
     labels = scores_pca$DEN_Z_URB,
     pos = 3, cex = 0.5)
abline(h = 0, v = 0, lty = 2, col = "grey")

scores_pca %>%
  group_by(cluster_kmeans) %>%
  summarise(
    vuln_media = mean(social_vulnerability, na.rm = TRUE),
    n_quartieri = n(),
    density_media = mean(accident_density_km2, na.rm = TRUE)
  ) %>%
  arrange(desc(vuln_media))


par(mfrow = c(1, 2))

# Vulnerability by cluster
boxplot(social_vulnerability ~ cluster_kmeans,
        data = scores_pca,
        col  = c("blue", "pink", "black", "green"),
        main = "Social vulnerability by cluster",
        xlab = "Cluster", ylab = "Vulnerability index")

# Accident density by cluster
boxplot(accident_density_km2 ~ cluster_kmeans,
        data = scores_pca,
        col  = c("blue", "pink", "black", "green"),
        main = "Accident density by cluster",
        xlab = "Cluster", ylab = "Accidents/km²")

library(cluster)

sil_kmeans   <- silhouette(km_result$cluster,       dist_scores)
cat("Silhouette K-means:         ", round(mean(sil_kmeans[,"sil_width"]), 3), "\n")


### SECOND METHOD 

# Silhouette: pick the k with the highest silhouette 
set.seed(123)

sil_kmeans <- data.frame()

for (k in 2:8) {
  
  km_tmp <- kmeans(
    scores_cluster,
    centers = k,
    nstart = 50
  )
  
  sil_tmp <- cluster::silhouette(
    km_tmp$cluster,
    dist(scores_cluster)
  )
  
  sil_kmeans <- rbind(
    sil_kmeans,
    data.frame(
      k = k,
      mean_silhouette = mean(sil_tmp[, 3])
    )
  )
}

print(sil_kmeans)

ggplot(sil_kmeans, aes(x = k, y = mean_silhouette)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "K-means: choosing the number of clusters",
    x = "Number of clusters",
    y = "Mean silhouette"
  )

best_k_kmeans <- sil_kmeans$k[which.max(sil_kmeans$mean_silhouette)]
best_k_kmeans


km_result <- kmeans(scores_cluster, centers = best_k_kmeans, nstart = 25)

scores_pca$cluster_kmeans <- as.factor(km_result$cluster)

# Plot PC1 vs PC2 colored by cluster
par(mfrow = c(1,1))
plot(scores_pca[, c("PC1", "PC2")],
     col = km_result$cluster,
     pch = 19,
     main = paste("K-means —", best_k_kmeans, "clusters"))
text(scores_pca$PC1, scores_pca$PC2,
     labels = scores_pca$DEN_Z_URB,
     pos = 3, cex = 0.5)
abline(h = 0, v = 0, lty = 2, col = "grey")

scores_pca %>%
  group_by(cluster_kmeans) %>%
  summarise(
    vuln_media = mean(social_vulnerability, na.rm = TRUE),
    n_quartieri = n(),
    density_media = mean(accident_density_km2, na.rm = TRUE)
  ) %>%
  arrange(desc(vuln_media))


par(mfrow = c(1, 2))

# Vulnerability by cluster
boxplot(social_vulnerability ~ cluster_kmeans,
        data = scores_pca,
        col  = c("blue", "pink", "black", "green"),
        main = "Social vulnerability by cluster",
        xlab = "Cluster", ylab = "Vulnerability index")

# Accident density by cluster
boxplot(accident_density_km2 ~ cluster_kmeans,
        data = scores_pca,
        col  = c("blue", "pink", "black", "green"),
        main = "Accident density by cluster",
        xlab = "Cluster", ylab = "Accidents")


# We also tried: 
# - DBSCAN 
# - HIERARCHICAL CLUSTERING 
# - K MEDOIDS 
# Without getting interpretable clusters, and with social vulnerability in some
