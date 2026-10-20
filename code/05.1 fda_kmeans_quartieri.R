load("data/Rome/Base_Esplorazione.RData")
# ============================================================
#  FDA k-means clustering of the neighborhoods  -  fdacluster::fdakmeans
#  FUNCTIONAL k-means with PHASE ALIGNMENT (phase warping)
#  Two views: HOURLY profile (Hour 0-23) and MONTHLY profile (Month 1-12)
#  Each neighborhood = one curve (profile shape, normalized to sum to 1)
# ============================================================

# install.packages("fdacluster")
library(fdacluster)
library(dplyr)
library(tidyr)
library(tibble)
library(sf)
library(lubridate)   # parsing the "HH:MM" time format
# library(ggplot2)

set.seed(1)

# ------------------------------------------------------------
# 0pre. Hour is "HH:MM" -> extract the integer hour 0-23
# ------------------------------------------------------------
dataset_quartieri_sf$Hour_int <- hour(hm(dataset_quartieri_sf$Hour))
cat("Hour_int distribution:\n"); print(table(dataset_quartieri_sf$Hour_int, useNA = "ifany"))
if (any(is.na(dataset_quartieri_sf$Hour_int)))
  warning("Hour_int contains NA values: check the format of the Hour column.")

# ------------------------------------------------------------
# 0pre2. Filter to the selected time range: years 2017-2022
#        (the functional profiles are built only on these years)
# ------------------------------------------------------------
anni_target <- 2017:2022
dataset_quartieri_sf <- dataset_quartieri_sf %>%
  dplyr::filter(as.integer(Year) %in% anni_target)
cat("Years kept:", paste(sort(unique(as.integer(dataset_quartieri_sf$Year))),
                          collapse = ", "), "\n")

# ------------------------------------------------------------
# 0pre3. Remove neighborhoods with TOO FEW accidents
#        A normalized profile (shape) built on few accidents is
#        unstable: it produces spurious spikes (e.g. 2 accidents out of 4 at
#        9am -> 0.5) that show up as anomalous curves on the plot and distort
#        the k-means. Filtering at the source is better than manually
#        removing the spikes afterwards.
# ------------------------------------------------------------
MIN_INCIDENTI <- 100   # threshold: raise/lower depending on your distribution

conteggio_q <- dataset_quartieri_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::count(DEN_Z_URB, name = "n_tot")
cat("\nAccidents per neighborhood (summary):\n")
print(summary(conteggio_q$n_tot))

quartieri_validi <- conteggio_q %>%
  dplyr::filter(n_tot >= MIN_INCIDENTI) %>%
  dplyr::pull(DEN_Z_URB)

cat("Neighborhoods kept:", length(quartieri_validi),
    "out of", nrow(conteggio_q),
    "(threshold >=", MIN_INCIDENTI, "accidents)\n")

dataset_quartieri_sf <- dataset_quartieri_sf %>%
  dplyr::filter(DEN_Z_URB %in% quartieri_validi)

# ------------------------------------------------------------
# 0. Helper: profile matrix (neighborhood x time points), shape summing to 1
#    Returns a list with: ids, grid (time vector), Y (n_units x n_times matrix)
# ------------------------------------------------------------
build_profile <- function(data, time_col, all_levels) {
  tab <- data %>%
    st_drop_geometry() %>%
    dplyr::count(DEN_Z_URB, .data[[time_col]], name = "n") %>%
    dplyr::rename(t = !!time_col) %>%
    dplyr::group_by(DEN_Z_URB) %>%
    dplyr::mutate(p = n / sum(n)) %>%          # normalize -> profile shape
    dplyr::ungroup() %>%
    dplyr::select(DEN_Z_URB, t, p) %>%
    tidyr::complete(DEN_Z_URB, t = all_levels, fill = list(p = 0)) %>%
    tidyr::pivot_wider(names_from = t, values_from = p, values_fill = 0) %>%
    dplyr::arrange(DEN_Z_URB)
  # NB: to cluster by VOLUME (not shape), use n instead of p above.
  
  ids <- tab$DEN_Z_URB
  Y   <- as.matrix(tab[, -1])                  # rows = neighborhoods, columns = time points
  storage.mode(Y) <- "numeric"
  list(ids = ids, grid = as.numeric(all_levels), Y = Y)
}

# ------------------------------------------------------------
# 0b. Helper: fdakmeans with alignment + choosing k via silhouette
#    warping_class options available in this version of fdacluster:
#      "none"     -> no alignment
#      "shift"    -> time translation: aligns the PHASE (where the peak falls).
#                    This is the most natural phase alignment for profiles on
#                    a linear grid -> used as the default here.
#      "dilation" -> dilation only (time scale)
#      "affine"   -> translation + dilation (the most flexible one allowed)
#      "bpd"      -> boundary-preserving diffeomorphisms: NOT usable here,
#                    it is only defined for curves on domains with boundaries
#                    (closed shapes), not for curves on the real line ->
#                    fdacluster rejects it. That's why phase alignment goes
#                    through shift/dilation/affine instead.
# ------------------------------------------------------------
run_fdakmeans <- function(prof, n_clusters, warp = "shift", metric = "l2") {
  fdakmeans(
    x             = prof$grid,
    y             = prof$Y,
    n_clusters    = n_clusters,
    seeding_strategy = "kmeans++",
    warping_class = warp,         # phase alignment: "shift"
    centroid_type = "mean",
    metric        = metric,
    use_verbose   = FALSE
  )
}

# chooses k by comparing the mean silhouette returned by fdakmeans
choose_k <- function(prof, k_range = 2:6, warp = "shift", metric = "l2") {
  res <- lapply(k_range, function(k) run_fdakmeans(prof, k, warp, metric))
  sil <- sapply(res, function(r) mean(r$silhouettes, na.rm = TRUE))
  best <- which.max(sil)
  list(
    table   = tibble::tibble(k = k_range, avg_sil = round(sil, 3)),
    best_k  = k_range[best],
    best    = res[[best]],
    all     = res
  )
}

# compares warping classes at a fixed k.
# IMPORTANT: the metric must be COMPATIBLE with the warping --
#   l2 works with "none" and "shift" (translation-invariant)
#   but NOT with "dilation"/"affine" (l2 is not scale-invariant);
#   for these a scale-invariant metric is needed, e.g. "pearson".
# So each warping class is paired with its valid metric.
WARP_METRIC <- list(
  none     = "l2",
  shift    = "l2",
  dilation = "pearson",
  affine   = "pearson"
)

compare_warps <- function(prof, n_clusters, warp_metric = WARP_METRIC) {
  out <- lapply(names(warp_metric), function(w) {
    r <- run_fdakmeans(prof, n_clusters, warp = w, metric = warp_metric[[w]])
    tibble::tibble(
      DEN_Z_URB = prof$ids,
      warp      = w,
      metric    = warp_metric[[w]],
      cluster   = r$memberships,
      avg_sil   = round(mean(r$silhouettes, na.rm = TRUE), 3)
    )
  })
  dplyr::bind_rows(out)
}

# ------------------------------------------------------------
# 1. HOURLY VIEW  (Hour 0-23)
#    NB: periodic domain -> warping on an open domain can produce
#    boundary effects (23 is not "close" to 0 for the algorithm).
#    This is still fine for profiles with internal peaks (morning/evening rush).
# ------------------------------------------------------------
prof_hour <- build_profile(dataset_quartieri_sf, "Hour_int", 0:23)
sel_hour  <- choose_k(prof_hour, k_range = 2:6, warp = "shift")

cat("\n=== HOURLY PROFILE (fdakmeans, shift phase warping) ===\n")
print(sel_hour$table)
cat("Chosen k:", sel_hour$best_k, "\n")
clusters_hour <- tibble::tibble(DEN_Z_URB = prof_hour$ids,
                                cluster   = sel_hour$best$memberships)
print(clusters_hour)

cat("\n--- Warping comparison (hourly, k =", sel_hour$best_k, ") ---\n")
warp_hour <- compare_warps(prof_hour, n_clusters = sel_hour$best_k)
print(tidyr::pivot_wider(warp_hour, names_from = warp, values_from = cluster,
                         id_cols = DEN_Z_URB), n = Inf)
cat("Mean silhouette per warping class:\n")
print(dplyr::distinct(warp_hour, warp, metric, avg_sil))

# ------------------------------------------------------------
# 2. MONTHLY VIEW  (Month 1-12)
# ------------------------------------------------------------
mese_ord <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")
dq_month <- dataset_quartieri_sf
if (is.character(dq_month$Month) || is.factor(dq_month$Month)) {
  dq_month$Month <- match(as.character(dq_month$Month), mese_ord)
}

prof_month <- build_profile(dq_month, "Month", 1:12)
sel_month  <- choose_k(prof_month, k_range = 2:6, warp = "none")

cat("\n=== MONTHLY PROFILE (fdakmeans, shift phase warping) ===\n")
print(sel_month$table)
cat("Chosen k:", sel_month$best_k, "\n")
clusters_month <- tibble::tibble(DEN_Z_URB = prof_month$ids,
                                 cluster   = sel_month$best$memberships)
print(clusters_month)

cat("\n--- Warping comparison (monthly, k =", sel_month$best_k, ") ---\n")
warp_month <- compare_warps(prof_month, n_clusters = sel_month$best_k)
print(tidyr::pivot_wider(warp_month, names_from = warp, values_from = cluster,
                         id_cols = DEN_Z_URB), n = Inf)
cat("Mean silhouette per warping class:\n")
print(dplyr::distinct(warp_month, warp, metric, avg_sil))

# ------------------------------------------------------------
# 3. Hourly vs monthly comparison
# ------------------------------------------------------------
confronto <- clusters_hour %>%
  dplyr::rename(cluster_hour = cluster) %>%
  dplyr::left_join(clusters_month %>% dplyr::rename(cluster_month = cluster),
                   by = "DEN_Z_URB")
cat("\n=== CLUSTER COMPARISON (hourly vs monthly) ===\n")
print(confronto, n = Inf)

# ------------------------------------------------------------
# 4. Visualization (fdacluster has its own built-in plot methods)
#    They show the original curves, the aligned curves, and the cluster centroids.
# ------------------------------------------------------------
plot(sel_hour$best)    # hourly profiles: curves + warping + centroids
plot(sel_month$best)   # monthly profiles


# autoplot(sel_hour$best) if you prefer ggplot (requires ggplot2)

# ------------------------------------------------------------
# Useful objects:
#   sel_hour$best / sel_month$best   -> full fdakmeans object
#       $memberships  = cluster of each neighborhood
#       $silhouettes  = per-unit silhouette
#       $warpings     = estimated warping functions (phase alignment)
#       $aligned_grids/$aligned_curves = curves after registration
#   clusters_hour / clusters_month   -> neighborhood -> cluster
#   confronto                        -> summary table
# ------------------------------------------------------------


# 1. Save the R environment
save(sel_hour, sel_month, clusters_hour, clusters_month, confronto, 
     file = "FDA_Clustering_Results.RData")

# 2. Save the CSV for Excel/maps
write.csv(confronto, file = "cluster_comparison.csv", row.names = FALSE)

# 3. Save the curve plots
pdf(file = "Interpretable_Plots.pdf", width = 11, height = 8)
plot(sel_hour$best)
plot(sel_month$best)
dev.off()

cat("\n=== ALL SAVES COMPLETED SUCCESSFULLY ===\n")
