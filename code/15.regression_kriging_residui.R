# ============================================================
#  REGRESSION KRIGING on the GLMM RESIDUALS
#  Model: m_glmm_dp_no_road_len_no_Z_urb  (without DEN_Z_URB)
#  Idea: the RESIDUAL spatial structure (residual Moran's I != 0) not
#        captured by the GLMM is modeled with kriging and added back.
#  Residual scale: PEARSON (dimensionless, ~symmetric, negatives OK).
#  Aggregation: per ROAD (road_id), across the 3 neighborhoods.
#  Final MULTIPLICATIVE correction -> prediction always POSITIVE.
#
#  Requires: m_glmm_dp_no_road_len_no_Z_urb, df (model data),
#            roads (geometries), and gstat/sf/dplyr.
# ============================================================
library(sf); library(dplyr); library(gstat); library(sp)

# ------------------------------------------------------------
# 1. GLMM PEARSON RESIDUALS, aggregated per road
#    Pearson = (observed - expected) / sd(expected). For nbinom2:
#    Var = mu + mu^2/theta. Averaged per road_id (as in DHARMa group=road_id).
# ------------------------------------------------------------
m <- m_glmm_dp_no_road_len_no_Z_URB
df=df2
mu    <- predict(m, type = "response")          # expected value per row
theta <- sigma(m)                               # nbinom2 parameter
varmu <- mu + mu^2 / theta
pear  <- (df$n_accidents - mu) / sqrt(varmu)    # Pearson residual

res_road <- df %>%
  mutate(pear = pear) %>%
  group_by(road_id) %>%
  summarise(resid = mean(pear, na.rm = TRUE), .groups = "drop") %>%
  mutate(road_id = as.integer(as.character(road_id)))

cat("Roads with a residual:", nrow(res_road), "\n")
cat("Mean residual:", round(mean(res_road$resid),4),
    "| sd:", round(sd(res_road$resid),4), "\n")

# ------------------------------------------------------------
# 2. Road geometries (UTM centroids) + residual join
# ------------------------------------------------------------
roads_m <- st_as_sf(roads) %>%
  mutate(road_id = as.integer(road_id)) %>%
  st_transform(32633)

ctr <- roads_m %>%
  mutate(X = st_coordinates(st_centroid(geometry))[,1],
         Y = st_coordinates(st_centroid(geometry))[,2]) %>%
  st_drop_geometry() %>%
  dplyr::select(road_id, X, Y)

dat <- res_road %>%
  inner_join(ctr, by = "road_id") %>%
  filter(is.finite(resid), is.finite(X), is.finite(Y))
cat("Points for kriging:", nrow(dat), "\n")

coordinates(dat) <- ~X + Y

# ------------------------------------------------------------
# 3. VARIOGRAM of the residuals (+ anisotropy check)
# ------------------------------------------------------------
vg_omni <- variogram(resid ~ 1, dat)
vg_dir  <- variogram(resid ~ 1, dat, alpha = c(0,45,90,135), tol.hor = 22.5)
print(plot(vg_dir, main = "Directional variogram - GLMM residuals"))

# automatic isotropic fit (starting point)
vfit <- fit.variogram(vg_omni, vgm(model = "Sph",
            psill = var(dat$resid), range = max(vg_omni$dist)/3,
            nugget = 0.5*var(dat$resid)))
cat("\n-- fitted variogram (isotropic) --\n"); print(vfit)
# if vg_dir shows anisotropy, add anis=c(angle,ratio) in vgm()

# ------------------------------------------------------------
# 4. ORDINARY KRIGING of the residual surface
#    Predict the residual on the SAME roads (and on a grid for the map)
# ------------------------------------------------------------
# (a) kriging on the road points (to correct the GLMM predictions)
k_road <- krige(resid ~ 1, dat, newdata = dat, model = vfit)
res_kr <- tibble(road_id = res_road$road_id[match(
                   paste(coordinates(dat)[,1], coordinates(dat)[,2]),
                   paste(coordinates(k_road)[,1], coordinates(k_road)[,2]))],
                 resid_kriged = k_road$var1.pred)
# robust alignment: rebuilt by index (same order as dat)
res_kr <- tibble(road_id = dat$road_id, resid_kriged = k_road$var1.pred)

# (b) kriging on a grid for the continuous MAP
grd <- st_make_grid(roads_m, cellsize = 250, square = TRUE) %>%
  st_sf(geometry = .) %>%
  st_filter(st_union(roads_m), .predicate = st_intersects)
grd_sp <- as_Spatial(st_centroid(grd))
k_grid <- krige(resid ~ 1, dat, newdata = grd_sp, model = vfit)
grd$resid_kriged <- k_grid$var1.pred

# ------------------------------------------------------------
# 5. CORRECTED PREDICTION = GLMM x exp(kriged residual)
#    multiplicative on the rate -> ALWAYS POSITIVE.
#    (the kriged Pearson residual acts as an "adjustment factor":
#     >0 raises the estimate where the model underestimates, <0 lowers it)
# ------------------------------------------------------------
# scale factor: convert the mean Pearson residual into a log correction.
# pragmatic and stable choice: correction = exp(alpha * resid_kriged)
# with alpha calibrated (1 = full; <1 more conservative)
ALPHA <- 1.0
mu_road <- df %>%
  mutate(mu = mu) %>%
  group_by(road_id) %>%
  summarise(mu_mean = mean(mu, na.rm = TRUE), .groups = "drop") %>%
  mutate(road_id = as.integer(as.character(road_id)))

pred_corr <- mu_road %>%
  inner_join(res_kr, by = "road_id") %>%
  mutate(mu_corretto = mu_mean * exp(ALPHA * resid_kriged))   # always > 0

cat("\n== GLMM prediction vs corrected (per road) ==\n")
print(summary(pred_corr$mu_mean))
print(summary(pred_corr$mu_corretto))
cat("Correlation mu vs mu_corretto:",
    round(cor(pred_corr$mu_mean, pred_corr$mu_corretto), 3), "\n")

# save for later use/mapping
saveRDS(pred_corr, "pred_corretta_kriging.rds")
saveRDS(grd, "superficie_residui_kriged.rds")
cat("\nsaved: pred_corretta_kriging.rds, superficie_residui_kriged.rds\n")

# ============================================================
#  NOTES
#  - PEARSON residuals: ~symmetric dimensionless scale -> ordinary kriging
#    is appropriate; negative values are legitimate (area less risky than expected).
#  - MULTIPLICATIVE exp() correction: the final prediction stays positive,
#    solving the count/negative-values problem.
#  - ALPHA calibrates the intensity of the correction (1=full). If the
#    residual structure is weak (Moran's I was ~0.034), exp(resid) will be
#    close to 1 and the correction small: CONSISTENT, the GLMM already
#    captures almost everything.
#  - If vg_dir shows anisotropy, refit vfit with anis=c(angle,ratio)
#    and the kriging will use it automatically.
#  - HONEST VALIDATION: to claim the correction IMPROVES the predictions
#    it should be tested with spatial cross-validation (krige.cv), not just
#    applied in-sample. Can be added if needed.
# ============================================================
