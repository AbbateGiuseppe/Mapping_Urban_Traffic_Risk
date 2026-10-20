# Methodology

Applied-statistics project analyzing road-accident risk in Rome (2017-2021;
2020 excluded), focused on three neighborhoods: **Centro Storico**,
**Esquilino**, and **Torre Angela**. This document summarizes the techniques
used; the same information is also queryable directly from the `techniques`
table in [`../database/mapping_urban_risk.db`](../database/) -- which also
holds the underlying row-level accident records in its `accidents` table.

## 1. Data preprocessing

- Road-network preprocessing & map matching: accident points (geocoded
  lat/lon) are snapped to the OSM road network, and a spatial mesh is built
  for later spatial modeling. (`code/01_preprocessing.R`)
- Data cleaning: filtering to 2017-2021 (2020 excluded), standardising
  accident-type / weather / road-surface categories, handling missing
  temporal and categorical data. (`code/02_cleaning.R`) -- the cleaned,
  row-level output of this step (restricted to the 3 study neighborhoods)
  is what's exposed as the `accidents` table.

## 2. Exploratory data analysis

Global and per-neighborhood descriptive analysis of accident counts,
day/night and weekday/weekend splits, and weather conditions.
(`code/03_eda_global.R`, `code/04_eda_neighborhoods.R`, `code/descrittiva_flag.R`)

## 3. Functional Data Analysis (FDA) + clustering

Accident counts are aggregated into smoothed temporal profiles (by hour of
day, day of week, and month of year) for each of Rome's 155 official urban
zones, using B-spline basis smoothing. A functional PCA is then run on these
profiles, and the resulting scores feed a k-means clustering of
neighborhoods by the *shape* of their temporal accident pattern (e.g.
"nightlife-driven" vs. "commute-driven" zones).
(`code/05_pca_clustering.R`, `code/05.1 fda_kmeans_quartieri.R`,
`code/05. Functional_Analysis_Hour_Month.R`)

## 4. Accident-frequency model: zero-inflated / negative-binomial GLMM

The core model is a `glmmTMB` generalized linear mixed model for the daily
accident count per road segment, with:

- fixed effects: month, day-period (Morning/Afternoon/Evening/Night),
  day-type (weekday/weekend), weather (Dry/Rain), log-traffic volume;
- a per-road random intercept (and, in some variants, a per-neighborhood
  random intercept);
- an offset for exposure (days of observation under each condition).

**Nine model variants** were compared by AIC: Poisson, zero-inflated
Poisson, and negative-binomial (nbinom2) families, each with/without road
length as a predictor and with/without the neighborhood random intercept.
The negative-binomial specification without road length and without the
neighborhood random intercept (`m_glmm_dp_no_road_len_no_Z_URB`) was used
for prediction and is the model backing the Shiny dashboards.
(`code/07_zi_glmm_v2_rain_quota.R`, `code/13. GLMM_roads.R`)

A spatial variant adds an exponential spatial covariance term on road
centroids (`exp(pos + 0 | group)`), as a check for residual spatial
structure not captured by the simpler model. (`code/16. Analisi Spaziale Supporto.R`)

## 5. Accident-severity model: logistic GLMM

A separate `lme4` logistic generalized linear mixed model predicts whether
an accident involved a serious/fatal injury, using the same covariate set
plus traffic and weather. (`code/08_glmm_severity.r`)

## 6. Spatial diagnostics

- **Moran's I** and **Getis-Ord Gi\*** local indicators of spatial
  association, to test for residual spatial autocorrelation in the fitted
  GLMM and to identify accident hot-spots / cold-spots.
  (`code/12.1 analisi_spaziale_preliminare.R`)
- **DHARMa** simulation-based residual diagnostics (uniformity, dispersion,
  zero-inflation, outliers). (`code/validazione_glmm.R`)

## 7. Validation: spatially-blocked cross-validation

Standard k-fold cross-validation is optimistic when observations are
spatially correlated (nearby roads share unmodeled risk factors), so the
project instead uses **leave-one-neighborhood-out** and **spatial block**
(grid-cell) cross-validation: the model is refit on the training blocks and
scored on held-out blocks (RMSE / Pearson correlation for the frequency
model, AUC for the severity model).
(`code/11_cross_validation.R`, `code/validazione_glmm.R`)

## 8. Regression kriging

Ordinary kriging of the GLMM's Pearson residuals over road centroids
produces a spatially corrected accident-rate surface layered on top of the
GLMM's prediction, to capture any remaining smooth spatial trend.
(`code/15.regression_kriging_residui.R`)

## 9. Visualization

- Two interactive Shiny dashboards (`leaflet`/`leafgl`) for exploring
  observed accidents and GLMM-predicted risk by neighborhood, month,
  day-period, day-type and weather. (`code/app_map.R`, `code/app_glmm.R`,
  `code/app.R`, `code/app/app.R`)
- High-resolution static map exports (shared color scale across
  neighborhoods, separate legend images) for a poster presentation.
  (`code/export_mappe_cartellone.R`)

## Where to find the numbers

Everything above that produced a saved result -- both the raw accident
records behind step 1 and the aggregated/model-derived results from every
later step -- is queryable from
[`../database/mapping_urban_risk.db`](../database/README.md), either
directly with a SQL client, or conversationally once the included
[MCP server](../mcp_server/README.md) is connected to Claude. For elementary
questions over the raw data ("how many accidents in 2019", "how many in the
rain", "breakdown by category") use the `accidents` table directly, or the
server's `count_accidents()` / `list_accidents()` tools. A few steps
(cross-validation, FDA clustering, kriging) have their tables created but
currently empty, because the corresponding script's output wasn't available
at export time -- see `database/README.md` for how to (re)populate them.
