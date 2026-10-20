# Database

`mapping_urban_risk.db` is a SQLite database containing both the project's
**raw, row-level accident records** and its **aggregated, model-derived
results** -- GLMM predictions, model-comparison and coefficient tables,
functional-PCA / clustering summaries, and road / neighborhood reference
geometry.

The raw records (`accidents` table) are **open municipal data** (Comune di
Roma's public traffic-accident dataset): no personal or vehicle-identifying
information is included -- no names, no license plates, no addresses, just
the circumstances of each crash (when, where, what kind of accident,
weather, injuries). They're filtered to the 3 study neighborhoods and to
2017-2021 (2020 excluded -- see `docs/techniques.md`), matching the scope of
the rest of the project.

A second, identical copy of this file lives at
[`../mcp_server/mapping_urban_risk.db`](../mcp_server/). That's not a
mistake -- it's there so the bundled [MCP server](../mcp_server/README.md)
keeps working when installed as a Claude Desktop "extension", a method that
copies only the `mcp_server/` folder and would otherwise lose access to a
database sitting one level up. See `mcp_server/README.md` for the full
explanation. If you rebuild the database (below), copy the result into both
locations.

## Schema

See [`schema.sql`](./schema.sql) for the full table definitions. Summary:

| table | what it holds |
|---|---|
| `neighborhoods` | All 155 official Rome urban zones; 3 flagged `is_study_area=1` (the project's focus area: Centro Storico, Esquilino, Torre Angela). |
| `roads` | Road segments inside the 3 study neighborhoods (type, length, centroid). |
| `accidents` | **Raw row-level accident records** (8,514 rows), 2017-2021 (2020 excluded), 3 study neighborhoods only. One row per crash: date/time, category, weather, injuries, road users involved, coordinates. The basis for elementary questions (counts by year/category/weather/...); see `mcp_server`'s `count_accidents()` / `list_accidents()` tools. |
| `model_registry` | Every GLMM variant fitted for accident frequency -- formula, family, AIC/BIC, n_obs. |
| `model_coefficients` | Fixed-effect estimates / SE / z / p-value for each model above. |
| `glmm_predictions` | **Core results table**: predicted expected accidents/day per road x month x day-period x day-type x weather (290,880 rows). |
| `fda_basis_coefficients` | B-spline coefficients of the smoothed hour/day/month accident-rate curve, per neighborhood. |
| `fpca_scores` / `fpca_variance_explained` | Functional-PCA results feeding the clustering step. |
| `techniques` | Curated reference: every technique used in the project, its script, and which table (if any) holds its numeric output. |
| `cv_*`, `fda_cluster_assignments`, `kriging_corrected_predictions`, `severity_model_coefficients`, `spatial_autocorrelation_tests` | Created empty -- see **Known gaps** below. |

## How it was built

1. `export_to_db.R` runs **in the original project folder** (where the
   `.rds` / `.RData` model outputs live -- that folder is private and not
   part of this repo) and writes aggregated CSVs into `db_exports/`.
2. `build_database.py` reads those CSVs plus a hand-curated list of
   techniques, and builds `mapping_urban_risk.db` from `schema.sql`.

```bash
# in the original project folder:
Rscript export_to_db.R

# then, in this repo:
python3 build_database.py --exports-dir /path/to/db_exports

# finally, sync the copy the MCP server extension install relies on:
cp mapping_urban_risk.db ../mcp_server/mapping_urban_risk.db
```

Running `build_database.py` with no arguments rebuilds the committed
database from the `db_exports/` CSVs shipped alongside it (if present) --
see the script's `--help` for all options. Don't forget the `cp` step above
afterwards -- the two copies are not kept in sync automatically.

## Known gaps (and how to fill them)

A few methodology steps from the project don't have their numeric output in
this database yet, because the corresponding script's output file wasn't
present when the export was last run:

- **Cross-validation** (`cv_zinb_metrics`, `cv_zinb_by_fold`,
  `cv_severity_metrics`, `cv_severity_by_fold`): re-run
  `11_cross_validation.R`, then re-run `export_to_db.R`.
- **FDA k-means cluster assignments** (`fda_cluster_assignments`): re-run
  `"05.1 fda_kmeans_quartieri.R"`, then re-run `export_to_db.R`.
- **Regression-kriging-corrected predictions** (`kriging_corrected_predictions`):
  re-run `"15.regression_kriging_residui.R"`, then re-run `export_to_db.R`.
- **Severity-model coefficients** (`severity_model_coefficients`) and
  **spatial-autocorrelation test statistics** (`spatial_autocorrelation_tests`):
  these aren't automatically exported yet -- `export_to_db.R` would need a
  small extension once `severity_model_objects.RData` exists and the Moran's
  I / Getis-Ord test results are saved to disk instead of only printed.

The `techniques` table documents all of these steps regardless, so the
*methodology* is fully described even where the *numbers* aren't filled in
yet.
