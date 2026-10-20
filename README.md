# Mapping Urban Traffic Risk

Applied-statistics project analyzing road-accident risk in Rome (2017-2022)
across three neighborhoods -- **Centro Storico**, **Esquilino**, and
**Torre Angela** -- combining generalized linear mixed models, functional
data analysis, spatial statistics and interactive dashboards.

This repo ships three things:

1. **`code/`** -- the full, cleaned, English-language R pipeline (data
   preprocessing through GLMM fitting, cross-validation, spatial diagnostics,
   kriging, and two Shiny dashboards).
2. **`database/`** -- a SQLite database with both the project's **raw
   (row-level) accident records**, filtered to the 3 study neighborhoods and
   2017-2021 (open municipal data, no personal/identifying information), and
   its **aggregated results** (model predictions, model comparison,
   functional-PCA / clustering summaries).
3. **`mcp_server/`** -- a small [MCP](https://modelcontextprotocol.io)
   server that exposes that database as a set of tools, so you can ask
   Claude (or any MCP client) questions about the project directly --
   from elementary counts ("how many accidents in 2019 involved rain?")
   to the model's predictions and methodology.

See [`docs/techniques.md`](docs/techniques.md) for a full write-up of the
methodology.

## Quick start: query the project with Claude

```bash
cd database && python3 build_database.py   # build mapping_urban_risk.db (first time only)
cd ../mcp_server && pip install -r requirements.txt
```

Then connect `mcp_server/server.py` to Claude Desktop or Claude Code --
see [`mcp_server/README.md`](mcp_server/README.md) for the exact config.
Once connected, try asking things like:

- *"How many accidents happened in Torre Angela in 2019?"*
- *"Break down accidents by category, or by weather condition."*
- *"Which GLMM variant fit best, and why?"*
- *"Which road in Esquilino has the highest predicted accident rate on a
  rainy weekend night?"*
- *"What statistical techniques were used for the spatial diagnostics?"*

## Project summary

- **Data**: Rome road-accident records (2017-2021; 2020 excluded) from
  open municipal data, restricted here to three neighborhoods chosen for
  contrast in density, land use and traffic pattern.
- **Frequency model**: zero-inflated / negative-binomial `glmmTMB` GLMM for
  daily accident counts per road segment (month, day-period, day-type,
  weather, traffic volume as predictors; nine model variants compared by
  AIC).
- **Severity model**: `lme4` logistic GLMM for serious/fatal-injury
  probability.
- **Functional data analysis**: B-spline smoothing of hourly/daily/monthly
  accident-rate profiles per neighborhood, followed by functional PCA and
  k-means clustering.
- **Spatial diagnostics**: Moran's I, Getis-Ord Gi\*, DHARMa residual
  simulation, a spatial-covariance GLMM robustness check.
- **Validation**: spatially-blocked / leave-one-neighborhood-out
  cross-validation (not plain k-fold, since nearby roads are spatially
  correlated).
- **Spatial smoothing**: regression kriging of GLMM residuals.
- **Visualization**: two interactive Shiny dashboards, plus static poster
  map exports.

Full details in [`docs/techniques.md`](docs/techniques.md).

## Repository layout

```
.
├── code/            cleaned R scripts (preprocessing -> modeling -> validation -> apps)
│   ├── utils/
│   └── app/
├── database/         SQLite db of aggregated results + how it was built
│   ├── schema.sql
│   ├── build_database.py
│   ├── export_to_db.R
│   └── mapping_urban_risk.db
├── mcp_server/        MCP server to query the database from Claude
│   ├── server.py
│   ├── manifest.json  Desktop Extension manifest (for the "install unpacked extension" flow)
│   ├── mapping_urban_risk.db  bundled copy of the database (see database/README.md)
│   └── requirements.txt
└── docs/
    └── techniques.md  full methodology write-up
```

## A note on data

The database includes row-level accident records (`accidents` table) --
this is **open municipal data** (Comune di Roma's public traffic-accident
dataset) with no personal or vehicle-identifying information: no names, no
license plates, no addresses, just the circumstances of each crash (when,
where, what kind, weather, injuries). It's filtered here to the 3 study
neighborhoods and to 2017-2021 (2020 excluded -- see
[`docs/techniques.md`](docs/techniques.md)), matching the rest of the
project's scope. See [`database/README.md`](database/README.md) for the
exact schema and column list.

## Author

Giuseppe Abbate -- Computer Science, Politecnico di Milano.
