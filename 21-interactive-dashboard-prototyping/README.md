# Child Welfare Transparency Dashboard — Prototype

A Shiny (shinydashboard) prototype that simulates the proposed federal public
real-time dashboards for four focal child welfare metrics:

1. **Substance-Related Removals** — opioid/meth/alcohol-driven entries
2. **Infant Foster Care Entries** — neonatal, substance-exposed flags
3. **Missing/Runaway Episodes** — trafficking flags, resolution tracking
4. **In-Care Maltreatment** — perpetrator, severity, substantiation

## Quick Start (Local)
```bash
# 1. Install R ≥ 4.3 and RStudio (recommended)

# 2. Install packages (run once in R console)
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders", "shinyWidgets",
  "plotly", "leaflet", "DT", "dplyr", "tidyr", "lubridate",
  "scales", "htmltools", "sf", "tigris", "networkD3",
  "RColorBrewer"
))

# 3. Generate simulated data
Rscript generate_sample_data.R

# 4. Launch dashboard
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

## Deploy to shinyapps.io
```bash
# 1. Install rsconnect
# install.packages("rsconnect")

# 2. Configure your account (one-time)
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# 3. Generate data locally first
source("generate_sample_data.R")

# 4. Deploy (include data/ folder)
rsconnect::deployApp(
  appDir  = ".",
  appName = "child-welfare-dashboard",
  appFiles = c("app.R", list.files("data", full.names = TRUE))
)
```

## Deploy via Docker
```dockerfile
FROM rocker/shiny:4.3.2
RUN install2.r shinydashboard shinycssloaders shinyWidgets \
    plotly leaflet DT dplyr tidyr lubridate scales htmltools \
    sf tigris networkD3 RColorBrewer
COPY . /srv/shiny-server/dashboard/
RUN cd /srv/shiny-server/dashboard && Rscript generate_sample_data.R
EXPOSE 3838
CMD ["/init"]
```

## Connecting Real Data

The dashboard is designed for drop-in replacement of simulated data:

| Simulated RDS | Real Source | Key Columns |
|---|---|---|
| `substance_removals.rds` | AFCARS Foster Care File | state, year, n_substance, rate_per_100k |
| `infant_entries.rds` | AFCARS Foster Care File | state, year, n_infant, rate_per_1000 |
| `missing_episodes.rds` | AFCARS Foster Care File | state, year, n_episodes, n_trafficking |
| `incare_maltreatment.rds` | NCANDS Child File | state, year, n_incidents, n_substantiated |
| `state_year_population.rds` | Census ACS | state, year, child_pop, fips |
| `episode_level.rds` | Combined ETL | episode_id, metric, state, report_date, demographics |

Replace `readRDS()` calls in `app.R` with your database connections or API
calls. The reactive filter functions will work unchanged as long as column
names match.

## Architecture Notes

- **Single-file app** (`app.R`) for easy deployment; split into `ui.R` /
  `server.R` / `global.R` if the codebase grows
- **Modular helpers** (`filter_episodes`, `make_trend_plot`, `make_choropleth`,
  `make_dt`) can be extracted into an R package for reuse
- **Choropleth maps** use `tigris::states()` for shapefiles (cached after first
  download); production should pre-cache or bundle the shapefile
- **Sankey diagram** via `networkD3::sankeyNetwork` shows placement →
  maltreatment type → severity flow
- All charts export to PNG via plotly's built-in camera button
- CSV downloads available on every data table tab




Key Design Decisions
Modularity for real data swap-in: Every readRDS() call at the top of app.R is a clearly marked injection point. The helper functions (filter_episodes, make_trend_plot, make_choropleth, make_dt) are schema-driven — they work on any dataframe with the documented column names, so swapping simulated RDS files for live AFCARS/NCANDS database queries requires changing only the data-loading block.
Legislative annotations: Each metric tab includes a styled callout box linking the visualization to specific legislation (FFPSA, CARA, P.L. 113-183, CAPTA), and the time-series charts carry pinned annotations at policy-relevant years.
Interactive layers: Plotly for hover/zoom/PNG export on all charts; Leaflet for state-level drill-down choropleths; DT with built-in CSV/Excel export buttons; networkD3 Sankey for the placement → maltreatment → severity flow on the in-care maltreatment tab.
To get running: save the three files, run Rscript generate_sample_data.R, then shiny::runApp(".").

