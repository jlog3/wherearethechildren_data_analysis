###############################################################################
#  Substance-Removal Linkages to External Factors
#  -----------------------------------------------
#  Links AFCARS substance-related child removals to:
#    • CDC WONDER overdose death rates
#    • Opioid prescription rates (CDC / IQVIA)
#    • Poverty & unemployment (ACS via tidycensus)
#  at the state–year level.
#
#  Outputs:
#    1. Merged analytic dataset  (CSV + .rds)
#    2. Correlation heatmap
#    3. Scatterplot panels
#    4. Multivariate regression tables (HTML & text)
#    5. Lagged-effect models
#    6. Faceted state maps ("opioid hotspot" dashboard)
###############################################################################

# ── 0. Packages ──────────────────────────────────────────────────────────────
required_pkgs <- c(
  "tidyverse", "readr", "haven", "janitor",
  "tidycensus",            # ACS poverty / unemployment
  "tigris",                # state shapefiles
  "sf",                    # spatial operations
  "corrplot",              # correlation heatmap
  "ggcorrplot",            # ggplot-based heatmap
  "broom",                 # tidy regression output
  "fixest",                # fast fixed-effects & lagged models
  "modelsummary",          # publication-ready tables
  "patchwork",             # plot composition
  "viridis",               # colour scales
  "scales"                 # axis formatters
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(required_pkgs, install_if_missing))
invisible(lapply(required_pkgs, library, character.only = TRUE))

options(tigris_use_cache = TRUE)

# ── Output directory ─────────────────────────────────────────────────────────
out_dir <- "output/substance_linkage"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("══════════════════════════════════════════════════════════════════\n")
cat("  Substance-Removal ↔ External-Factor Linkage Pipeline\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

###############################################################################
# SECTION 1 — DATA INGESTION & PREPARATION
###############################################################################

# ── 1A. AFCARS substance-related removals ────────────────────────────────────
#
# Expected input: a CSV/RDS with columns like
#   state_fips | state_abbr | year | n_substance_removals | total_removals
#
# If you are starting from raw AFCARS foster-care files, build this upstream
# by filtering on removal reason = "drug abuse (parent)" / "alcohol abuse
# (parent)" and aggregating to state-year.  A synthetic generator follows
# for demonstration.

build_synthetic_afcars <- function(years = 2015:2022) {
  set.seed(42)
  fips_tbl <- tigris::fips_codes %>%
    distinct(state, state_code, state_name) %>%
    filter(!state_code %in% c("60","66","69","72","78")) %>%
    rename(state_abbr = state, state_fips = state_code)

  crossing(fips_tbl, year = years) %>%
    mutate(
      total_removals     = round(runif(n(), 800, 12000)),
      n_substance_removals = round(total_removals * runif(n(), 0.20, 0.55)),
      substance_pct      = n_substance_removals / total_removals * 100
    )
}

# Try to read real data; fall back to synthetic
afcars_path <- "data/afcars_substance_state_year.csv"
if (file.exists(afcars_path)) {
  afcars <- read_csv(afcars_path, show_col_types = FALSE) %>% clean_names()
  cat("✓ AFCARS data read from", afcars_path, "\n")
} else {
  cat("ℹ No AFCARS file found — generating synthetic demonstration data.\n")
  afcars <- build_synthetic_afcars()
}

cat("  Rows:", nrow(afcars), " | Years:", paste(range(afcars$year), collapse = "–"), "\n\n")

# ── 1B. CDC WONDER overdose deaths ──────────────────────────────────────────
#
# Download from https://wonder.cdc.gov  →  Multiple-Cause-of-Death
# Group by: State, Year.  ICD-10 codes X40–X44, X60–X64, X85, Y10–Y14.
# The export typically has columns: State, Year, Deaths, Population, Crude Rate.

build_synthetic_overdose <- function(years = 2015:2022) {
  set.seed(43)
  state_tbl <- tigris::fips_codes %>%
    distinct(state, state_code, state_name) %>%
    filter(!state_code %in% c("60","66","69","72","78")) %>%
    rename(state_abbr = state, state_fips = state_code)

  crossing(state_tbl, year = years) %>%
    mutate(
      od_deaths        = round(runif(n(), 50, 5500)),
      state_pop        = round(runif(n(), 500000, 40000000)),
      od_rate_per100k  = od_deaths / state_pop * 1e5
    )
}

overdose_path <- "data/cdc_overdose_state_year.csv"
if (file.exists(overdose_path)) {
  overdose <- read_csv(overdose_path, show_col_types = FALSE) %>% clean_names()
  cat("✓ CDC overdose data read from", overdose_path, "\n")
} else {
  cat("ℹ No overdose file found — generating synthetic data.\n")
  overdose <- build_synthetic_overdose()
}

# ── 1C. Opioid prescription rates ───────────────────────────────────────────
#
# Source: CDC's "U.S. Opioid Dispensing Rate Maps"
# (https://www.cdc.gov/drugoverdose/rxrate-maps/)
# Columns: State, Year, Prescriptions_per_100

build_synthetic_rxrate <- function(years = 2015:2022) {
  set.seed(44)
  state_tbl <- tigris::fips_codes %>%
    distinct(state, state_code, state_name) %>%
    filter(!state_code %in% c("60","66","69","72","78")) %>%
    rename(state_abbr = state, state_fips = state_code)

  crossing(state_tbl, year = years) %>%
    mutate(rx_rate_per100 = round(runif(n(), 25, 110), 1))
}

rx_path <- "data/opioid_rx_rate_state_year.csv"
if (file.exists(rx_path)) {
  rx_rate <- read_csv(rx_path, show_col_types = FALSE) %>% clean_names()
  cat("✓ Opioid Rx data read from", rx_path, "\n")
} else {
  cat("ℹ No Rx-rate file found — generating synthetic data.\n")
  rx_rate <- build_synthetic_rxrate()
}

# ── 1D. ACS poverty & unemployment via tidycensus ───────────────────────────
#
# B17001_002 = persons below poverty level
# B17001_001 = total for whom poverty status is determined
# B23025_005 = unemployed (civilian labor force)
# B23025_003 = civilian labor force total
#
# Requires a Census API key:
#   tidycensus::census_api_key("YOUR_KEY", install = TRUE)

pull_acs_indicators <- function(years = 2015:2022) {
  acs_vars <- c(
    pov_num   = "B17001_002",
    pov_denom = "B17001_001",
    unemp_num = "B23025_005",
    unemp_den = "B23025_003"
  )

  # tidycensus requires one call per year for ACS-1
  map_dfr(years, function(yr) {
    tryCatch({
      get_acs(
        geography = "state",
        variables = acs_vars,
        year      = yr,
        survey    = "acs1",
        output    = "wide"
      ) %>%
        transmute(
          state_fips   = GEOID,
          state_name   = NAME,
          year         = yr,
          poverty_pct  = pov_numE / pov_denomE * 100,
          unemp_pct    = unemp_numE / unemp_denE * 100
        )
    }, error = function(e) {
      message("  ACS pull failed for ", yr, ": ", conditionMessage(e))
      tibble()
    })
  })
}

acs_path <- "data/acs_poverty_unemployment.csv"
if (file.exists(acs_path)) {
  acs <- read_csv(acs_path, show_col_types = FALSE) %>% clean_names()
  cat("✓ ACS data read from", acs_path, "\n")
} else {
  cat("ℹ Attempting ACS pull via tidycensus … ")
  acs <- tryCatch(pull_acs_indicators(), error = function(e) tibble())
  if (nrow(acs) == 0) {
    cat("failed — generating synthetic ACS data.\n")
    set.seed(45)
    state_tbl <- tigris::fips_codes %>%
      distinct(state, state_code, state_name) %>%
      filter(!state_code %in% c("60","66","69","72","78")) %>%
      rename(state_abbr = state, state_fips = state_code)
    acs <- crossing(state_tbl, year = 2015:2022) %>%
      mutate(
        poverty_pct = round(runif(n(), 7, 22), 1),
        unemp_pct   = round(runif(n(), 2.5, 9.5), 1)
      )
  } else {
    cat("success.\n")
  }
}

###############################################################################
# SECTION 2 — MERGE INTO ANALYTIC DATASET
###############################################################################

cat("\n── Merging datasets ─────────────────────────────────────────────\n")

# Standardise key columns across sources
standardize_fips <- function(df) {
  df %>% mutate(state_fips = str_pad(as.character(state_fips), 2, "left", "0"))
}

afcars   <- standardize_fips(afcars)
overdose <- standardize_fips(overdose)
rx_rate  <- standardize_fips(rx_rate)
acs      <- standardize_fips(acs)

analytic <- afcars %>%
  left_join(
    overdose %>% select(state_fips, year, od_deaths, od_rate_per100k),
    by = c("state_fips", "year")
  ) %>%
  left_join(
    rx_rate %>% select(state_fips, year, rx_rate_per100 = rx_rate_per100),
    by = c("state_fips", "year")
  ) %>%
  left_join(
    acs %>% select(state_fips, year, poverty_pct, unemp_pct),
    by = c("state_fips", "year")
  ) %>%
  arrange(state_fips, year)

# Create 1-year lags for lagged analysis (grouped by state)
analytic <- analytic %>%
  group_by(state_fips) %>%
  arrange(year) %>%
  mutate(
    lag1_od_rate      = lag(od_rate_per100k, 1),
    lag1_rx_rate      = lag(rx_rate_per100,  1),
    lag1_poverty      = lag(poverty_pct,     1),
    lag1_unemp        = lag(unemp_pct,       1),
    lag2_od_rate      = lag(od_rate_per100k, 2),
    lag2_rx_rate      = lag(rx_rate_per100,  2),
    delta_substance   = substance_pct - lag(substance_pct, 1),
    delta_od_rate     = od_rate_per100k - lag(od_rate_per100k, 1)
  ) %>%
  ungroup()

# Save merged dataset
write_csv(analytic, file.path(out_dir, "analytic_substance_linkage.csv"))
saveRDS(analytic,   file.path(out_dir, "analytic_substance_linkage.rds"))
cat("✓ Analytic dataset saved — ", nrow(analytic), " rows,",
    ncol(analytic), "columns.\n\n")

###############################################################################
# SECTION 3 — CORRELATION ANALYSIS & HEATMAP
###############################################################################

cat("── Correlation analysis ─────────────────────────────────────────\n")

cor_vars <- analytic %>%
  select(
    `Substance Removal %`  = substance_pct,
    `Overdose Rate`        = od_rate_per100k,
    `Opioid Rx Rate`       = rx_rate_per100,
    `Poverty %`            = poverty_pct,
    `Unemployment %`       = unemp_pct
  ) %>%
  drop_na()

cor_mat  <- cor(cor_vars, use = "pairwise.complete.obs")
cor_pmat <- cor_pmat(cor_vars)

cat("  Pearson correlation matrix:\n")
print(round(cor_mat, 3))

# Heatmap
p_heatmap <- ggcorrplot(
  cor_mat,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 4,
  p.mat    = cor_pmat,
  insig    = "pch",
  pch      = 4,
  pch.cex  = 3,
  colors   = c("#2166AC", "white", "#B2182B"),
  title    = "Correlation Matrix: Substance Removals & External Factors",
  ggtheme  = theme_minimal(base_size = 13)
) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(out_dir, "correlation_heatmap.png"),
       p_heatmap, width = 8, height = 7, dpi = 300)
cat("✓ Correlation heatmap saved.\n\n")

###############################################################################
# SECTION 4 — SCATTERPLOT PANELS
###############################################################################

cat("── Scatterplots ────────────────────────────────────────────────\n")

scatter_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    strip.text   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

make_scatter <- function(df, xvar, xlab, title_suffix) {
  ggplot(df, aes(.data[[xvar]], substance_pct)) +
    geom_point(aes(colour = factor(year)), alpha = 0.55, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey20",
                linewidth = 0.8, linetype = "dashed") +
    scale_colour_viridis_d(name = "Year", option = "plasma") +
    labs(
      x     = xlab,
      y     = "Substance-Related Removal %",
      title = paste("Substance Removals vs.", title_suffix)
    ) +
    scatter_theme
}

p_od  <- make_scatter(analytic, "od_rate_per100k",
                       "Overdose Deaths per 100 k", "Overdose Death Rate")
p_rx  <- make_scatter(analytic, "rx_rate_per100",
                       "Opioid Rx per 100 Persons", "Opioid Prescription Rate")
p_pov <- make_scatter(analytic, "poverty_pct",
                       "Poverty %", "Poverty Rate")
p_ue  <- make_scatter(analytic, "unemp_pct",
                       "Unemployment %", "Unemployment Rate")

p_scatter_grid <- (p_od | p_rx) / (p_pov | p_ue) +
  plot_annotation(
    title    = "Substance-Related Removals vs. External Risk Factors (State–Year)",
    subtitle = "Each point = one state–year; dashed line = OLS fit",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, colour = "grey40")
    )
  )

ggsave(file.path(out_dir, "scatterplot_panel.png"),
       p_scatter_grid, width = 14, height = 10, dpi = 300)
cat("✓ Scatterplot panel saved.\n\n")

###############################################################################
# SECTION 5 — MULTIVARIATE REGRESSIONS
###############################################################################

cat("── Multivariate regressions ────────────────────────────────────\n")

# Model 1: Pooled OLS
m1_ols <- lm(
  substance_pct ~ od_rate_per100k + rx_rate_per100 + poverty_pct + unemp_pct,
  data = analytic
)

# Model 2: State fixed effects (within-estimator via fixest)
m2_fe <- feols(
  substance_pct ~ od_rate_per100k + rx_rate_per100 + poverty_pct + unemp_pct
    | state_fips,
  data = analytic
)

# Model 3: State + Year fixed effects
m3_feyr <- feols(
  substance_pct ~ od_rate_per100k + rx_rate_per100 + poverty_pct + unemp_pct
    | state_fips + year,
  data = analytic
)

# Model 4: Lagged predictors (t-1) + state FE
m4_lag <- feols(
  substance_pct ~ lag1_od_rate + lag1_rx_rate + lag1_poverty + lag1_unemp
    | state_fips,
  data = analytic
)

# Model 5: Lagged predictors (t-2) + state FE
m5_lag2 <- feols(
  substance_pct ~ lag2_od_rate + lag2_rx_rate + lag1_poverty + lag1_unemp
    | state_fips,
  data = analytic
)

# Model 6: Change-on-change (first differences)
m6_delta <- lm(
  delta_substance ~ delta_od_rate,
  data = analytic %>% filter(!is.na(delta_substance))
)

models <- list(
  "Pooled OLS"        = m1_ols,
  "State FE"          = m2_fe,
  "State + Year FE"   = m3_feyr,
  "Lag-1 + State FE"  = m4_lag,
  "Lag-2 + State FE"  = m5_lag2,
  "First Differences" = m6_delta
)

# Console summary
modelsummary(models, fmt = 3, stars = c("*" = .1, "**" = .05, "***" = .01))

# Save as HTML
modelsummary(
  models,
  fmt       = 3,
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  title     = "Multivariate Models: Substance-Related Removal % → External Factors",
  notes     = c("Source: AFCARS, CDC WONDER, CDC Rx Maps, ACS.",
                 "State & year FE absorbed where indicated."),
  output    = file.path(out_dir, "regression_table.html")
)

# Also save plain-text version
sink(file.path(out_dir, "regression_table.txt"))
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  Regression Results — Substance Removals ↔ External Factors\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
modelsummary(models, fmt = 3, stars = c("*" = .1, "**" = .05, "***" = .01),
             output = "default")
sink()

cat("✓ Regression tables saved (HTML + TXT).\n\n")

###############################################################################
# SECTION 6 — LAGGED ANALYSIS VISUALISATION
###############################################################################

cat("── Lagged-effect coefficient plot ──────────────────────────────\n")

lag_coefs <- bind_rows(
  tidy(m2_fe,  conf.int = TRUE) %>% mutate(model = "Contemp."),
  tidy(m4_lag, conf.int = TRUE) %>% mutate(model = "Lag-1"),
  tidy(m5_lag2, conf.int = TRUE) %>% mutate(model = "Lag-2")
) %>%
  filter(str_detect(term, "od_rate|rx_rate|poverty|unemp")) %>%
  mutate(
    predictor = case_when(
      str_detect(term, "od_rate")  ~ "Overdose Rate",
      str_detect(term, "rx_rate")  ~ "Opioid Rx Rate",
      str_detect(term, "poverty")  ~ "Poverty %",
      str_detect(term, "unemp")   ~ "Unemployment %"
    )
  )

p_lagcoef <- ggplot(lag_coefs, aes(x = estimate, y = predictor,
                                    colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.5), size = 0.7) +
  scale_colour_manual(values = c("Contemp." = "#1b9e77",
                                  "Lag-1"    = "#d95f02",
                                  "Lag-2"    = "#7570b3"),
                      name = "Timing") +
  labs(
    title    = "Effect of External Factors on Substance-Removal % (State FE)",
    subtitle = "Contemporaneous vs. 1- and 2-year lagged predictors",
    x = "Coefficient estimate (95 % CI)", y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_dir, "lagged_coefficient_plot.png"),
       p_lagcoef, width = 10, height = 5.5, dpi = 300)
cat("✓ Lagged coefficient plot saved.\n\n")

###############################################################################
# SECTION 7 — FACETED STATE MAPS  ("Opioid Hotspot" Dashboard)
###############################################################################

cat("── Faceted maps (opioid-hotspot dashboard) ─────────────────────\n")

# Get state shapefile
states_sf <- tigris::states(cb = TRUE, year = 2020) %>%
  filter(!STATEFP %in% c("60","66","69","72","78")) %>%
  shift_geometry() %>%
  select(state_fips = STATEFP, geometry)

# Latest available year for mapping
latest_yr <- max(analytic$year, na.rm = TRUE)

map_data <- states_sf %>%
  left_join(
    analytic %>% filter(year == latest_yr),
    by = "state_fips"
  )

# Compute composite "hotspot score" (z-score average of key indicators)
map_data <- map_data %>%
  mutate(
    z_substance = as.numeric(scale(substance_pct)),
    z_od        = as.numeric(scale(od_rate_per100k)),
    z_rx        = as.numeric(scale(rx_rate_per100)),
    z_poverty   = as.numeric(scale(poverty_pct)),
    hotspot_idx = (z_substance + z_od + z_rx + z_poverty) / 4
  )

# Shared map theme
map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.key.width  = unit(1.2, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

make_map <- function(sf_df, fill_var, title, palette = "YlOrRd", label = "") {
  ggplot(sf_df) +
    geom_sf(aes(fill = .data[[fill_var]]), colour = "grey70", linewidth = 0.15) +
    scale_fill_viridis_c(option = palette, name = label, na.value = "grey90") +
    labs(title = title) +
    map_theme
}

m1 <- make_map(map_data, "substance_pct",
               "Substance-Related Removal %", "magma", "%")
m2 <- make_map(map_data, "od_rate_per100k",
               "Overdose Death Rate (per 100 k)", "inferno", "Rate")
m3 <- make_map(map_data, "rx_rate_per100",
               "Opioid Rx Rate (per 100)", "plasma", "Rate")
m4 <- make_map(map_data, "hotspot_idx",
               "Composite Opioid-Hotspot Index", "rocket", "Z-score")

p_maps <- (m1 | m2) / (m3 | m4) +
  plot_annotation(
    title    = paste("Opioid-Hotspot Dashboard —", latest_yr),
    subtitle = "Legislative urgency: States in darkest shading need targeted prevention investment",
    caption  = "Sources: AFCARS, CDC WONDER, CDC Rx Maps, ACS | Hotspot = avg z-score across indicators",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "grey30"),
      plot.caption  = element_text(size = 8, colour = "grey50")
    )
  )

ggsave(file.path(out_dir, "opioid_hotspot_dashboard.png"),
       p_maps, width = 16, height = 10, dpi = 300)
cat("✓ Faceted hotspot map saved.\n\n")

###############################################################################
# SECTION 8 — TREND SPARKLINES BY TOP-10 HOTSPOT STATES
###############################################################################

cat("── Top-10 hotspot state trends ─────────────────────────────────\n")

top10_states <- map_data %>%
  st_drop_geometry() %>%
  slice_max(hotspot_idx, n = 10) %>%
  pull(state_fips)

top10_trends <- analytic %>%
  filter(state_fips %in% top10_states) %>%
  select(state_abbr, year, substance_pct, od_rate_per100k, rx_rate_per100) %>%
  pivot_longer(cols = c(substance_pct, od_rate_per100k, rx_rate_per100),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
    substance_pct   = "Substance Removal %",
    od_rate_per100k = "Overdose Rate per 100 k",
    rx_rate_per100  = "Opioid Rx per 100"
  ))

p_trends <- ggplot(top10_trends, aes(year, value, colour = metric)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  facet_wrap(~ state_abbr, scales = "free_y", ncol = 5) +
  scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(
    title    = "Trend Lines for Top-10 Opioid-Hotspot States",
    subtitle = "Substance removals, overdose deaths & Rx rates over time",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    strip.text    = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(out_dir, "hotspot_trends_top10.png"),
       p_trends, width = 14, height = 7, dpi = 300)
cat("✓ Top-10 hotspot trends saved.\n\n")

###############################################################################
# SECTION 9 — YEAR-OVER-YEAR CHANGE MAP (DELTA)
###############################################################################

cat("── Year-over-year change map ───────────────────────────────────\n")

yoy_data <- analytic %>%
  filter(year %in% c(latest_yr - 1, latest_yr)) %>%
  select(state_fips, year, substance_pct) %>%
  pivot_wider(names_from = year, values_from = substance_pct,
              names_prefix = "yr_") %>%
  mutate(
    pct_change = .[[3]] - .[[2]]   # latest minus previous
  )

yoy_sf <- states_sf %>%
  left_join(yoy_data, by = "state_fips")

p_yoy <- ggplot(yoy_sf) +
  geom_sf(aes(fill = pct_change), colour = "grey70", linewidth = 0.15) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "Δ pp"
  ) +
  labs(
    title    = paste0("Change in Substance-Removal % (",
                       latest_yr - 1, " → ", latest_yr, ")"),
    subtitle = "Red = worsening; Blue = improving",
    caption  = "Source: AFCARS-derived removal rates"
  ) +
  map_theme +
  theme(plot.title = element_text(size = 14))

ggsave(file.path(out_dir, "yoy_change_map.png"),
       p_yoy, width = 10, height = 6.5, dpi = 300)
cat("✓ Year-over-year change map saved.\n\n")

###############################################################################
# SECTION 10 — LEGISLATIVE URGENCY SUMMARY TABLE
###############################################################################

cat("── Legislative urgency summary ─────────────────────────────────\n")

urgency <- map_data %>%
  st_drop_geometry() %>%
  select(state_abbr, state_name, substance_pct, od_rate_per100k,
         rx_rate_per100, poverty_pct, hotspot_idx) %>%
  arrange(desc(hotspot_idx)) %>%
  mutate(
    rank = row_number(),
    urgency_tier = case_when(
      hotspot_idx >= 1.0  ~ "CRITICAL",
      hotspot_idx >= 0.5  ~ "HIGH",
      hotspot_idx >= 0.0  ~ "MODERATE",
      TRUE                ~ "LOWER"
    )
  ) %>%
  select(rank, state_abbr, state_name, urgency_tier, hotspot_idx,
         substance_pct, od_rate_per100k, rx_rate_per100, poverty_pct)

write_csv(urgency, file.path(out_dir, "legislative_urgency_ranking.csv"))

cat("  Top-15 states by urgency:\n")
print(head(urgency, 15), n = 15)

###############################################################################
# SECTION 11 — SESSION INFO & MANIFEST
###############################################################################

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  Pipeline complete.  All outputs in:", normalizePath(out_dir), "\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# List outputs
cat("Output manifest:\n")
list.files(out_dir, full.names = FALSE) %>%
  walk(~ cat("  •", .x, "\n"))

cat("\nFiles produced:\n")
cat("  DATA:   analytic_substance_linkage.csv / .rds\n")
cat("  DATA:   legislative_urgency_ranking.csv\n")
cat("  TABLE:  regression_table.html / .txt\n")
cat("  VIS:    correlation_heatmap.png\n")
cat("  VIS:    scatterplot_panel.png\n")
cat("  VIS:    lagged_coefficient_plot.png\n")
cat("  VIS:    opioid_hotspot_dashboard.png\n")
cat("  VIS:    hotspot_trends_top10.png\n")
cat("  VIS:    yoy_change_map.png\n")
