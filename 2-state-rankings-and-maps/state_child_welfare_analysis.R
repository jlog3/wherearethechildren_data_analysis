#!/usr/bin/env Rscript
# =============================================================================
# STATE-BY-STATE CHILD WELFARE RANKINGS, RATES & VARIATION ANALYSIS
# =============================================================================
# Metrics:
#   1. Substance-related removals (AFCARS)
#   2. Infant entries into foster care (AFCARS)
#   3. Runaway episodes (AFCARS)
#   4. In-care maltreatment (NCANDS)
#
# Outputs: Choropleth maps (interactive Plotly HTML + static PNG),
#          ranked bar/lollipop charts (top/bottom 10), heatmaps,
#          coefficient-of-variation tables, CSV summaries with policy hooks.
# =============================================================================

# ── 0. LIBRARIES ─────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(plotly)
  library(viridis)
  library(scales)
  library(ggrepel)
  library(htmlwidgets)
  library(tigris)        # Census TIGER shapefiles
  library(patchwork)     # combine ggplots
  library(knitr)
  library(kableExtra)
})

options(tigris_use_cache = TRUE, tigris_class = "sf")

# ── 1. CONFIGURATION ────────────────────────────────────────────────────────
OUT_DIR   <- "outputs"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

LATEST_FY   <- 2023
FY_RANGE     <- 2019:2023
RATE_DENOM   <- 1000          # rates per 1,000 children

# FIPS-to-state lookup (50 states + DC)
STATE_FIPS <- tibble(
  state_fips = sprintf("%02d", c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,
                                  20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                  34,35,36,37,38,39,40,41,42,44,45,46,47,48,
                                  49,50,51,53,54,55,56)),
  state_abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
                  "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN",
                  "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                  "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
                  "WV","WI","WY"),
  state_name = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                  "Connecticut","Delaware","District of Columbia","Florida",
                  "Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
                  "Kentucky","Louisiana","Maine","Maryland","Massachusetts",
                  "Michigan","Minnesota","Mississippi","Missouri","Montana",
                  "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                  "New York","North Carolina","North Dakota","Ohio","Oklahoma",
                  "Oregon","Pennsylvania","Rhode Island","South Carolina",
                  "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia",
                  "Washington","West Virginia","Wisconsin","Wyoming")
)

# ── 2. SIMULATED INPUT DATA ─────────────────────────────────────────────────
# In production, replace these blocks with actual file reads:
#   afcars  <- read_csv("afcars_state_metrics.csv")
#   ncands  <- read_csv("ncands_state_metrics.csv")
#   pop     <- read_csv("census_child_population.csv")
# The structures below mirror official AFCARS/NCANDS aggregation outputs.

set.seed(42)

# 2a. Census child population (under 18) by state and FY
census_pop <- expand_grid(state_fips = STATE_FIPS$state_fips, fy = FY_RANGE) %>%
  left_join(STATE_FIPS, by = "state_fips") %>%
  mutate(
    base_pop = case_when(
      state_abbr == "CA" ~ 8800000,
      state_abbr == "TX" ~ 7400000,
      state_abbr == "FL" ~ 4400000,
      state_abbr == "NY" ~ 4100000,
      state_abbr == "IL" ~ 2800000,
      state_abbr == "PA" ~ 2600000,
      state_abbr == "OH" ~ 2600000,
      state_abbr == "WY" ~ 130000,
      state_abbr == "VT" ~ 115000,
      state_abbr == "DC" ~ 120000,
      TRUE ~ runif(1, 200000, 2500000)
    ),
    child_pop = round(base_pop * (1 + (fy - 2019) * runif(1, -0.005, 0.005)))
  ) %>%
  select(state_fips, state_abbr, state_name, fy, child_pop)

# 2b. AFCARS-derived state-level counts
# Structure: state_fips | fy | substance_removals | infant_entries | runaway_episodes
afcars_data <- census_pop %>%
  select(state_fips, state_abbr, state_name, fy, child_pop) %>%
  mutate(
    # Rates vary realistically; higher-burden states get elevated counts
    subst_rate   = runif(n(), 0.3, 3.5),
    infant_rate  = runif(n(), 0.15, 2.0),
    runaway_rate = runif(n(), 0.05, 0.8),
    substance_removals = round(child_pop * subst_rate / 1000),
    infant_entries     = round(child_pop * infant_rate / 1000),
    runaway_episodes   = round(child_pop * runaway_rate / 1000)
  ) %>%
  select(state_fips, state_abbr, state_name, fy,
         substance_removals, infant_entries, runaway_episodes)

# 2c. NCANDS-derived in-care maltreatment counts
ncands_data <- census_pop %>%
  select(state_fips, state_abbr, state_name, fy, child_pop) %>%
  mutate(
    malt_rate = runif(n(), 0.02, 0.45),
    incare_maltreatment = round(child_pop * malt_rate / 1000)
  ) %>%
  select(state_fips, state_abbr, state_name, fy, incare_maltreatment)

# ── 3. MERGE & COMPUTE RATES ────────────────────────────────────────────────
combined <- afcars_data %>%
  left_join(ncands_data, by = c("state_fips","state_abbr","state_name","fy")) %>%
  left_join(census_pop %>% select(state_fips, fy, child_pop),
            by = c("state_fips","fy")) %>%
  mutate(
    rate_substance  = substance_removals / child_pop * RATE_DENOM,
    rate_infant     = infant_entries     / child_pop * RATE_DENOM,
    rate_runaway    = runaway_episodes   / child_pop * RATE_DENOM,
    rate_maltreat   = incare_maltreatment/ child_pop * RATE_DENOM
  )

# Latest fiscal year snapshot
latest <- combined %>% filter(fy == LATEST_FY)

# Multi-year averages (FY_RANGE)
multi_yr_avg <- combined %>%
  group_by(state_fips, state_abbr, state_name) %>%
  summarise(across(starts_with("rate_"), mean, .names = "avg_{.col}"),
            across(c(substance_removals, infant_entries,
                     runaway_episodes, incare_maltreatment), mean,
                   .names = "avg_{.col}"),
            avg_child_pop = mean(child_pop),
            n_years = n(),
            .groups = "drop")

# ── 4. VARIATION STATISTICS ──────────────────────────────────────────────────
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
iqr_ratio <- function(x) IQR(x, na.rm = TRUE) / median(x, na.rm = TRUE)

rate_cols <- c("rate_substance","rate_infant","rate_runaway","rate_maltreat")
rate_labels <- c("Substance Removals","Infant Entries",
                 "Runaway Episodes","In-Care Maltreatment")

variation_stats <- tibble(
  metric = rate_labels,
  mean_rate   = map_dbl(rate_cols, ~mean(latest[[.x]], na.rm = TRUE)),
  median_rate = map_dbl(rate_cols, ~median(latest[[.x]], na.rm = TRUE)),
  sd_rate     = map_dbl(rate_cols, ~sd(latest[[.x]], na.rm = TRUE)),
  min_rate    = map_dbl(rate_cols, ~min(latest[[.x]], na.rm = TRUE)),
  max_rate    = map_dbl(rate_cols, ~max(latest[[.x]], na.rm = TRUE)),
  range_ratio = max_rate / min_rate,
  cv          = map_dbl(rate_cols, ~cv(latest[[.x]])),
  iqr_ratio   = map_dbl(rate_cols, ~iqr_ratio(latest[[.x]]))
) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

# Multi-year CV per state (within-state temporal variation)
temporal_cv <- combined %>%
  group_by(state_fips, state_abbr, state_name) %>%
  summarise(across(all_of(rate_cols), cv, .names = "cv_{.col}"), .groups = "drop")

cat("\n=== Interstate Variation Summary (Latest FY", LATEST_FY, ") ===\n")
print(variation_stats)

# ── 5. SPATIAL DATA ──────────────────────────────────────────────────────────
# Fetch US states shapefile; shift AK/HI for display
us_geo <- states(cb = TRUE, resolution = "20m", year = 2022) %>%
  shift_geometry() %>%
  rename(state_fips = STATEFP) %>%
  filter(state_fips %in% STATE_FIPS$state_fips)

# Join latest rates
map_data <- us_geo %>%
  left_join(latest, by = "state_fips")

# Join multi-year averages
map_data_avg <- us_geo %>%
  left_join(multi_yr_avg, by = "state_fips")

# ── 6. HELPER: CHOROPLETH MAP BUILDER ───────────────────────────────────────
build_choropleth <- function(sf_data, fill_col, title, legend_lab,
                             palette = "YlOrRd", save_prefix) {
  # Static ggplot version
  p <- ggplot(sf_data) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "grey40", linewidth = 0.15) +
    scale_fill_viridis_c(option = "inferno", direction = -1,
                         name = legend_lab,
                         labels = comma_format(accuracy = 0.01)) +
    labs(title = title,
         subtitle = paste0("Rate per ", format(RATE_DENOM, big.mark = ","),
                           " children"),
         caption = "Source: AFCARS / NCANDS / Census | Simulated data") +
    theme_void(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )

  ggsave(file.path(OUT_DIR, paste0(save_prefix, "_map.png")),
         p, width = 11, height = 7, dpi = 300, bg = "white")

  # Interactive Plotly version
  # Need centroid coordinates for hover; convert sf to data frame
  centroids <- st_centroid(sf_data) %>%
    mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry()

  fig <- plot_geo(centroids, locationmode = "USA-states") %>%
    add_trace(
      z         = ~get(fill_col),
      locations = ~state_abbr,
      color     = ~get(fill_col),
      colors    = "YlOrRd",
      text      = ~paste0(state_name, "\nRate: ",
                           round(get(fill_col), 2), " per 1k children"),
      hoverinfo = "text",
      marker    = list(line = list(color = "grey", width = 0.5))
    ) %>%
    colorbar(title = legend_lab) %>%
    layout(
      title = list(text = title, x = 0.5),
      geo   = list(scope = "usa",
                   projection = list(type = "albers usa"),
                   showlakes = FALSE)
    )

  saveWidget(fig, file.path(OUT_DIR, paste0(save_prefix, "_map.html")),
             selfcontained = TRUE)

  return(list(gg = p, plotly = fig))
}

# ── 7. HELPER: LOLLIPOP / BAR RANKING CHARTS ────────────────────────────────
build_ranking_chart <- function(df, rate_col, title_text, fill_color,
                                save_prefix, n_show = 10) {

  ranked <- df %>%
    arrange(desc(.data[[rate_col]])) %>%
    mutate(rank = row_number())

  top    <- ranked %>% slice_head(n = n_show)
  bottom <- ranked %>% slice_tail(n = n_show)

  make_lollipop <- function(data, direction, tag) {
    subtitle <- if (direction == "top") {
      paste("Highest", n_show, "states")
    } else {
      paste("Lowest", n_show, "states")
    }

    ordered <- if (direction == "top") {
      data %>% arrange(.data[[rate_col]])
    } else {
      data %>% arrange(desc(.data[[rate_col]]))
    }

    ordered <- ordered %>%
      mutate(state_label = paste0(state_abbr, " (#", rank, ")"),
             state_label = factor(state_label, levels = state_label))

    ggplot(ordered, aes(x = .data[[rate_col]], y = state_label)) +
      geom_segment(aes(x = 0, xend = .data[[rate_col]],
                       y = state_label, yend = state_label),
                   color = "grey60", linewidth = 0.6) +
      geom_point(size = 4, color = fill_color) +
      geom_text(aes(label = round(.data[[rate_col]], 2)),
                hjust = -0.3, size = 3.2, fontface = "bold") +
      labs(title = title_text, subtitle = subtitle,
           x = paste("Rate per", format(RATE_DENOM, big.mark = ","), "children"),
           y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40"),
        panel.grid.major.y = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  }

  p_top    <- make_lollipop(top,    "top",    "top")
  p_bottom <- make_lollipop(bottom, "bottom", "bottom")

  combined <- p_top + p_bottom +
    plot_annotation(caption = paste0("FY ", LATEST_FY,
                    " | Source: AFCARS/NCANDS/Census | Simulated"))

  ggsave(file.path(OUT_DIR, paste0(save_prefix, "_rankings.png")),
         combined, width = 14, height = 7, dpi = 300, bg = "white")

  return(combined)
}

# ── 8. HELPER: HEATMAP (ALL STATES × ALL METRICS) ───────────────────────────
build_heatmap <- function(df, rate_cols, rate_labels, save_prefix, title) {
  heat_data <- df %>%
    select(state_abbr, all_of(rate_cols)) %>%
    pivot_longer(-state_abbr, names_to = "metric", values_to = "rate") %>%
    mutate(metric = factor(metric, levels = rate_cols, labels = rate_labels))

  # Z-score within each metric for comparable color scale
  heat_data <- heat_data %>%
    group_by(metric) %>%
    mutate(z_rate = (rate - mean(rate, na.rm = TRUE)) / sd(rate, na.rm = TRUE)) %>%
    ungroup()

  # Order states by total z-score (highest burden at top)
  state_order <- heat_data %>%
    group_by(state_abbr) %>%
    summarise(total_z = sum(z_rate, na.rm = TRUE)) %>%
    arrange(total_z) %>%
    pull(state_abbr)

  heat_data <- heat_data %>%
    mutate(state_abbr = factor(state_abbr, levels = state_order))

  p <- ggplot(heat_data, aes(x = metric, y = state_abbr, fill = z_rate)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", direction = -1,
                         name = "Z-Score\n(std. devs\nfrom mean)") +
    labs(title = title,
         subtitle = "Z-scores allow cross-metric comparison (higher = more concerning)",
         x = NULL, y = NULL,
         caption = paste0("FY ", LATEST_FY,
                          " | Source: AFCARS/NCANDS/Census | Simulated")) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      axis.text.x     = element_text(angle = 25, hjust = 1, face = "bold"),
      axis.text.y     = element_text(size = 6),
      legend.position = "right",
      panel.grid      = element_blank()
    )

  ggsave(file.path(OUT_DIR, paste0(save_prefix, "_heatmap.png")),
         p, width = 9, height = 16, dpi = 300, bg = "white")
  return(p)
}

# ── 9. GENERATE ALL OUTPUTS ─────────────────────────────────────────────────
cat("\n=== Generating Choropleth Maps ===\n")

metric_info <- tibble(
  rate_col   = rate_cols,
  label      = rate_labels,
  prefix     = c("substance","infant","runaway","maltreatment"),
  color      = c("#D7263D","#F46036","#2E294E","#1B998B")
)

maps   <- list()
ranks  <- list()

for (i in seq_len(nrow(metric_info))) {
  mi <- metric_info[i, ]
  cat("  →", mi$label, "\n")

  # Latest FY choropleth
  maps[[mi$prefix]] <- build_choropleth(
    sf_data     = map_data,
    fill_col    = mi$rate_col,
    title       = paste0(mi$label, " Rate — FY ", LATEST_FY),
    legend_lab  = "Rate / 1k",
    save_prefix = paste0("latest_", mi$prefix)
  )

  # Multi-year average choropleth
  build_choropleth(
    sf_data     = map_data_avg,
    fill_col    = paste0("avg_", mi$rate_col),
    title       = paste0(mi$label, " Rate — ", min(FY_RANGE), "-", max(FY_RANGE),
                         " Average"),
    legend_lab  = "Avg Rate / 1k",
    save_prefix = paste0("multiyear_", mi$prefix)
  )

  # Rankings (lollipop)
  ranks[[mi$prefix]] <- build_ranking_chart(
    df          = latest,
    rate_col    = mi$rate_col,
    title_text  = paste0(mi$label, " — State Rankings"),
    fill_color  = mi$color,
    save_prefix = mi$prefix
  )
}

# Combined heatmap
cat("  → All-metrics heatmap\n")
heatmap_plot <- build_heatmap(
  df          = latest,
  rate_cols   = rate_cols,
  rate_labels = rate_labels,
  save_prefix = "all_metrics",
  title       = paste0("Child Welfare Indicator Heatmap — FY ", LATEST_FY)
)

# ── 10. TEMPORAL TREND SPARKLINES ────────────────────────────────────────────
cat("\n=== Generating Trend Panel ===\n")

trend_long <- combined %>%
  pivot_longer(all_of(rate_cols), names_to = "metric", values_to = "rate") %>%
  mutate(metric = factor(metric, levels = rate_cols, labels = rate_labels))

# National median trend by year
national_trend <- trend_long %>%
  group_by(fy, metric) %>%
  summarise(
    median_rate = median(rate),
    p25 = quantile(rate, 0.25),
    p75 = quantile(rate, 0.75),
    .groups = "drop"
  )

p_trend <- ggplot(national_trend, aes(x = fy)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = median_rate), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = median_rate), color = "steelblue", size = 2) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(title = "National Median Rates with IQR Band",
       subtitle = paste(min(FY_RANGE), "–", max(FY_RANGE)),
       x = "Fiscal Year", y = "Rate per 1,000 children",
       caption = "Shaded band = interquartile range across states") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold"))

ggsave(file.path(OUT_DIR, "national_trend_panel.png"),
       p_trend, width = 12, height = 8, dpi = 300, bg = "white")

# ── 11. COEFFICIENT-OF-VARIATION TREND ───────────────────────────────────────
cv_trend <- combined %>%
  group_by(fy) %>%
  summarise(across(all_of(rate_cols), cv, .names = "cv_{.col}"),
            .groups = "drop") %>%
  pivot_longer(-fy, names_to = "metric", values_to = "cv_value") %>%
  mutate(metric = str_remove(metric, "cv_"),
         metric = factor(metric, levels = rate_cols, labels = rate_labels))

p_cv <- ggplot(cv_trend, aes(x = fy, y = cv_value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Interstate Disparity: Coefficient of Variation Over Time",
       subtitle = "Higher CV = greater state-to-state inequality",
       x = "Fiscal Year", y = "CV", color = "Metric") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

ggsave(file.path(OUT_DIR, "cv_trend.png"),
       p_cv, width = 10, height = 6, dpi = 300, bg = "white")

# ── 12. CSV EXPORTS ─────────────────────────────────────────────────────────
cat("\n=== Writing CSVs ===\n")

# 12a. Full state-level data (all years)
write_csv(combined, file.path(OUT_DIR, "state_all_years.csv"))

# 12b. Latest FY ranked table with policy flags
policy_table <- latest %>%
  select(state_abbr, state_name, child_pop, all_of(rate_cols)) %>%
  mutate(
    # Rank each metric (1 = highest rate = most concerning)
    rank_substance  = rank(-rate_substance),
    rank_infant     = rank(-rate_infant),
    rank_runaway    = rank(-rate_runaway),
    rank_maltreat   = rank(-rate_maltreat),
    # Composite burden index (average rank; lower = worse)
    composite_rank  = (rank_substance + rank_infant +
                       rank_runaway + rank_maltreat) / 4,
    # Policy flags
    flag_substance_high  = rate_substance  > mean(rate_substance)  + sd(rate_substance),
    flag_infant_high     = rate_infant     > mean(rate_infant)     + sd(rate_infant),
    flag_runaway_high    = rate_runaway    > mean(rate_runaway)    + sd(rate_runaway),
    flag_maltreat_high   = rate_maltreat   > mean(rate_maltreat)  + sd(rate_maltreat),
    n_flags              = flag_substance_high + flag_infant_high +
                           flag_runaway_high + flag_maltreat_high,
    policy_hook = case_when(
      n_flags >= 3 ~ "CRITICAL: Multi-domain outlier — prioritize federal TA & monitoring",
      n_flags == 2 ~ "HIGH: Dual-domain concern — recommend targeted review",
      n_flags == 1 ~ "WATCH: Single-domain outlier — monitor next reporting cycle",
      TRUE         ~ "STANDARD: Within expected range"
    )
  ) %>%
  arrange(composite_rank)

write_csv(policy_table, file.path(OUT_DIR, "state_rankings_policy_hooks.csv"))

# 12c. Variation statistics
write_csv(variation_stats, file.path(OUT_DIR, "variation_statistics.csv"))

# 12d. Multi-year averages
write_csv(multi_yr_avg, file.path(OUT_DIR, "state_multiyear_averages.csv"))

# 12e. Temporal CV by state
write_csv(temporal_cv, file.path(OUT_DIR, "temporal_cv_by_state.csv"))

# ── 13. POLICY DISPARITY SUMMARY ────────────────────────────────────────────
cat("\n=== Policy Disparity Summary ===\n")

disparity_report <- list()
for (i in seq_along(rate_cols)) {
  rc <- rate_cols[i]
  rl <- rate_labels[i]
  vals <- latest[[rc]]

  top3 <- latest %>% slice_max(order_by = .data[[rc]], n = 3) %>%
    pull(state_abbr) %>% paste(collapse = ", ")
  bot3 <- latest %>% slice_min(order_by = .data[[rc]], n = 3) %>%
    pull(state_abbr) %>% paste(collapse = ", ")

  line <- sprintf(
    "  %s: Range %.2f–%.2f (%.0fx gap) | CV=%.1f%% | Top: %s | Bottom: %s",
    rl, min(vals), max(vals), max(vals)/min(vals),
    cv(vals)*100, top3, bot3
  )
  cat(line, "\n")
  disparity_report[[rl]] <- line
}

critical_states <- policy_table %>%
  filter(n_flags >= 3) %>%
  pull(state_abbr) %>% paste(collapse = ", ")
cat("\n  CRITICAL multi-domain outlier states:", critical_states, "\n")

# ── 14. INTERACTIVE COMPOSITE DASHBOARD MAP ─────────────────────────────────
cat("\n=== Building Interactive Composite Dashboard ===\n")

composite_map_data <- latest %>%
  st_drop_geometry() %>%
  left_join(policy_table %>% select(state_abbr, composite_rank, n_flags, policy_hook),
            by = "state_abbr")

fig_composite <- plot_geo(composite_map_data, locationmode = "USA-states") %>%
  add_trace(
    z         = ~composite_rank,
    locations = ~state_abbr,
    color     = ~composite_rank,
    colors    = "RdYlGn",
    text      = ~paste0(
      "<b>", state_name, " (", state_abbr, ")</b><br>",
      "Composite Rank: ", round(composite_rank, 1), "<br>",
      "Substance: ", round(rate_substance, 2), "/1k (#", rank(-rate_substance), ")<br>",
      "Infant: ", round(rate_infant, 2), "/1k<br>",
      "Runaway: ", round(rate_runaway, 2), "/1k<br>",
      "Maltreatment: ", round(rate_maltreat, 2), "/1k<br>",
      "Flags: ", n_flags, " | ", policy_hook
    ),
    hoverinfo = "text",
    marker    = list(line = list(color = "grey30", width = 0.5)),
    reversescale = TRUE
  ) %>%
  colorbar(title = "Composite\nRank\n(lower=\nmore\nconcerning)") %>%
  layout(
    title = list(text = paste0("Child Welfare Composite Burden — FY ", LATEST_FY),
                 x = 0.5),
    geo   = list(scope = "usa", projection = list(type = "albers usa"),
                 showlakes = FALSE)
  )

saveWidget(fig_composite,
           file.path(OUT_DIR, "composite_dashboard_map.html"),
           selfcontained = TRUE)

# ── 15. FINAL MANIFEST ──────────────────────────────────────────────────────
cat("\n✓ All outputs saved to:", normalizePath(OUT_DIR), "\n")
cat("\nFiles generated:\n")

output_files <- list.files(OUT_DIR, full.names = FALSE)
for (f in sort(output_files)) {
  size <- file.size(file.path(OUT_DIR, f))
  cat(sprintf("  %-50s %s\n", f, format(size, big.mark = ",")))
}

cat("\n══════════════════════════════════════════════════════════════")
cat("\n  FEDERAL DASHBOARD STANDARDIZATION NOTES")
cat("\n══════════════════════════════════════════════════════════════")
cat("\n  • Rates denominated per 1,000 children (Census ACS estimates)")
cat("\n  • Composite rank averages across 4 metric-specific ranks")
cat("\n  • Policy flags trigger at >1 SD above the national mean")
cat("\n  • CV tracks convergence/divergence of interstate practice")
cat("\n  • Replace simulated data with AFCARS/NCANDS extracts for production")
cat("\n══════════════════════════════════════════════════════════════\n")
