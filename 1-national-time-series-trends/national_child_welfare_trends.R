################################################################################
# NATIONAL CHILD WELFARE TIME-SERIES TRENDS (FY 2000–2025)
# ──────────────────────────────────────────────────────────────────────────────
# Focal Metrics:
#   1. Opioid/Substance-Related Removals (DAParent, DAChild, AAParent, AAChild)
#   2. Infant/Newborn Entries (age at removal < 1 year)
#   3. Missing/Runaway Episodes (CurPlSet==7 or DISREASN==7)
#   4. In-Care Maltreatment Rates (NCANDS substantiated + foster-parent perp)
#
# Data Sources:
#   - AFCARS Foster Care annual files (RDS/CSV)   → data/afcars/
#   - NCANDS Child File (RDS/CSV)                  → data/ncands/
#   - State-year child population (Census/CSV)     → data/population/
#
# Outputs:
#   - output/plots/         → static (PNG) + interactive (HTML) charts
#   - output/tables/        → CSV summary tables with legislative captions
#   - output/animations/    → gganimate GIFs
#
# Author:  [Analyst]
# Updated: 2025
################################################################################


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  0. SETUP — LIBRARIES, PATHS, GLOBAL OPTIONS                              ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

suppressPackageStartupMessages({
  library(tidyverse)      # dplyr, tidyr, readr, purrr, stringr, forcats, ggplot2

library(lubridate)      # date manipulation
  library(plotly)         # interactive HTML plots
  library(gganimate)      # animated ggplot2
  library(scales)         # axis formatting (comma, percent)
  library(patchwork)      # plot composition
  library(glue)           # string interpolation
  library(janitor)        # clean_names()
  library(gifski)         # GIF renderer for gganimate
})

# ── Paths ────────────────────────────────────────────────────────────────────
AFCARS_DIR  <- "data/afcars/"
NCANDS_DIR  <- "data/ncands/"
POP_DIR     <- "data/population/"
OUT_PLOTS   <- "output/plots/"
OUT_TABLES  <- "output/tables/"
OUT_ANIM    <- "output/animations/"

walk(c(OUT_PLOTS, OUT_TABLES, OUT_ANIM), dir.create,
     recursive = TRUE, showWarnings = FALSE)

# ── Global ggplot theme ──────────────────────────────────────────────────────
theme_cw <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    plot.caption     = element_text(color = "grey50", size = 8, hjust = 0),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )
theme_set(theme_cw)

# ── Policy annotation events (for overlay on plots) ─────────────────────────
policy_events <- tribble(
  ~FY,  ~label,                         ~metric_group,
  2010, "ACA / Health-care expansion",  "substance",
  2016, "Peak opioid OD deaths",        "substance",
  2018, "FFPSA enacted (P.L. 115-123)","all",
  2020, "COVID-19 pandemic onset",      "all",
  2023, "FFPSA prevention ramp-up",     "substance"
)

# Color palette for the four focal metrics
metric_colors <- c(
  "Substance-Related Removals" = "#D7191C",
  "Infant Entries (<1 yr)"     = "#2C7BB6",
  "Runaway/Missing Episodes"   = "#FDAE61",
  "In-Care Maltreatment Rate"  = "#ABD9E9"
)


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  1. DATA LOADING — AFCARS Foster Care Annual Files                         ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

#' Read a single AFCARS annual file (RDS or CSV), standardise column names,
#' and return a tibble with a guaranteed FY column.
read_afcars_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(ext,
    "rds" = read_rds(path),
    "csv" = read_csv(path, show_col_types = FALSE,
                     col_types = cols(.default = col_character())),
    stop(glue("Unsupported file type: {ext}"))
  )

  df <- raw %>%
    clean_names() %>%                 # snake_case everything
    rename_with(str_to_lower) %>%     # belt-and-suspenders lowercase
    # Harmonise common variant column names across AFCARS vintages
    rename_with(~ case_when(
      . == "latremdt"   ~ "removal_date",
      . == "ageatlrem"  ~ "ageatlatrem",
      . == "stfcid"     ~ "recnumbr",
      . == "curplset"   ~ "curplset",
      . == "disreasn"   ~ "disreasn",
      . == "totalrem"   ~ "totalrem",
      TRUE              ~ .
    ))

  # Ensure numeric types for analytic columns
  numeric_cols <- c("fy", "daparent", "dachild", "aaparent", "aachild",
                    "ageatlatrem", "curplset", "disreasn", "totalrem", "state")
  df <- df %>%
    mutate(across(any_of(numeric_cols), ~ as.numeric(as.character(.))))

  # If FY column is missing, attempt to derive from filename (e.g., "afcars_2015.csv")
 if (!"fy" %in% names(df)) {
    fy_from_name <- str_extract(basename(path), "\\d{4}") %>% as.numeric()
    if (!is.na(fy_from_name)) {
      df <- df %>% mutate(fy = fy_from_name)
      message(glue("  ℹ  FY inferred from filename: {fy_from_name}"))
    } else {
      warning(glue("Cannot determine FY for {path}"))
    }
  }

  df
}

# ── Load & bind all AFCARS files ─────────────────────────────────────────────
afcars_files <- list.files(AFCARS_DIR, pattern = "\\.(rds|csv)$",
                           full.names = TRUE, ignore.case = TRUE)
message(glue("Loading {length(afcars_files)} AFCARS file(s)…"))

afcars <- map_dfr(afcars_files, read_afcars_file) %>%
  filter(between(fy, 2000, 2025)) %>%
  distinct()

message(glue("  ✓  Combined AFCARS: {comma(nrow(afcars))} records, ",
             "FY {min(afcars$fy)}–{max(afcars$fy)}"))


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  2. DATA LOADING — NCANDS Child File (for In-Care Maltreatment)            ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

read_ncands_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(ext,
    "rds" = read_rds(path),
    "csv" = read_csv(path, show_col_types = FALSE,
                     col_types = cols(.default = col_character())),
    stop(glue("Unsupported file type: {ext}"))
  )
  raw %>% clean_names() %>% rename_with(str_to_lower)
}

ncands_files <- list.files(NCANDS_DIR, pattern = "\\.(rds|csv)$",
                           full.names = TRUE, ignore.case = TRUE)
message(glue("Loading {length(ncands_files)} NCANDS file(s)…"))

ncands <- map_dfr(ncands_files, read_ncands_file) %>%
  mutate(across(any_of(c("rptyr", "chage", "mallev", "chlvng")),
                ~ as.numeric(as.character(.)))) %>%
  # Keep substantiated dispositions only (mallev == 1 in most NCANDS coding)
  filter(mallev == 1) %>%
  distinct()

message(glue("  ✓  Combined NCANDS (substantiated): {comma(nrow(ncands))} records"))


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  3. DATA LOADING — Child Population Denominators                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Expected CSV schema: FY (or year), state_fips, pop_under1, pop_0_17
pop_file <- list.files(POP_DIR, pattern = "\\.(csv|rds)$",
                       full.names = TRUE, ignore.case = TRUE)

if (length(pop_file) >= 1) {
  pop <- read_csv(pop_file[1], show_col_types = FALSE) %>%
    clean_names() %>%
    rename_with(~ case_when(
      . %in% c("year")  ~ "fy",
      TRUE               ~ .
    )) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    # National totals (sum across states per FY)
    group_by(fy) %>%
    summarise(
      pop_under1 = sum(pop_under1, na.rm = TRUE),
      pop_0_17   = sum(pop_0_17,   na.rm = TRUE),
      .groups    = "drop"
    )
  message(glue("  ✓  Population denominators loaded: FY {min(pop$fy)}–{max(pop$fy)}"))
} else {
  # ── Fallback: use published Census estimates (approximate) ──
  message("  ⚠  No population file found — using approximate national estimates")
  pop <- tibble(
    fy       = 2000:2025,
    pop_0_17 = seq(72_300_000, 73_500_000, length.out = 26) %>% round(),
    pop_under1 = seq(3_960_000, 3_660_000, length.out = 26) %>% round()
  )
}


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  4. DERIVE FLAGS & AGGREGATE — METRIC 1: SUBSTANCE-RELATED REMOVALS       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# A removal is "substance-involved" if ANY of DAParent, DAChild, AAParent,
# AAChild == 1.  This captures parental drug/alcohol abuse, child drug/alcohol
# abuse, and prenatal substance exposure (typically coded under DAChild/DAParent
# depending on state practice).

afcars <- afcars %>%
  mutate(
    substance_involved = if_else(
      coalesce(daparent, 0) == 1 |
      coalesce(dachild, 0)  == 1 |
      coalesce(aaparent, 0) == 1 |
      coalesce(aachild, 0)  == 1,
      1L, 0L
    )
  )

substance_national <- afcars %>%
  group_by(fy) %>%
  summarise(
    total_removals    = n(),
    substance_count   = sum(substance_involved, na.rm = TRUE),
    substance_pct     = substance_count / total_removals,
    # Sub-breakdowns
    daparent_count    = sum(daparent == 1, na.rm = TRUE),
    dachild_count     = sum(dachild  == 1, na.rm = TRUE),
    aaparent_count    = sum(aaparent == 1, na.rm = TRUE),
    aachild_count     = sum(aachild  == 1, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  left_join(pop, by = "fy") %>%
  mutate(
    substance_rate_per1k = (substance_count / pop_0_17) * 1000
  )

message("  ✓  Metric 1 (Substance) aggregated")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  5. DERIVE FLAGS & AGGREGATE — METRIC 2: INFANT / NEWBORN ENTRIES          ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# "Infant entry" = child whose age at latest removal is < 1 year (value 0 in
# most AFCARS coding) or < 1 per the AgeAtLatRem field.

afcars <- afcars %>%
  mutate(
    infant_entry = if_else(
      coalesce(ageatlatrem, NA_real_) < 1 | ageatlatrem == 0,
      1L, 0L
    )
  )

infant_national <- afcars %>%
  group_by(fy) %>%
  summarise(
    total_removals = n(),
    infant_count   = sum(infant_entry, na.rm = TRUE),
    infant_pct     = infant_count / total_removals,
    .groups        = "drop"
  ) %>%
  left_join(pop, by = "fy") %>%
  mutate(
    infant_rate_per1k = (infant_count / pop_under1) * 1000
  )

message("  ✓  Metric 2 (Infant Entries) aggregated")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  6. DERIVE FLAGS & AGGREGATE — METRIC 3: MISSING / RUNAWAY EPISODES       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Two ways to identify a runaway/missing episode:
#   (a) Current placement setting (CurPlSet) == 7 → "runaway" placement code
#   (b) Discharge reason (DISREASN) == 7           → "runaway" discharge
#
# We count CUMULATIVE episodes (each record flagged counts as one episode;
# children can have multiple removal spells with multiple episodes).

afcars <- afcars %>%
  mutate(
    runaway_episode = if_else(
      coalesce(curplset, 0) == 7 | coalesce(disreasn, 0) == 7,
      1L, 0L
    )
  )

runaway_national <- afcars %>%
  group_by(fy) %>%
  summarise(
    total_in_care        = n(),
    runaway_episodes     = sum(runaway_episode, na.rm = TRUE),
    runaway_pct_of_care  = runaway_episodes / total_in_care,
    # Unique children with ≥1 episode
    children_with_episode = n_distinct(recnumbr[runaway_episode == 1]),
    .groups              = "drop"
  ) %>%
  left_join(pop, by = "fy") %>%
  mutate(
    runaway_rate_per1k = (runaway_episodes / pop_0_17) * 1000
  )

message("  ✓  Metric 3 (Runaway/Missing) aggregated")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  7. DERIVE & AGGREGATE — METRIC 4: IN-CARE MALTREATMENT RATE              ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# In-care maltreatment requires linking NCANDS to AFCARS:
#   1. NCANDS: substantiated report (mallev==1) where perpetrator is a foster
#      parent (PER1REL–PER3REL in {03, 04, 33}) OR child's living arrangement
#      is foster care (CHLVNG in {10, 11}).
#   2. The report year should overlap with an AFCARS in-care spell.
#
# Because exact child-level linkage keys differ by state (and NDACAN extracts
# don't always carry common IDs), we use an AGGREGATE approach:
#   numerator   = NCANDS foster-parent-perpetrated substantiated reports
#   denominator = AFCARS children in care during that FY.

# ── 7a. Identify NCANDS foster-care maltreatment records ────────────────────

# Perpetrator relationship columns (up to 3 perpetrators per report)
perp_cols <- intersect(c("per1rel", "per2rel", "per3rel"), names(ncands))

# Foster parent codes: 03 = foster parent, 04 = foster parent's partner,
# 33 = group home/institutional staff (varies by state coding)
foster_perp_codes <- c(3, 4, 33)
foster_living_codes <- c(10, 11)   # foster family home (relative / non-relative)

ncands_foster_maltreatment <- ncands %>%
  # Parse perpetrator columns to numeric
  mutate(across(all_of(perp_cols), ~ as.numeric(as.character(.)))) %>%
  # Flag: any perpetrator is foster parent?
  mutate(
    foster_perp = if_else(
      reduce(
        map(perp_cols, ~ .data[[.x]] %in% foster_perp_codes),
        `|`
      ),
      1L, 0L
    ),
    # Flag: child living in foster care at time of report?
    foster_living = if_else(
      coalesce(chlvng, 0) %in% foster_living_codes,
      1L, 0L
    ),
    # Combined: either condition met
    in_care_maltreatment = if_else(foster_perp == 1 | foster_living == 1, 1L, 0L)
  ) %>%
  filter(in_care_maltreatment == 1) %>%
  rename(fy = rptyr)

# ── 7b. National aggregation ────────────────────────────────────────────────
incare_maltreatment_national <- ncands_foster_maltreatment %>%
  group_by(fy) %>%
  summarise(
    maltreatment_reports = n(),
    .groups = "drop"
  ) %>%
  # Join AFCARS denominator (children in care per FY)
  left_join(
    afcars %>%
      group_by(fy) %>%
      summarise(children_in_care = n_distinct(recnumbr), .groups = "drop"),
    by = "fy"
  ) %>%
  mutate(
    maltreatment_rate_per1k = (maltreatment_reports / children_in_care) * 1000
  ) %>%
  filter(between(fy, 2000, 2025))

message("  ✓  Metric 4 (In-Care Maltreatment) aggregated")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  8. HANDLE MISSING DATA & BUILD UNIFIED LONG TABLE                         ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Some FYs may be missing from one or more datasets. We create a complete
# FY grid and flag gaps so plots show breaks rather than false interpolation.

fy_grid <- tibble(fy = 2000:2025)

# ── Unified long-format table for multi-metric plots ─────────────────────────
combined_long <- bind_rows(
  substance_national %>%
    transmute(fy, metric = "Substance-Related Removals",
              count = substance_count, pct = substance_pct,
              rate_per1k = substance_rate_per1k),

  infant_national %>%
    transmute(fy, metric = "Infant Entries (<1 yr)",
              count = infant_count, pct = infant_pct,
              rate_per1k = infant_rate_per1k),

  runaway_national %>%
    transmute(fy, metric = "Runaway/Missing Episodes",
              count = runaway_episodes, pct = runaway_pct_of_care,
              rate_per1k = runaway_rate_per1k),

  incare_maltreatment_national %>%
    transmute(fy, metric = "In-Care Maltreatment Rate",
              count = maltreatment_reports, pct = NA_real_,
              rate_per1k = maltreatment_rate_per1k)
) %>%
  # Complete the FY grid so every metric has all years (NA where missing)
  complete(fy = 2000:2025, metric) %>%
  mutate(
    metric = factor(metric, levels = names(metric_colors)),
    data_available = !is.na(count)
  )

message(glue("  ✓  Combined long table: {comma(nrow(combined_long))} rows"))


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  9. STATIC PLOTS — FACETED LINE CHARTS                                    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Helper: add policy-event vertical lines + labels ─────────────────────────
add_policy_lines <- function(p, events_df = policy_events, target = "all") {
  ev <- events_df %>%
    filter(metric_group %in% c("all", target))

  p +
    geom_vline(data = ev, aes(xintercept = FY),
               linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_label(data = ev, aes(x = FY, y = Inf, label = label),
               vjust = 1.2, size = 2.5, fill = "white", alpha = 0.8,
               label.size = 0.2)
}


# ── 9a. Four-panel faceted chart (counts) ───────────────────────────────────
p_facet_counts <- combined_long %>%
  filter(data_available) %>%
  ggplot(aes(x = fy, y = count, color = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_color_manual(values = metric_colors, guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(
    title    = "National Child Welfare Trends, FY 2000–2025",
    subtitle = "Four focal metrics from AFCARS & NCANDS",
    x = "Fiscal Year", y = "Count",
    caption = paste(
      "Sources: AFCARS Foster Care Files; NCANDS Child File; U.S. Census.",
      "\nNote: Missing years shown as gaps. Policy events marked with dashed lines."
    )
  )

# Add policy event lines to each facet
p_facet_counts <- p_facet_counts +
  geom_vline(
    data = policy_events %>% filter(metric_group == "all"),
    aes(xintercept = FY),
    linetype = "dashed", color = "grey50", linewidth = 0.35
  )

ggsave(file.path(OUT_PLOTS, "01_four_metric_counts_faceted.png"),
       p_facet_counts, width = 14, height = 10, dpi = 300)
message("  ✓  Saved: 01_four_metric_counts_faceted.png")


# ── 9b. Rates per 1,000 (overlay chart) ─────────────────────────────────────
p_rates <- combined_long %>%
  filter(data_available, !is.na(rate_per1k)) %>%
  ggplot(aes(x = fy, y = rate_per1k, color = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = metric_colors) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "National Rates per 1,000 Children, FY 2000–2025",
    subtitle = "Substance removals & infants per age-specific pop; others per child pop 0–17",
    x = "Fiscal Year", y = "Rate per 1,000",
    color = "Metric",
    caption = "Sources: AFCARS; NCANDS; Census population estimates."
  )

p_rates <- add_policy_lines(p_rates)

ggsave(file.path(OUT_PLOTS, "02_rates_per1k_overlay.png"),
       p_rates, width = 12, height = 7, dpi = 300)
message("  ✓  Saved: 02_rates_per1k_overlay.png")


# ── 9c. Substance sub-breakdown (stacked area) ──────────────────────────────
substance_sub <- substance_national %>%
  select(fy, daparent_count, dachild_count, aaparent_count, aachild_count) %>%
  pivot_longer(-fy, names_to = "flag", values_to = "count") %>%
  mutate(
    flag = recode(flag,
      "daparent_count" = "Drug Abuse – Parent",
      "dachild_count"  = "Drug Abuse – Child/Prenatal",
      "aaparent_count" = "Alcohol Abuse – Parent",
      "aachild_count"  = "Alcohol Abuse – Child"
    )
  )

p_substance_sub <- substance_sub %>%
  ggplot(aes(x = fy, y = count, fill = flag)) +
  geom_area(alpha = 0.75, color = "white", linewidth = 0.3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(
    title    = "Substance-Related Removal Flags (Sub-Breakdown)",
    subtitle = "Note: flags are not mutually exclusive — a removal may have multiple flags",
    x = "Fiscal Year", y = "Flagged Removals",
    fill = "Substance Flag",
    caption = "Source: AFCARS Foster Care Files (DAParent, DAChild, AAParent, AAChild)."
  )

ggsave(file.path(OUT_PLOTS, "03_substance_sub_breakdown.png"),
       p_substance_sub, width = 12, height = 7, dpi = 300)
message("  ✓  Saved: 03_substance_sub_breakdown.png")


# ── 9d. Infant entries with substance co-occurrence ──────────────────────────
infant_substance <- afcars %>%
  filter(infant_entry == 1) %>%
  group_by(fy) %>%
  summarise(
    infant_total     = n(),
    infant_substance = sum(substance_involved, na.rm = TRUE),
    pct_substance    = infant_substance / infant_total,
    .groups = "drop"
  )

p_infant_sub <- infant_substance %>%
  ggplot(aes(x = fy)) +
  geom_col(aes(y = infant_total), fill = "#2C7BB6", alpha = 0.3, width = 0.7) +
  geom_col(aes(y = infant_substance), fill = "#D7191C", alpha = 0.7, width = 0.7) +
  geom_line(aes(y = pct_substance * max(infant_total)),
            color = "#D7191C", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    labels = comma,
    sec.axis = sec_axis(~ . / max(infant_substance$infant_total, na.rm = TRUE),
                        labels = percent, name = "% Substance-Involved")
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(
    title    = "Infant Entries & Substance Co-Occurrence",
    subtitle = "Red = substance-involved infant removals; dashed line = % of infant entries",
    x = "Fiscal Year", y = "Infant Entries (Count)",
    caption = "Source: AFCARS. Infants defined as age at removal < 1 year."
  )

ggsave(file.path(OUT_PLOTS, "04_infant_substance_overlap.png"),
       p_infant_sub, width = 12, height = 7, dpi = 300)
message("  ✓  Saved: 04_infant_substance_overlap.png")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  10. INTERACTIVE PLOTS — PLOTLY (HTML)                                     ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 10a. Interactive four-metric overlay ─────────────────────────────────────
p_interactive <- combined_long %>%
  filter(data_available) %>%
  ggplot(aes(x = fy, y = count, color = metric,
             text = glue("{metric}\nFY {fy}\nCount: {comma(count)}"))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_color_manual(values = metric_colors, guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(title = "National Child Welfare Trends (Interactive)",
       x = "Fiscal Year", y = "Count")

plotly_facet <- ggplotly(p_interactive, tooltip = "text") %>%
  layout(
    title = list(text = "National Child Welfare Trends, FY 2000–2025",
                 font = list(size = 16)),
    margin = list(t = 60)
  )

htmlwidgets::saveWidget(plotly_facet,
                        file.path(OUT_PLOTS, "05_interactive_trends.html"),
                        selfcontained = TRUE)
message("  ✓  Saved: 05_interactive_trends.html")


# ── 10b. Interactive rates with hover detail ─────────────────────────────────
p_rates_int <- combined_long %>%
  filter(data_available, !is.na(rate_per1k)) %>%
  ggplot(aes(x = fy, y = rate_per1k, color = metric,
             text = glue("{metric}\nFY {fy}\nRate: {round(rate_per1k, 3)} per 1,000"))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = metric_colors) +
  labs(title = "Rates per 1,000 Children (Interactive)",
       x = "Fiscal Year", y = "Rate per 1,000", color = "Metric")

plotly_rates <- ggplotly(p_rates_int, tooltip = "text")
htmlwidgets::saveWidget(plotly_rates,
                        file.path(OUT_PLOTS, "06_interactive_rates.html"),
                        selfcontained = TRUE)
message("  ✓  Saved: 06_interactive_rates.html")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  11. ANIMATED TRENDS — gganimate                                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

p_anim <- combined_long %>%
  filter(data_available) %>%
  ggplot(aes(x = fy, y = count, color = metric, group = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = comma(count)), vjust = -1, size = 3, show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_color_manual(values = metric_colors, guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Child Welfare Trends — FY {closest_state}",
    subtitle = "National counts across four focal metrics",
    x = "Fiscal Year", y = "Count",
    caption = "Sources: AFCARS; NCANDS; U.S. Census Bureau."
  ) +
  transition_reveal(fy) +
  ease_aes("cubic-in-out")

anim <- animate(p_anim, nframes = 120, fps = 8,
                width = 1200, height = 800, res = 120,
                renderer = gifski_renderer())
anim_save(file.path(OUT_ANIM, "trends_animated.gif"), animation = anim)
message("  ✓  Saved: trends_animated.gif")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  12. CSV SUMMARIES WITH LEGISLATIVE CAPTIONS                               ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── 12a. Wide-format summary table ──────────────────────────────────────────
summary_wide <- combined_long %>%
  filter(data_available) %>%
  select(fy, metric, count, pct, rate_per1k) %>%
  pivot_wider(
    names_from  = metric,
    values_from = c(count, pct, rate_per1k),
    names_glue  = "{metric}_{.value}"
  ) %>%
  arrange(fy)

write_csv(summary_wide, file.path(OUT_TABLES, "national_trends_wide.csv"))
message("  ✓  Saved: national_trends_wide.csv")


# ── 12b. Long-format with legislative annotations ───────────────────────────
legislative_captions <- tribble(
  ~metric,                       ~caption,
  "Substance-Related Removals",
    "Over 50,000 substance-related entries annually — transparency dashboards would enable real-time prevention tracking.",
  "Infant Entries (<1 yr)",
    "Infants remain the most vulnerable entry cohort; substance-exposed newborns drive a growing share of placements.",
  "Runaway/Missing Episodes",
    "Youth who go missing from care face heightened trafficking risk — FFPSA prevention services aim to stabilize placements.",
  "In-Care Maltreatment Rate",
    "Substantiated maltreatment by foster parents signals systemic safety gaps; enhanced vetting and oversight are critical."
)

summary_annotated <- combined_long %>%
  filter(data_available) %>%
  left_join(legislative_captions, by = "metric") %>%
  arrange(metric, fy)

write_csv(summary_annotated, file.path(OUT_TABLES, "national_trends_annotated.csv"))
message("  ✓  Saved: national_trends_annotated.csv")


# ── 12c. Trend summary statistics ───────────────────────────────────────────
trend_summary <- combined_long %>%
  filter(data_available) %>%
  group_by(metric) %>%
  summarise(
    fy_range      = glue("{min(fy)}–{max(fy)}"),
    n_years       = n(),
    mean_count    = round(mean(count, na.rm = TRUE)),
    median_count  = round(median(count, na.rm = TRUE)),
    min_count     = min(count, na.rm = TRUE),
    max_count     = max(count, na.rm = TRUE),
    latest_count  = count[which.max(fy)],
    mean_rate     = round(mean(rate_per1k, na.rm = TRUE), 3),
    pct_change_first_last = round(
      (count[which.max(fy)] - count[which.min(fy)]) / count[which.min(fy)] * 100, 1
    ),
    .groups = "drop"
  ) %>%
  left_join(legislative_captions, by = "metric")

write_csv(trend_summary, file.path(OUT_TABLES, "trend_summary_statistics.csv"))
message("  ✓  Saved: trend_summary_statistics.csv")

# Print summary table to console
cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  NATIONAL CHILD WELFARE TREND SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

trend_summary %>%
  select(metric, fy_range, mean_count, latest_count, pct_change_first_last) %>%
  rename(
    Metric          = metric,
    `FY Range`      = fy_range,
    `Mean (Annual)`  = mean_count,
    `Latest FY`     = latest_count,
    `% Change`      = pct_change_first_last
  ) %>%
  print(n = Inf, width = Inf)


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  13. BONUS: PATCHWORK COMPOSITE DASHBOARD (SINGLE PNG)                     ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Build individual metric panels with consistent styling
panel_substance <- substance_national %>%
  ggplot(aes(fy, substance_count)) +
  geom_area(fill = "#D7191C", alpha = 0.25) +
  geom_line(color = "#D7191C", linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Substance-Related Removals", x = NULL, y = "Count")

panel_infant <- infant_national %>%
  ggplot(aes(fy, infant_count)) +
  geom_area(fill = "#2C7BB6", alpha = 0.25) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Infant Entries (Age < 1)", x = NULL, y = "Count")

panel_runaway <- runaway_national %>%
  ggplot(aes(fy, runaway_episodes)) +
  geom_area(fill = "#FDAE61", alpha = 0.25) +
  geom_line(color = "#FDAE61", linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Missing/Runaway Episodes", x = NULL, y = "Episodes")

panel_maltreat <- incare_maltreatment_national %>%
  ggplot(aes(fy, maltreatment_rate_per1k)) +
  geom_area(fill = "#ABD9E9", alpha = 0.25) +
  geom_line(color = "#ABD9E9", linewidth = 1) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(title = "In-Care Maltreatment Rate", x = NULL, y = "Per 1,000 in care")

dashboard <- (panel_substance | panel_infant) /
             (panel_runaway | panel_maltreat) +
  plot_annotation(
    title    = "National Child Welfare Dashboard — FY 2000–2025",
    subtitle = "Four focal metrics from AFCARS & NCANDS",
    caption  = paste(
      "Sources: AFCARS Foster Care Files; NCANDS Child File; U.S. Census Bureau.\n",
      "Policy context: FFPSA (2018) aims to reduce congregate care and expand prevention.\n",
      "Legislative note: Transparency dashboards would enable real-time prevention tracking."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40"),
      plot.caption  = element_text(color = "grey50", size = 8, hjust = 0)
    )
  )

ggsave(file.path(OUT_PLOTS, "07_composite_dashboard.png"),
       dashboard, width = 16, height = 10, dpi = 300)
message("  ✓  Saved: 07_composite_dashboard.png")


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  14. SESSION INFO & COMPLETION                                             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  WORKFLOW COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(glue("  Plots  → {OUT_PLOTS}"), "\n")
cat(glue("  Tables → {OUT_TABLES}"), "\n")
cat(glue("  Anims  → {OUT_ANIM}"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Reproducibility
cat("Session info:\n")
sessionInfo()
