###############################################################################
#
#  AFCARS TRIBAL & SPECIAL POPULATIONS DISPARITY WORKFLOW
#
#  Purpose : Analyse placement, permanency, maltreatment-entry, and
#            re-entry metrics across AI/AN (American Indian / Alaska Native),
#            rural/urban, and—where available—LGBTQ+ subgroups.
#
#  Sovereignty note: This analysis is designed to support Tribal Nations'
#  data-sovereignty interests.  All rates for AI/AN populations should be
#  interpreted within the context of the Indian Child Welfare Act (ICWA),
#  historical federal policy, and Tribal self-governance.  Dashboards built
#  from this workflow should include Tribal consultation language and avoid
#  deficit-only framing.
#
#  Data inputs
#    • AFCARS Foster Care (FC) file   – one row per child-report period
#    • AFCARS Adoption (AD) file      – one row per finalized adoption
#    • RUCA codes (USDA ERS)          – FIPS-to-rural crosswalk
#    • Tribal service area shapefile   – for choropleth mapping
#    • County shapefile (tigris)       – for geographic overlays
#
#  Key AFCARS variables used
#    Race flags  : APTS_AMIAKN, CLINDX_AMIAKN  (AI/AN indicator)
#    County FIPS : STATE, FIPSCODE (county of removal / placement)
#    Placement   : CURPLSET, TOTALREM, DISESSION, RF1AMAKN …
#    Dates       : LATREMDT, DLSTFCDT, AGEADOPT, APTS_* flags
#
#  LGBTQ+ data caveat
#    HHS removed sexual-orientation / gender-identity fields from AFCARS
#    after the 2020 final rule rollback.  This workflow stubs LGBTQ+
#    analysis using any available SOGIE flags (pre-2020 pilot data only)
#    and clearly marks the gap.
#
#  Outputs → /output  folder
#    disparity_tables.csv       – rate & index tables
#    choropleth_tribal.png      – map of AI/AN placement rates
#    trend_lines_population.png – time-series by subgroup
#    interaction_model.txt      – regression summary
#    dashboard_captions.txt     – equity-framed captions for dashboard
#
###############################################################################

# ── 0. SETUP ─────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)        # read SAS / SPSS AFCARS files
  library(janitor)      # clean_names
  library(survey)       # weighted estimates if needed
  library(sf)           # spatial
  library(tigris)       # county shapefiles
  library(viridis)      # colour-blind-safe palettes
  library(patchwork)    # compose plots
  library(broom)        # tidy model output
  library(knitr)        # tables
  library(kableExtra)   # styled tables
  library(scales)       # label helpers
})

options(tigris_use_cache = TRUE)

# ── Paths (adjust to your environment) ───────────────────────────────────────
path_fc    <- "data/afcars_fc.sav"      # Foster Care file (SPSS)
path_ad    <- "data/afcars_ad.sav"      # Adoption file
path_ruca  <- "data/ruca2010revised.csv" # USDA RUCA codes
path_tribal_shp <- "data/tribal_areas.shp" # BIA / Census AIANNH shapefile
out_dir    <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


###############################################################################
# SECTION 1 — DATA INGESTION & HARMONISATION
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 1: DATA INGESTION & HARMONISATION\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── 1a. Read AFCARS ──────────────────────────────────────────────────────────

read_afcars <- function(path) {
  ext <- tools::file_ext(path)
  df <- switch(ext,
    sav  = read_spss(path),
    sas7bdat = read_sas(path),
    csv  = read_csv(path, show_col_types = FALSE),
    tab  = read_tsv(path, show_col_types = FALSE),
    stop("Unsupported file type: ", ext)
  )
  df %>% clean_names()
}

# Uncomment when real files are available:
# fc_raw <- read_afcars(path_fc)
# ad_raw <- read_afcars(path_ad)

# ── SYNTHETIC DEMO DATA (remove when using real AFCARS) ──────────────────────
set.seed(2024)
n_fc <- 12000
n_ad <- 3000
ffy_range <- 2015:2023

fc_raw <- tibble(
  record_id       = 1:n_fc,
  ffy              = sample(ffy_range, n_fc, replace = TRUE),
  state            = sprintf("%02d", sample(c(1:56), n_fc, replace = TRUE)),
  fipscode         = sprintf("%05d", sample(c(1001:56999), n_fc, replace = TRUE)),
  # ── Race flags (AFCARS multi-race coding) ──
  amiakn           = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.85, 0.15)),
  white            = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.45, 0.55)),
  blkafram         = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.75, 0.25)),
  hawaiipi         = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.97, 0.03)),
  hiession         = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.78, 0.22)),
  # ── Placement setting ──
  curplset         = sample(1:6, n_fc, replace = TRUE,
                            prob = c(.40, .25, .12, .10, .08, .05)),
  # ── Outcomes ──
  totalrem         = sample(1:5, n_fc, replace = TRUE, prob = c(.55,.25,.12,.05,.03)),
  disression       = sample(c(1:6, 99), n_fc, replace = TRUE),
  ageadopt         = sample(0:17, n_fc, replace = TRUE),
  los_days         = pmax(1, rnbinom(n_fc, mu = 400, size = 2)),
  re_entry_flag    = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.88, 0.12)),
  malt_entry_flag  = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.30, 0.70)),
  # ── Tribal placement flag (mirrors RF1AMAKN) ──
  rf1amakn         = sample(c(0, 1), n_fc, replace = TRUE, prob = c(0.92, 0.08)),
  # ── LGBTQ+ stub (only in pilot years; NA after 2020) ──
  lgbtq_flag       = ifelse(
    sample(ffy_range, n_fc, replace = TRUE) <= 2020,
    sample(c(0, 1, NA), n_fc, replace = TRUE, prob = c(0.90, 0.05, 0.05)),
    NA_real_
  )
)

ad_raw <- tibble(
  record_id  = (n_fc + 1):(n_fc + n_ad),
  ffy        = sample(ffy_range, n_ad, replace = TRUE),
  state      = sprintf("%02d", sample(1:56, n_ad, replace = TRUE)),
  fipscode   = sprintf("%05d", sample(1001:56999, n_ad, replace = TRUE)),
  amiakn     = sample(c(0, 1), n_ad, replace = TRUE, prob = c(0.87, 0.13)),
  ageadopt   = sample(0:17, n_ad, replace = TRUE),
  time_to_adopt_days = pmax(30, rnbinom(n_ad, mu = 700, size = 2))
)

cat("  Foster Care records :", nrow(fc_raw), "\n")
cat("  Adoption records    :", nrow(ad_raw), "\n")

# ── 1b. Read RUCA crosswalk & build rural/urban flag ─────────────────────────

# Real RUCA file has columns: STATE_COUNTY_FIPS, RUCA1, RUCA2, etc.
# Codes 1-3 = Metropolitan (Urban), 4-6 = Micropolitan, 7-10 = Rural/Small town
# We collapse to a 3-level factor.

# Synthetic RUCA for demo:
ruca <- tibble(
  fipscode = sprintf("%05d", 1001:56999),
  ruca1    = sample(1:10, length(1001:56999), replace = TRUE,
                    prob = c(.25,.15,.10,.10,.08,.07,.06,.07,.06,.06))
) %>%
  mutate(
    rurality = case_when(
      ruca1 %in% 1:3  ~ "Urban/Metro",
      ruca1 %in% 4:6  ~ "Micropolitan",
      ruca1 %in% 7:10 ~ "Rural/Small Town"
    ) %>% factor(levels = c("Urban/Metro", "Micropolitan", "Rural/Small Town"))
  )

cat("  RUCA crosswalk rows :", nrow(ruca), "\n")

# ── 1c. Merge rurality onto AFCARS ──────────────────────────────────────────

fc <- fc_raw %>%
  left_join(ruca %>% select(fipscode, rurality), by = "fipscode") %>%
  mutate(rurality = replace_na(rurality, "Unknown") %>%
           fct_expand("Unknown"))

ad <- ad_raw %>%
  left_join(ruca %>% select(fipscode, rurality), by = "fipscode") %>%
  mutate(rurality = replace_na(rurality, "Unknown") %>%
           fct_expand("Unknown"))

# ── 1d. Derive core subgroup variable ────────────────────────────────────────

fc <- fc %>%
  mutate(
    race_group = case_when(
      amiakn == 1                    ~ "AI/AN",
      blkafram == 1                  ~ "Black/African American",
      hawaiipi == 1                  ~ "NH/PI",
      hiession == 1 & white == 0     ~ "Hispanic",
      white == 1                     ~ "White",
      TRUE                           ~ "Other/Multi"
    ) %>% factor(),
    # Placement type labels
    placement_cat = case_when(
      curplset == 1 ~ "Pre-Adoptive Home",
      curplset == 2 ~ "Foster Family (Relative)",
      curplset == 3 ~ "Foster Family (Non-Relative)",
      curplset == 4 ~ "Group Home",
      curplset == 5 ~ "Institution",
      curplset == 6 ~ "Supervised IL / Other",
      TRUE          ~ "Unknown"
    )
  )

cat("  Race distribution:\n")
print(table(fc$race_group))
cat("\n  Rurality distribution:\n")
print(table(fc$rurality))


###############################################################################
# SECTION 2 — FOUR CORE METRIC CALCULATIONS BY SUBGROUP
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 2: METRIC CALCULATIONS BY SUBGROUP\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# The four metrics:
#   M1  Placement rate (entries per 1 000 child population – proxy here)
#   M2  Permanency rate (exits to reunification, adoption, guardianship)
#   M3  Maltreatment-related entry rate
#   M4  Re-entry rate (return to care within 12 months)

# ── Helper: rate with CI ─────────────────────────────────────────────────────

rate_ci <- function(events, n, per = 1000, conf = 0.95) {
  if (n == 0) return(tibble(rate = NA_real_, lower = NA_real_, upper = NA_real_))
  p <- events / n
  se <- sqrt(p * (1 - p) / n)
  z  <- qnorm(1 - (1 - conf) / 2)
  tibble(
    rate  = round(p * per, 2),
    lower = round(max(0, (p - z * se)) * per, 2),
    upper = round(min(1, (p + z * se)) * per, 2)
  )
}

# ── 2a. By race_group × ffy ─────────────────────────────────────────────────

metrics_by_race <- fc %>%
  group_by(ffy, race_group) %>%
  summarise(
    n_children       = n(),
    n_permanency     = sum(disression %in% c(1, 2, 3)),
    n_malt_entry     = sum(malt_entry_flag == 1),
    n_reentry        = sum(re_entry_flag == 1),
    median_los       = median(los_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    permanency_rate  = round(n_permanency / n_children * 100, 1),
    malt_entry_rate  = round(n_malt_entry / n_children * 1000, 1),
    reentry_rate     = round(n_reentry / n_children * 100, 1)
  )

# ── 2b. By rurality × ffy ───────────────────────────────────────────────────

metrics_by_rural <- fc %>%
  filter(rurality != "Unknown") %>%
  group_by(ffy, rurality) %>%
  summarise(
    n_children       = n(),
    n_permanency     = sum(disression %in% c(1, 2, 3)),
    n_malt_entry     = sum(malt_entry_flag == 1),
    n_reentry        = sum(re_entry_flag == 1),
    median_los       = median(los_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    permanency_rate  = round(n_permanency / n_children * 100, 1),
    malt_entry_rate  = round(n_malt_entry / n_children * 1000, 1),
    reentry_rate     = round(n_reentry / n_children * 100, 1)
  )

# ── 2c. AI/AN × rurality interaction ────────────────────────────────────────

metrics_aian_rural <- fc %>%
  filter(rurality != "Unknown") %>%
  mutate(aian = ifelse(amiakn == 1, "AI/AN", "Non-AI/AN")) %>%
  group_by(ffy, aian, rurality) %>%
  summarise(
    n_children       = n(),
    permanency_rate  = round(sum(disression %in% 1:3) / n() * 100, 1),
    malt_entry_rate  = round(sum(malt_entry_flag) / n() * 1000, 1),
    reentry_rate     = round(sum(re_entry_flag) / n() * 100, 1),
    .groups = "drop"
  )

# ── 2d. LGBTQ+ stub (pre-2020 data only) ────────────────────────────────────

lgbtq_available <- fc %>% filter(!is.na(lgbtq_flag))
lgbtq_note <- paste0(
  "LGBTQ+ DATA AVAILABILITY: ",
  nrow(lgbtq_available), " records (",
  round(nrow(lgbtq_available) / nrow(fc) * 100, 1),
  "% of total) have SOGIE data — limited to FFY ≤ 2020 pilot collections. ",
  "HHS removed SOGIE fields in the 2020 AFCARS final rule rollback. ",
  "Findings below are exploratory and should NOT be generalized."
)
cat(" ", lgbtq_note, "\n\n")

metrics_lgbtq <- lgbtq_available %>%
  mutate(lgbtq = ifelse(lgbtq_flag == 1, "LGBTQ+", "Non-LGBTQ+")) %>%
  group_by(ffy, lgbtq) %>%
  summarise(
    n_children       = n(),
    permanency_rate  = round(sum(disression %in% 1:3) / n() * 100, 1),
    malt_entry_rate  = round(sum(malt_entry_flag) / n() * 1000, 1),
    reentry_rate     = round(sum(re_entry_flag) / n() * 100, 1),
    .groups = "drop"
  )


###############################################################################
# SECTION 3 — DISPARITY INDICES
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 3: DISPARITY INDICES\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Disparity Index (DI) = Subgroup rate / Reference group rate
# Reference group = White for race; Urban/Metro for rurality
# DI > 1 → over-representation; DI < 1 → under-representation

compute_disparity <- function(df, group_var, ref_level, metric_cols) {
  # Get reference rates per year
  ref_rates <- df %>%
    filter(!!sym(group_var) == ref_level) %>%
    select(ffy, all_of(metric_cols)) %>%
    rename_with(~ paste0(.x, "_ref"), all_of(metric_cols))

  df %>%
    left_join(ref_rates, by = "ffy") %>%
    mutate(across(
      all_of(metric_cols),
      ~ round(.x / get(paste0(cur_column(), "_ref")), 3),
      .names = "DI_{.col}"
    )) %>%
    select(-ends_with("_ref"))
}

metric_cols <- c("permanency_rate", "malt_entry_rate", "reentry_rate")

# Race disparity (reference = White)
disp_race <- compute_disparity(metrics_by_race, "race_group", "White", metric_cols)

# Rurality disparity (reference = Urban/Metro)
disp_rural <- compute_disparity(metrics_by_rural, "rurality", "Urban/Metro", metric_cols)

# Show AI/AN disparities as a highlight
aian_disp <- disp_race %>%
  filter(race_group == "AI/AN") %>%
  select(ffy, race_group, starts_with("DI_"))
cat("  AI/AN Disparity Indices (vs White reference):\n")
print(as.data.frame(aian_disp), row.names = FALSE)


###############################################################################
# SECTION 4 — INTERACTION MODELS
###############################################################################

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 4: INTERACTION MODELS (Race × Rurality)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── 4a. Logistic: re-entry ~ AI/AN * rurality + ffy ─────────────────────────

model_data <- fc %>%
  filter(rurality != "Unknown") %>%
  mutate(
    aian        = factor(amiakn, levels = c(0, 1), labels = c("Non-AI/AN", "AI/AN")),
    rurality    = relevel(rurality, ref = "Urban/Metro"),
    ffy_centred = ffy - median(ffy)
  )

mod_reentry <- glm(
  re_entry_flag ~ aian * rurality + ffy_centred,
  data   = model_data,
  family = binomial(link = "logit")
)

cat("  Model 1: Re-Entry ~ AI/AN × Rurality + FFY\n")
print(summary(mod_reentry))

# ── 4b. Linear: median LOS ~ AI/AN * rurality + ffy ─────────────────────────

mod_los <- lm(
  log1p(los_days) ~ aian * rurality + ffy_centred,
  data = model_data
)

cat("\n  Model 2: log(LOS + 1) ~ AI/AN × Rurality + FFY\n")
print(summary(mod_los))

# ── 4c. Permanency logistic ────────────────────────────────────────────────

model_data <- model_data %>%
  mutate(perm_flag = as.integer(disression %in% 1:3))

mod_perm <- glm(
  perm_flag ~ aian * rurality + ffy_centred,
  data   = model_data,
  family = binomial(link = "logit")
)

cat("\n  Model 3: Permanency ~ AI/AN × Rurality + FFY\n")
print(summary(mod_perm))

# Tidy summaries
model_summaries <- bind_rows(
  tidy(mod_reentry, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Re-Entry (OR)"),
  tidy(mod_los, conf.int = TRUE) %>% mutate(model = "log(LOS)"),
  tidy(mod_perm, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Permanency (OR)")
)

write_csv(model_summaries, file.path(out_dir, "interaction_model_summaries.csv"))
cat("\n  → Model summaries saved to output/interaction_model_summaries.csv\n")


###############################################################################
# SECTION 5 — VISUALIZATIONS
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 5: VISUALIZATIONS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

theme_equity <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    plot.caption  = element_text(colour = "grey50", size = 9, hjust = 0),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ── 5a. Trend lines: Permanency rate by race ────────────────────────────────

p_trend_perm <- metrics_by_race %>%
  ggplot(aes(ffy, permanency_rate, colour = race_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title    = "Permanency Outcome Rate by Race/Ethnicity",
    subtitle = "Percentage of children exiting to reunification, adoption, or guardianship",
    x = "Federal Fiscal Year", y = "Permanency Rate (%)",
    colour = "Race / Ethnicity",
    caption = paste0(
      "Source: AFCARS Foster Care File (synthetic demo data). ",
      "AI/AN identification uses AMIAKN race flag.\n",
      "Tribal sovereignty and ICWA compliance shape permanency pathways for AI/AN children."
    )
  ) +
  theme_equity

# ── 5b. Trend lines: Re-entry rate by rurality ──────────────────────────────

p_trend_reentry <- metrics_by_rural %>%
  ggplot(aes(ffy, reentry_rate, colour = rurality)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Urban/Metro" = "#2166AC",
                                  "Micropolitan" = "#F4A582",
                                  "Rural/Small Town" = "#B2182B")) +
  labs(
    title    = "Re-Entry Rate by Rural-Urban Classification",
    subtitle = "RUCA-derived rurality (codes 7–10 = rural)",
    x = "Federal Fiscal Year", y = "Re-Entry Rate (%)",
    colour = "Rurality (RUCA)",
    caption = paste0(
      "Source: AFCARS + USDA RUCA codes (synthetic demo data). ",
      "Rural communities face distinct service-access barriers\n",
      "that can contribute to re-entry. Tribal lands often overlap rural RUCA codes."
    )
  ) +
  theme_equity

# ── 5c. AI/AN × Rurality interaction plot ───────────────────────────────────

p_interaction <- metrics_aian_rural %>%
  ggplot(aes(ffy, malt_entry_rate, colour = rurality, linetype = aian)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Urban/Metro" = "#2166AC",
                                  "Micropolitan" = "#F4A582",
                                  "Rural/Small Town" = "#B2182B")) +
  labs(
    title    = "Maltreatment-Related Entry: AI/AN × Rurality Interaction",
    subtitle = "Rates per 1,000 children in care",
    x = "Federal Fiscal Year", y = "Maltreatment Entry Rate (per 1,000)",
    colour = "Rurality", linetype = "AI/AN Status",
    caption = paste0(
      "Source: AFCARS (synthetic demo). ",
      "Interaction between AI/AN identity and rurality captures\n",
      "compounding structural inequities in child welfare involvement."
    )
  ) +
  theme_equity

# ── 5d. Disparity Index heatmap ─────────────────────────────────────────────

disp_long <- disp_race %>%
  filter(ffy == max(ffy)) %>%
  select(race_group, starts_with("DI_")) %>%
  pivot_longer(starts_with("DI_"), names_to = "metric", values_to = "DI") %>%
  mutate(
    metric = str_remove(metric, "DI_") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

p_heatmap <- disp_long %>%
  ggplot(aes(metric, race_group, fill = DI)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", DI)), size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 1, name = "Disparity\nIndex"
  ) +
  labs(
    title    = "Disparity Index by Race/Ethnicity (Most Recent FFY)",
    subtitle = "Reference group = White  |  DI > 1 = over-representation",
    x = NULL, y = NULL,
    caption = paste0(
      "Source: AFCARS (synthetic demo). ",
      "Disparity indices should be interpreted alongside ICWA compliance\n",
      "rates, Tribal court jurisdiction data, and community-level context."
    )
  ) +
  theme_equity +
  theme(panel.grid = element_blank())

# ── 5e. LGBTQ+ stub plot (if data exist) ────────────────────────────────────

if (nrow(metrics_lgbtq) > 0) {
  p_lgbtq <- metrics_lgbtq %>%
    ggplot(aes(ffy, permanency_rate, colour = lgbtq, group = lgbtq)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = c("LGBTQ+" = "#7B3294", "Non-LGBTQ+" = "#008837")) +
    labs(
      title    = "Permanency Rate by LGBTQ+ Status (Pilot Data Only)",
      subtitle = "⚠ LIMITED DATA — SOGIE fields removed from AFCARS after 2020",
      x = "Federal Fiscal Year", y = "Permanency Rate (%)",
      colour = NULL,
      caption = paste0(
        "Source: AFCARS pilot SOGIE data (synthetic demo). ",
        "LGBTQ+ youth in foster care face documented disparities\n",
        "in placement stability and permanency. Federal data collection ",
        "ceased after the 2020 AFCARS rule rollback."
      )
    ) +
    theme_equity
} else {
  p_lgbtq <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 5, fontface = "italic",
             label = "No LGBTQ+ data available\n(SOGIE fields removed post-2020)") +
    theme_void()
}


###############################################################################
# SECTION 6 — CHOROPLETH MAP: AI/AN PLACEMENT RATES BY COUNTY
###############################################################################

cat("  Building choropleth map...\n")

# ── 6a. County-level AI/AN rates ─────────────────────────────────────────────

county_aian <- fc %>%
  group_by(fipscode) %>%
  summarise(
    n_total      = n(),
    n_aian       = sum(amiakn == 1),
    aian_pct     = round(n_aian / n_total * 100, 1),
    perm_rate    = round(sum(disression %in% 1:3) / n_total * 100, 1),
    reentry_rate = round(sum(re_entry_flag) / n_total * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n_total >= 10)  # suppress small cells for privacy

# ── 6b. Fetch county shapefile ──────────────────────────────────────────────
# Using tigris — downloads US county boundaries

# For real use, uncomment:
# counties_sf <- counties(cb = TRUE, year = 2020) %>%
#   mutate(fipscode = GEOID) %>%
#   left_join(county_aian, by = "fipscode")

# Synthetic spatial placeholder:
cat("  (Shapefile download skipped in demo mode — use tigris::counties() for real data)\n")

# When shapefiles are available, the choropleth code is:
choropleth_code <- '
p_choropleth <- counties_sf %>%
  filter(!is.na(aian_pct)) %>%
  ggplot() +
  geom_sf(aes(fill = aian_pct), colour = NA) +
  scale_fill_viridis_c(
    option = "inferno", direction = -1,
    name   = "AI/AN Children\\nin Care (%)",
    labels = scales::percent_format(scale = 1)
  ) +
  # Overlay Tribal service areas if shapefile available
  # geom_sf(data = tribal_sf, fill = NA, colour = "#E41A1C",
  #         linewidth = 0.6, linetype = "dashed") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(
    title    = "AI/AN Children as Share of Foster Care Population by County",
    subtitle = "Counties with ≥ 10 children in care | Dashed red = Tribal service areas",
    caption  = paste0(
      "Source: AFCARS + Census TIGER/Line. AI/AN identification via AMIAKN flag.\\n",
      "This map does not capture all Tribal jurisdictions, particularly those with\\n",
      "Tribal court systems that may adjudicate child welfare outside state systems.\\n",
      "Data sovereignty: Tribes are the primary authority on their children\'s welfare."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, colour = "grey40", size = 9),
    legend.position = c(0.92, 0.25)
  )

ggsave(file.path(out_dir, "choropleth_tribal.png"), p_choropleth,
       width = 14, height = 9, dpi = 300)
'
cat("  Choropleth code prepared (runs with real shapefiles).\n")


###############################################################################
# SECTION 7 — SAVE OUTPUTS
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 7: SAVING ALL OUTPUTS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── 7a. Combined disparity table ────────────────────────────────────────────

disparity_tables <- bind_rows(
  disp_race  %>% mutate(grouping = "Race/Ethnicity", subgroup = race_group),
  disp_rural %>% mutate(grouping = "Rurality",       subgroup = as.character(rurality))
) %>%
  select(grouping, subgroup, ffy, n_children,
         permanency_rate, malt_entry_rate, reentry_rate,
         starts_with("DI_"))

write_csv(disparity_tables, file.path(out_dir, "disparity_tables.csv"))
cat("  ✓ disparity_tables.csv\n")

# ── 7b. Plots ───────────────────────────────────────────────────────────────

ggsave(file.path(out_dir, "trend_permanency_race.png"),  p_trend_perm,
       width = 11, height = 7, dpi = 300)
ggsave(file.path(out_dir, "trend_reentry_rurality.png"), p_trend_reentry,
       width = 11, height = 7, dpi = 300)
ggsave(file.path(out_dir, "interaction_aian_rurality.png"), p_interaction,
       width = 11, height = 7, dpi = 300)
ggsave(file.path(out_dir, "disparity_heatmap.png"),      p_heatmap,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(out_dir, "lgbtq_stub.png"),             p_lgbtq,
       width = 10, height = 6, dpi = 300)

cat("  ✓ All plots saved to output/\n")

# ── 7c. Model summaries text file ───────────────────────────────────────────

sink(file.path(out_dir, "interaction_model_details.txt"))
cat("=" %>% strrep(70), "\n")
cat("INTERACTION MODEL RESULTS — AI/AN × RURALITY\n")
cat("=" %>% strrep(70), "\n\n")

cat("MODEL 1: Re-Entry (Logistic)\n")
cat("-" %>% strrep(40), "\n")
print(summary(mod_reentry))

cat("\n\nMODEL 2: Length of Stay (Log-Linear)\n")
cat("-" %>% strrep(40), "\n")
print(summary(mod_los))

cat("\n\nMODEL 3: Permanency (Logistic)\n")
cat("-" %>% strrep(40), "\n")
print(summary(mod_perm))
sink()

cat("  ✓ interaction_model_details.txt\n")


###############################################################################
# SECTION 8 — DASHBOARD CAPTIONS (Equity & Sovereignty Framing)
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 8: EQUITY-FRAMED DASHBOARD CAPTIONS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

captions <- c(
  "## Dashboard Caption Guide — Tribal & Special Populations",
  "",
  "### Sovereignty & ICWA Context",
  paste0(
    "Caption for AI/AN panels: \"American Indian and Alaska Native children ",
    "are subject to the Indian Child Welfare Act (ICWA, 25 U.S.C. §§ 1901–1963), ",
    "which affirms Tribal Nations' inherent sovereignty over child welfare ",
    "decisions. Disparities shown here reflect systemic inequities in state ",
    "child welfare systems and should be interpreted alongside ICWA compliance ",
    "data, Tribal court jurisdiction, and community-defined outcomes. ",
    "Tribal Nations are the primary authority regarding the welfare of their ",
    "children.\""
  ),
  "",
  "### Rurality Context",
  paste0(
    "Caption for rural/urban panels: \"Rural-urban classification uses USDA ",
    "RUCA codes derived from county FIPS in AFCARS. Rural communities face ",
    "distinct barriers including service deserts, workforce shortages, and ",
    "geographic isolation that affect child welfare outcomes. Many Tribal ",
    "lands are classified as rural, creating intersecting disparities that ",
    "compound structural disadvantage.\""
  ),
  "",
  "### LGBTQ+ Data Gap",
  paste0(
    "Caption for LGBTQ+ panels: \"LGBTQ+ identification data in AFCARS is ",
    "extremely limited. The 2016 AFCARS final rule included SOGIE (Sexual ",
    "Orientation, Gender Identity, and Expression) data elements, but the ",
    "2020 rule rollback removed these fields before nationwide implementation. ",
    "Research consistently shows LGBTQ+ youth are over-represented in foster ",
    "care and face elevated risks of placement instability, homelessness, ",
    "and poor permanency outcomes. The absence of federal data collection ",
    "constitutes a critical equity gap.\""
  ),
  "",
  "### Disparity Index Interpretation",
  paste0(
    "Caption for disparity heatmaps: \"Disparity Indices (DI) compare ",
    "subgroup rates to a reference group (White for race; Urban/Metro for ",
    "rurality). A DI of 1.00 = parity; DI > 1.00 = over-representation; ",
    "DI < 1.00 = under-representation. These indices capture system-level ",
    "patterns and should not be used to draw conclusions about families or ",
    "communities. Structural determinants — poverty, housing instability, ",
    "historical trauma, racism — drive the observed disparities.\""
  ),
  "",
  "### Interaction Effects",
  paste0(
    "Caption for interaction panels: \"Race × rurality interaction models ",
    "test whether the relationship between AI/AN identity and child welfare ",
    "outcomes differs across rural and urban contexts. Significant interactions ",
    "indicate that rurality compounds racial disparities, suggesting that ",
    "place-based interventions must address both geographic and cultural ",
    "dimensions of inequity.\""
  ),
  "",
  "### Data Limitations",
  paste0(
    "General disclaimer: \"AFCARS data reflect state-reported information ",
    "and may under-count AI/AN children due to misclassification, single-race ",
    "coding limitations, and exclusion of children under Tribal court ",
    "jurisdiction who never enter state systems. Counties with fewer than ",
    "10 children are suppressed to protect confidentiality. This analysis ",
    "uses synthetic demonstration data; replace with actual AFCARS extracts ",
    "for production use.\""
  ),
  "",
  "### Recommended Dashboard Requirements",
  "1. All AI/AN-focused visuals must include ICWA context language.",
  "2. Deficit framing prohibited — present strengths alongside disparities.",
  "3. Tribal consultation checkbox: confirm data were reviewed with Tribal partners.",
  "4. LGBTQ+ panels must display data-gap warning prominently.",
  "5. Rural panels must note overlap with Tribal land geographies.",
  "6. Small-cell suppression (n < 10) for all subgroup breakdowns.",
  "7. Colour palettes must be accessible (colour-blind safe).",
  "8. All rates must include confidence intervals or sample sizes."
)

writeLines(captions, file.path(out_dir, "dashboard_captions.txt"))
cat("  ✓ dashboard_captions.txt\n")


###############################################################################
# SECTION 9 — SUMMARY REPORT TABLE
###############################################################################

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SECTION 9: SUMMARY REPORT\n")
cat("══════════════════════════════════════════════════════════════\n\n")

latest_year <- max(fc$ffy)

summary_table <- disparity_tables %>%
  filter(ffy == latest_year) %>%
  select(grouping, subgroup, n_children,
         permanency_rate, malt_entry_rate, reentry_rate,
         DI_permanency_rate, DI_malt_entry_rate, DI_reentry_rate) %>%
  arrange(grouping, subgroup)

cat("  Most Recent FFY (", latest_year, ") Summary:\n\n")
print(as.data.frame(summary_table), row.names = FALSE)

write_csv(summary_table, file.path(out_dir, "summary_latest_year.csv"))

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("  WORKFLOW COMPLETE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("\n  Output files in:", normalizePath(out_dir, mustWork = FALSE), "\n")
cat("   • disparity_tables.csv\n")
cat("   • summary_latest_year.csv\n")
cat("   • interaction_model_summaries.csv\n")
cat("   • interaction_model_details.txt\n")
cat("   • dashboard_captions.txt\n")
cat("   • trend_permanency_race.png\n")
cat("   • trend_reentry_rurality.png\n")
cat("   • interaction_aian_rurality.png\n")
cat("   • disparity_heatmap.png\n")
cat("   • lgbtq_stub.png\n")
cat("   • choropleth_tribal.png  (requires real shapefiles)\n")
cat("\n")
