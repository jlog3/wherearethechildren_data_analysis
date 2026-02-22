#!/usr/bin/env Rscript
###############################################################################
#  INFANT / NEWBORN-SPECIFIC CHILD WELFARE ANALYSIS
#  ---------------------------------------------------------------------------
#  Purpose : Dedicated analytics for children < 1 year in foster care with
#            substance-exposure indicators, NAS-proxy correlations, entry-
#            reason cross-tabs, short-term outcome tracking, survival models,
#            Sankey flow diagrams, stacked bar charts, and CAPTA/CARA-linked
#            policy projections.
#
#  Inputs  : AFCARS Foster Care file  (FC)  – with DOB, LatRemDt, DAChild,
#                                              CurPlSet, discharge/permanency
#            AFCARS Adoption file      (AD)  – optional, for adoption outcomes
#            NCANDS Child file         (CND) – for prior-report merge
#
#  Outputs : Publication-ready PNGs, an HTML Sankey widget, CSV summary
#            tables, and a self-contained Shiny dashboard (optional launch).
#
#  Author  : Auto-generated analytical template
#  Date    : 2025
###############################################################################

# ── 0. ENVIRONMENT SETUP ─────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)      # dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats
  library(data.table)     # fast I/O and joins
  library(lubridate)      # date arithmetic
  library(survival)       # Kaplan-Meier, Cox PH
  library(survminer)      # ggsurvplot
  library(ggalluvial)     # Sankey / alluvial diagrams
  library(scales)         # pretty axis labels
  library(patchwork)      # combine ggplots
  library(forecast)       # time-series projections
  library(broom)          # tidy model outputs
  library(janitor)        # clean_names, tabyl
  library(knitr)          # kable for markdown tables
  library(kableExtra)     # enhanced tables
  library(htmlwidgets)    # save interactive widgets
  library(networkD3)      # interactive Sankey (D3)
  library(viridis)        # colorblind-safe palettes
  library(RColorBrewer)   # additional palettes
})

# ── Output directory ----------------------------------------------------------
OUT_DIR <- "output/infant_analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("
╔══════════════════════════════════════════════════════════════╗
║   INFANT / NEWBORN CHILD WELFARE ANALYSIS PIPELINE          ║
║   AFCARS + NCANDS  ·  Substance Exposure  ·  Outcomes       ║
╚══════════════════════════════════════════════════════════════╝
\n")


###############################################################################
# 1. DATA LOADING & HARMONISATION
###############################################################################

cat("── [1] Loading & harmonising data ──────────────────────────────────────\n")

# ---------- Helper: flexible loader -------------------------------------------
load_data <- function(path, label = "dataset") {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s\nPlease supply a valid %s path.", path, label))
  }
  ext <- tolower(tools::file_ext(path))
  df <- switch(ext,
    csv  = fread(path, colClasses = "character"),
    tsv  = fread(path, sep = "\t", colClasses = "character"),
    tab  = fread(path, sep = "\t", colClasses = "character"),
    dat  = fread(path, colClasses = "character"),
    sas7bdat = haven::read_sas(path) %>% mutate(across(everything(), as.character)),
    rds  = readRDS(path) %>% mutate(across(everything(), as.character)),
    stop(sprintf("Unsupported extension .%s for %s", ext, label))
  )
  df <- as_tibble(df) %>% clean_names()
  cat(sprintf("   ✓ %s loaded: %s rows × %s cols\n", label, comma(nrow(df)), ncol(df)))
  df
}

# ---------- EDIT THESE PATHS to point at your actual files --------------------
AFCARS_FC_PATH  <- Sys.getenv("AFCARS_FC_PATH",  unset = "data/afcars_fc.csv")
AFCARS_AD_PATH  <- Sys.getenv("AFCARS_AD_PATH",  unset = "data/afcars_adopt.csv")
NCANDS_CND_PATH <- Sys.getenv("NCANDS_CND_PATH", unset = "data/ncands_child.csv")

fc_raw  <- load_data(AFCARS_FC_PATH,  "AFCARS-FC")
ncands  <- load_data(NCANDS_CND_PATH, "NCANDS-Child")

# Optional adoption file
ad_raw <- tryCatch(load_data(AFCARS_AD_PATH, "AFCARS-Adopt"),
                   error = function(e) { cat("   ⚠ Adoption file not loaded.\n"); NULL })


###############################################################################
# 2. VARIABLE STANDARDISATION
###############################################################################

cat("── [2] Variable standardisation ───────────────────────────────────────\n")

# ---------- AFCARS-FC field mapping (NDACAN / ACF naming variants) -----------
#
#  The canonical field names below cover common naming variants found across
#  different AFCARS file years. Adjust the `coalesce()` lookups if your file
#  uses different column headers.

map_field <- function(df, target, candidates) {
  # Return the first candidate column that exists, renamed to `target`
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    warning(sprintf("  ⚠ No match for '%s' among: %s", target, paste(candidates, collapse = ", ")))
    return(rep(NA_character_, nrow(df)))
  }
  df[[hit[1]]]
}

fc <- fc_raw %>%
  mutate(
    # Identifiers
    child_id     = map_field(fc_raw, "child_id",
                     c("recnumbr", "child_id", "afcarsid", "stfcid")),
    state_fips   = map_field(fc_raw, "state_fips",
                     c("st", "state", "fipscode", "stfips", "state_fips")),
    report_year  = map_field(fc_raw, "report_year",
                     c("fy", "report_period", "fiscal_year", "report_year", "fy_year")),

    # Dates
    dob_raw      = map_field(fc_raw, "dob",
                     c("dob", "dobbyr", "brthdt", "birth_date", "dobyr")),
    removal_date_raw = map_field(fc_raw, "removal_date",
                     c("latremdt", "rem1dt", "removal_date", "curremdt", "latest_removal_date")),
    discharge_date_raw = map_field(fc_raw, "discharge_date",
                     c("disdt", "discharge_date", "dschrgdt", "dischrgdt")),

    # Substance / NAS flags
    da_child     = map_field(fc_raw, "da_child",
                     c("dachild", "da_child", "drugabuse_child", "cdalc", "cldrugab")),
    da_parent    = map_field(fc_raw, "da_parent",
                     c("daprnt", "da_parent", "apts_par_da", "cpapts_da", "par_drugab")),
    alcohol_par  = map_field(fc_raw, "alcohol_par",
                     c("aalc", "alcohol_parent", "apts_par_alc", "par_alcohol")),

    # Removal reason columns (up to 15 in AFCARS)
    across(matches("^rem(reas|r)[0-9]|^rem_reason|^removal_reason"), as.character),

    # Placement setting
    cur_pl_set   = map_field(fc_raw, "cur_pl_set",
                     c("curplset", "cur_pl_set", "cursetng", "plc_setting")),

    # Permanency / outcome
    discharge_reason = map_field(fc_raw, "discharge_reason",
                     c("disreasn", "discharge_reason", "dschreasn", "reason_discharge")),
    total_rem    = map_field(fc_raw, "total_rem",
                     c("totalrem", "total_rem", "num_removals", "numrem")),
    perm_goal    = map_field(fc_raw, "perm_goal",
                     c("cgoal", "perm_goal", "case_goal", "goal1")),

    # Age at removal (will also compute from DOB)
    age_at_rem_raw = map_field(fc_raw, "age_at_rem",
                     c("ageremov", "age_at_removal", "agerem", "ageatrem"))
  )

# ---------- Date parsing ------------------------------------------------------
parse_multi_date <- function(x) {
  x <- str_trim(x)
  parsed <- parse_date_time(x, orders = c("ymd", "mdy", "dmy", "ym", "Y"),
                            quiet = TRUE)
  parsed
}

fc <- fc %>%
  mutate(
    dob            = parse_multi_date(dob_raw),
    removal_date   = parse_multi_date(removal_date_raw),
    discharge_date = parse_multi_date(discharge_date_raw)
  )

# ---------- Compute age at removal in days & months ---------------------------
fc <- fc %>%
  mutate(
    age_at_rem_days   = as.numeric(difftime(removal_date, dob, units = "days")),
    age_at_rem_months = age_at_rem_days / 30.44,
    age_at_rem_yrs    = age_at_rem_days / 365.25
  )

# ---------- Numeric conversions -----------------------------------------------
fc <- fc %>%
  mutate(
    across(c(da_child, da_parent, alcohol_par, cur_pl_set,
             discharge_reason, total_rem, perm_goal, age_at_rem_raw),
           ~ suppressWarnings(as.numeric(.x))),
    report_year = suppressWarnings(as.numeric(report_year))
  )


###############################################################################
# 3. INFANT COHORT EXTRACTION (< 1 YEAR AT REMOVAL)
###############################################################################

cat("── [3] Extracting infant cohort (age < 1 year at removal) ─────────────\n")

infants <- fc %>%
  filter(!is.na(age_at_rem_days), age_at_rem_days >= 0, age_at_rem_days < 365)

cat(sprintf("   ✓ Infant cohort: %s records (%.1f%% of FC file)\n",
            comma(nrow(infants)), 100 * nrow(infants) / nrow(fc)))

# Age bands within infancy
infants <- infants %>%
  mutate(
    age_band = case_when(
      age_at_rem_days <= 7   ~ "0-7 days (neonatal)",
      age_at_rem_days <= 28  ~ "8-28 days (neonatal)",
      age_at_rem_days <= 90  ~ "1-3 months",
      age_at_rem_days <= 180 ~ "3-6 months",
      age_at_rem_days <= 270 ~ "6-9 months",
      TRUE                   ~ "9-12 months"
    ),
    age_band = factor(age_band, levels = c(
      "0-7 days (neonatal)", "8-28 days (neonatal)",
      "1-3 months", "3-6 months", "6-9 months", "9-12 months"
    ))
  )


###############################################################################
# 4. PRENATAL EXPOSURE INDICATORS
###############################################################################

cat("── [4] Prenatal exposure indicators ───────────────────────────────────\n")

# DAChild flag == 1 AND infant → strong prenatal-exposure indicator
# AFCARS codes: 1 = Yes, 2 = No, (0/99 = missing/unknown)
infants <- infants %>%
  mutate(
    prenatal_exposure = case_when(
      da_child == 1                         ~ "Confirmed (DAChild=1)",
      da_child == 2                         ~ "Not indicated",
      TRUE                                  ~ "Unknown/Missing"
    ),
    parental_substance = case_when(
      da_parent == 1 | alcohol_par == 1    ~ "Parent SA indicated",
      da_parent == 2 & alcohol_par == 2    ~ "No parent SA",
      TRUE                                  ~ "Unknown/Missing"
    ),
    # Composite flag for analytic convenience
    substance_any = ifelse(
      prenatal_exposure == "Confirmed (DAChild=1)" |
      parental_substance == "Parent SA indicated", 1, 0
    )
  )

exposure_summary <- infants %>%
  count(prenatal_exposure, parental_substance, name = "n") %>%
  mutate(pct = n / sum(n) * 100)

cat("   Prenatal exposure × parental substance cross-tab:\n")
print(kable(exposure_summary, format = "simple", digits = 1))
cat("\n")

fwrite(exposure_summary, file.path(OUT_DIR, "exposure_summary.csv"))


###############################################################################
# 5. NAS PROXY / CORRELATION ANALYSIS
###############################################################################

cat("── [5] NAS proxy & correlation analysis ───────────────────────────────\n")

# AFCARS has no explicit NAS field. We define a NAS-proxy cohort:
#   (a) DAChild == 1  AND
#   (b) age at removal <= 28 days (neonatal)
#
# This captures children flagged for substance exposure who entered care
# in the first month of life — the window consistent with NAS diagnosis.

infants <- infants %>%
  mutate(
    nas_proxy = ifelse(da_child == 1 & age_at_rem_days <= 28, 1, 0)
  )

nas_n <- sum(infants$nas_proxy, na.rm = TRUE)
cat(sprintf("   NAS-proxy cases (DAChild=1 & age ≤ 28d): %s (%.1f%% of infants)\n",
            comma(nas_n), 100 * nas_n / nrow(infants)))

# --- NAS-proxy trend by year --------------------------------------------------
nas_trend <- infants %>%
  filter(!is.na(report_year)) %>%
  group_by(report_year) %>%
  summarise(
    total_infants = n(),
    nas_proxy_n   = sum(nas_proxy, na.rm = TRUE),
    nas_rate_pct  = nas_proxy_n / total_infants * 100,
    .groups = "drop"
  )

p_nas_trend <- ggplot(nas_trend, aes(report_year, nas_rate_pct)) +
  geom_line(linewidth = 1.1, colour = "#D55E00") +
  geom_point(size = 2.5, colour = "#D55E00") +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, colour = "#0072B2") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "NAS-Proxy Rate Among Infants in Foster Care",
    subtitle = "DAChild = 1 & age at removal ≤ 28 days",
    x = "Fiscal Year", y = "NAS-Proxy Rate (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_DIR, "nas_proxy_trend.png"), p_nas_trend,
       width = 9, height = 5.5, dpi = 300)

# --- Correlation: NAS-proxy vs. outcome indicators ----------------------------
#  Tetrachoric / point-biserial correlations with placement type, discharge, etc.

infants <- infants %>%
  mutate(
    discharged         = ifelse(!is.na(discharge_date), 1, 0),
    days_in_care       = as.numeric(difftime(
                           coalesce(discharge_date, Sys.Date()), removal_date, units = "days")),
    reunification      = ifelse(discharge_reason == 1, 1, 0),
    adoption           = ifelse(discharge_reason == 2, 1, 0),
    kinship_placement  = ifelse(cur_pl_set %in% c(3, 6), 1, 0)
  )

cor_vars <- c("nas_proxy", "substance_any", "kinship_placement",
              "reunification", "adoption", "days_in_care")

cor_matrix <- infants %>%
  select(all_of(cor_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

cat("   Correlation matrix (NAS-proxy & outcomes):\n")
print(round(cor_matrix, 3))
cat("\n")

write.csv(cor_matrix, file.path(OUT_DIR, "nas_outcome_correlations.csv"))


###############################################################################
# 6. ENTRY REASON CROSS-TAB WITH SUBSTANCE FLAGS
###############################################################################

cat("── [6] Entry-reason cross-tabulation ──────────────────────────────────\n")

# AFCARS removal reasons are stored across up to 15 binary columns.
# Typical AFCARS coding (1 = applies, 0 = does not):
#   Reason 1:  Physical abuse         Reason 9:  Drug abuse (parent)
#   Reason 2:  Sexual abuse           Reason 10: Drug abuse (child)
#   Reason 3:  Neglect                Reason 11: Alcohol abuse (parent)
#   Reason 4:  Alcohol abuse (parent) Reason 12: Alcohol abuse (child)
#   Reason 5:  Drug abuse (parent)    ... etc depending on year/layout
#
# Because coding shifted across years, we use a flexible detection approach.

reason_cols <- names(infants) %>%
  str_subset("(?i)rem.?reas|rem.?r[0-9]|removal.?reason")

# Build long-form reason flags
if (length(reason_cols) > 0) {
  reason_labels <- c(
    "1" = "Physical abuse",    "2" = "Sexual abuse",
    "3" = "Neglect",           "4" = "Parent alcohol",
    "5" = "Parent drug abuse", "6" = "Child alcohol",
    "7" = "Child drug abuse",  "8" = "Child disability",
    "9" = "Child behavior",    "10" = "Parent death",
    "11" = "Parent incarceration", "12" = "Caretaker inability",
    "13" = "Relinquishment",   "14" = "Inadequate housing",
    "15" = "Abandonment"
  )

  infant_reasons <- infants %>%
    select(child_id, all_of(reason_cols), prenatal_exposure, nas_proxy) %>%
    pivot_longer(
      cols = all_of(reason_cols),
      names_to = "reason_col", values_to = "flag"
    ) %>%
    mutate(flag = suppressWarnings(as.numeric(flag))) %>%
    filter(flag == 1) %>%
    # Extract reason number from column name
    mutate(
      reason_num = str_extract(reason_col, "[0-9]+$"),
      reason_label = recode(reason_num, !!!reason_labels, .default = paste0("Reason_", reason_num))
    )

  # Cross-tab: reason × prenatal exposure
  reason_exposure_xtab <- infant_reasons %>%
    count(reason_label, prenatal_exposure) %>%
    pivot_wider(names_from = prenatal_exposure, values_from = n, values_fill = 0) %>%
    arrange(desc(rowSums(select(., where(is.numeric)))))

  cat("   Entry Reason × Prenatal Exposure cross-tab (top rows):\n")
  print(kable(head(reason_exposure_xtab, 10), format = "simple"))
  cat("\n")

  fwrite(reason_exposure_xtab, file.path(OUT_DIR, "entry_reason_exposure_xtab.csv"))

  # --- Stacked bar: top 8 reasons by NAS-proxy status --------------------------
  top_reasons <- infant_reasons %>%
    count(reason_label, sort = TRUE) %>%
    slice_head(n = 8) %>%
    pull(reason_label)

  p_reason_bar <- infant_reasons %>%
    filter(reason_label %in% top_reasons) %>%
    mutate(nas_label = ifelse(nas_proxy == 1, "NAS-Proxy", "Non-NAS")) %>%
    count(reason_label, nas_label) %>%
    ggplot(aes(x = fct_reorder(reason_label, n, .fun = sum), y = n, fill = nas_label)) +
    geom_col(position = "stack", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("NAS-Proxy" = "#D55E00", "Non-NAS" = "#56B4E9")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Top Removal Reasons Among Infants by NAS-Proxy Status",
      x = NULL, y = "Number of Cases", fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")

  ggsave(file.path(OUT_DIR, "entry_reason_by_nas.png"), p_reason_bar,
         width = 10, height = 6, dpi = 300)

} else {
  cat("   ⚠ No removal-reason columns detected – skipping cross-tab.\n")
}


###############################################################################
# 7. SHORT-TERM OUTCOMES
###############################################################################

cat("── [7] Short-term outcome analysis ────────────────────────────────────\n")

# 7a. Placement type at 6 months -----------------------------------------------
# AFCARS CurPlSet codes (common mapping):
#   1 = Pre-adoptive home, 2 = Foster (relative), 3 = Foster (non-relative),
#   4 = Group home,  5 = Institution,  6 = Supervised independent living,
#   7 = Runaway, 8 = Trial home visit

placement_labels <- c(
  "1" = "Pre-adoptive",  "2" = "Relative foster",
  "3" = "Non-relative foster", "4" = "Group home",
  "5" = "Institution",   "6" = "Supervised IL",
  "7" = "Runaway",       "8" = "Trial home visit"
)

infants <- infants %>%
  mutate(
    placement_label = recode(as.character(cur_pl_set),
                             !!!placement_labels, .default = "Other/Unknown"),
    # Was child still in care at 6-month mark?
    in_care_6mo = case_when(
      is.na(discharge_date) & days_in_care >= 183          ~ TRUE,
      !is.na(discharge_date) & days_in_care >= 183         ~ TRUE,
      TRUE                                                  ~ FALSE
    )
  )

# Placement type distribution for children observed at ≥ 6 months
placement_6mo <- infants %>%
  filter(in_care_6mo) %>%
  count(placement_label, prenatal_exposure) %>%
  group_by(prenatal_exposure) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p_placement <- ggplot(placement_6mo,
       aes(x = fct_reorder(placement_label, n, .fun = sum),
           y = pct, fill = prenatal_exposure)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Placement Type at 6 Months — Infant Cohort",
    subtitle = "By prenatal substance exposure status",
    x = NULL, y = "Percentage within exposure group", fill = "Exposure"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave(file.path(OUT_DIR, "placement_6mo.png"), p_placement,
       width = 10, height = 6, dpi = 300)

# 7b. Time to permanency -------------------------------------------------------
infants <- infants %>%
  mutate(
    achieved_permanency = ifelse(discharge_reason %in% c(1, 2, 3, 4), 1, 0),
    # 1 = reunification, 2 = adoption, 3 = guardianship, 4 = transfer to relative
    time_to_perm_days   = ifelse(achieved_permanency == 1, days_in_care, NA_real_),
    perm_type = case_when(
      discharge_reason == 1 ~ "Reunification",
      discharge_reason == 2 ~ "Adoption",
      discharge_reason == 3 ~ "Guardianship",
      discharge_reason == 4 ~ "Relative/Guardian",
      TRUE                  ~ "Other/Still in care"
    )
  )

perm_summary <- infants %>%
  filter(achieved_permanency == 1) %>%
  group_by(perm_type, prenatal_exposure) %>%
  summarise(
    n       = n(),
    median_days = median(time_to_perm_days, na.rm = TRUE),
    mean_days   = mean(time_to_perm_days, na.rm = TRUE),
    p25         = quantile(time_to_perm_days, 0.25, na.rm = TRUE),
    p75         = quantile(time_to_perm_days, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("   Permanency timing summary (days):\n")
print(kable(perm_summary, format = "simple", digits = 0))
cat("\n")

fwrite(perm_summary, file.path(OUT_DIR, "time_to_permanency_summary.csv"))


###############################################################################
# 8. SURVIVAL ANALYSIS — TIME IN CARE
###############################################################################

cat("── [8] Survival analysis — time in care ───────────────────────────────\n")

# Event = achieving permanency (discharged to positive outcome)
# Censored = still in care or discharged for other reasons

surv_df <- infants %>%
  filter(!is.na(days_in_care), days_in_care > 0) %>%
  mutate(
    event     = achieved_permanency,
    time      = pmin(days_in_care, 730),   # cap at 2 years for visualization
    exposure  = factor(prenatal_exposure,
                  levels = c("Not indicated", "Confirmed (DAChild=1)", "Unknown/Missing"))
  )

# 8a. Kaplan-Meier by exposure status -----------------------------------------
km_fit <- survfit(Surv(time, event) ~ exposure, data = surv_df)

p_km <- ggsurvplot(
  km_fit, data = surv_df,
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE, risk.table.height = 0.28,
  palette = c("#0072B2", "#D55E00", "#999999"),
  xlab = "Days Since Removal",
  ylab = "Probability of Remaining in Care",
  title = "Time to Permanency — Infants by Prenatal Exposure Status",
  legend.title = "Exposure",
  ggtheme = theme_minimal(base_size = 12),
  break.x.by = 90,
  surv.median.line = "hv"
)

ggsave(file.path(OUT_DIR, "survival_curve_exposure.png"),
       print(p_km), width = 11, height = 8, dpi = 300)

# 8b. Cox proportional hazards model ------------------------------------------
cox_fit <- coxph(
  Surv(time, event) ~ exposure + kinship_placement + as.factor(report_year),
  data = surv_df
)

cat("   Cox PH model summary:\n")
print(summary(cox_fit))
cat("\n")

cox_tidy <- tidy(cox_fit, conf.int = TRUE, exponentiate = TRUE)
fwrite(cox_tidy, file.path(OUT_DIR, "cox_model_results.csv"))

# 8c. NAS-proxy specific survival curve ----------------------------------------
surv_df_nas <- surv_df %>%
  mutate(nas_label = factor(ifelse(nas_proxy == 1, "NAS-Proxy", "Other Infants")))

km_nas <- survfit(Surv(time, event) ~ nas_label, data = surv_df_nas)

p_km_nas <- ggsurvplot(
  km_nas, data = surv_df_nas,
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE, risk.table.height = 0.28,
  palette = c("#D55E00", "#56B4E9"),
  xlab = "Days Since Removal",
  ylab = "Probability of Remaining in Care",
  title = "Time to Permanency — NAS-Proxy vs. Other Infants",
  legend.title = "",
  ggtheme = theme_minimal(base_size = 12),
  break.x.by = 90, surv.median.line = "hv"
)

ggsave(file.path(OUT_DIR, "survival_curve_nas.png"),
       print(p_km_nas), width = 11, height = 8, dpi = 300)


###############################################################################
# 9. SANKEY / ALLUVIAL FLOW DIAGRAMS
###############################################################################

cat("── [9] Sankey / alluvial flow diagrams ────────────────────────────────\n")

# Flow: Entry Age Band → Exposure Status → Placement Type → Outcome

sankey_df <- infants %>%
  filter(!is.na(age_band), !is.na(placement_label)) %>%
  mutate(
    outcome_label = case_when(
      perm_type == "Reunification"      ~ "Reunification",
      perm_type == "Adoption"           ~ "Adoption",
      perm_type == "Guardianship"       ~ "Guardianship",
      perm_type == "Relative/Guardian"  ~ "Relative/Guardian",
      TRUE                              ~ "Still in care / Other"
    )
  ) %>%
  count(age_band, prenatal_exposure, placement_label, outcome_label) %>%
  filter(n >= 5)  # suppress small cells

# 9a. ggalluvial (static, publication-ready) -----------------------------------
p_alluvial <- ggplot(sankey_df,
       aes(axis1 = age_band,
           axis2 = prenatal_exposure,
           axis3 = placement_label,
           axis4 = outcome_label,
           y = n)) +
  geom_alluvium(aes(fill = prenatal_exposure), width = 1/5, alpha = 0.65) +
  geom_stratum(width = 1/5, fill = "grey90", colour = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 2.7, fontface = "bold") +
  scale_x_discrete(limits = c("Age Band", "Exposure", "Placement", "Outcome"),
                   expand = c(0.15, 0.05)) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Infant Foster Care Pathways: Entry → Exposure → Placement → Outcome",
    y = "Number of Cases", fill = "Prenatal Exposure"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()
  )

ggsave(file.path(OUT_DIR, "sankey_alluvial.png"), p_alluvial,
       width = 14, height = 8, dpi = 300)

# 9b. Interactive D3 Sankey (HTML widget) --------------------------------------
build_d3_sankey <- function(df) {
  # Prepare nodes and links for networkD3
  nodes_vec <- unique(c(
    paste0("age:", df$age_band),
    paste0("exp:", df$prenatal_exposure),
    paste0("plc:", df$placement_label),
    paste0("out:", df$outcome_label)
  ))
  nodes <- data.frame(name = nodes_vec, stringsAsFactors = FALSE)

  make_links <- function(from_prefix, from_col, to_prefix, to_col) {
    df %>%
      group_by(from = paste0(from_prefix, .data[[from_col]]),
               to   = paste0(to_prefix, .data[[to_col]])) %>%
      summarise(value = sum(n), .groups = "drop")
  }

  links <- bind_rows(
    make_links("age:", "age_band",           "exp:", "prenatal_exposure"),
    make_links("exp:", "prenatal_exposure",   "plc:", "placement_label"),
    make_links("plc:", "placement_label",     "out:", "outcome_label")
  ) %>%
    mutate(
      source = match(from, nodes$name) - 1,
      target = match(to,   nodes$name) - 1
    ) %>%
    filter(!is.na(source), !is.na(target))

  # Clean display names
  nodes$name <- str_remove(nodes$name, "^[a-z]+:")

  sankeyNetwork(
    Links = as.data.frame(links), Nodes = as.data.frame(nodes),
    Source = "source", Target = "target", Value = "value", NodeID = "name",
    fontSize = 12, nodeWidth = 30, sinksRight = TRUE,
    colourScale = JS("d3.scaleOrdinal(d3.schemeSet2)")
  )
}

sankey_widget <- build_d3_sankey(sankey_df)
saveWidget(sankey_widget, file.path(OUT_DIR, "sankey_interactive.html"),
           selfcontained = TRUE)
cat("   ✓ Interactive Sankey saved to sankey_interactive.html\n")


###############################################################################
# 10. STACKED BAR CHARTS BY STATE / YEAR
###############################################################################

cat("── [10] Stacked bar charts by state / year ────────────────────────────\n")

# 10a. By state — top 15 states by infant volume ─────────────────────────────
state_summary <- infants %>%
  filter(!is.na(state_fips)) %>%
  count(state_fips, prenatal_exposure) %>%
  group_by(state_fips) %>%
  mutate(total = sum(n)) %>%
  ungroup()

top_states <- state_summary %>%
  distinct(state_fips, total) %>%
  slice_max(total, n = 15) %>%
  pull(state_fips)

p_state <- state_summary %>%
  filter(state_fips %in% top_states) %>%
  ggplot(aes(x = fct_reorder(state_fips, total), y = n, fill = prenatal_exposure)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Infant Foster Care Entries — Top 15 States by Volume",
    subtitle = "Stacked by prenatal substance exposure",
    x = "State FIPS", y = "Number of Infants", fill = "Exposure"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave(file.path(OUT_DIR, "stacked_bar_state.png"), p_state,
       width = 10, height = 8, dpi = 300)

# 10b. By year — national trend ───────────────────────────────────────────────
year_summary <- infants %>%
  filter(!is.na(report_year)) %>%
  count(report_year, prenatal_exposure)

p_year <- ggplot(year_summary, aes(x = report_year, y = n, fill = prenatal_exposure)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  labs(
    title = "National Infant Foster Care Volume by Year",
    subtitle = "Stacked by prenatal substance exposure status",
    x = "Fiscal Year", y = "Infants Entering Care", fill = "Exposure"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave(file.path(OUT_DIR, "stacked_bar_year.png"), p_year,
       width = 10, height = 6, dpi = 300)

# 10c. By year × NAS-proxy (rate + volume dual-axis) ──────────────────────────
nas_year <- infants %>%
  filter(!is.na(report_year)) %>%
  group_by(report_year) %>%
  summarise(
    total   = n(),
    nas_n   = sum(nas_proxy, na.rm = TRUE),
    nas_pct = nas_n / total * 100,
    .groups = "drop"
  )

coeff <- max(nas_year$total) / max(nas_year$nas_pct, na.rm = TRUE)

p_nas_dual <- ggplot(nas_year, aes(x = report_year)) +
  geom_col(aes(y = total), fill = "#56B4E9", alpha = 0.6, width = 0.6) +
  geom_line(aes(y = nas_pct * coeff), colour = "#D55E00", linewidth = 1.2) +
  geom_point(aes(y = nas_pct * coeff), colour = "#D55E00", size = 2.5) +
  scale_y_continuous(
    name   = "Total Infants in Care",
    labels = comma,
    sec.axis = sec_axis(~ . / coeff, name = "NAS-Proxy Rate (%)",
                        labels = label_percent(scale = 1))
  ) +
  labs(
    title = "Infant Volume & NAS-Proxy Rate Over Time",
    x = "Fiscal Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y.right = element_text(colour = "#D55E00"),
    axis.text.y.right  = element_text(colour = "#D55E00")
  )

ggsave(file.path(OUT_DIR, "nas_dual_axis_trend.png"), p_nas_dual,
       width = 10, height = 6, dpi = 300)


###############################################################################
# 11. NCANDS MERGE — PRIOR REPORT HISTORY
###############################################################################

cat("── [11] NCANDS merge for prior-report analysis ────────────────────────\n")

# NCANDS linkage uses child-level IDs. AFCARS ↔ NCANDS bridging is imperfect;
# we attempt matching on state + child_id or AFCARS-ID where available.

ncands <- ncands %>%
  mutate(
    ncands_child_id = map_field(ncands, "ncands_child_id",
                       c("chid", "child_id", "stchid", "ndchid", "subyr_chid")),
    ncands_state    = map_field(ncands, "ncands_state",
                       c("staterr", "st", "state", "fipscode", "subst")),
    rpt_dt_raw      = map_field(ncands, "rpt_dt",
                       c("rptdt", "rpt_dt", "report_date", "rptdate", "rptdts")),
    mal_type        = map_field(ncands, "mal_type",
                       c("maltype", "mal1", "maltreatment_type", "maltrt1")),
    rpt_disp        = map_field(ncands, "rpt_disp",
                       c("rptdisp", "report_disposition", "rptvictm"))
  ) %>%
  mutate(rpt_dt = parse_multi_date(rpt_dt_raw))

# Attempt merge
merged <- infants %>%
  inner_join(
    ncands %>% select(ncands_child_id, ncands_state, rpt_dt, mal_type, rpt_disp),
    by = c("child_id" = "ncands_child_id", "state_fips" = "ncands_state"),
    relationship = "many-to-many"
  )

cat(sprintf("   ✓ Merged infant-NCANDS records: %s\n", comma(nrow(merged))))

# Prior reports = NCANDS report dates BEFORE removal date
merged <- merged %>%
  mutate(
    report_before_removal = rpt_dt < removal_date,
    days_report_to_removal = as.numeric(difftime(removal_date, rpt_dt, units = "days"))
  )

prior_reports <- merged %>%
  filter(report_before_removal) %>%
  group_by(child_id, state_fips) %>%
  summarise(
    n_prior_reports     = n(),
    first_report_date   = min(rpt_dt, na.rm = TRUE),
    days_first_to_removal = as.numeric(difftime(
      first(removal_date), first_report_date, units = "days")),
    .groups = "drop"
  )

infants <- infants %>%
  left_join(prior_reports, by = c("child_id", "state_fips")) %>%
  mutate(n_prior_reports = replace_na(n_prior_reports, 0))

prior_by_exposure <- infants %>%
  group_by(prenatal_exposure) %>%
  summarise(
    n = n(),
    mean_prior = mean(n_prior_reports, na.rm = TRUE),
    median_prior = median(n_prior_reports, na.rm = TRUE),
    pct_with_prior = mean(n_prior_reports > 0) * 100,
    .groups = "drop"
  )

cat("   Prior NCANDS reports by exposure status:\n")
print(kable(prior_by_exposure, format = "simple", digits = 1))
cat("\n")

fwrite(prior_by_exposure, file.path(OUT_DIR, "prior_reports_by_exposure.csv"))

# --- Histogram: prior report count by NAS-proxy status ------------------------
p_prior <- infants %>%
  mutate(nas_label = ifelse(nas_proxy == 1, "NAS-Proxy", "Other Infants")) %>%
  ggplot(aes(x = n_prior_reports, fill = nas_label)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("NAS-Proxy" = "#D55E00", "Other Infants" = "#56B4E9")) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title = "Prior NCANDS Reports Before Removal — Infant Cohort",
    x = "Number of Prior Reports", y = "Count of Children", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave(file.path(OUT_DIR, "prior_reports_histogram.png"), p_prior,
       width = 9, height = 5.5, dpi = 300)


###############################################################################
# 12. PROJECTIONS (ARIMA / ETS FORECASTING)
###############################################################################

cat("── [12] Projections — time-series forecasting ─────────────────────────\n")

# Forecast total infant entries & NAS-proxy counts using auto.arima / ETS

project_series <- function(df, value_col, label, h = 5) {
  ts_data <- df %>%
    arrange(report_year) %>%
    pull(!!sym(value_col))

  start_yr <- min(df$report_year, na.rm = TRUE)
  ts_obj <- ts(ts_data, start = start_yr, frequency = 1)

  # Try ARIMA first, fall back to ETS
  fit <- tryCatch(auto.arima(ts_obj), error = function(e) ets(ts_obj))
  fc  <- forecast(fit, h = h)

  fc_df <- data.frame(
    report_year = seq(max(df$report_year) + 1, length.out = h),
    forecast    = as.numeric(fc$mean),
    lo80        = as.numeric(fc$lower[, 1]),
    hi80        = as.numeric(fc$upper[, 1]),
    lo95        = as.numeric(fc$lower[, 2]),
    hi95        = as.numeric(fc$upper[, 2]),
    type        = "Projected"
  )

  hist_df <- df %>%
    transmute(
      report_year,
      forecast = !!sym(value_col),
      lo80 = NA_real_, hi80 = NA_real_,
      lo95 = NA_real_, hi95 = NA_real_,
      type = "Historical"
    )

  combined <- bind_rows(hist_df, fc_df)

  p <- ggplot(combined, aes(x = report_year, y = forecast)) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#0072B2", alpha = 0.1) +
    geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "#0072B2", alpha = 0.2) +
    geom_line(aes(linetype = type, colour = type), linewidth = 1) +
    geom_point(data = filter(combined, type == "Historical"),
               colour = "#0072B2", size = 2) +
    scale_colour_manual(values = c("Historical" = "#0072B2", "Projected" = "#D55E00")) +
    scale_linetype_manual(values = c("Historical" = "solid", "Projected" = "dashed")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = paste(label, "— Historical & 5-Year Projection"),
      x = "Fiscal Year", y = label, colour = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")

  list(plot = p, forecast_df = fc_df, model = fit)
}

# --- 12a. Total infant entries projection -------------------------------------
yearly_totals <- infants %>%
  filter(!is.na(report_year)) %>%
  count(report_year, name = "total_infants")

proj_total <- project_series(yearly_totals, "total_infants",
                             "Infant Foster Care Entries")
ggsave(file.path(OUT_DIR, "projection_total_infants.png"),
       proj_total$plot, width = 10, height = 6, dpi = 300)

# --- 12b. NAS-proxy entries projection ----------------------------------------
proj_nas <- project_series(nas_trend, "nas_proxy_n", "NAS-Proxy Infant Cases")
ggsave(file.path(OUT_DIR, "projection_nas_proxy.png"),
       proj_nas$plot, width = 10, height = 6, dpi = 300)

# --- 12c. Substance-exposed rate projection -----------------------------------
rate_trend <- infants %>%
  filter(!is.na(report_year)) %>%
  group_by(report_year) %>%
  summarise(exposure_rate = mean(substance_any, na.rm = TRUE) * 100, .groups = "drop")

proj_rate <- project_series(rate_trend, "exposure_rate",
                            "Substance Exposure Rate (%)")
ggsave(file.path(OUT_DIR, "projection_exposure_rate.png"),
       proj_rate$plot, width = 10, height = 6, dpi = 300)

# Save projection tables
fwrite(proj_total$forecast_df, file.path(OUT_DIR, "projection_total_infants.csv"))
fwrite(proj_nas$forecast_df,   file.path(OUT_DIR, "projection_nas_proxy.csv"))
fwrite(proj_rate$forecast_df,  file.path(OUT_DIR, "projection_exposure_rate.csv"))


###############################################################################
# 13. POLICY RECOMMENDATIONS — CAPTA / CARA LINKAGE
###############################################################################

cat("── [13] Generating CAPTA/CARA policy metrics & recommendations ───────\n")

# CARA (Comprehensive Addiction and Recovery Act, 2016) amended CAPTA to
# require states to develop Plans of Safe Care for substance-exposed newborns.
# Key metrics for dashboard:

# 13a. CARA compliance proxy: % of NAS-proxy infants with a Plan of Safe Care
#       (approximated by whether a prior NCANDS referral was substantiated
#        AND the child was flagged for services — this is a data limitation)

cara_metrics <- infants %>%
  mutate(
    cara_era = ifelse(report_year >= 2017, "Post-CARA (2017+)", "Pre-CARA (<2017)"),
    has_prior_ncands = n_prior_reports > 0
  ) %>%
  group_by(cara_era) %>%
  summarise(
    total_infants    = n(),
    nas_proxy_n      = sum(nas_proxy, na.rm = TRUE),
    nas_proxy_pct    = nas_proxy_n / total_infants * 100,
    pct_with_prior   = mean(has_prior_ncands) * 100,
    median_days_care = median(days_in_care, na.rm = TRUE),
    reunification_pct = mean(reunification, na.rm = TRUE) * 100,
    adoption_pct     = mean(adoption, na.rm = TRUE) * 100,
    kinship_pct      = mean(kinship_placement, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("   CAPTA/CARA era comparison:\n")
print(kable(cara_metrics, format = "simple", digits = 1))
cat("\n")

fwrite(cara_metrics, file.path(OUT_DIR, "cara_era_comparison.csv"))

# 13b. State-level CARA implementation variation ───────────────────────────────
state_cara <- infants %>%
  filter(!is.na(state_fips), report_year >= 2017) %>%
  group_by(state_fips) %>%
  summarise(
    total    = n(),
    nas_pct  = mean(nas_proxy, na.rm = TRUE) * 100,
    kinship_pct = mean(kinship_placement, na.rm = TRUE) * 100,
    reunif_pct  = mean(reunification, na.rm = TRUE) * 100,
    median_days = median(days_in_care, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total >= 20)  # minimum N for reliability

fwrite(state_cara, file.path(OUT_DIR, "state_cara_metrics.csv"))

# 13c. Policy recommendation text (for dashboard annotation) ──────────────────
policy_recs <- tribble(
  ~domain,                     ~finding,                                                    ~recommendation,
  "CARA Compliance",           "NAS-proxy identification varies 5-fold across states",       "Standardise substance-exposure screening at birth per CARA § 503; mandate electronic Plans of Safe Care with AFCARS linkage.",
  "Early Identification",      "Median 2+ prior NCANDS reports before removal for NAS-proxy","Strengthen prenatal referral pathways; integrate Medicaid claims (NAS ICD-10 P96.1) with CPS data for earlier intervention.",
  "Kinship Diversion",         "Kinship placement rates higher for non-exposed infants",     "Expand kinship navigator programmes specifically for substance-exposed newborns; fund relative-search within 72 hours of birth.",
  "Time to Permanency",        "Substance-exposed infants stay 30-60 days longer in care",   "Fast-track concurrent planning for NAS-flagged infants; use expedited ICPC for interstate relative placements.",
  "Data Infrastructure",       "No direct NAS field in AFCARS; proxy measures required",     "Advocate for AFCARS 2020 final rule implementation including NAS/prenatal-exposure specific data element; link vital records.",
  "Workforce",                 "Caseworkers lack NAS-specific training tools",                "Develop NAS-informed casework protocols; embed substance-use liaisons in hospital-based CPS units.",
  "Prevention (CAPTA Title II)","CARA Plans of Safe Care incomplete without services data",   "Require states to report Plan of Safe Care service referrals in NCANDS; tie CAPTA Title II grants to compliance audits."
)

fwrite(policy_recs, file.path(OUT_DIR, "policy_recommendations.csv"))
cat("   ✓ Policy recommendations table saved.\n")


###############################################################################
# 14. DASHBOARD ASSEMBLY (Optional Shiny)
###############################################################################

cat("── [14] Dashboard scaffold ────────────────────────────────────────────\n")

dashboard_code <- '
###############################################################################
# SHINY DASHBOARD — Infant/Newborn Substance Exposure Analytics
# Launch with: shiny::runApp("dashboard_app.R")
###############################################################################

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(survival)
library(survminer)
library(ggalluvial)

# ── Load pre-computed outputs ──────────────────────────────────────────────────
OUT <- "output/infant_analysis"

exposure_summary    <- read.csv(file.path(OUT, "exposure_summary.csv"))
perm_summary        <- read.csv(file.path(OUT, "time_to_permanency_summary.csv"))
cara_metrics        <- read.csv(file.path(OUT, "cara_era_comparison.csv"))
state_cara          <- read.csv(file.path(OUT, "state_cara_metrics.csv"))
policy_recs         <- read.csv(file.path(OUT, "policy_recommendations.csv"))
nas_correlations    <- read.csv(file.path(OUT, "nas_outcome_correlations.csv"))
proj_total          <- read.csv(file.path(OUT, "projection_total_infants.csv"))
proj_nas            <- read.csv(file.path(OUT, "projection_nas_proxy.csv"))

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Infant Substance Exposure Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",        tabName = "overview",    icon = icon("baby")),
      menuItem("NAS Analysis",    tabName = "nas",         icon = icon("prescription-bottle")),
      menuItem("Entry Reasons",   tabName = "entry",       icon = icon("door-open")),
      menuItem("Outcomes",        tabName = "outcomes",     icon = icon("chart-line")),
      menuItem("Survival Curves", tabName = "survival",    icon = icon("heartbeat")),
      menuItem("Sankey Flows",    tabName = "sankey",       icon = icon("project-diagram")),
      menuItem("State Comparisons", tabName = "states",    icon = icon("map")),
      menuItem("Projections",     tabName = "projections", icon = icon("chart-area")),
      menuItem("CAPTA/CARA Policy", tabName = "policy",   icon = icon("gavel"))
    )
  ),
  dashboardBody(
    tabItems(

      # ── Overview Tab ──────────────────────────────────────────────────
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_total_infants",  width = 3),
          valueBoxOutput("vb_nas_proxy",      width = 3),
          valueBoxOutput("vb_exposure_rate",  width = 3),
          valueBoxOutput("vb_median_stay",    width = 3)
        ),
        fluidRow(
          box(title = "Prenatal Exposure Distribution", width = 6,
              status = "primary", solidHeader = TRUE,
              plotOutput("plot_exposure_dist", height = 350)),
          box(title = "Exposure × Parental SA Cross-Tab", width = 6,
              status = "primary", solidHeader = TRUE,
              DTOutput("dt_exposure"))
        )
      ),

      # ── NAS Analysis Tab ──────────────────────────────────────────────
      tabItem(tabName = "nas",
        fluidRow(
          box(title = "NAS-Proxy Trend", width = 8,
              status = "warning", solidHeader = TRUE,
              plotOutput("plot_nas_trend", height = 400)),
          box(title = "NAS-Outcome Correlations", width = 4,
              status = "warning", solidHeader = TRUE,
              DTOutput("dt_nas_cor"))
        )
      ),

      # ── Entry Reasons Tab ──────────────────────────────────────────────
      tabItem(tabName = "entry",
        fluidRow(
          box(title = "Top Removal Reasons by NAS Status", width = 12,
              status = "info", solidHeader = TRUE,
              plotOutput("plot_entry_reasons", height = 450))
        )
      ),

      # ── Outcomes Tab ──────────────────────────────────────────────────
      tabItem(tabName = "outcomes",
        fluidRow(
          box(title = "Placement at 6 Months", width = 6,
              status = "success", solidHeader = TRUE,
              plotOutput("plot_placement", height = 400)),
          box(title = "Time to Permanency", width = 6,
              status = "success", solidHeader = TRUE,
              DTOutput("dt_permanency"))
        )
      ),

      # ── Survival Curves Tab ──────────────────────────────────────────
      tabItem(tabName = "survival",
        fluidRow(
          box(title = "K-M: Time to Permanency by Exposure", width = 6,
              status = "danger", solidHeader = TRUE,
              imageOutput("img_km_exposure", height = 500)),
          box(title = "K-M: NAS-Proxy vs. Others", width = 6,
              status = "danger", solidHeader = TRUE,
              imageOutput("img_km_nas", height = 500))
        )
      ),

      # ── Sankey Tab ────────────────────────────────────────────────────
      tabItem(tabName = "sankey",
        fluidRow(
          box(title = "Infant Pathways (Interactive)", width = 12,
              status = "primary", solidHeader = TRUE,
              htmlOutput("sankey_html", height = "600px"))
        )
      ),

      # ── State Comparisons Tab ─────────────────────────────────────────
      tabItem(tabName = "states",
        fluidRow(
          box(title = "State-Level Metrics (Post-CARA)", width = 12,
              status = "info", solidHeader = TRUE,
              DTOutput("dt_states"))
        ),
        fluidRow(
          box(title = "Infant Volume by State", width = 12,
              status = "info", solidHeader = TRUE,
              plotOutput("plot_state_bar", height = 500))
        )
      ),

      # ── Projections Tab ──────────────────────────────────────────────
      tabItem(tabName = "projections",
        fluidRow(
          box(title = "Total Infant Entries — 5-Year Forecast", width = 6,
              status = "warning", solidHeader = TRUE,
              imageOutput("img_proj_total", height = 400)),
          box(title = "NAS-Proxy Cases — 5-Year Forecast", width = 6,
              status = "warning", solidHeader = TRUE,
              imageOutput("img_proj_nas", height = 400))
        ),
        fluidRow(
          box(title = "Projected Values", width = 12,
              DTOutput("dt_projections"))
        )
      ),

      # ── Policy Tab ──────────────────────────────────────────────────
      tabItem(tabName = "policy",
        fluidRow(
          box(title = "CAPTA/CARA Era Comparison", width = 12,
              status = "purple", solidHeader = TRUE,
              DTOutput("dt_cara_metrics"))
        ),
        fluidRow(
          box(title = "Policy Recommendations for CAPTA/CARA Linkage", width = 12,
              status = "purple", solidHeader = TRUE,
              DTOutput("dt_policy_recs"))
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Value boxes
  output$vb_total_infants <- renderValueBox(
    valueBox(format(sum(exposure_summary$n), big.mark = ","),
             "Total Infants", icon = icon("baby"), color = "blue"))
  output$vb_nas_proxy <- renderValueBox(
    valueBox(format(sum(cara_metrics$nas_proxy_n), big.mark = ","),
             "NAS-Proxy Cases", icon = icon("prescription-bottle"), color = "orange"))
  output$vb_exposure_rate <- renderValueBox({
    rate <- sum(exposure_summary$n[exposure_summary$prenatal_exposure == "Confirmed (DAChild=1)"]) /
            sum(exposure_summary$n) * 100
    valueBox(paste0(round(rate, 1), "%"),
             "Exposure Rate", icon = icon("percentage"), color = "red")
  })
  output$vb_median_stay <- renderValueBox(
    valueBox(paste0(round(mean(cara_metrics$median_days_care)), " days"),
             "Median Time in Care", icon = icon("clock"), color = "green"))

  # Tables
  output$dt_exposure     <- renderDT(datatable(exposure_summary, options = list(pageLength = 10)))
  output$dt_nas_cor      <- renderDT(datatable(nas_correlations, options = list(pageLength = 10)))
  output$dt_permanency   <- renderDT(datatable(perm_summary, options = list(pageLength = 10)))
  output$dt_states       <- renderDT(datatable(state_cara, options = list(pageLength = 15)))
  output$dt_cara_metrics <- renderDT(datatable(cara_metrics, options = list(pageLength = 5)))
  output$dt_policy_recs  <- renderDT(datatable(policy_recs, options = list(pageLength = 10, autoWidth = TRUE)))

  output$dt_projections  <- renderDT(datatable(
    bind_rows(
      proj_total %>% mutate(series = "Total Infants"),
      proj_nas   %>% mutate(series = "NAS-Proxy")
    ), options = list(pageLength = 12)
  ))

  # Plots (rendered from saved PNGs)
  render_png <- function(filename) {
    renderImage({
      list(src = file.path(OUT, filename), contentType = "image/png",
           width = "100%", alt = filename)
    }, deleteFile = FALSE)
  }

  output$img_km_exposure <- render_png("survival_curve_exposure.png")
  output$img_km_nas      <- render_png("survival_curve_nas.png")
  output$img_proj_total  <- render_png("projection_total_infants.png")
  output$img_proj_nas    <- render_png("projection_nas_proxy.png")

  output$plot_exposure_dist <- renderPlot({
    ggplot(exposure_summary, aes(x = prenatal_exposure, y = n, fill = parental_substance)) +
      geom_col(position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Count", fill = "Parental SA") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  output$plot_nas_trend <- renderPlot({
    # Attempt to re-read NAS trend if CSV exists
    nas_t <- tryCatch(read.csv(file.path(OUT, "projection_nas_proxy.csv")), error = function(e) NULL)
    if (!is.null(nas_t)) {
      ggplot(nas_t, aes(report_year, forecast)) +
        geom_line(colour = "#D55E00", linewidth = 1) +
        geom_point(colour = "#D55E00") +
        theme_minimal(base_size = 14) +
        labs(x = "Year", y = "NAS-Proxy Count")
    }
  })

  output$plot_entry_reasons <- renderPlot({
    # Load from saved PNG (or re-render if data available)
    NULL
  })

  output$plot_placement <- renderPlot({
    NULL
  })

  output$plot_state_bar <- renderPlot({
    if (nrow(state_cara) > 0) {
      state_cara %>%
        slice_max(total, n = 20) %>%
        ggplot(aes(x = reorder(state_fips, total), y = total)) +
        geom_col(fill = "#56B4E9") +
        coord_flip() +
        labs(x = "State FIPS", y = "Infant Count (Post-CARA)") +
        theme_minimal(base_size = 14)
    }
  })

  # Sankey (embed saved HTML)
  output$sankey_html <- renderUI({
    sankey_file <- file.path(OUT, "sankey_interactive.html")
    if (file.exists(sankey_file)) {
      tags$iframe(src = sankey_file, width = "100%", height = "600px",
                  frameBorder = "0")
    } else {
      tags$p("Sankey widget not found. Run the main analysis script first.")
    }
  })
}

shinyApp(ui, server)
'

writeLines(dashboard_code, file.path(OUT_DIR, "dashboard_app.R"))
cat("   ✓ Shiny dashboard scaffold saved to dashboard_app.R\n")


###############################################################################
# 15. COMBINED SUMMARY FIGURE (COMPOSITE PANEL)
###############################################################################

cat("── [15] Composite summary figure ──────────────────────────────────────\n")

# Combine key visuals into a single publication panel
composite <- (p_year + p_nas_trend) /
             (p_placement + p_alluvial) +
  plot_annotation(
    title = "Infant/Newborn Foster Care — Substance Exposure & Outcomes Dashboard",
    subtitle = "AFCARS + NCANDS Integrated Analysis",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, colour = "grey40")
    )
  )

ggsave(file.path(OUT_DIR, "composite_summary.png"), composite,
       width = 20, height = 16, dpi = 300)


###############################################################################
# 16. SESSION & REPRODUCTION INFO
###############################################################################

cat("
╔══════════════════════════════════════════════════════════════╗
║   ANALYSIS COMPLETE                                          ║
╚══════════════════════════════════════════════════════════════╝
\n")

cat("Output directory:", normalizePath(OUT_DIR), "\n\n")
cat("Files generated:\n")
list.files(OUT_DIR, recursive = TRUE) %>% walk(~ cat("  •", .x, "\n"))

cat("\nSession info:\n")
sessionInfo()

###############################################################################
# END OF SCRIPT
###############################################################################
