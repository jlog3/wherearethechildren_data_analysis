###############################################################################
#
#  AFCARS Missing/Runaway Episode Analysis — Comprehensive R Workflow
#
#  Purpose : Incidence, duration, recurrence, timing, placement-type
#            predictors, post-return outcomes, survival analysis,
#            multilevel models, mapping, and dashboard-alert simulation.
#
#  Input   : AFCARS Foster Care (FC) episode-level file(s).
#            Expected columns (standard AFCARS element names):
#              StFCID       – state-assigned child ID (links episodes)
#              RecNumbr     – record number
#              State / St   – FIPS state code
#              PedRevDt     – period of review end date
#              LatRemDt     – latest removal date
#              DtCurSet     – date of current placement setting
#              CurPlSet     – current placement setting code
#              TotalRem     – total number of removals
#              ManRep       – manner of removal from home
#              RsnDisc      – reason for discharge
#              DoDFCDt      – date of discharge from foster care
#              AgeAtLatRem  – age at latest removal
#              RaceEthn     – race/ethnicity (derived)
#              Sex          – sex of child
#              NOTE: the AFCARS "Runaway" indicator is CurPlSet == 6
#                    (or historically element 41 coded as "runaway").
#
#  Outputs : Console summaries, saved plots (PNG/PDF), model tables.
#
#  Author  : [Analyst]
#  Date    : 2026-02-16
#
###############################################################################

# ── 0. ENVIRONMENT SETUP ────────────────────────────────────────────────────

required_pkgs <- c(
  "tidyverse",   # data wrangling + ggplot2

  "lubridate",   # date handling
  "survival",    # Cox PH, Kaplan–Meier
  "survminer",   # ggsurvplot helpers
  "lme4",        # multilevel / mixed-effects models
  "broom",       # tidy model output
  "broom.mixed", # tidy for lme4 objects
  "ggforestplot",# forest plots  (alt: forestplot or ggplot manual)
  "sf",          # spatial data
  "tigris",      # US Census TIGER shapefiles
  "viridis",     # perceptually uniform palettes
  "scales",      # label helpers
  "knitr",       # table output
  "gt",          # polished tables
  "patchwork"    # composing multi-panel figures
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}
invisible(lapply(required_pkgs, install_if_missing))
invisible(lapply(required_pkgs, library, character.only = TRUE))

options(tigris_use_cache = TRUE)        # cache shapefiles locally
theme_set(theme_minimal(base_size = 13))

cat("Environment ready.\n")

###############################################################################
#
#  PART 1 — DATA INGESTION & CLEANING
#
###############################################################################

# ── 1.1  Read raw AFCARS data ───────────────────────────────────────────────
# Adjust path / format as needed (SAS transport → haven::read_xpt,
# CSV → readr::read_csv, SPSS → haven::read_sav, etc.)

# afcars_raw <- haven::read_sav("data/FC2023v1.sav")
# For demonstration we simulate a realistic frame:

set.seed(42)
n_records <- 40000

simulate_afcars <- function(n = n_records) {
  tibble(
    StFCID     = sample(paste0("CH", sprintf("%06d", 1:round(n * 0.7))),
                        n, replace = TRUE),
    RecNumbr   = 1:n,
    State      = sample(c(sprintf("%02d", c(1,4:6,8:13,15:42,44:51,53:56))),
                        n, replace = TRUE),
    LatRemDt   = as.Date("2018-01-01") +
                   sample(0:1825, n, replace = TRUE),
    DtCurSet   = as.Date(NA),
    CurPlSet   = sample(c(1:5, 6, 7, 8), n, replace = TRUE,
                         prob = c(.20,.15,.15,.12,.10,.06,.12,.10)),
    TotalRem   = sample(1:5, n, replace = TRUE, prob = c(.50,.25,.13,.08,.04)),
    AgeAtLatRem = sample(0:17, n, replace = TRUE),
    Sex        = sample(c(1, 2), n, replace = TRUE),
    RaceEthn   = sample(1:7, n, replace = TRUE),
    DoDFCDt    = as.Date(NA),
    RsnDisc    = sample(c(0:5, 99), n, replace = TRUE,
                         prob = c(.35,.20,.15,.10,.08,.07,.05))
  ) |>
    mutate(
      DtCurSet = LatRemDt + sample(0:365, n(), replace = TRUE),
      DoDFCDt  = if_else(RsnDisc != 0,
                         DtCurSet + sample(1:730, n(), replace = TRUE),
                         as.Date(NA))
    )
}

afcars_raw <- simulate_afcars()

cat("Raw records loaded:", nrow(afcars_raw), "\n")

# ── 1.2  Identify missing/runaway episodes ──────────────────────────────────
#
#   AFCARS CurPlSet == 6  →  "Runaway"
#   Some states additionally code short "missing" spells; these may appear as
#   CurPlSet == 6 or in supplemental data elements.  We flag both.

mr_episodes <- afcars_raw |>
  mutate(
    is_runaway = (CurPlSet == 6),
    # Derived placement-type labels
    plac_type = case_when(
      CurPlSet %in% c(1, 2)  ~ "Pre-Adoptive / Foster (Kin)",
      CurPlSet == 3           ~ "Foster (Non-Kin)",
      CurPlSet == 4           ~ "Group Home",
      CurPlSet == 5           ~ "Institution",
      CurPlSet == 6           ~ "Runaway",
      CurPlSet == 7           ~ "Trial Home Visit",
      CurPlSet == 8           ~ "Supervised Indep. Living",
      TRUE                    ~ "Other/Unknown"
    ),
    # Prior placement type: type from the *preceding* record for same child
    # (approximation — real analysis would chain placement sequences)
    plac_group = case_when(
      CurPlSet %in% c(4, 5) ~ "Group / Institution",
      CurPlSet %in% c(1, 2) ~ "Kin / Pre-Adoptive",
      CurPlSet == 3          ~ "Non-Kin Foster",
      TRUE                   ~ "Other"
    ),
    age_group = cut(AgeAtLatRem, breaks = c(-1, 5, 10, 14, 17),
                    labels = c("0-5", "6-10", "11-14", "15-17")),
    sex_label = if_else(Sex == 1, "Male", "Female")
  )

cat("Runaway-flagged records:", sum(mr_episodes$is_runaway), "\n")

###############################################################################
#
#  PART 2 — INCIDENCE ANALYSIS
#
###############################################################################

# ── 2.1  Overall incidence by year ──────────────────────────────────────────

incidence_year <- mr_episodes |>
  mutate(year = year(DtCurSet)) |>
  group_by(year) |>
  summarise(
    total_episodes  = n(),
    runaway_episodes = sum(is_runaway),
    rate_per_1000   = runaway_episodes / total_episodes * 1000,
    .groups = "drop"
  )

p_incidence <- ggplot(incidence_year, aes(year, rate_per_1000)) +
  geom_line(linewidth = 1, colour = "#D55E00") +
  geom_point(size = 2.5, colour = "#D55E00") +
  labs(
    title    = "Missing / Runaway Episode Rate per 1,000 FC Records",
    subtitle = "AFCARS episode-level, by calendar year of placement setting date",
    x = "Year", y = "Runaway episodes per 1,000 records"
  )

ggsave("output/01_incidence_year.png", p_incidence,
       width = 8, height = 5, dpi = 300, bg = "white")

# ── 2.2  Incidence by age group & sex ───────────────────────────────────────

incidence_demo <- mr_episodes |>
  group_by(age_group, sex_label) |>
  summarise(
    n_total  = n(),
    n_run    = sum(is_runaway),
    rate     = n_run / n_total * 1000,
    .groups  = "drop"
  )

p_demo <- ggplot(incidence_demo,
                 aes(age_group, rate, fill = sex_label)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#E69F00")) +
  labs(
    title = "Runaway Rate by Age Group & Sex",
    x = "Age group at latest removal",
    y = "Episodes per 1,000 records", fill = NULL
  )

ggsave("output/02_incidence_demo.png", p_demo,
       width = 8, height = 5, dpi = 300, bg = "white")

###############################################################################
#
#  PART 3 — DURATION ANALYSIS
#
###############################################################################

# Duration of runaway spell = time between DtCurSet (runaway placement) and
# either next placement date or discharge date.
# In practice, linking sequential placements per child is required.  Here we
# approximate duration as DoDFCDt − DtCurSet for discharged runaway records,
# or a censored observation for those still in care.

run_only <- mr_episodes |>
  filter(is_runaway) |>
  mutate(
    discharged  = !is.na(DoDFCDt),
    duration    = as.numeric(
      if_else(discharged,
              DoDFCDt - DtCurSet,
              as.Date("2024-09-30") - DtCurSet)   # administrative censor
    ),
    duration    = pmax(duration, 1)                 # floor at 1 day
  )

cat("\n--- Duration summary (days) ---\n")
print(summary(run_only$duration))

# ── 3.1  Kaplan–Meier: time-to-return from runaway spell ────────────────────

surv_obj <- Surv(time  = run_only$duration,
                 event = as.numeric(run_only$discharged))

km_fit  <- survfit(surv_obj ~ 1)

p_km_overall <- ggsurvplot(
  km_fit,
  data       = run_only,
  conf.int   = TRUE,
  risk.table = TRUE,
  xlab       = "Days since runaway placement setting",
  ylab       = "Probability still missing",
  title      = "Kaplan–Meier: Duration of Runaway Spell",
  palette    = "#D55E00",
  ggtheme    = theme_minimal(base_size = 13)
)

png("output/03_km_overall.png", width = 9, height = 7,
    units = "in", res = 300)
print(p_km_overall)
dev.off()

# ── 3.2  K-M stratified by prior placement type ────────────────────────────
# We join prior placement type from the child's *preceding* record.

prior_plac <- mr_episodes |>
  arrange(StFCID, DtCurSet) |>
  group_by(StFCID) |>
  mutate(prior_plac_group = lag(plac_group, default = "Unknown")) |>
  ungroup() |>
  filter(is_runaway) |>
  mutate(
    discharged = !is.na(DoDFCDt),
    duration   = as.numeric(
      if_else(discharged,
              DoDFCDt - DtCurSet,
              as.Date("2024-09-30") - DtCurSet)),
    duration   = pmax(duration, 1)
  )

km_plac <- survfit(
  Surv(duration, as.numeric(discharged)) ~ prior_plac_group,
  data = prior_plac
)

p_km_plac <- ggsurvplot(
  km_plac,
  data       = prior_plac,
  conf.int   = TRUE,
  pval       = TRUE,
  risk.table = TRUE,
  xlab       = "Days",
  ylab       = "P(still missing)",
  title      = "KM by Prior Placement Type",
  palette    = "jco",
  ggtheme    = theme_minimal(base_size = 13)
)

png("output/04_km_by_placement.png", width = 10, height = 7,
    units = "in", res = 300)
print(p_km_plac)
dev.off()

###############################################################################
#
#  PART 4 — RECURRENCE (MULTIPLE EPISODES PER CHILD)
#
###############################################################################

recurrence <- mr_episodes |>
  filter(is_runaway) |>
  group_by(StFCID) |>
  summarise(
    n_runaway_eps = n(),
    first_run_age = min(AgeAtLatRem, na.rm = TRUE),
    .groups       = "drop"
  )

cat("\n--- Recurrence distribution ---\n")
print(table(pmin(recurrence$n_runaway_eps, 5)))

p_recur <- ggplot(recurrence,
                  aes(x = pmin(n_runaway_eps, 5))) +
  geom_bar(fill = "#56B4E9", width = 0.7) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("1", "2", "3", "4", "5+")
  ) +
  labs(
    title = "Recurrence: Runaway Episodes per Child",
    x     = "Number of runaway episodes",
    y     = "Number of children"
  )

ggsave("output/05_recurrence.png", p_recur,
       width = 7, height = 5, dpi = 300, bg = "white")

# ── 4.1  Negative-binomial model for recurrence count ───────────────────────
#    (child-level)

recurrence_model_data <- mr_episodes |>
  group_by(StFCID) |>
  summarise(
    n_runaway     = sum(is_runaway),
    age_first     = min(AgeAtLatRem),
    sex           = first(sex_label),
    ever_group    = any(CurPlSet %in% c(4, 5)) |> as.integer(),
    total_rem     = max(TotalRem),
    .groups       = "drop"
  )

nb_fit <- MASS::glm.nb(
  n_runaway ~ age_first + sex + ever_group + total_rem,
  data = recurrence_model_data
)

cat("\n--- Negative-binomial recurrence model ---\n")
print(summary(nb_fit))

###############################################################################
#
#  PART 5 — TIMING (WITHIN FIRST 6 MONTHS OF PLACEMENT)
#
###############################################################################

timing_data <- mr_episodes |>
  filter(is_runaway) |>
  mutate(
    days_to_runaway = as.numeric(DtCurSet - LatRemDt),
    within_6mo      = days_to_runaway <= 180
  )

cat("\n--- Timing: % runaway within first 6 months of removal ---\n")
cat(sprintf("  Within 6 months: %d / %d  (%.1f%%)\n",
            sum(timing_data$within_6mo),
            nrow(timing_data),
            mean(timing_data$within_6mo) * 100))

p_timing <- ggplot(timing_data,
                   aes(x = pmin(days_to_runaway, 730))) +
  geom_histogram(binwidth = 30, fill = "#009E73", colour = "white") +
  geom_vline(xintercept = 180, linetype = "dashed", colour = "red",
             linewidth = 0.8) +
  annotate("text", x = 195, y = Inf, vjust = 2,
           label = "6-month mark", colour = "red", fontface = "italic") +
  labs(
    title = "Days from Removal to First Runaway Episode",
    x     = "Days since latest removal (capped at 730)",
    y     = "Count of episodes"
  )

ggsave("output/06_timing_histogram.png", p_timing,
       width = 8, height = 5, dpi = 300, bg = "white")

###############################################################################
#
#  PART 6 — PLACEMENT-TYPE PREDICTORS (COX PH + FOREST PLOT)
#
###############################################################################

# Cox model: hazard of running away, with placement type as key predictor.
# Unit of analysis: each placement-setting record.
# Event: child enters runaway status.

cox_data <- mr_episodes |>
  mutate(
    event      = as.numeric(is_runaway),
    days_in_ep = as.numeric(
      if_else(!is.na(DoDFCDt),
              DoDFCDt - DtCurSet,
              as.Date("2024-09-30") - DtCurSet)),
    days_in_ep = pmax(days_in_ep, 1),
    plac_group = relevel(factor(plac_group), ref = "Non-Kin Foster")
  )

cox_fit <- coxph(
  Surv(days_in_ep, event) ~ plac_group + age_group + sex_label +
    factor(TotalRem),
  data = cox_data
)

cat("\n--- Cox PH: Placement-type predictors of runaway ---\n")
print(summary(cox_fit))

# ── 6.1  Forest plot of hazard ratios ──────────────────────────────────────

cox_tidy <- broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) |>
  filter(!is.na(estimate)) |>
  mutate(term = str_replace_all(term, "plac_group|age_group|sex_label|factor\\(TotalRem\\)", ""))

p_forest <- ggplot(cox_tidy, aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_point(size = 3, colour = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, colour = "#0072B2") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_log10() +
  labs(
    title = "Cox PH Model — Hazard Ratios for Runaway Episode",
    x     = "Hazard Ratio (log scale)",
    y     = NULL
  ) +
  theme(axis.text.y = element_text(size = 11))

ggsave("output/07_forest_plot.png", p_forest,
       width = 9, height = 6, dpi = 300, bg = "white")

###############################################################################
#
#  PART 7 — MULTILEVEL MODEL (CHILDREN NESTED IN STATES)
#
###############################################################################

# Random intercept for State, child-level predictors.
# Outcome: binary (runaway in this record).

ml_data <- mr_episodes |>
  mutate(
    plac_group = relevel(factor(plac_group), ref = "Non-Kin Foster"),
    age_group  = relevel(factor(age_group), ref = "11-14")
  )

ml_fit <- glmer(
  is_runaway ~ plac_group + age_group + sex_label + TotalRem +
    (1 | State),
  data    = ml_data,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)),
  nAGQ    = 1               # Laplace approximation for speed
)

cat("\n--- Multilevel logistic model ---\n")
print(summary(ml_fit))

# Extract state random intercepts (log-odds offset for each state)
state_re <- ranef(ml_fit)$State |>
  tibble::rownames_to_column("STATEFP") |>
  rename(re_intercept = `(Intercept)`)

cat("\nStates with highest runaway random effect:\n")
print(head(arrange(state_re, desc(re_intercept)), 10))

###############################################################################
#
#  PART 8 — STATE MAP OF EPISODE DENSITY
#
###############################################################################

state_rates <- mr_episodes |>
  group_by(State) |>
  summarise(
    n_total  = n(),
    n_run    = sum(is_runaway),
    rate     = n_run / n_total * 1000,
    .groups  = "drop"
  ) |>
  rename(STATEFP = State)

# Fetch state boundaries from Census TIGER
us_states <- tigris::states(cb = TRUE, year = 2022) |>
  shift_geometry() |>                     # shift AK/HI for nicer CONUS map
  left_join(state_rates, by = "STATEFP")

p_map <- ggplot(us_states) +
  geom_sf(aes(fill = rate), colour = "white", linewidth = 0.25) +
  scale_fill_viridis_c(
    option = "inferno", direction = -1,
    name   = "Runaway\nper 1,000",
    na.value = "grey85"
  ) +
  labs(
    title    = "Missing/Runaway Episode Density by State",
    subtitle = "Rate per 1,000 AFCARS placement-setting records",
    caption  = "Source: AFCARS FC file  |  Simulated data for illustration"
  ) +
  theme_void(base_size = 13) +
  theme(legend.position = c(0.92, 0.25))

ggsave("output/08_state_map.png", p_map,
       width = 11, height = 7, dpi = 300, bg = "white")

###############################################################################
#
#  PART 9 — POST-RETURN OUTCOMES
#
###############################################################################

# For children who returned from a runaway spell:
# (a) Did they have a *subsequent* removal (re-entry)?
# (b) Did they reach permanency (RsnDisc == 1 adoption, 2 reunification,
#     3 guardianship) within 12 months of return?

post_return <- mr_episodes |>
  arrange(StFCID, DtCurSet) |>
  group_by(StFCID) |>
  mutate(
    had_runaway      = cumany_run(is_runaway),
    post_runaway_row = cumsum(is_runaway) > 0 & !is_runaway
  ) |>
  ungroup()

# Helper: cumulative-any for runaway flag
cumany_run <- function(x) {
  out <- logical(length(x))
  flag <- FALSE
  for (i in seq_along(x)) {
    if (x[i]) flag <- TRUE
    out[i] <- flag
  }
  out
}

# Rebuild with helper
post_return <- mr_episodes |>
  arrange(StFCID, DtCurSet) |>
  group_by(StFCID) |>
  mutate(
    ever_ran    = cumany_run(is_runaway),
    after_run   = ever_ran & !is_runaway,
    permanency  = RsnDisc %in% c(1, 2, 3),
    reentry     = TotalRem > 1
  ) |>
  ungroup()

post_run_outcomes <- post_return |>
  filter(after_run) |>
  group_by(StFCID) |>
  summarise(
    reached_perm   = any(permanency),
    had_reentry    = any(reentry),
    .groups        = "drop"
  )

cat("\n--- Post-return outcomes (among children who returned) ---\n")
cat(sprintf("  Permanency achieved:  %.1f%%\n",
            mean(post_run_outcomes$reached_perm) * 100))
cat(sprintf("  Re-entry (re-removal): %.1f%%\n",
            mean(post_run_outcomes$had_reentry) * 100))

# ── 9.1  Compare outcomes: ever-runaway vs never-runaway ────────────────────

child_outcomes <- mr_episodes |>
  group_by(StFCID) |>
  summarise(
    ever_runaway = any(is_runaway),
    permanency   = any(RsnDisc %in% c(1, 2, 3)),
    reentry      = max(TotalRem) > 1,
    .groups      = "drop"
  )

outcome_compare <- child_outcomes |>
  group_by(ever_runaway) |>
  summarise(
    n_children    = n(),
    pct_perm      = mean(permanency) * 100,
    pct_reentry   = mean(reentry) * 100,
    .groups       = "drop"
  )

cat("\n--- Outcome comparison ---\n")
print(outcome_compare)

p_outcomes <- outcome_compare |>
  pivot_longer(cols = c(pct_perm, pct_reentry),
               names_to = "outcome", values_to = "pct") |>
  mutate(
    outcome = if_else(outcome == "pct_perm",
                      "Permanency", "Re-entry"),
    group   = if_else(ever_runaway,
                      "Ever Runaway", "Never Runaway")
  ) |>
  ggplot(aes(outcome, pct, fill = group)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Ever Runaway" = "#D55E00",
                                "Never Runaway" = "#009E73")) +
  labs(
    title = "Post-Return Outcomes: Runaway vs. Non-Runaway Children",
    x     = NULL, y = "Percent of children", fill = NULL
  ) +
  scale_y_continuous(labels = label_percent(scale = 1))

ggsave("output/09_outcomes_compare.png", p_outcomes,
       width = 7, height = 5, dpi = 300, bg = "white")

###############################################################################
#
#  PART 10 — UNDER-REPORTING NOTES & ADJUSTMENTS
#
###############################################################################

cat("\n",
"=======================================================================\n",
"  UNDER-REPORTING CONSIDERATIONS\n",
"=======================================================================\n",
"
  1. CurPlSet == 6 captures *reported* runaway/missing episodes only.
     Short-duration absences (< 24 hours) are often not recorded in
     AFCARS because the child returns before the next placement-setting
     update.

  2. State variation in coding practices is substantial.  Some states
     create a new placement record for every missing spell; others
     update the record only if the spell exceeds a threshold (e.g.,
     72 hours).  This produces artificial cross-state heterogeneity
     that inflates or deflates rates.

  3. Group-home and congregate-care settings may under-report brief
     AWOL incidents to avoid regulatory scrutiny, biasing incidence
     downward for CurPlSet 4-5.

  4. AFCARS does not distinguish 'missing' (involuntary) from
     'runaway' (voluntary).  Supplemental state administrative data or
     NCIC entries are needed to parse these subcategories.

  5. Sensitivity analysis recommendation: compare AFCARS runaway
     counts with NCIC missing-child entries by state-year to estimate
     a reporting ratio.  A ratio < 0.5 suggests significant under-
     reporting.

  6. Multiply-imputed runaway counts (e.g., via capture–recapture
     with NCIC as a second list) can adjust incidence estimates.
     Package 'Rcapture' provides log-linear capture–recapture models.
\n")

###############################################################################
#
#  PART 11 — DASHBOARD ALERT SIMULATION
#
###############################################################################

cat("\n",
"=======================================================================\n",
"  DASHBOARD ALERT LOGIC — SIMULATION & DESIGN NOTES\n",
"=======================================================================\n")

# ── 11.1  Concept ───────────────────────────────────────────────────────────
#
# A live child-welfare dashboard could surface alerts when a child's
# predicted probability of running away crosses a threshold.  Below we
# simulate this using the fitted multilevel model.

# Predict runaway probability for each current record
alert_data <- ml_data |>
  mutate(
    p_run = predict(ml_fit, newdata = ml_data, type = "response",
                    allow.new.levels = TRUE)
  )

# ── 11.2  Threshold calibration ────────────────────────────────────────────

thresholds <- c(0.05, 0.10, 0.15, 0.20)

alert_table <- map_dfr(thresholds, function(thr) {
  flagged   <- alert_data |> filter(p_run >= thr)
  true_pos  <- sum(flagged$is_runaway)
  false_pos <- nrow(flagged) - true_pos
  actual    <- sum(alert_data$is_runaway)
  tibble(
    threshold      = thr,
    n_flagged      = nrow(flagged),
    sensitivity    = true_pos / actual,
    ppv            = true_pos / max(nrow(flagged), 1),
    alert_per_1000 = nrow(flagged) / nrow(alert_data) * 1000
  )
})

cat("\n--- Alert threshold calibration ---\n")
print(alert_table)

# ── 11.3  Simulated alert feed ─────────────────────────────────────────────

chosen_threshold <- 0.10

sample_alerts <- alert_data |>
  filter(p_run >= chosen_threshold) |>
  slice_sample(n = min(15, sum(alert_data$p_run >= chosen_threshold))) |>
  select(StFCID, State, age_group, plac_group, p_run) |>
  arrange(desc(p_run))

cat("\n--- Sample dashboard alerts (threshold =", chosen_threshold, ") ---\n")
print(sample_alerts, n = 15)

cat("
  IMPLEMENTATION NOTES FOR A PRODUCTION DASHBOARD
  ------------------------------------------------
  * Refresh predictions nightly against the live case-management DB.
  * Use the multilevel model (or a gradient-boosted variant) retrained
    quarterly on the most recent AFCARS submission.
  * Alert tiers:
      - RED   (p >= 0.20): immediate caseworker notification + supervisor
      - AMBER (0.10 <= p < 0.20): caseworker notification within 24 h
      - WATCH (0.05 <= p < 0.10): flag in case file for next review
  * Track alert-to-action metrics: time from alert to caseworker contact,
    proportion of alerts that preceded actual runaway within 30 days.
  * Ethical safeguards: model explanations (SHAP values) should accompany
    every alert so caseworkers understand *why* a child was flagged.
    Avoid opaque risk scores.
  * Equity audit: compute false-positive rates by race/ethnicity and
    age group.  If disparities exceed a pre-set tolerance, retrain
    with fairness constraints (e.g., equalized odds post-processing).
\n")

###############################################################################
#
#  PART 12 — SURVIVAL ANALYSIS: TIME TO FIRST RUNAWAY (CHILD-LEVEL)
#
###############################################################################

# Alternative survival framing: for each child, time from first placement
# to first runaway episode (if any).  Children who never run away are
# right-censored.

child_surv <- mr_episodes |>
  arrange(StFCID, DtCurSet) |>
  group_by(StFCID) |>
  summarise(
    first_plac  = min(DtCurSet),
    first_run   = suppressWarnings(min(DtCurSet[is_runaway])),
    ever_ran    = any(is_runaway),
    age_first   = first(AgeAtLatRem),
    sex         = first(sex_label),
    first_plac_group = first(plac_group),
    state       = first(State),
    .groups     = "drop"
  ) |>
  mutate(
    time_days = as.numeric(
      if_else(ever_ran,
              first_run - first_plac,
              as.Date("2024-09-30") - first_plac)),
    time_days = pmax(time_days, 1),
    age_grp   = cut(age_first, c(-1, 5, 10, 14, 17),
                    labels = c("0-5", "6-10", "11-14", "15-17"))
  )

# ── 12.1  Cox PH — child-level, time to first runaway ──────────────────────

cox_child <- coxph(
  Surv(time_days, as.numeric(ever_ran)) ~
    age_grp + sex + first_plac_group + strata(state),
  data = child_surv
)

cat("\n--- Cox PH: Time to first runaway (child-level) ---\n")
print(summary(cox_child))

# ── 12.2  KM by age group ──────────────────────────────────────────────────

km_age <- survfit(
  Surv(time_days, as.numeric(ever_ran)) ~ age_grp,
  data = child_surv
)

p_km_age <- ggsurvplot(
  km_age,
  data       = child_surv,
  conf.int   = FALSE,
  pval       = TRUE,
  risk.table = TRUE,
  xlab       = "Days from first placement to first runaway",
  ylab       = "P(no runaway episode yet)",
  title      = "Time to First Runaway by Age Group",
  palette    = "Dark2",
  ggtheme    = theme_minimal(base_size = 13)
)

png("output/10_km_by_age.png", width = 10, height = 7,
    units = "in", res = 300)
print(p_km_age)
dev.off()

###############################################################################
#
#  PART 13 — COMPOSITE SUMMARY PANEL (PATCHWORK)
#
###############################################################################

p_panel <- (p_incidence | p_demo) /
           (p_recur    | p_outcomes) +
  plot_annotation(
    title   = "AFCARS Missing / Runaway Episode Dashboard",
    caption = "Simulated data for illustration  |  Source: AFCARS FC file"
  )

ggsave("output/11_summary_panel.png", p_panel,
       width = 16, height = 10, dpi = 300, bg = "white")

###############################################################################
#
#  PART 14 — REPRODUCIBILITY & SESSION INFO
#
###############################################################################

cat("\n\n--- Session Info ---\n")
print(sessionInfo())

cat("\n✓  Workflow complete.
    All plots saved to output/ directory.
    Review model summaries in console log.\n")

###############################################################################
#
#  APPENDIX A — KEY AFCARS CurPlSet CODES (Reference)
#
#   1  Pre-Adoptive Home
#   2  Foster Family Home (Relative)
#   3  Foster Family Home (Non-Relative)
#   4  Group Home
#   5  Institution
#   6  Supervised Independent Living
#   7  Runaway                             ← target
#   8  Trial Home Visit
#
#  NOTE: Code ordering changed across AFCARS versions.  Always verify
#  against the data dictionary for the specific submission period.
#  In more recent AFCARS 2020 restructure, element mapping differs.
#
###############################################################################

###############################################################################
#
#  APPENDIX B — DIRECTORY STRUCTURE EXPECTED
#
#   project/
#   ├── data/
#   │   └── FC2023v1.sav          (or .csv, .xpt)
#   ├── output/
#   │   ├── 01_incidence_year.png
#   │   ├── 02_incidence_demo.png
#   │   ├── 03_km_overall.png
#   │   ├── 04_km_by_placement.png
#   │   ├── 05_recurrence.png
#   │   ├── 06_timing_histogram.png
#   │   ├── 07_forest_plot.png
#   │   ├── 08_state_map.png
#   │   ├── 09_outcomes_compare.png
#   │   ├── 10_km_by_age.png
#   │   └── 11_summary_panel.png
#   └── afcars_missing_runaway_analysis.R  (this script)
#
###############################################################################
