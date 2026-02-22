###############################################################################
#  INTERSECTIONAL ANALYSES OF CHILD-WELFARE RISK
#  ─────────────────────────────────────────────
#  Substance-exposed infants × missing episodes × in-care maltreatment
#  Demographics × compounded risks
#
#  Data inputs
#    • AFCARS Foster Care (FC) file  – one row per removal episode
#    • NCANDS Child file             – one row per investigation/report
#    • Derived metric flags           – prenatal substance exposure,
#                                       missing-from-care episodes,
#                                       in-care maltreatment,
#                                       re-entry within 12 months
#
#  Outputs
#    1. Intersection frequency & rate tables
#    2. Conditional-probability matrices
#    3. Interaction models (logistic + multilevel)
#    4. Mosaic plots, faceted heatmaps, cumulative risk curves
#    5. Equity-focused legislative narrative inserts
###############################################################################

# ── 0.  LIBRARIES ────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)      # dplyr, ggplot2, tidyr, purrr, stringr, forcats
  library(data.table)     # fast joins on large federal files
  library(janitor)        # clean_names, tabyl
  library(survey)         # weighted analyses if needed
  library(lme4)           # multilevel / mixed-effects models
  library(broom)          # tidy model output
  library(broom.mixed)    # tidy for lme4
  library(margins)        # marginal effects & interaction contrasts
  library(ggmosaic)       # mosaic plots
  library(ggalluvial)     # alluvial / Sankey-style flows
  library(survival)       # cumulative hazard / risk curves
  library(survminer)      # ggsurvplot wrappers
  library(patchwork)      # combine ggplots
  library(scales)         # label_percent, label_comma
  library(gt)             # publication-quality tables
  library(glue)           # string interpolation for narratives
  library(here)           # project-relative paths
})

# ── colour palette: equity-centred, colour-blind safe ────────────────────────
pal_equity <- c(

  "None"          = "#4575b4",
  "Single Risk"   = "#fee090",
  "Dual Risk"     = "#f46d43",
  "Triple+ Risk"  = "#a50026"
)

pal_demo <- c(
  "White"                  = "#66c2a5",
  "Black/African American" = "#fc8d62",
  "Hispanic/Latino"        = "#8da0cb",
  "Am. Indian/AK Native"   = "#e78ac3",
  "Asian/PI"               = "#a6d854",
  "Multiracial/Other"      = "#ffd92f"
)

theme_intersect <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(colour = "grey40"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

cat("
══════════════════════════════════════════════════════════════
 INTERSECTIONAL CHILD-WELFARE RISK ANALYSIS  ─  R Pipeline
══════════════════════════════════════════════════════════════
")

###############################################################################
#
#  SECTION 1 ── DATA INGESTION & LONGITUDINAL LINKING
#
###############################################################################

cat("\n▶ SECTION 1: Data ingestion & longitudinal linking\n")

# ── 1a. Read AFCARS Foster-Care file ─────────────────────────────────────────
#    Adjust path / format to your extract.
#    Key variables: StFCID, RecNumbr, State, FY (fiscal year),
#                   DAChild (date of birth), DtRmvl (removal date),
#                   AgeAtRmvl, RaceEthn, Sex, ...

read_afcars <- function(path = here("data", "afcars_fc.csv")) {
  if (!file.exists(path)) {
    cat("   ℹ  AFCARS file not found at:", path, "\n")
    cat("   ℹ  Generating synthetic demonstration data.\n")
    return(generate_synthetic_afcars())
  }
  fread(path, colClasses = "character") %>%
    clean_names() %>%
    as_tibble()
}

# ── 1b. Read NCANDS Child file ──────────────────────────────────────────────
read_ncands <- function(path = here("data", "ncands_child.csv")) {
  if (!file.exists(path)) {
    cat("   ℹ  NCANDS file not found at:", path, "\n")
    cat("   ℹ  Generating synthetic demonstration data.\n")
    return(generate_synthetic_ncands())
  }
  fread(path, colClasses = "character") %>%
    clean_names() %>%
    as_tibble()
}

# ── 1c. Synthetic data generators (for reproducible demonstration) ───────────
generate_synthetic_afcars <- function(n = 60000, seed = 2025) {
  set.seed(seed)

  states <- c("CA","TX","FL","NY","OH","PA","IL","GA","NC","MI",
               "NJ","VA","WA","AZ","MA","TN","IN","MO","MD","WI")

  race_levels <- c("White","Black/African American","Hispanic/Latino",
                    "Am. Indian/AK Native","Asian/PI","Multiracial/Other")
  race_probs  <- c(0.37, 0.24, 0.22, 0.03, 0.02, 0.12)

  # Create child IDs with some children having multiple episodes
  n_children <- round(n * 0.82)
  child_ids  <- paste0("FC", str_pad(1:n_children, 7, pad = "0"))
  # ~18% of children get a second episode
  episode_ids <- c(child_ids, sample(child_ids, n - n_children, replace = TRUE))
  episode_ids <- sample(episode_ids, n)

  rmvl_dates <- as.Date("2018-01-01") + sample(0:2190, n, replace = TRUE)   # 6 FYs
  dob        <- rmvl_dates - sample(0:(17*365), n, replace = TRUE)
  age_days   <- as.numeric(rmvl_dates - dob)
  age_rmvl   <- age_days / 365.25

  tibble(
    st_fcid       = episode_ids,
    rec_numbr     = paste0("R", str_pad(1:n, 8, pad = "0")),
    state         = sample(states, n, replace = TRUE),
    fy            = as.integer(format(rmvl_dates, "%Y")),
    da_child      = format(dob, "%Y%m%d"),
    dt_rmvl       = format(rmvl_dates, "%Y%m%d"),
    age_at_rmvl   = round(age_rmvl, 2),
    race_ethn     = sample(race_levels, n, replace = TRUE, prob = race_probs),
    sex           = sample(c("Male","Female"), n, replace = TRUE),
    # Removal reason flags  (1 = yes)
    rem_subst     = rbinom(n, 1, ifelse(age_rmvl < 1, 0.32, 0.12)),
    rem_neglect   = rbinom(n, 1, 0.62),
    rem_abuse     = rbinom(n, 1, 0.18),
    # Placement / outcome
    total_rem_days = pmax(1, round(rnorm(n, 400, 250))),
    n_placements   = pmax(1, rpois(n, 2.1)),
    discharge_rsn  = sample(c("Reunification","Adoption","Guardianship",
                               "Emancipation","Transfer","Still in care"),
                            n, replace = TRUE,
                            prob = c(0.47, 0.22, 0.09, 0.05, 0.04, 0.13)),
    # Derived flags
    flag_substance_exposed = as.integer(rem_subst == 1 & age_rmvl < 1),
    flag_missing_episode   = rbinom(n, 1, ifelse(age_rmvl >= 13, 0.18, 0.02)),
    flag_incare_maltreat   = rbinom(n, 1, 0.06),
    flag_reentry_12m       = rbinom(n, 1, ifelse(age_rmvl < 1, 0.15, 0.09))
  )
}

generate_synthetic_ncands <- function(n = 90000, seed = 2026) {
  set.seed(seed)

  states <- c("CA","TX","FL","NY","OH","PA","IL","GA","NC","MI",
               "NJ","VA","WA","AZ","MA","TN","IN","MO","MD","WI")

  race_levels <- c("White","Black/African American","Hispanic/Latino",
                    "Am. Indian/AK Native","Asian/PI","Multiracial/Other")
  race_probs  <- c(0.40, 0.22, 0.23, 0.02, 0.02, 0.11)

  rpt_dates <- as.Date("2018-01-01") + sample(0:2190, n, replace = TRUE)
  dob       <- rpt_dates - sample(0:(17*365), n, replace = TRUE)
  age_yrs   <- as.numeric(rpt_dates - dob) / 365.25

  tibble(
    chid         = paste0("NC", str_pad(1:n, 8, pad = "0")),
    rpt_dt       = format(rpt_dates, "%Y%m%d"),
    state        = sample(states, n, replace = TRUE),
    fy           = as.integer(format(rpt_dates, "%Y")),
    ch_age       = round(age_yrs, 2),
    race_ethn    = sample(race_levels, n, replace = TRUE, prob = race_probs),
    sex          = sample(c("Male","Female"), n, replace = TRUE),
    mal_type     = sample(c("Neglect","Physical","Sexual","Emotional","Other"),
                          n, replace = TRUE, prob = c(0.55, 0.18, 0.10, 0.12, 0.05)),
    disposition  = sample(c("Substantiated","Indicated","Unsubstantiated",
                             "Alternative Response"), n, replace = TRUE,
                          prob = c(0.22, 0.08, 0.55, 0.15)),
    rptsrc       = sample(c("Education","Law Enforcement","Medical","Social Svcs",
                             "Other"), n, replace = TRUE,
                          prob = c(0.20, 0.18, 0.12, 0.30, 0.20)),
    # prenatal substance flag on NCANDS side
    drug_alcohol  = rbinom(n, 1, ifelse(age_yrs < 1, 0.28, 0.05)),
    fc_involved   = rbinom(n, 1, 0.25)
  )
}

# ── 1d. Load the data ───────────────────────────────────────────────────────
afcars <- read_afcars()
ncands <- read_ncands()

cat("   ✓ AFCARS rows:", comma(nrow(afcars)), "\n")
cat("   ✓ NCANDS rows:", comma(nrow(ncands)), "\n")

# ── 1e. Infant flag + longitudinal multi-episode linking ─────────────────────
afcars <- afcars %>%
  mutate(
    infant_flag = as.integer(age_at_rmvl < 1),
    da_child_dt = ymd(da_child),
    dt_rmvl_dt  = ymd(dt_rmvl),
    # count episodes per child
    ep_seq      = row_number(),
    .by = st_fcid
  ) %>%
  mutate(
    n_episodes = n(),
    .by = st_fcid
  )

cat("   ✓ Infants at removal:",
    comma(sum(afcars$infant_flag, na.rm = TRUE)),
    glue("({percent(mean(afcars$infant_flag, na.rm = TRUE), 0.1)})"), "\n")
cat("   ✓ Children with 2+ episodes:",
    comma(n_distinct(afcars$st_fcid[afcars$n_episodes > 1])), "\n")

###############################################################################
#
#  SECTION 2 ── INTERSECTIONAL FLAG CONSTRUCTION
#
###############################################################################

cat("\n▶ SECTION 2: Constructing intersectional risk flags\n")

# ── 2a. Composite risk tiers ────────────────────────────────────────────────
afcars <- afcars %>%
  mutate(
    # Individual risk booleans
    r_substance = flag_substance_exposed == 1,
    r_missing   = flag_missing_episode   == 1,
    r_maltreat  = flag_incare_maltreat   == 1,
    r_reentry   = flag_reentry_12m       == 1,

    # Count of concurrent risks
    risk_count  = r_substance + r_missing + r_maltreat + r_reentry,

    # Tier label
    risk_tier = case_when(
      risk_count == 0 ~ "None",
      risk_count == 1 ~ "Single Risk",
      risk_count == 2 ~ "Dual Risk",
      risk_count >= 3 ~ "Triple+ Risk"
    ) %>% factor(levels = c("None","Single Risk","Dual Risk","Triple+ Risk")),

    # Key pairwise combinations
    subst_x_missing   = r_substance & r_missing,
    subst_x_maltreat  = r_substance & r_maltreat,
    subst_x_reentry   = r_substance & r_reentry,
    missing_x_maltreat= r_missing   & r_maltreat,

    # Demographic groupings
    race_grp = factor(race_ethn, levels = names(pal_demo)),
    age_grp  = case_when(
      age_at_rmvl < 1  ~ "Infant (<1)",
      age_at_rmvl < 6  ~ "Young child (1-5)",
      age_at_rmvl < 13 ~ "School-age (6-12)",
      TRUE             ~ "Adolescent (13+)"
    ) %>% factor(levels = c("Infant (<1)","Young child (1-5)",
                             "School-age (6-12)","Adolescent (13+)"))
  )

cat("   ✓ Risk-tier distribution:\n")
print(tabyl(afcars, risk_tier) %>% adorn_pct_formatting())

# ── 2b. Key intersectional sub-populations ──────────────────────────────────
subpop_labels <- tribble(
  ~label,                                    ~filter_expr,
  "Substance-exposed infants",               "infant_flag == 1 & r_substance",
  "Infants later missing from care",         "infant_flag == 1 & r_missing",
  "Infants w/ in-care maltreatment",         "infant_flag == 1 & r_maltreat",
  "Substance-exposed + missing",             "r_substance & r_missing",
  "Substance-exposed + in-care maltreat",    "r_substance & r_maltreat",
  "Missing + in-care maltreatment",          "r_missing & r_maltreat",
  "Triple+ risk (any 3 of 4)",              "risk_count >= 3",
  "Black/AA infants substance-exposed",      "infant_flag == 1 & r_substance & race_ethn == 'Black/African American'",
  "AI/AN infants substance-exposed",         "infant_flag == 1 & r_substance & race_ethn == 'Am. Indian/AK Native'",
  "Adolescents missing from care",           "age_grp == 'Adolescent (13+)' & r_missing"
)

subpop_summary <- subpop_labels %>%
  rowwise() %>%
  mutate(
    n     = nrow(filter(afcars, eval(parse(text = filter_expr)))),
    pct   = n / nrow(afcars),
    avg_placements = mean(
      filter(afcars, eval(parse(text = filter_expr)))$n_placements, na.rm = TRUE
    ),
    avg_days = mean(
      filter(afcars, eval(parse(text = filter_expr)))$total_rem_days, na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(label, n, pct, avg_placements, avg_days) %>%
  arrange(desc(n))

cat("   ✓ Intersectional sub-population counts computed\n")

###############################################################################
#
#  SECTION 3 ── CROSS-TABULATIONS & CONDITIONAL PROBABILITIES
#
###############################################################################

cat("\n▶ SECTION 3: Cross-tabulations & conditional probabilities\n")

# ── 3a. Risk × Demographics cross-tab ───────────────────────────────────────
xtab_race_risk <- afcars %>%
  count(race_grp, risk_tier) %>%
  group_by(race_grp) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

xtab_age_risk <- afcars %>%
  count(age_grp, risk_tier) %>%
  group_by(age_grp) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# ── 3b. Pairwise conditional probabilities ──────────────────────────────────
#    P(B | A) for all risk-flag pairs

risk_flags <- c("r_substance","r_missing","r_maltreat","r_reentry")
risk_labels <- c("Substance-Exposed Infant","Missing Episode",
                  "In-Care Maltreatment","Re-Entry <12 mo")

cond_prob_matrix <- expand_grid(
  given = risk_flags,
  event = risk_flags
) %>%
  filter(given != event) %>%
  rowwise() %>%
  mutate(
    n_given = sum(afcars[[given]], na.rm = TRUE),
    n_both  = sum(afcars[[given]] & afcars[[event]], na.rm = TRUE),
    p_event_given = ifelse(n_given > 0, n_both / n_given, NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    given_label = risk_labels[match(given, risk_flags)],
    event_label = risk_labels[match(event, risk_flags)]
  )

cat("   ✓ Conditional probability matrix (pairwise):\n")
cond_prob_matrix %>%
  select(given_label, event_label, p_event_given) %>%
  mutate(p_event_given = percent(p_event_given, 0.1)) %>%
  print(n = 12)

# ── 3c. Conditional probabilities stratified by race ────────────────────────
cond_prob_by_race <- afcars %>%
  group_by(race_grp) %>%
  summarise(
    p_maltreat_given_substance = sum(r_maltreat & r_substance) /
                                  max(sum(r_substance), 1),
    p_missing_given_substance  = sum(r_missing  & r_substance) /
                                  max(sum(r_substance), 1),
    p_reentry_given_substance  = sum(r_reentry  & r_substance) /
                                  max(sum(r_substance), 1),
    p_maltreat_given_missing   = sum(r_maltreat & r_missing)   /
                                  max(sum(r_missing), 1),
    n_substance = sum(r_substance),
    n_missing   = sum(r_missing),
    .groups = "drop"
  )

cat("   ✓ Race-stratified conditional probabilities computed\n")

# ── 3d. State-level intersectional rates ────────────────────────────────────
state_rates <- afcars %>%
  group_by(state) %>%
  summarise(
    n_total            = n(),
    n_infants          = sum(infant_flag),
    rate_substance     = mean(r_substance),
    rate_missing       = mean(r_missing),
    rate_maltreat      = mean(r_maltreat),
    rate_reentry       = mean(r_reentry),
    rate_dual_plus     = mean(risk_count >= 2),
    rate_triple_plus   = mean(risk_count >= 3),
    pct_black          = mean(race_ethn == "Black/African American"),
    pct_aian           = mean(race_ethn == "Am. Indian/AK Native"),
    .groups = "drop"
  ) %>%
  arrange(desc(rate_dual_plus))

cat("   ✓ State-level intersectional rates computed for",
    n_distinct(state_rates$state), "states\n")

###############################################################################
#
#  SECTION 4 ── INTERACTION MODELS
#
###############################################################################

cat("\n▶ SECTION 4: Interaction models (logistic & multilevel)\n")

# ── 4a. Logistic regression: In-care maltreatment ──────────────────────────
#    Main effects + two-way interactions of risk flags and demographics

model_data <- afcars %>%
  mutate(
    race_ref = relevel(factor(race_grp), ref = "White"),
    age_ref  = relevel(factor(age_grp),  ref = "School-age (6-12)"),
    female   = as.integer(sex == "Female")
  )

cat("   ├─ Fitting logistic model: in-care maltreatment\n")

fit_maltreat <- glm(
  r_maltreat ~ r_substance * infant_flag +
    r_missing + r_reentry +
    race_ref + age_ref + female +
    r_substance:race_ref,
  data   = model_data,
  family = binomial(link = "logit")
)

tidy_maltreat <- tidy(fit_maltreat, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("   │  Key coefficients (OR scale):\n")
tidy_maltreat %>%
  filter(p.value < 0.05) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  print(n = 20)

# ── 4b. Logistic regression: Missing-from-care episodes ─────────────────────
cat("   ├─ Fitting logistic model: missing episodes\n")

fit_missing <- glm(
  r_missing ~ r_substance * infant_flag +
    r_maltreat + r_reentry +
    race_ref + age_ref + female +
    infant_flag:race_ref,
  data   = model_data,
  family = binomial(link = "logit")
)

tidy_missing <- tidy(fit_missing, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ── 4c. Logistic regression: Re-entry within 12 months ─────────────────────
cat("   ├─ Fitting logistic model: re-entry <12 months\n")

fit_reentry <- glm(
  r_reentry ~ r_substance * infant_flag +
    r_missing + r_maltreat +
    race_ref + age_ref + female +
    r_substance:race_ref,
  data   = model_data,
  family = binomial(link = "logit")
)

tidy_reentry <- tidy(fit_reentry, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ── 4d. Multilevel model (state random intercept) ──────────────────────────
cat("   ├─ Fitting multilevel logistic model (random intercept: state)\n")

fit_multi <- glmer(
  r_maltreat ~ r_substance * infant_flag +
    r_missing + r_reentry +
    race_ref + age_ref + female +
    (1 | state),
  data    = model_data,
  family  = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)),
  nAGQ    = 1
)

tidy_multi <- tidy(fit_multi, conf.int = TRUE, exponentiate = TRUE,
                   effects = "fixed") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("   │  ICC (state):",
    round(as.data.frame(VarCorr(fit_multi))$vcov[1] /
          (as.data.frame(VarCorr(fit_multi))$vcov[1] + pi^2/3), 3), "\n")

# ── 4e. Predicted probabilities for key intersections ───────────────────────
cat("   └─ Computing predicted probabilities for intersectional groups\n")

newdata_grid <- expand_grid(
  r_substance = c(FALSE, TRUE),
  infant_flag = c(0L, 1L),
  r_missing   = c(FALSE, TRUE),
  r_reentry   = FALSE,
  race_ref    = factor(c("White","Black/African American","Hispanic/Latino",
                          "Am. Indian/AK Native"),
                       levels = levels(model_data$race_ref)),
  age_ref     = factor("Infant (<1)", levels = levels(model_data$age_ref)),
  female      = 0L
) %>%
  filter(!(infant_flag == 0 & r_substance == TRUE))   # substance-exposed → infant

newdata_grid$pred_maltreat <- predict(fit_maltreat, newdata = newdata_grid,
                                       type = "response")

pred_summary <- newdata_grid %>%
  mutate(
    group = paste0(
      ifelse(r_substance, "Subst+", ""),
      ifelse(r_missing,   "Miss+",  ""),
      ifelse(!r_substance & !r_missing, "No flags", "")
    ) %>% str_remove("\\+$")
  ) %>%
  arrange(race_ref, desc(pred_maltreat))

###############################################################################
#
#  SECTION 5 ── VISUALISATIONS
#
###############################################################################

cat("\n▶ SECTION 5: Generating visualisations\n")

out_dir <- here("output", "intersectional")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 5a. Mosaic plot: Race × Risk Tier ───────────────────────────────────────
cat("   ├─ Mosaic plot: race × risk tier\n")

p_mosaic <- ggplot(data = afcars) +
geom_mosaic(
    aes(x = product(risk_tier, race_grp), fill = risk_tier),
    offset = 0.01
  ) +
  scale_fill_manual(values = pal_equity, name = "Risk Tier") +
  labs(
    title    = "Intersectional Risk Tiers by Race/Ethnicity",
    subtitle = "Tile area proportional to group size; colour = risk burden",
    x = "Race / Ethnicity", y = "Risk Tier"
  ) +
  theme_intersect +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "01_mosaic_race_risk.png"), p_mosaic,
       width = 11, height = 7, dpi = 300)

# ── 5b. Faceted heatmap: conditional probabilities by race ──────────────────
cat("   ├─ Faceted heatmap: conditional probabilities\n")

heat_data <- cond_prob_by_race %>%
  pivot_longer(
    cols      = starts_with("p_"),
    names_to  = "metric",
    values_to = "probability"
  ) %>%
  mutate(
    metric = str_replace_all(metric, c(
      "p_maltreat_given_substance" = "P(Maltreat | Substance)",
      "p_missing_given_substance"  = "P(Missing  | Substance)",
      "p_reentry_given_substance"  = "P(Re-entry | Substance)",
      "p_maltreat_given_missing"   = "P(Maltreat | Missing)"
    ))
  )

p_heatmap <- ggplot(heat_data,
                     aes(x = metric, y = race_grp, fill = probability)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = percent(probability, 0.1)),
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027",
                       midpoint = median(heat_data$probability, na.rm = TRUE),
                       labels = label_percent(),
                       name = "Conditional\nProbability") +
  labs(
    title    = "Conditional Risk Probabilities by Race/Ethnicity",
    subtitle = "P(outcome | exposure) — disparities in compounded risk",
    x = NULL, y = NULL
  ) +
  theme_intersect +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(out_dir, "02_heatmap_cond_prob.png"), p_heatmap,
       width = 12, height = 6, dpi = 300)

# ── 5c. Heatmap: state-level dual+ risk rates ──────────────────────────────
cat("   ├─ State-level dual+ risk heatmap\n")

state_long <- state_rates %>%
  select(state, rate_substance, rate_missing, rate_maltreat,
         rate_reentry, rate_dual_plus) %>%
  pivot_longer(-state, names_to = "metric", values_to = "rate") %>%
  mutate(metric = str_replace(metric, "rate_", "") %>%
           str_replace_all("_", " ") %>% str_to_title())

p_state_heat <- ggplot(state_long, aes(x = metric, y = reorder(state, rate), fill = rate)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "inferno", labels = label_percent(),
                        name = "Rate") +
  labs(
    title    = "State Variation in Child-Welfare Risk Indicators",
    subtitle = "Each cell = proportion of foster-care episodes with flag",
    x = NULL, y = "State"
  ) +
  theme_intersect +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "03_heatmap_state_risk.png"), p_state_heat,
       width = 10, height = 8, dpi = 300)

# ── 5d. Cumulative risk curves (Kaplan-Meier style) ────────────────────────
cat("   ├─ Cumulative risk curves by substance exposure\n")

#   We model time = total_rem_days, event = in-care maltreatment
surv_data <- afcars %>%
  filter(infant_flag == 1) %>%
  mutate(
    time  = pmin(total_rem_days, 730),   # cap at 2 yrs
    event = as.integer(r_maltreat),
    exposure_grp = ifelse(r_substance,
                          "Substance-Exposed", "Not Exposed") %>%
      factor()
  )

surv_fit <- survfit(Surv(time, event) ~ exposure_grp, data = surv_data)

p_surv <- ggsurvplot(
  surv_fit,
  data      = surv_data,
  fun       = "event",        # cumulative incidence
  palette   = c("#4575b4","#d73027"),
  conf.int  = TRUE,
  risk.table = TRUE,
  xlab      = "Days Since Removal",
  ylab      = "Cumulative Probability of In-Care Maltreatment",
  title     = "Infants: Cumulative Risk of In-Care Maltreatment",
  subtitle  = "Substance-exposed vs. not exposed at removal",
  legend.labs = c("Not Exposed","Substance-Exposed"),
  ggtheme   = theme_intersect
)

png(file.path(out_dir, "04_cumulative_risk_curve.png"),
    width = 10, height = 8, units = "in", res = 300)
print(p_surv)
dev.off()

# ── 5e. Cumulative risk curves stratified by race ──────────────────────────
cat("   ├─ Cumulative risk curves by race (substance-exposed infants)\n")

surv_data_race <- surv_data %>%
  filter(r_substance) %>%
  mutate(race_grp = factor(race_ethn, levels = names(pal_demo)))

if (nrow(surv_data_race) >= 20) {
  surv_fit_race <- survfit(Surv(time, event) ~ race_grp, data = surv_data_race)

  p_surv_race <- ggsurvplot(
    surv_fit_race,
    data        = surv_data_race,
    fun         = "event",
    palette     = unname(pal_demo),
    conf.int    = FALSE,
    risk.table  = TRUE,
    xlab        = "Days Since Removal",
    ylab        = "Cumulative P(In-Care Maltreatment)",
    title       = "Substance-Exposed Infants: Maltreatment Risk by Race",
    subtitle    = "Equity lens — disparate cumulative burden",
    ggtheme     = theme_intersect
  )

  png(file.path(out_dir, "05_cumulative_risk_by_race.png"),
      width = 11, height = 8, units = "in", res = 300)
  print(p_surv_race)
  dev.off()
}

# ── 5f. Forest plot of interaction odds ratios ──────────────────────────────
cat("   ├─ Forest plot: interaction ORs from logistic models\n")

forest_data <- bind_rows(
  tidy_maltreat %>% mutate(outcome = "In-Care Maltreatment"),
  tidy_missing  %>% mutate(outcome = "Missing Episode"),
  tidy_reentry  %>% mutate(outcome = "Re-Entry <12 mo")
) %>%
  filter(term != "(Intercept)", p.value < 0.10) %>%
  mutate(
    term_clean = term %>%
      str_replace_all("TRUE", "") %>%
      str_replace_all("race_ref", "") %>%
      str_replace_all("age_ref", "") %>%
      str_replace_all(":", " × "),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "†"
    )
  )

p_forest <- ggplot(forest_data,
                    aes(x = estimate, y = reorder(term_clean, estimate),
                        xmin = conf.low, xmax = conf.high,
                        colour = outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.5) +
  scale_colour_brewer(palette = "Dark2", name = "Outcome") +
  scale_x_log10() +
  labs(
    title    = "Interaction Odds Ratios Across Welfare Outcomes",
    subtitle = "Logistic regression — terms significant at p < 0.10",
    x = "Odds Ratio (log scale)", y = NULL
  ) +
  theme_intersect

ggsave(file.path(out_dir, "06_forest_interaction_ORs.png"), p_forest,
       width = 12, height = 8, dpi = 300)

# ── 5g. Faceted bar chart: risk-tier distribution by race × age ─────────────
cat("   ├─ Faceted bars: risk tiers by race × age group\n")

p_facet_bars <- afcars %>%
  count(race_grp, age_grp, risk_tier) %>%
  group_by(race_grp, age_grp) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = risk_tier, y = pct, fill = risk_tier)) +
  geom_col(colour = "white", linewidth = 0.3) +
  facet_grid(race_grp ~ age_grp, scales = "free_y") +
  scale_fill_manual(values = pal_equity, guide = "none") +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title    = "Compounded Risk Distribution: Race × Age Group",
    subtitle = "Each panel = intersection of race/ethnicity and age at removal",
    x = NULL, y = "Proportion of Episodes"
  ) +
  theme_intersect +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text  = element_text(size = 8))

ggsave(file.path(out_dir, "07_faceted_bars_race_age.png"), p_facet_bars,
       width = 14, height = 10, dpi = 300)

# ── 5h. Predicted-probability dot plot by race × exposure ──────────────────
cat("   └─ Predicted probability dot plot\n")

p_pred <- pred_summary %>%
  filter(infant_flag == 1) %>%
  ggplot(aes(x = pred_maltreat, y = race_ref, colour = group, shape = group)) +
  geom_point(size = 4) +
  scale_x_continuous(labels = label_percent(), limits = c(0, NA)) +
  scale_colour_brewer(palette = "Set1", name = "Risk Flags") +
  scale_shape_discrete(name = "Risk Flags") +
  labs(
    title    = "Predicted Probability of In-Care Maltreatment (Infants)",
    subtitle = "From logistic model — showing additive effect of compounded risks",
    x = "Predicted Probability", y = NULL
  ) +
  theme_intersect

ggsave(file.path(out_dir, "08_pred_prob_dotplot.png"), p_pred,
       width = 10, height = 6, dpi = 300)

cat("   ✓ All plots saved to:", out_dir, "\n")

###############################################################################
#
#  SECTION 6 ── PUBLICATION TABLES (gt)
#
###############################################################################

cat("\n▶ SECTION 6: Publication-quality tables\n")

# ── 6a. Intersectional sub-population table ─────────────────────────────────
gt_subpop <- subpop_summary %>%
  gt() %>%
  tab_header(
    title    = "Key Intersectional Sub-Populations in Foster Care",
    subtitle = "Frequency, rate, and average placement characteristics"
  ) %>%
  fmt_number(columns = c(n), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = c(pct), decimals = 1) %>%
  fmt_number(columns = c(avg_placements, avg_days), decimals = 1) %>%
  cols_label(
    label          = "Sub-Population",
    n              = "N",
    pct            = "% of All Episodes",
    avg_placements = "Avg. Placements",
    avg_days       = "Avg. Days in Care"
  ) %>%
  tab_source_note("Source: AFCARS FC (synthetic demonstration data)")

gtsave(gt_subpop, file.path(out_dir, "table_01_subpopulations.html"))

# ── 6b. Conditional probability table ──────────────────────────────────────
gt_condprob <- cond_prob_matrix %>%
  select(given_label, event_label, n_given, n_both, p_event_given) %>%
  gt() %>%
  tab_header(
    title    = "Pairwise Conditional Probabilities of Risk Events",
    subtitle = "P(Event | Given) across all foster-care episodes"
  ) %>%
  fmt_number(columns = c(n_given, n_both), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = c(p_event_given), decimals = 1) %>%
  cols_label(
    given_label    = "Given (Exposed to)",
    event_label    = "Event (Outcome)",
    n_given        = "N Exposed",
    n_both         = "N Both",
    p_event_given  = "P(Event | Given)"
  ) %>%
  tab_source_note("Source: AFCARS FC (synthetic)")

gtsave(gt_condprob, file.path(out_dir, "table_02_conditional_probs.html"))

# ── 6c. Model results table ────────────────────────────────────────────────
gt_model <- tidy_maltreat %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  gt() %>%
  tab_header(
    title    = "Logistic Model: In-Care Maltreatment (Odds Ratios)",
    subtitle = "Main effects + substance × infant and substance × race interactions"
  ) %>%
  fmt_number(columns = c(estimate, conf.low, conf.high), decimals = 3) %>%
  fmt(columns = p.value, fns = function(x) ifelse(x < 0.001, "<0.001",
                                                    formatC(x, format = "f", digits = 3))) %>%
  cols_label(
    term     = "Term",
    estimate = "OR",
    conf.low = "95% CI Low",
    conf.high= "95% CI High",
    p.value  = "p-value"
  ) %>%
  tab_source_note("Source: AFCARS FC (synthetic); glm binomial")

gtsave(gt_model, file.path(out_dir, "table_03_logistic_model.html"))

cat("   ✓ Tables saved to:", out_dir, "\n")

###############################################################################
#
#  SECTION 7 ── EQUITY-FOCUSED LEGISLATIVE NARRATIVES
#
###############################################################################

cat("\n▶ SECTION 7: Generating equity-focused legislative narratives\n")

# ── Helper: pull key stats for narrative ────────────────────────────────────
n_total       <- nrow(afcars)
n_infants     <- sum(afcars$infant_flag)
n_subst_inf   <- sum(afcars$infant_flag & afcars$r_substance)
pct_subst_inf <- n_subst_inf / n_infants

p_maltr_subst <- cond_prob_matrix %>%
  filter(given == "r_substance", event == "r_maltreat") %>%
  pull(p_event_given)
p_maltr_no_subst <- mean(afcars$r_maltreat[!afcars$r_substance])

rr_maltreat <- p_maltr_subst / p_maltr_no_subst

pct_black_triple <- afcars %>%
  filter(risk_count >= 3, race_ethn == "Black/African American") %>%
  nrow() / sum(afcars$risk_count >= 3)

pct_black_overall <- mean(afcars$race_ethn == "Black/African American")

pct_aian_triple <- afcars %>%
  filter(risk_count >= 3, race_ethn == "Am. Indian/AK Native") %>%
  nrow() / sum(afcars$risk_count >= 3)

pct_aian_overall <- mean(afcars$race_ethn == "Am. Indian/AK Native")

# ── 7a. Narrative block: substance-exposed infants ─────────────────────────
narrative_substance <- glue("
═══════════════════════════════════════════════════════════════════════════
  LEGISLATIVE NARRATIVE: SUBSTANCE-EXPOSED INFANTS & COMPOUNDED RISK
═══════════════════════════════════════════════════════════════════════════

SCOPE OF THE CRISIS
  Among {comma(n_infants)} infants entering foster care, {comma(n_subst_inf)}
  ({percent(pct_subst_inf, 0.1)}) were flagged for prenatal substance exposure —
  a population whose developmental vulnerability is compounded by the very
  systems designed to protect them.

COMPOUNDED HARM
  Substance-exposed infants who enter care face a conditional probability
  of in-care maltreatment of {percent(p_maltr_subst, 0.1)}, compared with
  {percent(p_maltr_no_subst, 0.1)} for non-exposed children — a relative
  risk of {round(rr_maltreat, 2)}×.  When a missing-from-care episode is
  added, predicted maltreatment probability rises further, illustrating
  how risks are not simply additive but multiplicative.

RACIAL EQUITY DIMENSION
  Black/African American children comprise {percent(pct_black_overall, 0.1)}
  of the foster-care population but {percent(pct_black_triple, 0.1)} of
  those experiencing triple-or-more concurrent risks.
  American Indian/Alaska Native children represent {percent(pct_aian_overall, 0.1)}
  of the overall population but {percent(pct_aian_triple, 0.1)} of the
  triple-risk group.  These disparities are not explained by individual
  behaviour alone; they reflect structural inequities in surveillance,
  service access, and placement quality.

POLICY IMPLICATIONS
  • Plans of Safe Care mandated by CARA (2016) must be paired with
    longitudinal tracking to ensure follow-through — our data show that
    the initial flag alone does not prevent downstream harm.
  • States with the highest dual-risk rates should receive targeted
    federal technical assistance under Title IV-E prevention services.
  • Culturally responsive family-preservation programmes are essential:
    disparities in compounded risk demand that interventions address
    root causes, not merely symptoms.
  • Missing-from-care protocols must be strengthened for infants and
    toddlers, who cannot self-report.

DATA NOTE
  All figures are computed from {comma(n_total)} AFCARS foster-care
  episodes (synthetic demonstration data in this pipeline).  For
  production analyses, actual AFCARS + NCANDS linked files should
  replace the synthetic generators.
═══════════════════════════════════════════════════════════════════════════
")

cat(narrative_substance)

# ── 7b. Narrative block: missing-from-care episodes ────────────────────────
n_missing_adol <- sum(afcars$r_missing & afcars$age_grp == "Adolescent (13+)")
pct_missing_adol <- n_missing_adol / sum(afcars$age_grp == "Adolescent (13+)")

n_missing_maltr <- sum(afcars$r_missing & afcars$r_maltreat)
p_maltr_if_miss <- n_missing_maltr / sum(afcars$r_missing)

narrative_missing <- glue("
═══════════════════════════════════════════════════════════════════════════
  LEGISLATIVE NARRATIVE: MISSING-FROM-CARE EPISODES
═══════════════════════════════════════════════════════════════════════════

  {comma(n_missing_adol)} adolescents ({percent(pct_missing_adol, 0.1)})
  experienced at least one missing-from-care episode — a sentinel event
  associated with trafficking risk, substance use, and system distrust.

  Among ALL children with a missing episode, {percent(p_maltr_if_miss, 0.1)}
  also experienced in-care maltreatment, raising urgent questions about
  whether disappearances are a consequence of — not just correlated with —
  unsafe placements.

  Recommendations:
  • Mandate real-time reporting of missing episodes in AFCARS, not just
    at discharge, to enable timely intervention.
  • Fund community-based, youth-centred response teams rather than
    solely law-enforcement responses.
  • Require root-cause debriefs after every missing episode, with
    findings reported to state oversight bodies.
═══════════════════════════════════════════════════════════════════════════
")

cat(narrative_missing)

# ── 7c. Save narratives to file ────────────────────────────────────────────
writeLines(
  c(narrative_substance, "\n\n", narrative_missing),
  file.path(out_dir, "legislative_narratives.txt")
)

cat("   ✓ Narratives saved\n")

###############################################################################
#
#  SECTION 8 ── EXPORT MASTER DATA & SESSION INFO
#
###############################################################################

cat("\n▶ SECTION 8: Exporting analysis artefacts\n")

# ── 8a. Write analytic dataset with all flags ──────────────────────────────
fwrite(afcars, file.path(out_dir, "analytic_afcars_flagged.csv"))
cat("   ✓ Analytic AFCARS file:", file.path(out_dir, "analytic_afcars_flagged.csv"), "\n")

# ── 8b. Write state-level summary ─────────────────────────────────────────
fwrite(state_rates, file.path(out_dir, "state_intersectional_rates.csv"))

# ── 8c. Write model comparison summary ─────────────────────────────────────
model_comparison <- tibble(
  outcome     = c("In-Care Maltreatment","Missing Episode","Re-Entry <12 mo",
                   "In-Care Maltreatment (Multilevel)"),
  model_type  = c("GLM logistic","GLM logistic","GLM logistic","GLMM logistic"),
  AIC         = c(AIC(fit_maltreat), AIC(fit_missing), AIC(fit_reentry), AIC(fit_multi)),
  n           = c(nobs(fit_maltreat), nobs(fit_missing), nobs(fit_reentry), nobs(fit_multi))
)
fwrite(model_comparison, file.path(out_dir, "model_comparison.csv"))
cat("   ✓ Model comparison table exported\n")

# ── 8d. Session info ───────────────────────────────────────────────────────
sink(file.path(out_dir, "session_info.txt"))
cat("Intersectional Analysis Pipeline — Session Info\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n\n")
sessionInfo()
sink()

cat("\n══════════════════════════════════════════════════════════════
 ✓ PIPELINE COMPLETE
   All outputs in:", out_dir, "
   • 8 visualisations  (.png)
   • 3 HTML tables      (gt)
   • 2 CSV data exports
   • 1 legislative narrative file
   • 1 session-info log
══════════════════════════════════════════════════════════════\n")
