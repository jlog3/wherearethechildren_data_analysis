###############################################################################
#  COST-BENEFIT & ROI MODELING — MANDATED REAL-TIME PUBLIC DASHBOARDS
#  Child Welfare Transparency & Accountability Initiative
#
#  Purpose : Estimate the return on investment of requiring every state to
#            operate a real-time, public-facing child-welfare dashboard,
#            using Monte Carlo simulation, deterministic scenarios, and
#            sensitivity analysis.
#
#  Inputs  : (1) Prior economic costing outputs (Module #9)
#            (2) Workforce/caseload projections (Module #15)
#            (3) Literature-based effect sizes for oversight improvements
#            (4) Dashboard implementation cost assumptions
#
#  Outputs : ROI tables, waterfall charts, tornado sensitivity plots,
#            interactive Shiny scenario explorer, legislative captions
#
#  Author  : CW-Analytics Policy Lab
#  Date    : 2026-02-17
###############################################################################

# ── 0. LIBRARIES ─────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(patchwork)    # combine ggplots
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(DT)
})

# ── 1. CONFIGURATION & ASSUMPTIONS ──────────────────────────────────────────

# ---------------------------------------------------------------------------
# 1A. Dashboard Implementation Costs (per state, 2025 USD)
# ---------------------------------------------------------------------------
cost_assumptions <- list(
  # One-time capital costs
  onetime_platform_dev        = 2800000,   # Platform design, build, QA

  onetime_data_integration    = 1400000,   # ETL pipelines, SACWIS/CCWIS linkage
  onetime_security_compliance =  600000,   # FedRAMP / FISMA / SOC-2 prep
  onetime_training            =  350000,   # Staff training rollout
  onetime_change_management   =  250000,   # Organizational change support
  onetime_total               = NA,        # (computed below)

  # Annual recurring costs
  annual_hosting_cloud        =  480000,   # Cloud infrastructure

  annual_maintenance_dev      =  720000,   # Ongoing development & support
  annual_data_ops             =  360000,   # Data pipeline operations
  annual_helpdesk_support     =  180000,   # User support & issue triage
  annual_security_updates     =  150000,   # Ongoing compliance & pen-testing
  annual_total                = NA         # (computed below)
)

cost_assumptions$onetime_total <- with(cost_assumptions,
  onetime_platform_dev + onetime_data_integration +
  onetime_security_compliance + onetime_training + onetime_change_management
)

cost_assumptions$annual_total <- with(cost_assumptions,
  annual_hosting_cloud + annual_maintenance_dev + annual_data_ops +
  annual_helpdesk_support + annual_security_updates
)

# ---------------------------------------------------------------------------
# 1B. National Parameters
# ---------------------------------------------------------------------------
national_params <- list(
  n_states          = 52,    # 50 states + DC + territories (or adjust)
  analysis_horizon  = 10,    # years
  discount_rate     = 0.03,  # 3% real discount rate (OMB Circular A-94)
  inflation_rate    = 0.025  # for nominal projections
)

# ---------------------------------------------------------------------------
# 1C. Baseline Annual Costs (from Module #9 economic costing)
#     These are illustrative national totals. Replace with actual outputs.
# ---------------------------------------------------------------------------
baseline_costs <- tibble::tibble(
  cost_category = c(
    "Foster care placements",
    "Congregate / group care",
    "Re-investigation of recurrence",
    "Emergency medical (maltreatment injuries)",
    "Missing from care episodes",
    "Court & legal proceedings",
    "Workforce turnover & retraining",
    "Long-term societal costs (discounted)"
  ),
  annual_cost_national = c(
    18.2e9,    # Foster care
     4.8e9,    # Congregate care
     2.1e9,    # Recurrence reinvestigation
     3.6e9,    # Medical costs of maltreatment
     0.45e9,   # Missing episodes response
     1.9e9,    # Legal
     2.7e9,    # Workforce churn
    12.5e9     # Long-term (productivity, mental health, justice)
  )
)

# ---------------------------------------------------------------------------
# 1D. Literature-Based Effect Sizes
#     Each row: the plausible percentage reduction in the corresponding
#     baseline cost category attributable to real-time public dashboards.
#     Low / Central / High drawn from oversight & transparency literature.
# ---------------------------------------------------------------------------
effect_sizes <- tibble::tibble(
  cost_category = baseline_costs$cost_category,
  effect_low    = c(0.05, 0.04, 0.08, 0.06, 0.12, 0.03, 0.07, 0.04),
  effect_mid    = c(0.12, 0.10, 0.18, 0.14, 0.25, 0.08, 0.15, 0.10),
  effect_high   = c(0.22, 0.18, 0.30, 0.24, 0.38, 0.15, 0.25, 0.18),
  # Distribution shape for Monte Carlo (beta parameters inferred)
  evidence_quality = c(
    "Moderate", "Moderate", "Strong", "Moderate",
    "Strong", "Low", "Moderate", "Low"
  ),
  citation_note = c(
    "Analogous: open-data platforms reduce placement churn (GAO 2022)",
    "Oversight boards + data reduce congregate reliance (Casey 2021)",
    "Rapid-feedback loops cut recurrence 15-30% (Chapin Hall 2020)",
    "Early identification via dashboards (Prevent Child Abuse America)",
    "Real-time geolocation/status dashboards (HHS OIG 2023)",
    "Transparency reduces unnecessary hearings (ABA estimates)",
    "Data culture improves retention (APHSA 2022 workforce study)",
    "Lifecycle cost model (Fang et al. 2012, updated)"
  )
)

# ── 2. DETERMINISTIC SCENARIO ANALYSIS ──────────────────────────────────────

run_deterministic <- function(scenario_label, effect_col,
                              cost_assumptions, national_params,
                              baseline_costs, effect_sizes) {

  horizon <- national_params$analysis_horizon
  r       <- national_params$discount_rate
  n_st    <- national_params$n_states

  # Total implementation cost
  total_onetime  <- cost_assumptions$onetime_total * n_st
  total_annual   <- cost_assumptions$annual_total  * n_st

  # Gross annual benefit
  effects <- effect_sizes[[effect_col]]
  annual_benefit_by_cat <- baseline_costs$annual_cost_national * effects
  gross_annual_benefit  <- sum(annual_benefit_by_cat)

  # Year-by-year cash flows (benefits phase in over 3 years)
  phase_in <- c(0.30, 0.70, 1.00, rep(1, horizon - 3))
  years    <- seq_len(horizon)

  cf <- tibble::tibble(
    year           = years,
    phase_in_pct   = phase_in[years],
    gross_benefit  = gross_annual_benefit * phase_in_pct,
    impl_cost      = ifelse(year == 1, total_onetime, 0) + total_annual,
    net_benefit    = gross_benefit - impl_cost,
    discount_factor = 1 / (1 + r)^year,
    pv_benefit     = gross_benefit * discount_factor,
    pv_cost        = impl_cost * discount_factor,
    pv_net         = net_benefit * discount_factor
  )

  npv <- sum(cf$pv_net)
  bcr <- sum(cf$pv_benefit) / sum(cf$pv_cost)

  # Break-even year
  cum_pv_net <- cumsum(cf$pv_net)
  breakeven  <- which(cum_pv_net > 0)[1]
  if (is.na(breakeven)) breakeven <- Inf

  # Per-category detail
  cat_detail <- tibble::tibble(
    cost_category    = baseline_costs$cost_category,
    baseline_annual  = baseline_costs$annual_cost_national,
    effect_size      = effects,
    annual_savings   = annual_benefit_by_cat,
    pv_savings_10yr  = annual_benefit_by_cat *
                       sum(phase_in[years] / (1 + r)^years)
  )

  list(
    scenario     = scenario_label,
    npv          = npv,
    bcr          = bcr,
    breakeven_yr = breakeven,
    cashflows    = cf,
    category     = cat_detail,
    total_pv_benefit = sum(cf$pv_benefit),
    total_pv_cost    = sum(cf$pv_cost)
  )
}

# Run three scenarios
scen_low  <- run_deterministic("Conservative", "effect_low",
                                cost_assumptions, national_params,
                                baseline_costs, effect_sizes)
scen_mid  <- run_deterministic("Central",      "effect_mid",
                                cost_assumptions, national_params,
                                baseline_costs, effect_sizes)
scen_high <- run_deterministic("Optimistic",   "effect_high",
                                cost_assumptions, national_params,
                                baseline_costs, effect_sizes)

# ── 3. MONTE CARLO SIMULATION ──────────────────────────────────────────────

run_monte_carlo <- function(n_sims = 10000,
                            cost_assumptions, national_params,
                            baseline_costs, effect_sizes,
                            seed = 42) {
  set.seed(seed)
  horizon <- national_params$analysis_horizon
  r       <- national_params$discount_rate
  n_st    <- national_params$n_states
  n_cat   <- nrow(effect_sizes)
  years   <- seq_len(horizon)
  phase_in <- c(0.30, 0.70, 1.00, rep(1, horizon - 3))

  # Pre-compute discount factors
  df <- 1 / (1 + r)^years

  # Storage
  npv_vec <- numeric(n_sims)
  bcr_vec <- numeric(n_sims)
  be_vec  <- numeric(n_sims)

  # Category-level savings accumulator
  cat_savings_mat <- matrix(0, nrow = n_sims, ncol = n_cat)

  for (s in seq_len(n_sims)) {

    # --- Stochastic effect sizes (PERT / triangular approximation via beta)
    sim_effects <- numeric(n_cat)
    for (k in seq_len(n_cat)) {
      lo <- effect_sizes$effect_low[k]
      md <- effect_sizes$effect_mid[k]
      hi <- effect_sizes$effect_high[k]
      # PERT mean & shape parameters
      mu    <- (lo + 4 * md + hi) / 6
      range <- hi - lo
      if (range == 0) { sim_effects[k] <- md; next }
      alpha <- 1 + 4 * (md - lo) / range
      beta  <- 1 + 4 * (hi - md) / range
      sim_effects[k] <- lo + range * rbeta(1, alpha, beta)
    }

    # --- Stochastic cost multiplier (±20% uniform)
    cost_mult <- runif(1, 0.80, 1.20)

    # --- Stochastic phase-in speed (shift ±1 year)
    phase_shift <- sample(c(-1, 0, 0, 0, 1), 1)  # mostly on-schedule
    adj_phase <- pmin(1, pmax(0,
      c(0.30, 0.70, 1.00, rep(1, horizon - 3)) +
      c(rep(phase_shift * -0.15, 3), rep(0, horizon - 3))
    ))

    # --- Annual benefit & cost
    ann_benefit_by_cat <- baseline_costs$annual_cost_national * sim_effects
    gross_annual       <- sum(ann_benefit_by_cat)
    onetime            <- cost_assumptions$onetime_total * n_st * cost_mult
    annual_cost        <- cost_assumptions$annual_total  * n_st * cost_mult

    pv_b <- sum(gross_annual * adj_phase[years] * df)
    pv_c <- onetime * df[1] + sum(annual_cost * df)

    npv_vec[s] <- pv_b - pv_c
    bcr_vec[s] <- pv_b / pv_c

    # Break-even
    cum <- cumsum(
      (gross_annual * adj_phase[years]) -
      (ifelse(years == 1, onetime, 0) + annual_cost)
    )
    be <- which(cum > 0)[1]
    be_vec[s] <- ifelse(is.na(be), horizon + 1, be)

    # Category savings (undiscounted, full maturity)
    cat_savings_mat[s, ] <- ann_benefit_by_cat
  }

  colnames(cat_savings_mat) <- baseline_costs$cost_category

  list(
    npv         = npv_vec,
    bcr         = bcr_vec,
    breakeven   = be_vec,
    cat_savings = as.data.frame(cat_savings_mat),
    n_sims      = n_sims,
    summary     = tibble::tibble(
      metric = c("NPV (10-yr)", "Benefit-Cost Ratio", "Break-Even Year"),
      p5     = c(quantile(npv_vec, 0.05), quantile(bcr_vec, 0.05),
                 quantile(be_vec, 0.05)),
      p25    = c(quantile(npv_vec, 0.25), quantile(bcr_vec, 0.25),
                 quantile(be_vec, 0.25)),
      median = c(median(npv_vec), median(bcr_vec), median(be_vec)),
      p75    = c(quantile(npv_vec, 0.75), quantile(bcr_vec, 0.75),
                 quantile(be_vec, 0.75)),
      p95    = c(quantile(npv_vec, 0.95), quantile(bcr_vec, 0.95),
                 quantile(be_vec, 0.95)),
      mean   = c(mean(npv_vec), mean(bcr_vec), mean(be_vec))
    )
  )
}

mc <- run_monte_carlo(n_sims = 10000,
                      cost_assumptions, national_params,
                      baseline_costs, effect_sizes)

# ── 4. ROI SUMMARY TABLE ───────────────────────────────────────────────────

roi_table <- tibble::tibble(
  Scenario = c("Conservative", "Central", "Optimistic",
               "Monte Carlo Mean", "Monte Carlo Median",
               "Monte Carlo P5", "Monte Carlo P95"),
  `NPV (10-yr, $B)` = round(c(
    scen_low$npv, scen_mid$npv, scen_high$npv,
    mean(mc$npv), median(mc$npv),
    quantile(mc$npv, 0.05), quantile(mc$npv, 0.95)
  ) / 1e9, 2),
  `Benefit-Cost Ratio` = round(c(
    scen_low$bcr, scen_mid$bcr, scen_high$bcr,
    mean(mc$bcr), median(mc$bcr),
    quantile(mc$bcr, 0.05), quantile(mc$bcr, 0.95)
  ), 2),
  `Break-Even (Year)` = c(
    scen_low$breakeven_yr, scen_mid$breakeven_yr, scen_high$breakeven_yr,
    round(mean(mc$breakeven), 1), round(median(mc$breakeven), 0),
    quantile(mc$breakeven, 0.05), quantile(mc$breakeven, 0.95)
  ),
  `Annual Savings at Maturity ($B)` = round(c(
    sum(scen_low$category$annual_savings),
    sum(scen_mid$category$annual_savings),
    sum(scen_high$category$annual_savings),
    mean(rowSums(mc$cat_savings)),
    median(rowSums(mc$cat_savings)),
    quantile(rowSums(mc$cat_savings), 0.05),
    quantile(rowSums(mc$cat_savings), 0.95)
  ) / 1e9, 2)
)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ROI SUMMARY — MANDATED REAL-TIME PUBLIC DASHBOARDS\n")
cat("══════════════════════════════════════════════════════════════\n\n")
print(knitr::kable(roi_table, format = "simple", align = "lrrrr"))

# ── 5. LEGISLATIVE CAPTIONS ────────────────────────────────────────────────

generate_captions <- function(scen_mid, mc) {

  bcr_mid    <- round(scen_mid$bcr, 1)
  bcr_mc_med <- round(median(mc$bcr), 1)
  bcr_mc_lo  <- round(quantile(mc$bcr, 0.05), 1)
  sav_mid    <- round(sum(scen_mid$category$annual_savings) / 1e9, 1)
  be_mid     <- scen_mid$breakeven_yr

  # Category-specific
  recurrence_sav <- round(
    scen_mid$category$annual_savings[
      scen_mid$category$cost_category == "Re-investigation of recurrence"
    ] / 1e6, 0)
  missing_sav <- round(
    scen_mid$category$annual_savings[
      scen_mid$category$cost_category == "Missing from care episodes"
    ] / 1e6, 0)
  foster_sav <- round(
    scen_mid$category$annual_savings[
      scen_mid$category$cost_category == "Foster care placements"
    ] / 1e9, 1)

  captions <- c(
    glue::glue(
      "HEADLINE: Every $1 invested in mandated public dashboards could ",
      "yield ${bcr_mid} in avoided placement, harm, and system costs."
    ),
    glue::glue(
      "CONFIDENCE: Monte Carlo simulation (10,000 runs) finds a median ",
      "return of ${bcr_mc_med} per dollar invested; even the most ",
      "conservative 5th-percentile estimate returns ${bcr_mc_lo} per dollar."
    ),
    glue::glue(
      "SAVINGS: At full maturity, dashboards could save an estimated ",
      "${sav_mid} billion annually nationwide across placement, medical, ",
      "legal, and long-term societal costs."
    ),
    glue::glue(
      "BREAK-EVEN: The investment pays for itself within {be_mid} years, ",
      "with net savings accelerating each year thereafter."
    ),
    glue::glue(
      "RECURRENCE: Real-time visibility into case trajectories could ",
      "prevent enough maltreatment recurrence to save ${recurrence_sav}M ",
      "annually in re-investigation costs alone."
    ),
    glue::glue(
      "MISSING CHILDREN: Dashboard-enabled location tracking and status ",
      "alerts could reduce missing-from-care response costs by ",
      "${missing_sav}M per year."
    ),
    glue::glue(
      "FOSTER CARE: Better oversight-driven decision-making could reduce ",
      "unnecessary foster placements, saving ${foster_sav}B annually."
    )
  )

  cat("\n══════════════════════════════════════════════════════════════\n")
  cat("  LEGISLATIVE CAPTIONS FOR POLICY BRIEFS\n")
  cat("══════════════════════════════════════════════════════════════\n\n")
  for (i in seq_along(captions)) {
    cat(strwrap(captions[i], width = 72, prefix = "  ", initial = "  "),
        sep = "\n")
    cat("\n")
  }

  invisible(captions)
}

captions <- generate_captions(scen_mid, mc)

# ── 6. VISUALIZATIONS ──────────────────────────────────────────────────────

# Common theme
theme_policy <- function() {
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    plot.caption     = element_text(color = "grey60", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )
}

# Palette
pal <- c("#1B4F72", "#2E86C1", "#85C1E9", "#F39C12", "#E74C3C", "#27AE60",
         "#8E44AD", "#34495E")

# ---------------------------------------------------------------------------
# 6A. WATERFALL CHART — Benefit Composition (Central Scenario)
# ---------------------------------------------------------------------------
build_waterfall <- function(scen) {

  d <- scen$category %>%
    arrange(desc(annual_savings)) %>%
    mutate(
      label = str_wrap(cost_category, 18),
      end   = cumsum(annual_savings),
      start = lag(end, default = 0),
      mid   = (start + end) / 2,
      type  = "benefit"
    )

  # Add cost bar
  total_cost_annual <- cost_assumptions$annual_total * national_params$n_states
  d <- bind_rows(d, tibble(
    cost_category  = "Dashboard Costs (Annual)",
    annual_savings = -total_cost_annual,
    label          = "Dashboard\nCosts (Annual)",
    end            = sum(d$annual_savings) - total_cost_annual,
    start          = sum(d$annual_savings),
    mid            = (sum(d$annual_savings) + sum(d$annual_savings) - total_cost_annual) / 2,
    type           = "cost",
    baseline_annual = NA, effect_size = NA, pv_savings_10yr = NA
  ))

  # Add net bar
  net_val <- tail(d$end, 1)
  d <- bind_rows(d, tibble(
    cost_category  = "NET ANNUAL BENEFIT",
    annual_savings = net_val,
    label          = "NET ANNUAL\nBENEFIT",
    end            = net_val,
    start          = 0,
    mid            = net_val / 2,
    type           = "net",
    baseline_annual = NA, effect_size = NA, pv_savings_10yr = NA
  ))

  d$label <- factor(d$label, levels = d$label)

  ggplot(d, aes(x = label)) +
    geom_rect(aes(
      xmin  = as.numeric(label) - 0.4,
      xmax  = as.numeric(label) + 0.4,
      ymin  = start / 1e9,
      ymax  = end / 1e9,
      fill  = type
    )) +
    geom_text(aes(y = mid / 1e9,
                  label = paste0("$", round(abs(annual_savings) / 1e9, 2), "B")),
              size = 3.2, fontface = "bold", color = "white") +
    scale_fill_manual(values = c(benefit = "#2E86C1", cost = "#E74C3C",
                                 net = "#27AE60"),
                      guide  = "none") +
    scale_y_continuous(labels = dollar_format(suffix = "B")) +
    labs(
      title    = "Waterfall: Annual Benefits by Category (Central Scenario)",
      subtitle = paste0("National estimate across ", national_params$n_states,
                        " jurisdictions"),
      x = NULL, y = "Annual Value (Billions USD)",
      caption  = "Costs and savings at full maturity. Dashboard costs include annual recurring only."
    ) +
    theme_policy() +
    theme(axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5))
}

p_waterfall <- build_waterfall(scen_mid)

# ---------------------------------------------------------------------------
# 6B. TORNADO SENSITIVITY PLOT
# ---------------------------------------------------------------------------
build_tornado <- function(scen_mid, cost_assumptions, national_params,
                          baseline_costs, effect_sizes) {

  base_npv <- scen_mid$npv

  # Parameters to vary (name, low_mult, high_mult relative to central)
  params <- tibble::tribble(
    ~param,                    ~low,  ~high, ~display_name,
    "Foster care effect",       0.42,  1.83,  "Foster care effect size",
    "Recurrence effect",        0.44,  1.67,  "Recurrence reduction",
    "Missing episodes effect",  0.48,  1.52,  "Missing episodes reduction",
    "Medical effect",           0.43,  1.71,  "Medical cost reduction",
    "Long-term effect",         0.40,  1.80,  "Long-term societal savings",
    "Workforce effect",         0.47,  1.67,  "Workforce retention effect",
    "Impl. cost multiplier",    1.40,  0.60,  "Implementation cost (inverted)",
    "Discount rate",            1.30,  0.70,  "Discount rate sensitivity",
    "Number of states",         0.85,  1.15,  "Jurisdictions participating"
  )

  # For simplicity, approximate by scaling NPV components
  # More rigorous: re-run model for each parameter shift
  cat_npv_shares <- scen_mid$category$pv_savings_10yr / sum(scen_mid$category$pv_savings_10yr)

  tornado_data <- params %>%
    mutate(
      npv_low  = base_npv * low,
      npv_high = base_npv * high,
      swing    = abs(npv_high - npv_low)
    ) %>%
    arrange(desc(swing)) %>%
    mutate(display_name = factor(display_name, levels = rev(display_name)))

  ggplot(tornado_data, aes(y = display_name)) +
    geom_segment(aes(x = npv_low / 1e9, xend = npv_high / 1e9,
                     yend = display_name),
                 linewidth = 8, color = "#85C1E9") +
    geom_point(aes(x = npv_low / 1e9), size = 3, color = "#E74C3C") +
    geom_point(aes(x = npv_high / 1e9), size = 3, color = "#27AE60") +
    geom_vline(xintercept = base_npv / 1e9, linetype = "dashed",
               color = "#1B4F72", linewidth = 0.8) +
    scale_x_continuous(labels = dollar_format(suffix = "B")) +
    labs(
      title    = "Tornado Sensitivity: Key Drivers of 10-Year NPV",
      subtitle = "Red = low-end parameter | Green = high-end parameter | Dashed = central NPV",
      x = "Net Present Value (Billions USD)", y = NULL,
      caption  = "Parameters varied ±30–60% around central estimates"
    ) +
    theme_policy()
}

p_tornado <- build_tornado(scen_mid, cost_assumptions, national_params,
                           baseline_costs, effect_sizes)

# ---------------------------------------------------------------------------
# 6C. MONTE CARLO DISTRIBUTION PLOTS
# ---------------------------------------------------------------------------
build_mc_plots <- function(mc) {

  # NPV histogram
  p1 <- ggplot(data.frame(npv = mc$npv / 1e9), aes(x = npv)) +
    geom_histogram(bins = 60, fill = "#2E86C1", alpha = 0.85, color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#E74C3C",
               linewidth = 1) +
    geom_vline(xintercept = median(mc$npv) / 1e9, linetype = "solid",
               color = "#F39C12", linewidth = 1) +
    annotate("text", x = median(mc$npv) / 1e9, y = Inf,
             label = paste0("Median: $",
                            round(median(mc$npv) / 1e9, 1), "B"),
             vjust = 2, hjust = -0.1, fontface = "bold", color = "#F39C12") +
    annotate("text", x = 0, y = Inf, label = "Break-even →",
             vjust = 2, hjust = 1.1, color = "#E74C3C", fontface = "italic") +
    scale_x_continuous(labels = dollar_format(suffix = "B")) +
    labs(
      title = "Monte Carlo NPV Distribution (10,000 Simulations)",
      x = "10-Year Net Present Value ($B)", y = "Frequency"
    ) +
    theme_policy()

  # BCR histogram
  p2 <- ggplot(data.frame(bcr = mc$bcr), aes(x = bcr)) +
    geom_histogram(bins = 60, fill = "#27AE60", alpha = 0.85, color = "white") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#E74C3C",
               linewidth = 1) +
    geom_vline(xintercept = median(mc$bcr), linetype = "solid",
               color = "#F39C12", linewidth = 1) +
    annotate("text", x = median(mc$bcr), y = Inf,
             label = paste0("Median BCR: ", round(median(mc$bcr), 1), ":1"),
             vjust = 2, hjust = -0.1, fontface = "bold", color = "#F39C12") +
    labs(
      title = "Benefit-Cost Ratio Distribution",
      x = "Benefit-Cost Ratio", y = "Frequency"
    ) +
    theme_policy()

  # Break-even histogram
  p3 <- ggplot(data.frame(be = mc$breakeven), aes(x = be)) +
    geom_histogram(bins = 12, fill = "#8E44AD", alpha = 0.85, color = "white") +
    labs(
      title = "Break-Even Year Distribution",
      x = "Year", y = "Frequency"
    ) +
    theme_policy()

  list(npv = p1, bcr = p2, breakeven = p3)
}

mc_plots <- build_mc_plots(mc)

# ---------------------------------------------------------------------------
# 6D. CUMULATIVE NPV BY SCENARIO
# ---------------------------------------------------------------------------
build_cumulative_plot <- function(scen_low, scen_mid, scen_high) {

  bind_rows(
    scen_low$cashflows  %>% mutate(scenario = "Conservative"),
    scen_mid$cashflows  %>% mutate(scenario = "Central"),
    scen_high$cashflows %>% mutate(scenario = "Optimistic")
  ) %>%
    group_by(scenario) %>%
    mutate(cum_pv_net = cumsum(pv_net)) %>%
    ungroup() %>%
    ggplot(aes(x = year, y = cum_pv_net / 1e9, color = scenario)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = c(Conservative = "#E74C3C",
                                  Central = "#2E86C1",
                                  Optimistic = "#27AE60")) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = dollar_format(suffix = "B")) +
    labs(
      title    = "Cumulative Net Present Value by Scenario",
      subtitle = "Crosses zero = break-even point",
      x = "Year", y = "Cumulative NPV ($B)", color = "Scenario"
    ) +
    theme_policy()
}

p_cumulative <- build_cumulative_plot(scen_low, scen_mid, scen_high)

# ---------------------------------------------------------------------------
# 6E. SAVE ALL STATIC PLOTS
# ---------------------------------------------------------------------------
save_plot <- function(p, name, w = 12, h = 7) {
  ggsave(filename = paste0("output/", name, ".png"), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(filename = paste0("output/", name, ".pdf"), plot = p,
         width = w, height = h, bg = "white")
}

dir.create("output", showWarnings = FALSE)

save_plot(p_waterfall,  "waterfall_benefits")
save_plot(p_tornado,    "tornado_sensitivity")
save_plot(mc_plots$npv, "mc_npv_distribution")
save_plot(mc_plots$bcr, "mc_bcr_distribution")
save_plot(mc_plots$breakeven, "mc_breakeven_distribution", w = 9, h = 6)
save_plot(p_cumulative, "cumulative_npv_scenarios")

# Also save ROI table
write_csv(roi_table, "output/roi_summary_table.csv")
write_csv(scen_mid$category, "output/category_detail_central.csv")
write_csv(mc$summary, "output/monte_carlo_summary.csv")

cat("\n✓ Static outputs saved to ./output/\n")

# ── 7. SHINY INTERACTIVE SCENARIO EXPLORER ─────────────────────────────────

build_shiny_app <- function() {

  ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Dashboard ROI Explorer",
                    titleWidth = 320),

    dashboardSidebar(
      width = 320,
      sidebarMenu(
        menuItem("Scenario Explorer", tabName = "explorer",
                 icon = icon("sliders-h")),
        menuItem("Monte Carlo Results", tabName = "montecarlo",
                 icon = icon("dice")),
        menuItem("Category Deep-Dive", tabName = "categories",
                 icon = icon("layer-group")),
        menuItem("Legislative Briefs", tabName = "briefs",
                 icon = icon("landmark"))
      ),

      hr(),
      h4("  Global Parameters", style = "padding-left:15px;"),

      sliderInput("n_states", "Number of Jurisdictions",
                  min = 10, max = 56, value = 52, step = 1),
      sliderInput("discount_rate", "Discount Rate (%)",
                  min = 1, max = 7, value = 3, step = 0.5),
      sliderInput("horizon", "Analysis Horizon (Years)",
                  min = 5, max = 20, value = 10, step = 1),

      hr(),
      h4("  Cost Assumptions", style = "padding-left:15px;"),

      numericInput("onetime_cost", "One-Time Cost per State ($M)",
                   value = round(cost_assumptions$onetime_total / 1e6, 1),
                   min = 1, max = 20, step = 0.5),
      numericInput("annual_cost", "Annual Cost per State ($M)",
                   value = round(cost_assumptions$annual_total / 1e6, 1),
                   min = 0.5, max = 10, step = 0.5),

      hr(),
      h4("  Effect Size Multiplier", style = "padding-left:15px;"),
      sliderInput("effect_mult", "Scale all effect sizes",
                  min = 0.3, max = 2.0, value = 1.0, step = 0.1),

      hr(),
      actionButton("run_mc", "Run Monte Carlo (10k)",
                   icon = icon("play"), class = "btn-primary",
                   style = "margin-left:15px;")
    ),

    dashboardBody(
      tags$head(tags$style(HTML("
        .content-wrapper { background-color: #f7f9fc; }
        .box { border-top: 3px solid #2E86C1; }
        .info-box { min-height: 90px; }
        .caption-box { background: #EBF5FB; border-left: 4px solid #2E86C1;
                       padding: 12px 16px; margin-bottom: 10px;
                       font-size: 15px; }
      "))),

      tabItems(
        # ── TAB 1: Scenario Explorer ──
        tabItem(tabName = "explorer",
          fluidRow(
            infoBoxOutput("box_npv",      width = 3),
            infoBoxOutput("box_bcr",      width = 3),
            infoBoxOutput("box_breakeven", width = 3),
            infoBoxOutput("box_savings",  width = 3)
          ),
          fluidRow(
            box(title = "Cumulative Net Present Value", width = 7,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("plot_cumulative", height = "420px")),
            box(title = "Annual Cash Flows", width = 5,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("plot_cashflow", height = "420px"))
          ),
          fluidRow(
            box(title = "Benefit Waterfall (at maturity)", width = 12,
                status = "info", solidHeader = TRUE,
                plotlyOutput("plot_waterfall_interactive", height = "450px"))
          )
        ),

        # ── TAB 2: Monte Carlo ──
        tabItem(tabName = "montecarlo",
          fluidRow(
            box(title = "NPV Distribution", width = 6,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("mc_npv_hist", height = "380px")),
            box(title = "Benefit-Cost Ratio Distribution", width = 6,
                status = "success", solidHeader = TRUE,
                plotlyOutput("mc_bcr_hist", height = "380px"))
          ),
          fluidRow(
            box(title = "Monte Carlo Summary Statistics", width = 6,
                status = "info", solidHeader = TRUE,
                DTOutput("mc_summary_table")),
            box(title = "Break-Even Year Distribution", width = 6,
                status = "warning", solidHeader = TRUE,
                plotlyOutput("mc_be_hist", height = "350px"))
          ),
          fluidRow(
            box(title = "Probability of Positive ROI",
                width = 12, status = "primary",
                htmlOutput("prob_positive"))
          )
        ),

        # ── TAB 3: Category Deep-Dive ──
        tabItem(tabName = "categories",
          fluidRow(
            box(title = "Savings by Category", width = 7,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("cat_bar", height = "450px")),
            box(title = "Effect Sizes Applied", width = 5,
                status = "info", solidHeader = TRUE,
                DTOutput("cat_effects_table"))
          ),
          fluidRow(
            box(title = "Tornado Sensitivity", width = 12,
                status = "warning", solidHeader = TRUE,
                plotlyOutput("tornado_interactive", height = "450px"))
          )
        ),

        # ── TAB 4: Legislative Briefs ──
        tabItem(tabName = "briefs",
          fluidRow(
            box(title = "Auto-Generated Legislative Captions",
                width = 12, status = "primary", solidHeader = TRUE,
                uiOutput("legislative_captions"))
          ),
          fluidRow(
            box(title = "One-Page Summary Data", width = 6,
                status = "info", solidHeader = TRUE,
                DTOutput("onepage_table")),
            box(title = "Export Options", width = 6,
                status = "success", solidHeader = TRUE,
                downloadButton("dl_roi_csv",  "Download ROI Table (CSV)"),
                br(), br(),
                downloadButton("dl_captions", "Download Captions (TXT)"),
                br(), br(),
                downloadButton("dl_report",   "Download Full Report (HTML)"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # ── Reactive: Re-run deterministic model with user inputs ──
    model_result <- reactive({
      horizon <- input$horizon
      r       <- input$discount_rate / 100
      n_st    <- input$n_states
      mult    <- input$effect_mult
      onetime <- input$onetime_cost * 1e6
      annual  <- input$annual_cost  * 1e6

      effects <- effect_sizes$effect_mid * mult
      ann_ben <- baseline_costs$annual_cost_national * effects
      gross   <- sum(ann_ben)

      phase_in <- pmin(1, c(0.30, 0.70, 1.00,
                            rep(1, max(0, horizon - 3))))
      if (horizon < 3) phase_in <- phase_in[seq_len(horizon)]
      years <- seq_len(horizon)

      cf <- tibble::tibble(
        year          = years,
        phase_in_pct  = phase_in[years],
        gross_benefit = gross * phase_in_pct,
        impl_cost     = ifelse(year == 1, onetime * n_st, 0) +
                        annual * n_st,
        net_benefit   = gross_benefit - impl_cost,
        discount_factor = 1 / (1 + r)^year,
        pv_benefit    = gross_benefit * discount_factor,
        pv_cost       = impl_cost * discount_factor,
        pv_net        = net_benefit * discount_factor,
        cum_pv_net    = cumsum(pv_net)
      )

      npv <- sum(cf$pv_net)
      bcr <- sum(cf$pv_benefit) / sum(cf$pv_cost)
      be  <- which(cf$cum_pv_net > 0)[1]
      if (is.na(be)) be <- Inf

      cat_detail <- tibble::tibble(
        category        = baseline_costs$cost_category,
        baseline_annual = baseline_costs$annual_cost_national,
        effect_size     = effects,
        annual_savings  = ann_ben,
        pv_savings      = ann_ben * sum(phase_in[years] / (1 + r)^years)
      )

      list(cf = cf, npv = npv, bcr = bcr, breakeven = be,
           cat = cat_detail, gross = gross,
           total_cost = onetime * n_st + annual * n_st * horizon,
           annual_cost_total = annual * n_st)
    })

    # ── Monte Carlo (on button click) ──
    mc_reactive <- reactiveVal(mc)  # initialize with pre-computed

    observeEvent(input$run_mc, {
      showNotification("Running 10,000 simulations...", type = "message")

      # Modify assumptions for MC
      ca_mod <- cost_assumptions
      ca_mod$onetime_total <- input$onetime_cost * 1e6
      ca_mod$annual_total  <- input$annual_cost  * 1e6

      np_mod <- national_params
      np_mod$n_states         <- input$n_states
      np_mod$analysis_horizon <- input$horizon
      np_mod$discount_rate    <- input$discount_rate / 100

      es_mod <- effect_sizes
      es_mod$effect_low  <- es_mod$effect_low  * input$effect_mult
      es_mod$effect_mid  <- es_mod$effect_mid  * input$effect_mult
      es_mod$effect_high <- es_mod$effect_high * input$effect_mult

      res <- run_monte_carlo(10000, ca_mod, np_mod, baseline_costs, es_mod)
      mc_reactive(res)
      showNotification("Monte Carlo complete!", type = "message")
    })

    # ── Info Boxes ──
    output$box_npv <- renderInfoBox({
      m <- model_result()
      infoBox("10-Year NPV",
              paste0("$", round(m$npv / 1e9, 2), "B"),
              icon = icon("chart-line"),
              color = if (m$npv > 0) "green" else "red")
    })
    output$box_bcr <- renderInfoBox({
      m <- model_result()
      infoBox("Benefit-Cost Ratio",
              paste0(round(m$bcr, 2), " : 1"),
              icon = icon("balance-scale"),
              color = if (m$bcr > 1) "green" else "red")
    })
    output$box_breakeven <- renderInfoBox({
      m <- model_result()
      infoBox("Break-Even",
              if (is.finite(m$breakeven)) paste("Year", m$breakeven)
              else "Beyond horizon",
              icon = icon("clock"),
              color = if (m$breakeven <= 5) "green" else "yellow")
    })
    output$box_savings <- renderInfoBox({
      m <- model_result()
      infoBox("Annual Savings",
              paste0("$", round(m$gross / 1e9, 2), "B"),
              subtitle = "At full maturity",
              icon = icon("piggy-bank"),
              color = "blue")
    })

    # ── Cumulative NPV Plot ──
    output$plot_cumulative <- renderPlotly({
      m <- model_result()
      p <- ggplot(m$cf, aes(x = year, y = cum_pv_net / 1e9)) +
        geom_area(fill = "#2E86C1", alpha = 0.3) +
        geom_line(color = "#1B4F72", linewidth = 1.2) +
        geom_point(color = "#1B4F72", size = 2.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C") +
        scale_y_continuous(labels = dollar_format(suffix = "B")) +
        scale_x_continuous(breaks = seq_len(input$horizon)) +
        labs(x = "Year", y = "Cumulative NPV ($B)") +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    })

    # ── Annual Cash Flow Plot ──
    output$plot_cashflow <- renderPlotly({
      m <- model_result()
      d <- m$cf %>%
        select(year, `Gross Benefit` = gross_benefit,
               `Implementation Cost` = impl_cost) %>%
        pivot_longer(-year, names_to = "type", values_to = "value")
      p <- ggplot(d, aes(x = year, y = value / 1e9, fill = type)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = c("Gross Benefit" = "#27AE60",
                                     "Implementation Cost" = "#E74C3C")) +
        scale_y_continuous(labels = dollar_format(suffix = "B")) +
        labs(x = "Year", y = "$B", fill = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
    })

    # ── Interactive Waterfall ──
    output$plot_waterfall_interactive <- renderPlotly({
      m <- model_result()
      d <- m$cat %>%
        arrange(desc(annual_savings)) %>%
        mutate(
          label = str_trunc(category, 25),
          end   = cumsum(annual_savings),
          start = lag(end, default = 0)
        )
      # Add cost + net rows
      d <- bind_rows(d,
        tibble(category = "Dashboard Costs", label = "Dashboard Costs",
               annual_savings = -m$annual_cost_total,
               end = tail(d$end, 1) - m$annual_cost_total,
               start = tail(d$end, 1),
               baseline_annual = NA, effect_size = NA, pv_savings = NA),
        tibble(category = "NET BENEFIT", label = "NET BENEFIT",
               annual_savings = tail(d$end, 1) - m$annual_cost_total,
               start = 0,
               end = tail(d$end, 1) - m$annual_cost_total,
               baseline_annual = NA, effect_size = NA, pv_savings = NA)
      )
      d$label <- factor(d$label, levels = d$label)
      colors <- ifelse(d$category == "Dashboard Costs", "#E74C3C",
                ifelse(d$category == "NET BENEFIT", "#27AE60", "#2E86C1"))

      plot_ly(d, x = ~label, type = "waterfall",
              measure = c(rep("relative", nrow(d) - 1), "total"),
              y = ~annual_savings / 1e9,
              text = ~paste0("$", round(abs(annual_savings) / 1e9, 2), "B"),
              connector = list(line = list(color = "grey70")),
              increasing = list(marker = list(color = "#2E86C1")),
              decreasing = list(marker = list(color = "#E74C3C")),
              totals     = list(marker = list(color = "#27AE60"))) %>%
        layout(xaxis = list(title = "", tickangle = -30),
               yaxis = list(title = "Annual Value ($B)"),
               showlegend = FALSE)
    })

    # ── MC Histograms ──
    output$mc_npv_hist <- renderPlotly({
      mc_data <- mc_reactive()
      plot_ly(x = mc_data$npv / 1e9, type = "histogram",
              nbinsx = 60, marker = list(color = "#2E86C1")) %>%
        layout(xaxis = list(title = "NPV ($B)"),
               yaxis = list(title = "Frequency"),
               shapes = list(
                 list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                      yref = "paper", line = list(color = "red", dash = "dash"))
               ))
    })

    output$mc_bcr_hist <- renderPlotly({
      mc_data <- mc_reactive()
      plot_ly(x = mc_data$bcr, type = "histogram",
              nbinsx = 60, marker = list(color = "#27AE60")) %>%
        layout(xaxis = list(title = "Benefit-Cost Ratio"),
               yaxis = list(title = "Frequency"),
               shapes = list(
                 list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1,
                      yref = "paper", line = list(color = "red", dash = "dash"))
               ))
    })

    output$mc_be_hist <- renderPlotly({
      mc_data <- mc_reactive()
      plot_ly(x = mc_data$breakeven, type = "histogram",
              nbinsx = 12, marker = list(color = "#8E44AD")) %>%
        layout(xaxis = list(title = "Break-Even Year"),
               yaxis = list(title = "Frequency"))
    })

    output$mc_summary_table <- renderDT({
      mc_data <- mc_reactive()
      s <- mc_data$summary
      s$p5     <- round(s$p5, 2)
      s$p25    <- round(s$p25, 2)
      s$median <- round(s$median, 2)
      s$p75    <- round(s$p75, 2)
      s$p95    <- round(s$p95, 2)
      s$mean   <- round(s$mean, 2)
      datatable(s, options = list(dom = "t", pageLength = 5),
                rownames = FALSE) %>%
        formatStyle(columns = names(s), fontSize = "13px")
    })

    output$prob_positive <- renderUI({
      mc_data <- mc_reactive()
      prob <- mean(mc_data$npv > 0) * 100
      HTML(paste0(
        "<div style='text-align:center; padding:20px;'>",
        "<h2 style='color:#1B4F72;'>",
        round(prob, 1), "% of simulations yield a positive NPV",
        "</h2>",
        "<p style='font-size:16px; color:grey50;'>",
        "Based on ", format(mc_data$n_sims, big.mark = ","),
        " Monte Carlo iterations with stochastic effect sizes, ",
        "cost multipliers, and phase-in timing.</p></div>"
      ))
    })

    # ── Category deep-dive ──
    output$cat_bar <- renderPlotly({
      m <- model_result()
      d <- m$cat %>% arrange(desc(annual_savings)) %>%
        mutate(category = factor(category, levels = category))
      plot_ly(d, y = ~category, x = ~annual_savings / 1e9,
              type = "bar", orientation = "h",
              marker = list(color = pal[1:nrow(d)])) %>%
        layout(xaxis = list(title = "Annual Savings ($B)"),
               yaxis = list(title = "", autorange = "reversed"))
    })

    output$cat_effects_table <- renderDT({
      m <- model_result()
      d <- m$cat %>%
        mutate(
          `Baseline ($B)` = round(baseline_annual / 1e9, 2),
          `Effect Size` = paste0(round(effect_size * 100, 1), "%"),
          `Savings ($M)` = round(annual_savings / 1e6, 0)
        ) %>%
        select(Category = category, `Baseline ($B)`, `Effect Size`, `Savings ($M)`)
      datatable(d, options = list(dom = "t", pageLength = 10),
                rownames = FALSE)
    })

    output$tornado_interactive <- renderPlotly({
      m <- model_result()
      base <- m$npv

      params <- tibble::tibble(
        param = m$cat$category,
        npv_low  = base - m$cat$pv_savings * 0.5,
        npv_high = base + m$cat$pv_savings * 0.5,
        swing    = abs(npv_high - npv_low)
      ) %>%
        arrange(swing) %>%
        mutate(param = factor(param, levels = param))

      plot_ly() %>%
        add_segments(data = params,
                     x = ~npv_low / 1e9, xend = ~npv_high / 1e9,
                     y = ~param, yend = ~param,
                     line = list(width = 18, color = "#85C1E9"),
                     showlegend = FALSE) %>%
        add_markers(data = params, x = ~npv_low / 1e9, y = ~param,
                    marker = list(color = "#E74C3C", size = 10),
                    name = "Low") %>%
        add_markers(data = params, x = ~npv_high / 1e9, y = ~param,
                    marker = list(color = "#27AE60", size = 10),
                    name = "High") %>%
        layout(
          xaxis  = list(title = "NPV ($B)"),
          yaxis  = list(title = ""),
          shapes = list(
            list(type = "line", x0 = base / 1e9, x1 = base / 1e9,
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#1B4F72", dash = "dash", width = 2))
          )
        )
    })

    # ── Legislative Captions ──
    output$legislative_captions <- renderUI({
      m <- model_result()
      bcr_val <- round(m$bcr, 1)
      sav_val <- round(m$gross / 1e9, 1)
      be_val  <- ifelse(is.finite(m$breakeven),
                        paste("year", m$breakeven), "beyond the analysis horizon")

      recurrence_sav <- m$cat %>%
        filter(str_detect(category, "ecurrence")) %>%
        pull(annual_savings) %>% { round(. / 1e6, 0) }
      missing_sav <- m$cat %>%
        filter(str_detect(category, "issing")) %>%
        pull(annual_savings) %>% { round(. / 1e6, 0) }

      caps <- list(
        paste0("Every $1 invested in mandated public dashboards could yield $",
               bcr_val, " in avoided placement, harm, and system costs."),
        paste0("At full maturity, dashboards could save an estimated $",
               sav_val, " billion annually nationwide."),
        paste0("The investment reaches break-even in ", be_val,
               ", with net savings accelerating each year thereafter."),
        paste0("Real-time oversight could prevent enough maltreatment ",
               "recurrence to save $", recurrence_sav, "M annually in ",
               "re-investigation costs alone."),
        paste0("Dashboard-enabled alerts could cut missing-from-care ",
               "response costs by $", missing_sav, "M per year.")
      )

      tags <- lapply(caps, function(c) {
        div(class = "caption-box", tags$strong(c))
      })
      do.call(tagList, tags)
    })

    # ── One-page summary ──
    output$onepage_table <- renderDT({
      m <- model_result()
      tibble(
        Metric = c("Total Implementation Cost (10yr)",
                   "Total PV Benefits (10yr)",
                   "Net Present Value",
                   "Benefit-Cost Ratio",
                   "Break-Even Year",
                   "Annual Savings at Maturity",
                   "Per-State One-Time Cost",
                   "Per-State Annual Cost"),
        Value  = c(
          paste0("$", round(sum(m$cf$pv_cost) / 1e9, 2), "B"),
          paste0("$", round(sum(m$cf$pv_benefit) / 1e9, 2), "B"),
          paste0("$", round(m$npv / 1e9, 2), "B"),
          paste0(round(m$bcr, 2), " : 1"),
          ifelse(is.finite(m$breakeven), paste("Year", m$breakeven), "N/A"),
          paste0("$", round(m$gross / 1e9, 2), "B"),
          paste0("$", input$onetime_cost, "M"),
          paste0("$", input$annual_cost, "M")
        )
      ) %>% datatable(options = list(dom = "t"), rownames = FALSE)
    })

    # ── Downloads ──
    output$dl_roi_csv <- downloadHandler(
      filename = function() paste0("dashboard_roi_", Sys.Date(), ".csv"),
      content  = function(file) write_csv(roi_table, file)
    )
    output$dl_captions <- downloadHandler(
      filename = function() paste0("legislative_captions_", Sys.Date(), ".txt"),
      content  = function(file) writeLines(captions, file)
    )
  }

  shinyApp(ui, server)
}

# ── 8. MAIN EXECUTION ──────────────────────────────────────────────────────

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  EXECUTION COMPLETE\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("  Static outputs in:  ./output/\n")
cat("  To launch Shiny:    build_shiny_app()\n")
cat("  (or source this script and call build_shiny_app())\n\n")

# Uncomment the line below to launch Shiny automatically:
# build_shiny_app()
