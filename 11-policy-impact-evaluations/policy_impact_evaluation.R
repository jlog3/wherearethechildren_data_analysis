###############################################################################
#  CHILD WELFARE POLICY IMPACT EVALUATION
#  ITS and DiD analysis of four focal child-welfare metrics.
#  Required: tidyverse, fixest, modelsummary, ggthemes, plotly, htmlwidgets,
#            sandwich, lmtest, scales, glue
###############################################################################

library(tidyverse); library(fixest); library(modelsummary)
library(ggthemes); library(plotly); library(htmlwidgets)
library(sandwich); library(lmtest); library(scales); library(glue)

set.seed(42)
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# -- Custom theme -------------------------------------------------------------
theme_policy <- function(base_size = 13) {
  theme_fivethirtyeight(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", size = base_size + 2),
          plot.subtitle = element_text(colour = "grey40", size = base_size - 1),
          plot.caption = element_text(colour = "grey50", size = base_size - 3,
                                      hjust = 0, face = "italic"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom", legend.title = element_blank(),
          panel.grid.minor = element_blank())
}

metric_colours <- c("Substance Removals" = "#E63946",
                     "Infant Entries" = "#457B9D",
                     "Missing/Runaway" = "#2A9D8F",
                     "In-Care Maltreatment" = "#E9C46A")

policy_lines <- tribble(~year, ~label,
  2016, "CARA\n(Jul 2016)", 2018, "FFPSA Enacted\n(Feb 2018)",
  2019, "FFPSA Prev.\nServices (Oct 2019)")

# == 1. DATA (simulated -- replace with real AFCARS/NCANDS) ===================
# Uncomment for real data:
# afcars <- read_csv("data/afcars_fy2000_2025.csv")
# ncands <- read_csv("data/ncands_fy2000_2025.csv")
# pop    <- read_csv("data/state_child_pop.csv")

states <- state.abb; years <- 2000:2025

sim <- expand_grid(state = states, year = years) %>%
  mutate(state_id = as.integer(factor(state)),
         child_pop = round(runif(n(), 1e5, 2e6)),
         infant_pop = round(child_pop * 0.06),
         t = year - 2010,
         post_cara = as.integer(year >= 2017),
         post_ffpsa = as.integer(year >= 2018),
         post_impl = as.integer(year >= 2020),
         opioid_adopt_year = if_else(state_id %% 3 == 0,
                                      2015L + (state_id %% 6), NA_integer_),
         treated_opioid = as.integer(!is.na(opioid_adopt_year) &
                                       year >= opioid_adopt_year),
         unemployment = 5 + 0.3*sin(t/3) + rnorm(n(), 0, 0.8),
         pct_minority = 20 + state_id*0.5 + rnorm(n(), 0, 1),
         poverty_rate = 12 + 0.2*t + rnorm(n(), 0, 1.5))

sim <- sim %>% mutate(
  substance_rate = pmax(0, 3 + 0.15*t + 0.8*post_cara - 0.6*post_ffpsa
    - 1.0*post_impl - 0.5*treated_opioid + 0.05*unemployment
    + rnorm(n(),0,0.4) + rnorm(length(unique(state)),0,1)[state_id]),
  infant_entry_rate = pmax(0, 5 + 0.10*t + 0.5*post_cara - 0.8*post_ffpsa
    - 1.2*post_impl - 0.3*treated_opioid + 0.04*unemployment
    + rnorm(n(),0,0.3) + rnorm(length(unique(state)),0,0.8)[state_id]),
  missing_rate = pmax(0, 0.8 - 0.01*t - 0.05*post_ffpsa - 0.10*post_impl
    + 0.01*unemployment + rnorm(n(),0,0.05)
    + rnorm(length(unique(state)),0,0.1)[state_id]),
  incare_maltreat_rate = pmax(0, 2.5 - 0.05*t - 0.15*post_ffpsa
    - 0.30*post_impl - 0.20*treated_opioid + 0.03*unemployment
    + rnorm(n(),0,0.2) + rnorm(length(unique(state)),0,0.5)[state_id]))

df_long <- sim %>%
  pivot_longer(c(substance_rate, infant_entry_rate,
                 missing_rate, incare_maltreat_rate),
               names_to = "metric_raw", values_to = "rate") %>%
  mutate(metric = recode(metric_raw,
    substance_rate = "Substance Removals", infant_entry_rate = "Infant Entries",
    missing_rate = "Missing/Runaway",
    incare_maltreat_rate = "In-Care Maltreatment"),
    metric = factor(metric, levels = names(metric_colours)))

national <- df_long %>%
  group_by(year, metric) %>%
  summarise(rate = weighted.mean(rate, child_pop, na.rm = TRUE),
            total_pop = sum(child_pop), .groups = "drop") %>%
  mutate(t = year - 2010)
# == 2. NATIONAL TREND PLOTS ===================================================

p_national <- ggplot(national, aes(year, rate, colour = metric)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.8) +
  geom_vline(data = policy_lines, aes(xintercept = year),
             linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  geom_text(data = policy_lines, aes(x = year, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.3, hjust = 0.5,
            size = 2.8, colour = "grey30") +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = metric_colours) +
  labs(title = "National Child-Welfare Outcome Trends, FY 2000-2025",
       subtitle = "Rates per 1,000 children (population-weighted state means)",
       x = "Fiscal Year", y = "Rate per 1,000",
       caption = paste("Sources: AFCARS & NCANDS (simulated). Dashed lines:",
                        "CARA, FFPSA enactment, FFPSA rollout.")) +
  theme_policy() + theme(legend.position = "none")

ggsave("output/fig1_national_trends.png", p_national, width=10, height=7, dpi=300)
htmlwidgets::saveWidget(
  ggplotly(p_national, tooltip = c("x","y","colour")) %>%
    layout(legend = list(orientation = "h", y = -0.15)),
  file = "output/fig1_national_trends.html", selfcontained = TRUE)

# State heat map
p_heat <- sim %>%
  ggplot(aes(year, reorder(state, substance_rate), fill = substance_rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", name = "Rate / 1,000") +
  geom_vline(xintercept = c(2016, 2018, 2019),
             linetype = "dashed", colour = "white", linewidth = 0.4) +
  labs(title = "Substance-Related Removal Rates by State",
       subtitle = "Darker = higher rate. Dashed: CARA / FFPSA / Implementation",
       x = "Fiscal Year", y = NULL) +
  theme_policy(base_size = 10) + theme(axis.text.y = element_text(size = 6))
ggsave("output/fig2_state_heatmap.png", p_heat, width=10, height=12, dpi=300)

# == 3. INTERRUPTED TIME SERIES ================================================
# Y_t = b0 + b1*t + b2*Post + b3*Post*(t-t*) + e; Newey-West HAC SEs

run_its <- function(data, policy_year, policy_label) {
  data <- data %>%
    mutate(post = as.integer(year >= policy_year),
           t_pre = year - min(year),
           t_post = pmax(0L, as.integer(year - policy_year)))
  map_dfr(unique(data$metric), function(m) {
    d <- filter(data, metric == m)
    mod <- lm(rate ~ t_pre + post + t_post, data = d)
    robust <- coeftest(mod, vcov = NeweyWest(mod, lag = 2, prewhite = FALSE))
    tibble(metric = m, policy = policy_label, policy_year = policy_year,
           term = rownames(robust), estimate = robust[,"Estimate"],
           std_error = robust[,"Std. Error"], t_value = robust[,"t value"],
           p_value = robust[,"Pr(>|t|)"],
           ci_lo = estimate - 1.96*std_error,
           ci_hi = estimate + 1.96*std_error)
  })
}

its_ffpsa <- run_its(national, 2018, "FFPSA Enactment (2018)")
its_impl  <- run_its(national, 2020, "FFPSA Implementation (2020)")
its_cara  <- run_its(national, 2017, "CARA (2016 -> FY2017)")
its_all   <- bind_rows(its_ffpsa, its_impl, its_cara)
its_level <- its_all %>% filter(term == "post")

p_its <- ggplot(its_level, aes(x = metric, y = estimate, colour = policy)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                  position = position_dodge(width = 0.5),
                  size = 0.8, linewidth = 0.9) +
  coord_flip() + scale_colour_brewer(palette = "Set2") +
  labs(title = "ITS Level-Shift Estimates (Post-Policy)",
       subtitle = "Point estimates +/- 95% CI, Newey-West HAC SEs",
       x = NULL, y = "Change in rate per 1,000 children",
       caption = "Negative values = post-policy reduction.") +
  theme_policy()
ggsave("output/fig3_its_coefficients.png", p_its, width=10, height=6, dpi=300)
htmlwidgets::saveWidget(
  ggplotly(p_its) %>% layout(legend = list(orientation="h", y=-0.2)),
  file = "output/fig3_its_coefficients.html", selfcontained = TRUE)
# == 4. DIFFERENCE-IN-DIFFERENCES -- TWFE ======================================

outcomes <- c("substance_rate","infant_entry_rate",
              "missing_rate","incare_maltreat_rate")
outcome_labels <- c("Substance Removals","Infant Entries",
                     "Missing/Runaway","In-Care Maltreatment")

did_models <- map(outcomes, function(y) {
  fml <- as.formula(paste0(y,
    " ~ treated_opioid + unemployment + pct_minority + poverty_rate",
    " | state + year"))
  feols(fml, data = sim, cluster = ~state)
})
names(did_models) <- outcome_labels

modelsummary(did_models,
  stars = c("*"=.1,"**"=.05,"***"=.01),
  coef_rename = c(treated_opioid = "State Opioid Initiative (DiD)",
                  unemployment = "Unemployment Rate (%)",
                  pct_minority = "% Minority Population",
                  poverty_rate = "Poverty Rate (%)"),
  gof_omit = "AIC|BIC|Log|RMSE|Std",
  title = "Two-Way Fixed-Effects DiD: State Opioid Initiatives",
  notes = c("State and year FE included.",
            "Standard errors clustered at the state level."),
  output = "output/table1_did_results.html")

modelsummary(did_models,
  stars = c("*"=.1,"**"=.05,"***"=.01),
  coef_rename = c(treated_opioid = "State Opioid Initiative (DiD)",
                  unemployment = "Unemployment Rate",
                  pct_minority = "Pct Minority", poverty_rate = "Poverty Rate"),
  gof_omit = "AIC|BIC|Log|RMSE|Std",
  output = "output/table1_did_results.tex")

did_coefs <- map_dfr(names(did_models), function(m) {
  ct <- as.data.frame(coeftable(did_models[[m]]))
  ct$term <- rownames(ct); ct$metric <- m; ct
}) %>% filter(term == "treated_opioid") %>%
  rename(estimate = Estimate, std_error = `Std. Error`,
         p_value = `Pr(>|t|)`) %>%
  mutate(ci_lo = estimate - 1.96*std_error,
         ci_hi = estimate + 1.96*std_error)

p_did <- ggplot(did_coefs, aes(x = metric, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(ymin=ci_lo, ymax=ci_hi, colour=metric),
                  size=1, linewidth=1.1) +
  coord_flip() + scale_colour_manual(values = metric_colours) +
  labs(title = "DiD Estimates: Effect of State Opioid Initiatives",
       subtitle = "TWFE with state & year FE, clustered SEs, 95% CI",
       x = NULL, y = "Estimated change in rate per 1,000",
       caption = paste("Treatment = state opioid initiative adoption.",
                        "Controls: unemployment, % minority, poverty.")) +
  theme_policy() + theme(legend.position = "none")
ggsave("output/fig4_did_coefficients.png", p_did, width=10, height=5, dpi=300)
htmlwidgets::saveWidget(ggplotly(p_did),
  file = "output/fig4_did_coefficients.html", selfcontained = TRUE)

# == 5. EVENT STUDY ============================================================

es_data <- sim %>%
  filter(!is.na(opioid_adopt_year)) %>%
  mutate(rel_year = year - opioid_adopt_year,
         rel_year_binned = case_when(rel_year < -5 ~ -5L,
                                     rel_year > 5 ~ 5L,
                                     TRUE ~ as.integer(rel_year)),
         rel_factor = relevel(factor(rel_year_binned), ref = "-1"))

es_models <- map(setNames(outcomes, outcome_labels), function(y) {
  fml <- as.formula(paste0(y,
    " ~ i(rel_factor) + unemployment + poverty_rate | state + year"))
  feols(fml, data = es_data, cluster = ~state)
})

es_coefs <- map_dfr(names(es_models), function(m) {
  ct <- as.data.frame(coeftable(es_models[[m]]))
  ct$term <- rownames(ct); ct$metric <- m; ct
}) %>% filter(str_detect(term, "rel_factor")) %>%
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+")),
         ci_lo = Estimate - 1.96 * `Std. Error`,
         ci_hi = Estimate + 1.96 * `Std. Error`,
         metric = factor(metric, levels = names(metric_colours)))

ref_rows <- tibble(
  metric = factor(names(metric_colours), levels = names(metric_colours)),
  rel_year = -1L, Estimate = 0, ci_lo = 0, ci_hi = 0)
es_plot_data <- bind_rows(es_coefs, ref_rows) %>% arrange(metric, rel_year)

p_es <- ggplot(es_plot_data, aes(rel_year, Estimate, colour = metric)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi, fill=metric),
              alpha=0.15, colour=NA) +
  geom_point(size = 2.2) + geom_line(linewidth = 0.8) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = metric_colours) +
  scale_fill_manual(values = metric_colours) +
  scale_x_continuous(breaks = -5:5) +
  labs(title = "Event-Study: Dynamic Effects of State Opioid Initiatives",
       subtitle = "Coefficients relative to k=-1, 95% CI",
       x = "Years Relative to Policy Adoption",
       y = "Estimated Effect (rate / 1,000)",
       caption = paste("State & year FE. Clustered SEs.",
                        "Flat pre-trends support parallel-trends.")) +
  theme_policy() + theme(legend.position = "none")
ggsave("output/fig5_event_study.png", p_es, width=10, height=7, dpi=300)
htmlwidgets::saveWidget(ggplotly(p_es),
  file = "output/fig5_event_study.html", selfcontained = TRUE)
# == 6. BEFORE / AFTER DISTRIBUTIONS ===========================================

ba_data <- df_long %>%
  filter(year %in% c(2017:2019, 2020:2022)) %>%
  mutate(period = if_else(year < 2020, "Pre-FFPSA (2017-19)",
                          "Post-FFPSA (2020-22)"))

p_ba <- ggplot(ba_data, aes(rate, fill = period)) +
  geom_density(alpha = 0.55, colour = NA) +
  facet_wrap(~ metric, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Pre-FFPSA (2017-19)" = "#264653",
                                "Post-FFPSA (2020-22)" = "#E76F51")) +
  labs(title = "Distribution Shift: Pre vs. Post FFPSA Implementation",
       subtitle = "State-level rate densities, 3 years before & after FY 2020",
       x = "Rate per 1,000", y = "Density",
       caption = "Leftward shifts indicate post-policy reductions.") +
  theme_policy()
ggsave("output/fig6_before_after.png", p_ba, width=10, height=7, dpi=300)
htmlwidgets::saveWidget(ggplotly(p_ba),
  file = "output/fig6_before_after.html", selfcontained = TRUE)

# == 7. FFPSA-SPECIFIC MODELS =================================================
# State FE + quadratic trend (avoids year-FE collinearity with uniform timing)

ffpsa_models <- map(setNames(outcomes, outcome_labels), function(y) {
  fml <- as.formula(paste0(y,
    " ~ post_ffpsa + post_impl + t + I(t^2)",
    " + unemployment + pct_minority + poverty_rate | state"))
  feols(fml, data = sim, cluster = ~state)
})

modelsummary(ffpsa_models,
  stars = c("*"=.1,"**"=.05,"***"=.01),
  coef_rename = c(post_ffpsa = "Post-FFPSA (>= 2018)",
                  post_impl = "Post-Implementation (>= 2020)",
                  t = "Linear Trend", `I(t^2)` = "Quadratic Trend",
                  unemployment = "Unemployment (%)",
                  pct_minority = "% Minority", poverty_rate = "Poverty (%)"),
  gof_omit = "AIC|BIC|Log|RMSE|Std",
  title = "FFPSA Policy Effects: State FE with Time Trends",
  notes = "SEs clustered at the state level.",
  output = "output/table2_ffpsa_results.html")

# == 8. EFFECT-SIZE SUMMARY WITH LEGISLATIVE CAPTIONS ==========================

pct_change <- function(est, metric_name, data, policy_yr) {
  pre_mean <- data %>% filter(metric == metric_name, year < policy_yr) %>%
    pull(rate) %>% mean(na.rm = TRUE)
  round(100 * est / pre_mean, 1)
}

effect_table_its <- its_level %>% rowwise() %>%
  mutate(pct_chg = pct_change(estimate, metric, national, policy_year),
         caption = glue(
           "{policy} associated with {abs(pct_chg)}% ",
           "{ifelse(pct_chg<0,'reduction','increase')} in ",
           "{tolower(metric)} -- real-time dashboards would ",
           "accelerate similar ",
           "{ifelse(pct_chg<0,'successes','responses')}.")) %>%
  ungroup() %>%
  select(metric, policy, estimate, std_error, ci_lo, ci_hi,
         p_value, pct_chg, caption)

effect_table_did <- did_coefs %>% rowwise() %>%
  mutate(pre_mean = national %>%
           filter(metric == !!metric, year < 2017) %>%
           pull(rate) %>% mean(na.rm = TRUE),
         pct_chg = round(100 * estimate / pre_mean, 1),
         policy = "State Opioid Initiative (DiD)",
         caption = glue(
           "State opioid initiatives associated with ",
           "{abs(pct_chg)}% ",
           "{ifelse(pct_chg<0,'reduction','increase')} in ",
           "{tolower(metric)} -- coordinated federal-state ",
           "action amplifies impact.")) %>%
  ungroup() %>%
  select(metric, policy, estimate, std_error, ci_lo, ci_hi,
         p_value, pct_chg, caption)

effect_sizes <- bind_rows(effect_table_its, effect_table_did) %>%
  arrange(metric, policy)
write_csv(effect_sizes, "output/effect_sizes_with_captions.csv")

cat("\n=== SAMPLE LEGISLATIVE CAPTIONS ===\n\n")
walk(head(effect_sizes$caption, 6), function(x) cat("*", x, "\n\n"))

# == 9. ROBUSTNESS =============================================================

# 9a. Placebo test (policy shifted 3 years early)
placebo_its <- run_its(national, 2015, "Placebo (2015)")
placebo_level <- placebo_its %>% filter(term == "post")
cat("\n-- Placebo test (2015) -- expect non-significant:\n")
print(placebo_level %>% select(metric, estimate, p_value))

# 9b. Leave-one-state-out jackknife
jackknife <- map_dfr(states, function(s) {
  d <- sim %>% filter(state != s)
  mod <- feols(substance_rate ~ treated_opioid + unemployment +
                 poverty_rate | state + year,
               data = d, cluster = ~state)
  tibble(dropped_state = s,
         estimate = coef(mod)["treated_opioid"],
         se = sqrt(vcov(mod)["treated_opioid","treated_opioid"]))
})

full_est <- did_coefs$estimate[did_coefs$metric == "Substance Removals"]

p_jk <- ggplot(jackknife, aes(reorder(dropped_state, estimate), estimate)) +
  geom_hline(yintercept = full_est, colour = "#E63946", linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se),
                  size = 0.4, colour = "grey30") +
  coord_flip() +
  labs(title = "Jackknife Sensitivity: Substance-Removal DiD",
       subtitle = "Each point omits one state. Red = full-sample estimate",
       x = "Dropped State", y = "DiD Estimate") +
  theme_policy(base_size = 10)
ggsave("output/fig7_jackknife.png", p_jk, width=8, height=10, dpi=300)

# == 10. WRAP-UP ===============================================================
cat("\n=== ALL OUTPUTS -> ./output/ ===\n")
cat("  fig1-fig7 (.png + .html)\n")
cat("  table1_did, table2_ffpsa (.html/.tex)\n")
cat("  effect_sizes_with_captions.csv\n\n")
sessionInfo()
