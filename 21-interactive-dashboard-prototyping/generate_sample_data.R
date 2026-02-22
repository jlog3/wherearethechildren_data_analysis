#!/usr/bin/env Rscript
# =============================================================================
# generate_sample_data.R
# Child Welfare Transparency Dashboard — Simulated Data Generator
# =============================================================================
# Run this ONCE before launching the dashboard.
# Outputs 10 RDS files to ./data/ that mirror cleaned AFCARS/NCANDS structure.
# To connect real data: replace this script with your ETL pipeline and ensure
# output schemas match (see column comments below).
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)

set.seed(2025)

# --- Reference tables --------------------------------------------------------

states <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
  "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC"
)

state_names <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
  "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
  "Maine","Maryland","Massachusetts","Michigan","Minnesota",
  "Mississippi","Missouri","Montana","Nebraska","Nevada",
  "New Hampshire","New Jersey","New Mexico","New York",
  "North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
  "Pennsylvania","Rhode Island","South Carolina","South Dakota",
  "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West Virginia","Wisconsin","Wyoming","District of Columbia"
)

state_lookup <- setNames(state_names, states)

# FIPS codes for choropleth joins
state_fips <- c(
  AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",
  FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",KS="20",
  KY="21",LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",MS="28",
  MO="29",MT="30",NE="31",NV="32",NH="33",NJ="34",NM="35",NY="36",
  NC="37",ND="38",OH="39",OK="40",OR="41",PA="42",RI="44",SC="45",
  SD="46",TN="47",TX="48",UT="49",VT="50",VA="51",WA="53",WV="54",
  WI="55",WY="56",DC="11"
)

years       <- 2018:2025
race_groups <- c("White","Black","Hispanic","AIAN","Asian/PI","Multiracial","Unknown")
age_groups  <- c("< 1","1-5","6-12","13-17")
placements  <- c(
  "Foster Home (Relative)","Foster Home (Non-Relative)",
  "Group Home","Institution","Trial Home Visit",
  "Pre-Adoptive Home","Supervised Independent Living"
)

# Rough child population proportional to real state sizes
state_pop_base <- setNames(round(runif(length(states), 80000, 2000000)), states)
state_pop_base["CA"] <- 8800000; state_pop_base["TX"] <- 7200000
state_pop_base["NY"] <- 4100000; state_pop_base["FL"] <- 4400000
state_pop_base["IL"] <- 2900000; state_pop_base["PA"] <- 2700000
state_pop_base["OH"] <- 2600000; state_pop_base["WY"] <- 120000

# --- 1. State-year population denominators -----------------------------------

pop_df <- expand.grid(state = states, year = years, stringsAsFactors = FALSE) %>%
  mutate(
    state_name = state_lookup[state],
    fips       = state_fips[state],
    child_pop  = round(state_pop_base[state] * (1 + 0.005 * (year - 2018))),
    pop_under1 = round(child_pop * 0.055),
    pop_1to5   = round(child_pop * 0.28),
    pop_6to12  = round(child_pop * 0.39),
    pop_13to17 = round(child_pop * 0.275)
  ) %>%
  as_tibble()

# --- 2. Episode generator helper ---------------------------------------------

generate_episodes <- function(n, metric_label) {
  tibble(
    episode_id  = paste0(substr(metric_label, 1, 3), "_", sprintf("%06d", 1:n)),
    metric      = metric_label,
    state       = sample(states, n, replace = TRUE,
                         prob = state_pop_base[states] / sum(state_pop_base)),
    report_date = as.Date("2018-01-01") +
      sample(0:as.numeric(difftime(as.Date("2025-12-31"),
                                    as.Date("2018-01-01"), units="days")),
             n, replace = TRUE),
    age_group   = sample(age_groups, n, replace = TRUE, prob = c(.12,.35,.30,.23)),
    race        = sample(race_groups, n, replace = TRUE,
                         prob = c(.38,.22,.22,.03,.04,.08,.03)),
    placement   = sample(placements, n, replace = TRUE,
                         prob = c(.25,.30,.10,.08,.12,.10,.05)),
    sex         = sample(c("Male","Female"), n, replace = TRUE),
    duration_days = pmax(1, round(rnorm(n, 180, 120)))
  ) %>%
    mutate(
      year       = year(report_date),
      quarter    = quarter(report_date),
      month      = month(report_date),
      state_name = state_lookup[state],
      fips       = state_fips[state]
    ) %>%
    filter(year %in% years)
}

# --- 3. Substance-related removals -------------------------------------------

substance_raw <- generate_episodes(42000, "Substance Removal") %>%
  mutate(
    substance_flag = TRUE,
    substance_type = sample(c("Opioid","Methamphetamine","Alcohol",
                               "Cocaine","Polysubstance","Other"),
                            n(), replace = TRUE,
                            prob = c(.35,.20,.18,.10,.12,.05)),
    opioid_flag    = substance_type == "Opioid"
  )

substance_agg <- substance_raw %>%
  group_by(state, state_name, fips, year) %>%
  summarise(n_substance = n(), n_opioid = sum(opioid_flag),
            pct_opioid  = round(n_opioid / n_substance * 100, 1),
            .groups = "drop") %>%
  left_join(pop_df %>% select(state, year, child_pop), by = c("state","year")) %>%
  mutate(rate_per_100k = round(n_substance / child_pop * 1e5, 1))

# --- 4. Infant entries -------------------------------------------------------

infant_raw <- generate_episodes(18000, "Infant Entry") %>%
  mutate(
    age_group         = "< 1",
    neonatal_flag     = runif(n()) < 0.35,
    substance_exposed = runif(n()) < 0.28,
    plan_at_entry     = sample(c("Reunification","Adoption","Guardianship","Other"),
                               n(), replace = TRUE, prob = c(.55,.20,.15,.10))
  )

infant_agg <- infant_raw %>%
  group_by(state, state_name, fips, year) %>%
  summarise(n_infant = n(), n_neonatal = sum(neonatal_flag),
            n_substance_exp = sum(substance_exposed), .groups = "drop") %>%
  left_join(pop_df %>% select(state, year, pop_under1), by = c("state","year")) %>%
  mutate(rate_per_1000 = round(n_infant / pop_under1 * 1000, 1))

# --- 5. Missing / runaway episodes -------------------------------------------

missing_raw <- generate_episodes(12000, "Missing Episode") %>%
  mutate(
    age_group        = sample(age_groups, n(), replace = TRUE, prob = c(.02,.05,.28,.65)),
    missing_type     = sample(c("Runaway","Abducted","Unknown"),
                              n(), replace = TRUE, prob = c(.78,.05,.17)),
    days_missing     = pmax(1, round(rexp(n(), rate = 0.08))),
    trafficking_flag = runif(n()) < 0.06,
    resolved         = runif(n()) < 0.92
  )

missing_agg <- missing_raw %>%
  group_by(state, state_name, fips, year) %>%
  summarise(n_episodes = n(), n_runaway = sum(missing_type == "Runaway"),
            n_trafficking = sum(trafficking_flag),
            median_days = median(days_missing),
            pct_resolved = round(mean(resolved)*100, 1), .groups = "drop") %>%
  left_join(pop_df %>% select(state, year, child_pop), by = c("state","year")) %>%
  mutate(rate_per_100k = round(n_episodes / child_pop * 1e5, 1))

# --- 6. In-care maltreatment -------------------------------------------------

maltreat_raw <- generate_episodes(15000, "In-Care Maltreatment") %>%
  mutate(
    maltreatment_type = sample(c("Neglect","Physical Abuse","Sexual Abuse",
                                  "Emotional Abuse","Medical Neglect"),
                               n(), replace = TRUE, prob = c(.45,.25,.12,.10,.08)),
    perpetrator = sample(c("Foster Parent","Facility Staff","Other Child",
                            "Biological Parent (visit)","Unknown"),
                         n(), replace = TRUE, prob = c(.40,.20,.10,.15,.15)),
    substantiated = runif(n()) < 0.55,
    severity      = sample(c("Low","Moderate","Severe"), n(),
                           replace = TRUE, prob = c(.35,.45,.20))
  )

maltreat_agg <- maltreat_raw %>%
  group_by(state, state_name, fips, year) %>%
  summarise(n_incidents = n(), n_substantiated = sum(substantiated),
            pct_substantiated = round(mean(substantiated)*100, 1),
            n_severe = sum(severity == "Severe"), .groups = "drop") %>%
  left_join(pop_df %>% select(state, year, child_pop), by = c("state","year")) %>%
  mutate(rate_per_100k = round(n_incidents / child_pop * 1e5, 1))

# --- 7. Combined episode-level -----------------------------------------------

common_cols <- c("episode_id","metric","state","state_name","fips","report_date",
                 "year","month","quarter","age_group","race","placement",
                 "sex","duration_days")

episode_all <- bind_rows(
  substance_raw %>% select(all_of(common_cols)),
  infant_raw    %>% select(all_of(common_cols)),
  missing_raw   %>% select(all_of(common_cols)),
  maltreat_raw  %>% select(all_of(common_cols))
)

# --- 8. Write to disk --------------------------------------------------------

dir.create("data", showWarnings = FALSE)

saveRDS(pop_df,         "data/state_year_population.rds")
saveRDS(substance_agg,  "data/substance_removals.rds")
saveRDS(substance_raw,  "data/substance_removals_episodes.rds")
saveRDS(infant_agg,     "data/infant_entries.rds")
saveRDS(infant_raw,     "data/infant_entries_episodes.rds")
saveRDS(missing_agg,    "data/missing_episodes.rds")
saveRDS(missing_raw,    "data/missing_episodes_detail.rds")
saveRDS(maltreat_agg,   "data/incare_maltreatment.rds")
saveRDS(maltreat_raw,   "data/incare_maltreatment_episodes.rds")
saveRDS(episode_all,    "data/episode_level.rds")

cat("\n=== Data Generation Complete ===\n")
cat("10 RDS files written to ./data/\n\n")
for (f in sort(list.files("data", pattern = "\\.rds$"))) {
  cat(sprintf("  %-42s %s bytes\n", f, format(file.size(file.path("data",f)),
              big.mark=",")))
}
