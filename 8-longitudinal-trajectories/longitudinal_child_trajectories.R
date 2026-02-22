###############################################################################
#  LONGITUDINAL CHILD-WELFARE TRAJECTORY ANALYSIS
#  Linked AFCARS / NCANDS files  —  Survival, Multi-Spell, Sankey, & GBTM
#
#  Purpose:
#    Build individual longitudinal histories by linking AFCARS Foster Care
#    (FC) and NCANDS Child files across federal fiscal years through the
#    stable identifiers StFCID (AFCARS) and RECNUMBR / AFCARSID (NCANDS).
#    Then run:
#      1. Survival analysis   – time-to-missing, time-to-re-maltreatment
#      2. Multi-spell analysis – recurrent removal episodes (TotalRem)
#      3. Sankey / alluvial pathway visualisations
#      4. Group-based trajectory modelling (GBTM)
#      5. Cumulative-risk dashboards for a legislative audience
#
#  Data assumptions (see §0 below):
#    - One CSV per FFY for AFCARS-FC  (e.g., fc2018.csv … fc2023.csv)
#    - One CSV per FFY for NCANDS-Child (e.g., ncands2018.csv … ncands2023.csv)
#    - Column names follow NDACAN codebooks (case-insensitive mapping below)
#    - All person-level IDs are already de-identified by NDACAN
#
#  Author:  Analytical template – adapt column names to your extract
#  Date:    2025
###############################################################################

# ═══════════════════════════════════════════════════════════════════════════════
# §0.  SETUP & CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

## 0-A.  Packages -----------------------------------------------------------
required_pkgs <- c(
  # data wrangling

"data.table", "dplyr", "tidyr", "purrr", "lubridate", "stringr", "janitor",
  # survival analysis
"survival", "survminer", "flexsurv", "frailtypack",
  # multi-spell / recurrent events
"reReg",
  # group-based trajectory modelling
"crimCV", "lcmm",
  # visualisation
"ggplot2", "ggalluvial", "networkD3", "patchwork", "scales", "viridis",
"RColorBrewer", "kableExtra",
  # reporting helpers
"htmlwidgets"
)

install_if_missing <- function(pkgs) {
  new <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
}
install_if_missing(required_pkgs)

invisible(lapply(required_pkgs, library, character.only = TRUE))

cat("All packages loaded.\n")

## 0-B.  Paths & parameters -------------------------------------------------
# *** EDIT THESE to point at your data ***
AFCARS_DIR   <- "data/afcars"
NCANDS_DIR   <- "data/ncands"
OUTPUT_DIR   <- "output"
YEARS        <- 2018:2023            # FFYs to stack

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Helper: flexible date parser for AFCARS / NCANDS date columns
parse_date_flex <- function(x) {
  x <- trimws(as.character(x))
  out <- suppressWarnings(mdy(x))                # MM/DD/YYYY common

  still_na <- is.na(out)
  out[still_na] <- suppressWarnings(ymd(x[still_na]))
  out
}

cat("Configuration complete.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §1.  DATA INGESTION & LONGITUDINAL LINKAGE
# ═══════════════════════════════════════════════════════════════════════════════

## 1-A.  Read & stack AFCARS-FC files ----------------------------------------
read_afcars <- function(yr) {
  fp <- list.files(AFCARS_DIR, pattern = as.character(yr), full.names = TRUE)
  if (length(fp) == 0) { message("No AFCARS file for ", yr); return(NULL) }
  dt <- fread(fp[1], colClasses = "character")
  dt <- clean_names(dt)                           # janitor → snake_case
  dt[, ffy := yr]
  dt
}

afcars_raw <- rbindlist(lapply(YEARS, read_afcars), fill = TRUE)

# Standardise the linking ID — NDACAN uses StFCID (or stfcid after clean)
id_col_af <- intersect(names(afcars_raw),
                        c("stfcid", "st_fcid", "stfcid_1"))
if (length(id_col_af) == 0) stop("Cannot find StFCID in AFCARS columns.")
setnames(afcars_raw, id_col_af[1], "child_id")

cat("AFCARS rows stacked:", nrow(afcars_raw), "\n")

## 1-B.  Read & stack NCANDS-Child files -------------------------------------
read_ncands <- function(yr) {
  fp <- list.files(NCANDS_DIR, pattern = as.character(yr), full.names = TRUE)
  if (length(fp) == 0) { message("No NCANDS file for ", yr); return(NULL) }
  dt <- fread(fp[1], colClasses = "character")
  dt <- clean_names(dt)
  dt[, ffy := yr]
  dt
}

ncands_raw <- rbindlist(lapply(YEARS, read_ncands), fill = TRUE)

# NCANDS link field: AFCARSID maps to StFCID; fallback to RECNUMBR
id_col_nc <- intersect(names(ncands_raw),
                        c("afcarsid", "stfcid", "recnumbr"))
if (length(id_col_nc) == 0) stop("Cannot find linkage ID in NCANDS columns.")
setnames(ncands_raw, id_col_nc[1], "child_id")

cat("NCANDS rows stacked:", nrow(ncands_raw), "\n")

## 1-C.  Parse key dates & derive durations ----------------------------------
date_cols_af <- c("dob", "latrem", "dlstfce", "cursetdt",
                  "remdt",  "disdt")
for (col in intersect(date_cols_af, names(afcars_raw))) {
  set(afcars_raw, j = col, value = parse_date_flex(afcars_raw[[col]]))
}

date_cols_nc <- c("rptdt", "subdt", "invdate")
for (col in intersect(date_cols_nc, names(ncands_raw))) {
  set(ncands_raw, j = col, value = parse_date_flex(ncands_raw[[col]]))
}

# Age at latest removal (months)
if (all(c("dob", "latrem") %in% names(afcars_raw))) {
  afcars_raw[, age_at_entry_mo := as.numeric(
    difftime(latrem, dob, units = "days")) / 30.44]
}

# Duration of current episode (days)
if (all(c("latrem", "dlstfce") %in% names(afcars_raw))) {
  afcars_raw[, episode_days := as.numeric(
    difftime(dlstfce, latrem, units = "days"))]
}

## 1-D.  Build unified child-level panel ------------------------------------
#  Each row = one child-year observation with AFCARS placement info
#  merged to any NCANDS maltreatment reports in the same FFY.

ncands_summary <- ncands_raw[, .(
  n_reports    = .N,
  any_subst    = as.integer(any(as.integer(chmal1) == 1  |
                                 as.integer(chmal2) == 1  |
                                 as.integer(chmal3) == 1  |
                                 as.integer(chmal4) == 1, na.rm = TRUE)),
  rpt_date_min = suppressWarnings(min(rptdt, na.rm = TRUE)),
  rpt_date_max = suppressWarnings(max(rptdt, na.rm = TRUE))
), by = .(child_id, ffy)]

panel <- merge(afcars_raw, ncands_summary,
               by = c("child_id", "ffy"), all.x = TRUE)
panel[is.na(n_reports), n_reports := 0L]
panel[is.na(any_subst), any_subst := 0L]

cat("Panel rows (child × year):", nrow(panel), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §2.  SURVIVAL ANALYSIS — TIME-TO-EVENT
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §2  SURVIVAL ANALYSIS ══════\n")

## 2-A.  Prepare analytic dataset -------------------------------------------
#  Endpoint A: "missing from care" — AFCARS discharge reason (DISREASN == 14)
#  Endpoint B: "re-maltreatment"   — any substantiated NCANDS report after entry

surv_data <- panel[!is.na(latrem)][order(child_id, ffy)]

# Keep one record per child (latest observation = last FFY seen)
surv_child <- surv_data[, .SD[.N], by = child_id]

# --- Endpoint A: time to runaway / missing (DISREASN == 14 in codebook) ---
if ("disreasn" %in% names(surv_child)) {
  surv_child[, event_missing := fifelse(
    as.integer(disreasn) == 14, 1L, 0L, na = 0L)]
} else {
  surv_child[, event_missing := 0L]
  message("DISREASN column not found; defaulting event_missing = 0.")
}

# Time variable: episode_days or days from latest removal to last-known date
surv_child[, time_days := fifelse(
  !is.na(episode_days) & episode_days > 0,
  episode_days,
  as.numeric(difftime(max(dlstfce, na.rm = TRUE), latrem, units = "days"))),
  by = child_id]

# Censor at 0.5 day to avoid zero-length intervals
surv_child[time_days <= 0, time_days := 0.5]

# --- Endpoint B: re-maltreatment after entry ---
surv_child[, event_remal := fifelse(any_subst == 1L, 1L, 0L)]

# Time for re-maltreatment: earliest substantiated report date minus entry
if ("rpt_date_min" %in% names(surv_child)) {
  surv_child[, time_remal_days := as.numeric(
    difftime(rpt_date_min, latrem, units = "days"))]
  surv_child[time_remal_days <= 0 | is.na(time_remal_days),
             time_remal_days := time_days]
} else {
  surv_child[, time_remal_days := time_days]
}

cat("Survival dataset N:", nrow(surv_child), "\n")
cat("  Events – missing:", sum(surv_child$event_missing),
    " | re-maltreatment:", sum(surv_child$event_remal), "\n")

## 2-B.  Kaplan-Meier: time-to-missing -------------------------------------
surv_obj_miss <- Surv(
  time  = surv_child$time_days,
  event = surv_child$event_missing
)

km_miss <- survfit(surv_obj_miss ~ 1)

p_km_miss <- ggsurvplot(
  km_miss,
  data         = surv_child,
  risk.table   = TRUE,
  conf.int     = TRUE,
  palette      = "#2C7BB6",
  title        = "Time-to-Missing from Care (Kaplan-Meier)",
  xlab         = "Days since latest removal",
  ylab         = "Survival probability (not missing)",
  ggtheme      = theme_minimal(base_size = 13),
  risk.table.height = 0.25,
  break.time.by     = 90
)

ggsave(file.path(OUTPUT_DIR, "km_time_to_missing.png"),
       print(p_km_miss), width = 10, height = 7, dpi = 300)
cat("Saved: km_time_to_missing.png\n")

## 2-C.  Kaplan-Meier: time-to-re-maltreatment -----------------------------
surv_obj_remal <- Surv(
  time  = surv_child$time_remal_days,
  event = surv_child$event_remal
)

km_remal <- survfit(surv_obj_remal ~ 1)

p_km_remal <- ggsurvplot(
  km_remal,
  data         = surv_child,
  risk.table   = TRUE,
  conf.int     = TRUE,
  palette      = "#D7191C",
  title        = "Time-to-Re-Maltreatment (Kaplan-Meier)",
  xlab         = "Days since latest removal",
  ylab         = "Survival probability (no re-maltreatment)",
  ggtheme      = theme_minimal(base_size = 13),
  risk.table.height = 0.25,
  break.time.by     = 90
)

ggsave(file.path(OUTPUT_DIR, "km_time_to_remaltreatment.png"),
       print(p_km_remal), width = 10, height = 7, dpi = 300)
cat("Saved: km_time_to_remaltreatment.png\n")

## 2-D.  Stratified KM by age group & placement type ------------------------
surv_child[, age_group := fcase(
  age_at_entry_mo <   12, "<1 yr",
  age_at_entry_mo <   72, "1-5 yr",
  age_at_entry_mo <  144, "6-11 yr",
  age_at_entry_mo <  216, "12-17 yr",
  default = "Unknown"
)]

# Map AFCARS CURPLSET codes to labels
placement_map <- c(
  "1" = "Pre-Adoptive", "2" = "Foster (Relative)",
  "3" = "Foster (Non-Relative)", "4" = "Group Home",
  "5" = "Institution",  "6" = "Supervised IL",
  "7" = "Runaway",      "8" = "Trial Home Visit"
)
if ("curplset" %in% names(surv_child)) {
  surv_child[, placement_label := fifelse(
    as.character(curplset) %in% names(placement_map),
    placement_map[as.character(curplset)],
    "Other / Unknown")]
} else {
  surv_child[, placement_label := "Unknown"]
}

# Stratified KM — by age group
km_age <- survfit(surv_obj_miss ~ age_group, data = surv_child)
p_km_age <- ggsurvplot(
  km_age, data = surv_child,
  conf.int = FALSE, palette = "Set2",
  title = "Time-to-Missing by Age Group",
  xlab = "Days", ylab = "P(not missing)",
  legend.title = "Age at Entry",
  ggtheme = theme_minimal(base_size = 12),
  break.time.by = 90
)
ggsave(file.path(OUTPUT_DIR, "km_missing_by_age.png"),
       print(p_km_age), width = 10, height = 6, dpi = 300)

# Stratified KM — by placement type
km_place <- survfit(surv_obj_miss ~ placement_label, data = surv_child)
p_km_place <- ggsurvplot(
  km_place, data = surv_child,
  conf.int = FALSE, palette = "Dark2",
  title = "Time-to-Missing by Placement Type",
  xlab = "Days", ylab = "P(not missing)",
  legend.title = "Placement",
  ggtheme = theme_minimal(base_size = 12),
  break.time.by = 90
)
ggsave(file.path(OUTPUT_DIR, "km_missing_by_placement.png"),
       print(p_km_place), width = 11, height = 6, dpi = 300)

cat("Stratified KM plots saved.\n")

## 2-E.  Cox proportional-hazards model -------------------------------------
# Covariates: age group, placement, number of prior removals, race/ethnicity
if ("totalrem" %in% names(surv_child)) {
  surv_child[, total_removals := as.integer(totalrem)]
} else {
  surv_child[, total_removals := 1L]
}

cox_miss <- coxph(
  Surv(time_days, event_missing) ~
    age_group + placement_label + total_removals,
  data = surv_child
)

sink(file.path(OUTPUT_DIR, "cox_model_missing_summary.txt"))
cat("═══ Cox PH — Time-to-Missing ═══\n\n")
print(summary(cox_miss))
cat("\n\nSchoenfeld test for PH assumption:\n")
print(cox.zph(cox_miss))
sink()

cat("Cox model summary saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §3.  MULTI-SPELL (RECURRENT EPISODE) ANALYSIS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §3  MULTI-SPELL ANALYSIS ══════\n")

## 3-A.  Build episode-level dataset ----------------------------------------
# Each row in AFCARS can represent an episode; TOTALREM tells us the spell #.

episode_data <- surv_data[, .(
  child_id, ffy, latrem, dlstfce, episode_days,
  totalrem  = as.integer(totalrem),
  disreasn  = as.integer(disreasn),
  any_subst,
  age_at_entry_mo,
  placement_label = fifelse(
    "curplset" %in% names(.SD) & as.character(curplset) %in% names(placement_map),
    placement_map[as.character(curplset)], "Other"),
  age_group
)]

# Deduplicate: one row per child × spell
episode_data <- unique(episode_data, by = c("child_id", "totalrem", "ffy"))

# Gap time between spells
episode_data <- episode_data[order(child_id, totalrem, ffy)]
episode_data[, prev_exit := shift(dlstfce, 1L), by = child_id]
episode_data[, gap_days := as.numeric(
  difftime(latrem, prev_exit, units = "days")), by = child_id]

# Spell-count summary
spell_counts <- episode_data[, .(max_spells = max(totalrem, na.rm = TRUE)),
                              by = child_id]

cat("Children with >1 removal:",
    sum(spell_counts$max_spells > 1, na.rm = TRUE), "\n")

## 3-B.  Recurrent-event survival (Andersen-Gill) ---------------------------
# Each spell is a row with (start, stop, event) in calendar time

episode_data[, start_time := as.numeric(
  difftime(latrem, min(latrem, na.rm = TRUE), units = "days")),
  by = child_id]
episode_data[, stop_time := start_time + fifelse(
  !is.na(episode_days) & episode_days > 0, episode_days, 0.5)]
episode_data[, event_exit := fifelse(!is.na(disreasn) & disreasn > 0, 1L, 0L)]

ag_model <- tryCatch(
  coxph(
    Surv(start_time, stop_time, event_exit) ~
      age_group + cluster(child_id),
    data = episode_data
  ),
  error = function(e) { message("AG model error: ", e$message); NULL }
)

if (!is.null(ag_model)) {
  sink(file.path(OUTPUT_DIR, "andersen_gill_model.txt"))
  cat("═══ Andersen-Gill Recurrent-Event Model ═══\n\n")
  print(summary(ag_model))
  sink()
  cat("Andersen-Gill model saved.\n")
}

## 3-C.  Re-entry rate plot --------------------------------------------------
reentry <- spell_counts[, .N, by = max_spells][order(max_spells)]
setnames(reentry, c("n_spells", "n_children"))

p_reentry <- ggplot(reentry, aes(x = factor(n_spells), y = n_children)) +
  geom_col(fill = "#4575B4", width = 0.65) +
  geom_text(aes(label = scales::comma(n_children)), vjust = -0.4, size = 3.8) +
  labs(
    title    = "Distribution of Removal Spells per Child",
    subtitle = "Based on AFCARS TotalRem across linked years",
    x = "Number of removal spells", y = "Children"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13)

ggsave(file.path(OUTPUT_DIR, "reentry_spell_distribution.png"),
       p_reentry, width = 8, height = 5, dpi = 300)
cat("Re-entry distribution plot saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §4.  PATHWAY VISUALISATION — SANKEY / ALLUVIAL
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §4  SANKEY PATHWAYS ══════\n")

## 4-A.  Build pathway data (entry reason → placement → event → exit) -------

# Map removal reason (AFCARS REM1)
removal_map <- c(
  "1"  = "Physical Abuse",  "2"  = "Sexual Abuse",
  "3"  = "Neglect",         "4"  = "Parent Alcohol",
  "5"  = "Parent Drug",     "6"  = "Child Alcohol/Drug",
  "7"  = "Child Disability","8"  = "Child Behavior",
  "9"  = "Parent Death",    "10" = "Parent Incarceration",
  "11" = "Caretaker Inability", "12" = "Relinquishment",
  "13" = "Inadequate Housing",  "14" = "Abandonment",
  "15" = "Other"
)

# Map discharge reason
discharge_map <- c(
  "1"  = "Reunification",  "2"  = "Living w/ Relative",
  "3"  = "Adoption",       "4"  = "Emancipation",
  "5"  = "Guardianship",   "6"  = "Transfer",
  "7"  = "Runaway",        "8"  = "Death of Child",
  "14" = "Missing",        "99" = "Still in Care"
)

# Derive labels
if ("rem1" %in% names(panel)) {
  panel[, entry_reason := fifelse(
    as.character(rem1) %in% names(removal_map),
    removal_map[as.character(rem1)], "Other")]
} else {
  panel[, entry_reason := "Unknown"]
}

if ("curplset" %in% names(panel)) {
  panel[, placement := fifelse(
    as.character(curplset) %in% names(placement_map),
    placement_map[as.character(curplset)], "Other")]
} else {
  panel[, placement := "Unknown"]
}

# In-care event: whether child had maltreatment report during placement
panel[, in_care_event := fifelse(any_subst == 1, "Re-Maltreatment", "No Event")]

if ("disreasn" %in% names(panel)) {
  panel[, exit_type := fifelse(
    as.character(disreasn) %in% names(discharge_map),
    discharge_map[as.character(disreasn)], "Still in Care")]
} else {
  panel[, exit_type := "Still in Care"]
}

# Collapse small categories for readability
collapse_rare <- function(x, min_pct = 0.02) {
  tbl <- prop.table(table(x))
  rare <- names(tbl[tbl < min_pct])
  ifelse(x %in% rare, "Other", x)
}

pathway_df <- panel[, .(
  entry_reason  = collapse_rare(entry_reason),
  placement     = collapse_rare(placement),
  in_care_event,
  exit_type     = collapse_rare(exit_type)
)]

## 4-B.  ggalluvial — static Sankey-style plot ------------------------------
alluvial_agg <- pathway_df %>%
  count(entry_reason, placement, in_care_event, exit_type) %>%
  filter(n >= 5)     # suppress small cells

p_alluvial <- ggplot(alluvial_agg,
  aes(axis1 = entry_reason, axis2 = placement,
      axis3 = in_care_event, axis4 = exit_type, y = n)) +
  geom_alluvium(aes(fill = entry_reason), width = 1/5, alpha = 0.65) +
  geom_stratum(width = 1/5, fill = "grey90", color = "grey40") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 2.8) +
  scale_x_discrete(
    limits = c("Entry Reason", "Placement", "In-Care Event", "Exit"),
    expand = c(0.15, 0.05)
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "Child-Welfare Pathway: Entry → Placement → Event → Exit",
    subtitle = "Linked AFCARS/NCANDS longitudinal records",
    y = "Number of children"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(file.path(OUTPUT_DIR, "sankey_alluvial_pathway.png"),
       p_alluvial, width = 14, height = 9, dpi = 300)
cat("Alluvial pathway plot saved.\n")

## 4-C.  networkD3 interactive Sankey (HTML) --------------------------------
# Build node/link lists for a 4-stage Sankey

build_sankey <- function(df) {
  stages <- c("entry_reason", "placement", "in_care_event", "exit_type")

  # Create unique node labels prefixed by stage number to avoid collisions
  nodes_list <- character()
  for (i in seq_along(stages)) {
    vals <- unique(df[[stages[i]]])
    nodes_list <- c(nodes_list, paste0("S", i, ": ", vals))
  }
  nodes <- data.frame(name = nodes_list, stringsAsFactors = FALSE)

  # Links between consecutive stages
  links <- data.frame()
  for (i in 1:(length(stages) - 1)) {
    agg <- df %>%
      count(from = .data[[stages[i]]], to = .data[[stages[i + 1]]])
    agg$from <- paste0("S", i, ": ", agg$from)
    agg$to   <- paste0("S", i + 1, ": ", agg$to)
    links <- bind_rows(links, agg)
  }

  # Map to 0-based indices
  links$source <- match(links$from, nodes$name) - 1
  links$target <- match(links$to,   nodes$name) - 1
  links$value  <- links$n
  links <- links[!is.na(links$source) & !is.na(links$target), ]

  sankeyNetwork(
    Links     = links,
    Nodes     = nodes,
    Source    = "source",
    Target   = "target",
    Value    = "value",
    NodeID   = "name",
    fontSize = 11,
    nodeWidth = 20,
    sinksRight = TRUE
  )
}

sankey_widget <- build_sankey(pathway_df)
saveWidget(sankey_widget,
           file.path(OUTPUT_DIR, "sankey_interactive.html"),
           selfcontained = TRUE)
cat("Interactive Sankey saved: sankey_interactive.html\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §5.  INDIVIDUAL TRAJECTORY SAMPLES (ANONYMISED)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §5  INDIVIDUAL TRAJECTORY SAMPLES ══════\n")

## 5-A.  Select representative cases ----------------------------------------
# Stratified sample: pick N children per trajectory "archetype"
set.seed(42)
sample_ids <- surv_child[, .(child_id, age_group, event_missing,
                              event_remal, total_removals)]

# Define archetypes
sample_ids[, archetype := fcase(
  total_removals > 1 & event_remal == 1, "Multi-Spell + Re-Maltreatment",
  total_removals > 1,                    "Multi-Spell Re-entry",
  event_missing == 1,                    "Missing from Care",
  event_remal == 1,                      "Re-Maltreatment (single spell)",
  default =                              "Stable Placement"
)]

N_PER_TYPE <- 3
sampled <- sample_ids[, .SD[sample(.N, min(N_PER_TYPE, .N))], by = archetype]

# Assign anonymous labels
sampled[, anon_label := paste0("Child-", sprintf("%03d", seq_len(.N)))]

cat("Sampled", nrow(sampled), "anonymised trajectories across",
    uniqueN(sampled$archetype), "archetypes.\n")

## 5-B.  Build timeline table for sampled children --------------------------
sampled_detail <- merge(
  episode_data, sampled[, .(child_id, anon_label, archetype)],
  by = "child_id"
)

trajectory_table <- sampled_detail[, .(
  `Anonymous ID` = anon_label,
  Archetype      = archetype,
  `Spell #`      = totalrem,
  FFY            = ffy,
  `Entry Date`   = as.character(latrem),
  `Exit Date`    = as.character(dlstfce),
  `Days in Care` = episode_days,
  `Maltreatment` = fifelse(any_subst == 1, "Yes", "No"),
  `Age Group`    = age_group
)]

# Save to CSV
fwrite(trajectory_table,
       file.path(OUTPUT_DIR, "anonymised_trajectory_samples.csv"))
cat("Trajectory sample table saved.\n")

## 5-C.  Swimlane plot of sampled trajectories --------------------------------
if (nrow(sampled_detail) > 0) {
  p_swim <- ggplot(sampled_detail,
    aes(y = reorder(anon_label, -as.numeric(latrem)),
        xmin = latrem, xmax = dlstfce,
        colour = archetype)) +
    geom_linerange(linewidth = 4, alpha = 0.8) +
    geom_point(aes(x = latrem), shape = 16, size = 2.5) +
    geom_point(aes(x = dlstfce), shape = 4, size = 2.5) +
    scale_colour_brewer(palette = "Set1") +
    labs(
      title = "Individual Placement Timelines (Anonymised Sample)",
      x = "Date", y = NULL, colour = "Archetype"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(file.path(OUTPUT_DIR, "swimlane_trajectories.png"),
         p_swim, width = 12, height = 7, dpi = 300)
  cat("Swimlane plot saved.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# §6.  GROUP-BASED TRAJECTORY MODELLING (GBTM)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §6  GROUP-BASED TRAJECTORY MODELS ══════\n")

## 6-A.  Prepare repeated-measures outcome ----------------------------------
# Outcome: binary indicator of "active in care" at each observed FFY
gbtm_data <- panel[, .(
  child_id, ffy,
  in_care    = 1L,   # present in AFCARS this year → in care
  any_subst
)]

# Pivot wide: one row per child, columns = FFY outcomes
gbtm_wide <- dcast(gbtm_data, child_id ~ ffy,
                   value.var = "in_care", fill = 0L)

# Build matrix for lcmm / crimCV
outcome_cols <- setdiff(names(gbtm_wide), "child_id")
Y_mat <- as.matrix(gbtm_wide[, ..outcome_cols])

cat("GBTM matrix:", nrow(Y_mat), "children ×", ncol(Y_mat), "years.\n")

## 6-B.  Fit models with 2-5 groups via crimCV (ZIP/logit) -----------------
# crimCV fits zero-inflated Poisson trajectory models
# Wrap in tryCatch — GBTM can be computationally demanding

bic_results <- data.frame(groups = integer(), bic = numeric())

for (g in 2:5) {
  cat("  Fitting", g, "group model …\n")
  fit <- tryCatch(
    crimCV(Y_mat, ng = g, dpolyp = 2, dpolyl = 2, init = 5),
    error = function(e) {
      message("    crimCV error (", g, " groups): ", e$message)
      NULL
    }
  )
  if (!is.null(fit)) {
    bic_results <- rbind(bic_results,
                          data.frame(groups = g, bic = fit$BIC))
    cat("    BIC =", round(fit$BIC, 1), "\n")
  }
}

# Select best model by lowest BIC
if (nrow(bic_results) > 0) {
  best_g <- bic_results$groups[which.min(bic_results$bic)]
  cat("Optimal groups by BIC:", best_g, "\n")

  best_fit <- crimCV(Y_mat, ng = best_g, dpolyp = 2, dpolyl = 2, init = 5)

  # Plot trajectories
  png(file.path(OUTPUT_DIR, "gbtm_trajectories.png"),
      width = 10, height = 6, units = "in", res = 300)
  plot(best_fit, main = paste("GBTM:", best_g, "Trajectory Groups"),
       xlab = "Year", ylab = "P(in care)")
  dev.off()
  cat("GBTM trajectory plot saved.\n")

  # Group assignments
  gbtm_wide[, traj_group := apply(best_fit$gwt, 1, which.max)]

  # BIC comparison table
  fwrite(bic_results, file.path(OUTPUT_DIR, "gbtm_bic_comparison.csv"))
} else {
  message("No GBTM models converged — skipping.")
}

## 6-C.  Latent-class mixed model (lcmm) — alternative GBTM ----------------
# Useful if crimCV doesn't converge; lcmm handles continuous outcomes too

gbtm_long <- melt(gbtm_wide, id.vars = c("child_id"),
                  variable.name = "ffy", value.name = "in_care")
gbtm_long[, ffy_num := as.integer(as.character(ffy)) - min(YEARS)]

if (requireNamespace("lcmm", quietly = TRUE)) {
  cat("Fitting lcmm models …\n")

  lcmm_1 <- hlme(in_care ~ ffy_num, subject = "child_id",
                  data = as.data.frame(gbtm_long), ng = 1)

  lcmm_bic <- data.frame(groups = 1L, bic = BIC(lcmm_1))

  for (g in 2:4) {
    cat("  lcmm", g, "groups …\n")
    fit <- tryCatch(
      hlme(in_care ~ ffy_num, mixture = ~ ffy_num,
           subject = "child_id",
           data = as.data.frame(gbtm_long), ng = g, B = lcmm_1),
      error = function(e) {
        message("    lcmm error: ", e$message); NULL
      }
    )
    if (!is.null(fit))
      lcmm_bic <- rbind(lcmm_bic, data.frame(groups = g, bic = BIC(fit)))
  }

  fwrite(lcmm_bic, file.path(OUTPUT_DIR, "lcmm_bic_comparison.csv"))
  cat("lcmm BIC comparison saved.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# §7.  CUMULATIVE-RISK DASHBOARD VISUALS (LEGISLATIVE AUDIENCE)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §7  LEGISLATIVE DASHBOARD VISUALS ══════\n")

## 7-A.  Cumulative incidence of adverse events ------------------------------

# Nelson-Aalen cumulative hazard → cumulative risk of missing & re-maltreatment

ci_missing <- survfit(
  Surv(time_days, event_missing) ~ 1, data = surv_child,
  type = "fleming-harrington"
)
ci_remal <- survfit(
  Surv(time_remal_days, event_remal) ~ 1, data = surv_child,
  type = "fleming-harrington"
)

# Tidy for ggplot
tidy_ci <- function(sf, label) {
  data.frame(
    time    = sf$time,
    cum_risk = 1 - sf$surv,
    lower   = 1 - sf$upper,
    upper   = 1 - sf$lower,
    outcome = label
  )
}

ci_df <- rbind(
  tidy_ci(ci_missing, "Missing from Care"),
  tidy_ci(ci_remal,   "Re-Maltreatment")
)

p_cumrisk <- ggplot(ci_df, aes(x = time, y = cum_risk,
                                colour = outcome, fill = outcome)) +
  geom_step(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15, colour = NA) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, NA)) +
  scale_colour_manual(values = c("Missing from Care" = "#2C7BB6",
                                  "Re-Maltreatment"  = "#D7191C")) +
  scale_fill_manual(  values = c("Missing from Care" = "#2C7BB6",
                                  "Re-Maltreatment"  = "#D7191C")) +
  labs(
    title    = "Cumulative Risk of Adverse Events After Placement",
    subtitle = paste("N =", scales::comma(nrow(surv_child)),
                     "children across FFY", min(YEARS), "-", max(YEARS)),
    x = "Days Since Removal",
    y = "Cumulative Risk",
    colour = NULL, fill = NULL
  ) +
  annotate("rect", xmin = 0, xmax = 90,
           ymin = -Inf, ymax = Inf,
           fill = "gold", alpha = 0.08) +
  annotate("text", x = 45, y = Inf, vjust = 1.5,
           label = "Critical\n90-day\nwindow",
           size = 3, colour = "grey40") +
  theme_minimal(base_size = 14) +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(OUTPUT_DIR, "cumulative_risk_dashboard.png"),
       p_cumrisk, width = 11, height = 7, dpi = 300)
cat("Cumulative risk dashboard saved.\n")

## 7-B.  Milestone table: risk at key time-points ---------------------------
milestones <- c(30, 90, 180, 365, 730)

extract_risk <- function(sf, times) {
  s <- summary(sf, times = times, extend = TRUE)
  data.frame(days = times, survival = s$surv,
             cum_risk = 1 - s$surv,
             lower = 1 - s$upper, upper = 1 - s$lower)
}

risk_table <- rbind(
  cbind(outcome = "Missing",        extract_risk(ci_missing, milestones)),
  cbind(outcome = "Re-Maltreatment", extract_risk(ci_remal,   milestones))
)
risk_table$cum_risk_pct <- paste0(round(risk_table$cum_risk * 100, 1), "%")

fwrite(risk_table, file.path(OUTPUT_DIR, "milestone_risk_table.csv"))

cat("\nMilestone risk table (for legislative briefs):\n")
print(risk_table[, c("outcome", "days", "cum_risk_pct")])

## 7-C.  Bar chart: risk at legislative milestones --------------------------
p_bar_risk <- ggplot(risk_table,
  aes(x = factor(days), y = cum_risk,
      fill = outcome)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = cum_risk_pct),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Missing" = "#2C7BB6",
                                "Re-Maltreatment" = "#D7191C")) +
  labs(
    title = "Cumulative Risk at Key Time-Points",
    subtitle = "Evidence supporting real-time tracking mandate",
    x = "Days Since Removal", y = "Cumulative Risk",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(file.path(OUTPUT_DIR, "legislative_risk_barplot.png"),
       p_bar_risk, width = 9, height = 6, dpi = 300)
cat("Legislative bar-chart saved.\n")

## 7-D.  Summary dashboard — composite panel --------------------------------
# Combine four key plots into one figure using patchwork

p_composite <- (
  (p_km_miss$plot + ggtitle("A. KM: Time-to-Missing")) +
  (p_km_remal$plot + ggtitle("B. KM: Time-to-Re-Maltreatment"))
) / (
  p_cumrisk + ggtitle("C. Cumulative Risk") +
  p_reentry + ggtitle("D. Spell Distribution")
) +
  plot_annotation(
    title    = "Child-Welfare Longitudinal Trajectory Dashboard",
    subtitle = paste("AFCARS/NCANDS linked data |",
                     min(YEARS), "-", max(YEARS)),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 13)
    )
  )

ggsave(file.path(OUTPUT_DIR, "composite_dashboard.png"),
       p_composite, width = 18, height = 14, dpi = 300)
cat("Composite dashboard saved.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# §8.  HANDLE CENSORED DATA — ROBUSTNESS & DIAGNOSTICS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════ §8  CENSORING DIAGNOSTICS ══════\n")

## 8-A.  Summarise censoring patterns ----------------------------------------
censor_summary <- surv_child[, .(
  total_children   = .N,
  event_missing    = sum(event_missing),
  censored_missing = sum(event_missing == 0),
  pct_censored     = round(100 * mean(event_missing == 0), 1),
  event_remal      = sum(event_remal),
  censored_remal   = sum(event_remal == 0),
  median_follow_up = round(median(time_days, na.rm = TRUE))
)]

cat("Censoring summary:\n")
print(censor_summary)

fwrite(censor_summary,
       file.path(OUTPUT_DIR, "censoring_summary.csv"))

## 8-B.  Informative censoring check — compare censored vs. uncensored ------
# Simple comparison of covariates between censored / uncensored groups

compare_cols <- c("age_at_entry_mo", "total_removals")
for (col in compare_cols) {
  if (col %in% names(surv_child)) {
    cat("\n", col, "by censoring status (missing endpoint):\n")
    print(surv_child[, .(
      mean   = round(mean(get(col), na.rm = TRUE), 1),
      median = round(median(get(col), na.rm = TRUE), 1),
      n      = .N
    ), by = event_missing])
  }
}

## 8-C.  Inverse-probability-of-censoring weighting (IPCW) sketch ----------
# Demonstrates the approach; full implementation requires domain tuning

if (all(c("age_at_entry_mo", "total_removals") %in% names(surv_child))) {
  # Model probability of being censored
  cens_model <- glm(
    I(1 - event_missing) ~ age_at_entry_mo + total_removals,
    data = surv_child, family = binomial
  )
  surv_child[, p_censor := predict(cens_model, type = "response")]
  surv_child[, ipcw := fifelse(p_censor > 0.01, 1 / (1 - p_censor), 1)]
  surv_child[, ipcw := pmin(ipcw, quantile(ipcw, 0.99, na.rm = TRUE))]

  cat("\nIPCW weights computed (median:", round(median(surv_child$ipcw), 2),
      ", max:", round(max(surv_child$ipcw), 2), ")\n")

  # Weighted Cox model
  cox_ipcw <- coxph(
    Surv(time_days, event_missing) ~ age_group + placement_label,
    data    = surv_child,
    weights = ipcw
  )

  sink(file.path(OUTPUT_DIR, "cox_ipcw_weighted.txt"))
  cat("═══ Cox PH — IPCW Adjusted ═══\n\n")
  print(summary(cox_ipcw))
  sink()
  cat("IPCW-weighted Cox model saved.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# §9.  FINAL OUTPUT MANIFEST
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════\n")
cat("  ALL OUTPUTS SAVED TO:", normalizePath(OUTPUT_DIR), "\n")
cat("═══════════════════════════════════════════════\n\n")

outputs <- list.files(OUTPUT_DIR, full.names = TRUE)
cat(paste(" ", outputs, collapse = "\n"), "\n\n")

cat("Expected output files:\n")
cat("  VISUALS:\n")
cat("    km_time_to_missing.png          – KM curve (missing)\n")
cat("    km_time_to_remaltreatment.png   – KM curve (re-maltreatment)\n")
cat("    km_missing_by_age.png           – Stratified KM by age\n")
cat("    km_missing_by_placement.png     – Stratified KM by placement\n")
cat("    sankey_alluvial_pathway.png     – Static alluvial diagram\n")
cat("    sankey_interactive.html         – Interactive Sankey (networkD3)\n")
cat("    swimlane_trajectories.png       – Individual timelines\n")
cat("    gbtm_trajectories.png           – Group trajectory model\n")
cat("    cumulative_risk_dashboard.png   – Dual cumulative-risk plot\n")
cat("    legislative_risk_barplot.png    – Bar chart for milestones\n")
cat("    composite_dashboard.png         – 4-panel composite\n")
cat("    reentry_spell_distribution.png  – Spell count distribution\n")
cat("  TABLES:\n")
cat("    anonymised_trajectory_samples.csv\n")
cat("    milestone_risk_table.csv\n")
cat("    gbtm_bic_comparison.csv\n")
cat("    lcmm_bic_comparison.csv\n")
cat("    censoring_summary.csv\n")
cat("  MODELS:\n")
cat("    cox_model_missing_summary.txt\n")
cat("    andersen_gill_model.txt\n")
cat("    cox_ipcw_weighted.txt\n")

cat("\n✔ Script complete.\n")
