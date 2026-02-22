###############################################################################
#  MALTREATMENT IN FOSTER CARE — ANALYSIS PIPELINE
#  Links AFCARS (foster-care spells) with NCANDS (maltreatment reports)
#  to calculate in-care maltreatment rates by perpetrator, placement type,
#  substantiation status, recurrence, and state. Compares to community rates.
#
#  DATA REQUIREMENTS
#  -----------------
#  1. AFCARS Foster Care File (annual, child-level)
#     Key fields: STATE, CHILDID (or FIPS+RecNumbr), REPDATYR, REPDATMO,
#                 LATREMDT (latest removal date), CURPLSET (current placement),
#                 DISDT (discharge date), AGEREM, SEX, RACE, HIESSION
#  2. NCANDS Child File (annual, report-level)
#     Key fields: STATERR, CHID (or SubYr+RptID+ChID), RPTDT, SUBYR,
#                 RPTDISP (substantiation), MALTX (maltreatment type),
#                 PER*REL (perpetrator relationship codes), PER*CAGE
#  3. Community population denominators (e.g., census child pop by state)
#
#  NOTES
#  -----
#  * NDACAN/NCANDS and AFCARS are restricted-use. This script uses their
#    standard variable naming conventions. Adjust field names to match
#    your specific extract year(s).
#  * Approximate linkage is used (state + overlapping dates ± age/sex)
#    because a universal child ID across systems does not exist in the
#    public-use files.  If your jurisdiction supplies a common ID, use it.
###############################################################################

# ── 0.  LIBRARIES ────────────────────────────────────────────────────────────
required_pkgs <- c(
  "tidyverse", "data.table", "lubridate", "janitor", "scales",
  "patchwork", "gt", "ggthemes", "glue", "slider"
)
invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

cat("══════════════════════════════════════════════════════════════════\n")
cat("  Maltreatment-in-Foster-Care Analysis Pipeline\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# ── 1.  CONFIGURATION ───────────────────────────────────────────────────────
CONFIG <- list(
  # ---- File paths (EDIT THESE) ----
  afcars_path        = "data/afcars_fc.csv",
  ncands_path        = "data/ncands_child.csv",
  community_pop_path = "data/state_child_population.csv",

  # ---- Analysis years ----
  years              = 2018:2023,

  # ---- NCANDS perpetrator-relationship codes for foster parents ----
  #   03 = Foster Parent (Female)
  #   04 = Foster Parent (Male)
  #   33 = Foster Parent (Unspecified)
  foster_perp_codes  = c("03", "04", "33"),

  # ---- AFCARS placement-type mapping (CURPLSET) ----
  #   1 = Pre-Adoptive Home          → non-kin
  #   2 = Foster Family Home (Rel)   → kin
  #   3 = Foster Family Home (Non-Rel) → non-kin
  #   4 = Group Home                 → group
  #   5 = Institution                → group
  #   6 = Supervised Independent Living → other
  #   7 = Runaway                    → other
  #   8 = Trial Home Visit           → other
  placement_map = c(
    "1" = "Non-Kin Foster",
    "2" = "Kin Foster",
    "3" = "Non-Kin Foster",
    "4" = "Group/Institutional",
    "5" = "Group/Institutional",
    "6" = "Other",
    "7" = "Other",
    "8" = "Other"
  ),

  # ---- Substantiation disposition codes (RPTDISP) ----
  #   01 = Substantiated
  #   02 = Indicated
  #   03 = Alternative Response – Victim
  substantiated_codes = c("01", "02"),

  # ---- Date-overlap tolerance (days) for approximate linkage ----
  date_buffer_days   = 30,

  # ---- Output directory ----
  output_dir         = "output"
)

dir.create(CONFIG$output_dir, showWarnings = FALSE, recursive = TRUE)


###############################################################################
#                       SECTION A — DATA INGESTION
###############################################################################

# ── 2.  LOAD AND STANDARDISE AFCARS ─────────────────────────────────────────
load_afcars <- function(path) {
  cat("Loading AFCARS from:", path, "\n")

  af <- fread(path, colClasses = "character") |> clean_names()

  # Harmonise common column-name variants
  rename_map <- c(
    state      = "st",
    childid    = "childid",
    repdatyr   = "repdatyr",
    latremdt   = "latremdt",     # latest removal date (YYYYMMDD)
    disdt      = "disdt",        # discharge date
    curplset   = "curplset",     # current placement setting
    agerem     = "agerem",
    sex        = "sex",
    race       = "amiakn"        # placeholder — build composite below
  )
  # Apply only renames that exist
  existing <- intersect(names(rename_map), names(af))
  # (skip renaming — field names kept as-is after clean_names())

  af <- af |>
    mutate(
      # Parse dates — AFCARS stores YYYYMMDD or MM/DD/YYYY depending on extract
      removal_date  = parse_date_time(latremdt, orders = c("Ymd", "mdY"), quiet = TRUE),
      discharge_date = parse_date_time(disdt, orders = c("Ymd", "mdY"), quiet = TRUE),
      report_yr     = as.integer(repdatyr),
      state_cd      = str_pad(st, 2, pad = "0"),
      placement_cat = CONFIG$placement_map[as.character(curplset)],
      child_age     = as.numeric(agerem)
    ) |>
    # If discharge date missing → still in care; set far-future sentinel
    mutate(
      discharge_date = if_else(is.na(discharge_date),
                               as.POSIXct("2099-12-31"), discharge_date)
    ) |>
    filter(report_yr %in% CONFIG$years)

  cat("  AFCARS records loaded:", comma(nrow(af)), "\n")
  af
}

# ── 3.  LOAD AND STANDARDISE NCANDS ─────────────────────────────────────────
load_ncands <- function(path) {
  cat("Loading NCANDS from:", path, "\n")

  nc <- fread(path, colClasses = "character") |> clean_names()

  # ---- Identify perpetrator-relationship columns (PER1REL … PER3REL) ----
  perp_rel_cols <- grep("^per\\d+rel$", names(nc), value = TRUE)
  if (length(perp_rel_cols) == 0) {
    warning("No PER*REL columns found — check NCANDS field names.")
    perp_rel_cols <- "per1rel"
  }

  nc <- nc |>
    mutate(
      report_date    = parse_date_time(rptdt, orders = c("Ymd", "mdY"), quiet = TRUE),
      sub_year       = as.integer(subyr),
      state_cd       = str_pad(staterr, 2, pad = "0"),
      substantiated  = rptdisp %in% CONFIG$substantiated_codes,
      child_age      = as.numeric(chage)
    ) |>
    filter(sub_year %in% CONFIG$years)

  # ---- Flag reports involving a foster-parent perpetrator ----
  nc$foster_perp <- apply(
    nc[, ..perp_rel_cols], 1,
    function(row) any(row %in% CONFIG$foster_perp_codes, na.rm = TRUE)
  )

  # ---- Extract maltreatment types (up to 4 types per record) ----
  malt_cols <- grep("^mal\\d+typ$|^maltx$", names(nc), value = TRUE)
  if (length(malt_cols) > 0) {
    nc$malt_types <- apply(nc[, ..malt_cols], 1, function(row) {
      paste(na.omit(unique(row[row != "" & row != "99"])), collapse = ",")
    })
  } else {
    nc$malt_types <- NA_character_
  }

  cat("  NCANDS records loaded:", comma(nrow(nc)), "\n")
  nc
}

# ── 4.  LOAD COMMUNITY POPULATION DENOMINATORS ──────────────────────────────
load_community_pop <- function(path) {
  cat("Loading community child population from:", path, "\n")
  pop <- fread(path) |> clean_names()
  # Expected columns: state_cd (FIPS 2-digit), year, child_pop
  pop <- pop |>
    mutate(state_cd = str_pad(state_cd, 2, pad = "0"),
           year     = as.integer(year))
  pop
}


###############################################################################
#                       SECTION B — RECORD LINKAGE
###############################################################################

# ── 5.  APPROXIMATE LINKAGE: AFCARS ↔ NCANDS ────────────────────────────────
#
#  Strategy (when no common child ID exists):
#    1. Exact match on STATE
#    2. NCANDS report_date falls within [AFCARS removal_date − buffer,
#       AFCARS discharge_date + buffer]
#    3. Optional: fuzzy match on child_age (±1 yr) and sex
#
#  When a jurisdiction-supplied common ID is available, set use_common_id=TRUE
#  and specify the column name.
# ─────────────────────────────────────────────────────────────────────────────

link_afcars_ncands <- function(af, nc,
                               use_common_id = FALSE,
                               common_id_col = "childid") {
  cat("\n── Linking AFCARS ↔ NCANDS ──\n")

  if (use_common_id && common_id_col %in% names(af) && common_id_col %in% names(nc)) {
    cat("  Using common child ID:", common_id_col, "\n")

    linked <- inner_join(
      nc, af,
      by        = setNames("state_cd", "state_cd") |> c(setNames(common_id_col, common_id_col)),
      suffix    = c(".nc", ".af"),
      relationship = "many-to-many"
    ) |>
      filter(report_date >= (removal_date - days(CONFIG$date_buffer_days)),
             report_date <= (discharge_date + days(CONFIG$date_buffer_days)))

  } else {
    cat("  No common ID — using approximate linkage (state + date overlap + demographics)\n")

    # Pre-filter NCANDS to foster-perp reports for efficiency,
    # but keep all reports for community-rate comparison later
    nc_foster <- nc |> filter(foster_perp == TRUE)

    # Use data.table non-equi join for speed
    af_dt <- as.data.table(af)[, .(
      state_cd, removal_date, discharge_date, placement_cat,
      child_age_af = child_age, report_yr, childid
    )]
    nc_dt <- as.data.table(nc_foster)[, .(
      state_cd, report_date, sub_year, substantiated,
      foster_perp, malt_types, child_age_nc = child_age, chid
    )]

    # Add buffered boundaries
    af_dt[, `:=`(
      spell_start = removal_date  - days(CONFIG$date_buffer_days),
      spell_end   = discharge_date + days(CONFIG$date_buffer_days)
    )]

    # Non-equi join: same state, report_date within spell window
    setkey(af_dt, state_cd, spell_start, spell_end)
    setkey(nc_dt, state_cd, report_date, report_date)

    linked <- af_dt[nc_dt,
      on = .(state_cd = state_cd,
             spell_start <= report_date,
             spell_end   >= report_date),
      nomatch = 0,
      allow.cartesian = TRUE
    ]

    # Optional age filter (within ±1 year)
    if ("child_age_af" %in% names(linked) && "child_age_nc" %in% names(linked)) {
      linked <- linked[abs(child_age_af - child_age_nc) <= 1 |
                        is.na(child_age_af) | is.na(child_age_nc)]
    }
  }

  n_linked <- nrow(linked)
  cat("  Linked maltreatment-in-care records:", comma(n_linked), "\n")
  as_tibble(linked)
}


###############################################################################
#                     SECTION C — RATE CALCULATIONS
###############################################################################

# ── 6.  DENOMINATORS: FOSTER CARE DAYS / CHILD-YEARS ────────────────────────
calc_fc_denominators <- function(af) {
  cat("\n── Calculating foster-care denominators ──\n")

  af |>
    mutate(
      # Cap discharge at end of analysis window for ongoing spells
      eff_discharge = pmin(discharge_date, as.POSIXct("2023-12-31")),
      fc_days       = as.numeric(difftime(eff_discharge, removal_date, units = "days")),
      fc_days       = pmax(fc_days, 1)
    ) |>
    group_by(state_cd, report_yr, placement_cat) |>
    summarise(
      n_children     = n_distinct(childid),
      total_fc_days  = sum(fc_days, na.rm = TRUE),
      child_years    = total_fc_days / 365.25,
      .groups = "drop"
    )
}

# ── 7.  RATE: MALTREATMENT IN CARE PER 100K CHILD-DAYS ──────────────────────
calc_mic_rates <- function(linked, denominators) {
  cat("── Calculating maltreatment-in-care rates ──\n")

  events <- linked |>
    group_by(state_cd, report_yr, placement_cat) |>
    summarise(
      n_reports         = n(),
      n_substantiated   = sum(substantiated, na.rm = TRUE),
      .groups = "drop"
    )

  rates <- left_join(denominators, events,
                     by = c("state_cd", "report_yr", "placement_cat")) |>
    replace_na(list(n_reports = 0, n_substantiated = 0)) |>
    mutate(
      rate_per_100k_days     = (n_reports / total_fc_days) * 1e5,
      sub_rate_per_100k_days = (n_substantiated / total_fc_days) * 1e5,
      rate_per_1k_childyrs   = (n_reports / child_years) * 1000,
      sub_rate_per_1k_cy     = (n_substantiated / child_years) * 1000
    )

  rates
}

# ── 8.  COMMUNITY MALTREATMENT RATES (COMPARISON) ───────────────────────────
calc_community_rates <- function(nc, community_pop) {
  cat("── Calculating community maltreatment rates ──\n")

  comm_events <- nc |>
    group_by(state_cd, sub_year) |>
    summarise(
      total_reports       = n(),
      total_substantiated = sum(substantiated, na.rm = TRUE),
      .groups = "drop"
    )

  comm_rates <- left_join(
    comm_events,
    community_pop,
    by = c("state_cd", "sub_year" = "year")
  ) |>
    filter(!is.na(child_pop), child_pop > 0) |>
    mutate(
      comm_rate_per_1k  = (total_reports / child_pop) * 1000,
      comm_sub_rate_1k  = (total_substantiated / child_pop) * 1000
    )

  comm_rates
}

# ── 9.  RECURRENCE ──────────────────────────────────────────────────────────
#  A child has recurrence if >1 substantiated maltreatment report within
#  the same foster-care spell (or within 12 months of the first report).
calc_recurrence <- function(linked) {
  cat("── Calculating recurrence ──\n")

  # Use child-level grouping
  child_id_col <- if ("childid" %in% names(linked)) "childid" else "chid"

  recur <- linked |>
    filter(substantiated == TRUE) |>
    arrange(state_cd, .data[[child_id_col]], report_date) |>
    group_by(state_cd, report_yr, .data[[child_id_col]]) |>
    summarise(
      n_sub_reports    = n(),
      first_report     = min(report_date, na.rm = TRUE),
      last_report      = max(report_date, na.rm = TRUE),
      days_span        = as.numeric(difftime(last_report, first_report, units = "days")),
      has_recurrence   = n_sub_reports > 1,
      .groups = "drop"
    )

  recurrence_summary <- recur |>
    group_by(state_cd, report_yr) |>
    summarise(
      unique_victims     = n(),
      victims_recurring  = sum(has_recurrence),
      recurrence_rate    = victims_recurring / unique_victims,
      median_days_btwn   = median(days_span[days_span > 0], na.rm = TRUE),
      .groups = "drop"
    )

  recurrence_summary
}

# ── 10. PERPETRATOR BREAKDOWN ────────────────────────────────────────────────
calc_perpetrator_breakdown <- function(linked) {
  cat("── Perpetrator relationship breakdown ──\n")

  # Check which PER*REL columns survived the join

  perp_cols <- grep("^per\\d+rel", names(linked), value = TRUE)

  if (length(perp_cols) == 0) {
    cat("  No perpetrator columns found in linked data.\n")
    return(tibble())
  }

  # Pivot perpetrator columns long
  perp_long <- linked |>
    select(state_cd, report_yr, placement_cat, substantiated, all_of(perp_cols)) |>
    pivot_longer(cols = all_of(perp_cols), names_to = "perp_slot", values_to = "perp_code") |>
    filter(!is.na(perp_code), perp_code != "", perp_code != "99") |>
    mutate(
      perp_label = case_when(
        perp_code == "03" ~ "Foster Mother",
        perp_code == "04" ~ "Foster Father",
        perp_code == "33" ~ "Foster Parent (Unspec.)",
        perp_code %in% c("01","02") ~ "Biological Parent",
        perp_code %in% c("05","06") ~ "Residential Staff",
        perp_code == "07" ~ "Day-Care Provider",
        perp_code == "08" ~ "Unmarried Partner of Parent",
        perp_code == "09" ~ "Legal Guardian",
        perp_code %in% c("10","11","12") ~ "Other Relative",
        TRUE ~ paste0("Other (", perp_code, ")")
      )
    )

  perp_summary <- perp_long |>
    group_by(state_cd, report_yr, perp_label) |>
    summarise(
      n_events       = n(),
      n_substantiated = sum(substantiated, na.rm = TRUE),
      .groups = "drop"
    )

  perp_summary
}


###############################################################################
#                     SECTION D — TABLES & VISUALISATION
###############################################################################

# ── Colour palette ───────────────────────────────────────────────────────────
pal <- c(
  "Kin Foster"          = "#2c7fb8",
  "Non-Kin Foster"      = "#41b6c4",
  "Group/Institutional" = "#fd8d3c",
  "Other"               = "#bdbdbd",
  "Community"           = "#e31a1c"
)

theme_dashboard <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 3),
      plot.subtitle    = element_text(colour = "grey40"),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

# ── 11. RATE TABLE (GT) ─────────────────────────────────────────────────────
make_rate_table <- function(mic_rates) {
  cat("── Generating rate summary table ──\n")

  # National summary by placement type and year
  tbl_data <- mic_rates |>
    group_by(report_yr, placement_cat) |>
    summarise(
      n_children   = sum(n_children, na.rm = TRUE),
      n_reports    = sum(n_reports, na.rm = TRUE),
      n_sub        = sum(n_substantiated, na.rm = TRUE),
      child_years  = sum(child_years, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      rate_per_1k_cy  = round((n_reports / child_years) * 1000, 2),
      sub_rate_1k_cy  = round((n_sub / child_years) * 1000, 2),
      pct_sub         = scales::percent(n_sub / n_reports, accuracy = 0.1)
    ) |>
    filter(!is.na(placement_cat))

  gt_table <- tbl_data |>
    gt(groupname_col = "report_yr") |>
    tab_header(
      title    = "Maltreatment-in-Foster-Care Rates by Placement Type",
      subtitle = glue("National estimates, {min(CONFIG$years)}–{max(CONFIG$years)}")
    ) |>
    cols_label(
      placement_cat  = "Placement Type",
      n_children     = "Children in Care",
      n_reports      = "Reports",
      n_sub          = "Substantiated",
      child_years    = "Child-Years",
      rate_per_1k_cy = "Rate / 1K CY",
      sub_rate_1k_cy = "Sub. Rate / 1K CY",
      pct_sub        = "% Substantiated"
    ) |>
    fmt_number(columns = c(n_children, n_reports, n_sub), decimals = 0) |>
    fmt_number(columns = child_years, decimals = 1) |>
    tab_source_note("Source: AFCARS + NCANDS linked data. CY = child-years.") |>
    tab_options(table.font.size = 11)

  gt_table
}

# ── 12. COMPARATIVE BAR CHART — FC vs COMMUNITY ─────────────────────────────
plot_fc_vs_community <- function(mic_rates, comm_rates) {
  cat("── Plotting foster-care vs community rates ──\n")

  fc_agg <- mic_rates |>
    group_by(report_yr, placement_cat) |>
    summarise(rate = sum(n_reports, na.rm = TRUE) /
                     sum(child_years, na.rm = TRUE) * 1000,
              .groups = "drop") |>
    filter(!is.na(placement_cat))

  comm_agg <- comm_rates |>
    group_by(sub_year) |>
    summarise(rate = sum(total_reports, na.rm = TRUE) /
                     sum(child_pop, na.rm = TRUE) * 1000,
              .groups = "drop") |>
    mutate(placement_cat = "Community") |>
    rename(report_yr = sub_year)

  combined <- bind_rows(fc_agg, comm_agg)

  p <- ggplot(combined, aes(x = factor(report_yr), y = rate, fill = placement_cat)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
    scale_fill_manual(values = pal, name = NULL) +
    labs(
      title    = "Maltreatment Rates: Foster Care Placements vs Community",
      subtitle = "Reports per 1,000 child-years (foster care) or per 1,000 children (community)",
      x = "Fiscal Year", y = "Rate per 1,000"
    ) +
    theme_dashboard() +
    coord_cartesian(expand = FALSE)

  p
}

# ── 13. TREND LINES ─────────────────────────────────────────────────────────
plot_trend_lines <- function(mic_rates) {
  cat("── Plotting trend lines ──\n")

  trend <- mic_rates |>
    filter(!is.na(placement_cat)) |>
    group_by(report_yr, placement_cat) |>
    summarise(
      rate  = sum(n_reports, na.rm = TRUE) / sum(child_years, na.rm = TRUE) * 1000,
      sub_r = sum(n_substantiated, na.rm = TRUE) / sum(child_years, na.rm = TRUE) * 1000,
      .groups = "drop"
    )

  p1 <- ggplot(trend, aes(report_yr, rate, colour = placement_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(title = "All Reports — Rate per 1K Child-Years",
         x = NULL, y = "Rate") +
    theme_dashboard()

  p2 <- ggplot(trend, aes(report_yr, sub_r, colour = placement_cat)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(title = "Substantiated Only — Rate per 1K Child-Years",
         x = NULL, y = "Rate") +
    theme_dashboard()

  p1 / p2 + plot_annotation(
    title = "Maltreatment-in-Foster-Care Trends by Placement Type",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
}

# ── 14. STATE-LEVEL BAR CHART ────────────────────────────────────────────────
plot_state_comparison <- function(mic_rates, top_n = 15) {
  cat("── Plotting state-level comparisons ──\n")

  state_summary <- mic_rates |>
    group_by(state_cd) |>
    summarise(
      rate = sum(n_reports, na.rm = TRUE) / sum(child_years, na.rm = TRUE) * 1000,
      n    = sum(n_reports, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Exclude tiny denominators
    filter(n >= 20) |>
    slice_max(rate, n = top_n)

  ggplot(state_summary, aes(x = reorder(state_cd, rate), y = rate)) +
    geom_col(fill = "#2c7fb8", alpha = 0.85) +
    geom_text(aes(label = round(rate, 1)), hjust = -0.2, size = 3) +
    coord_flip(expand = FALSE) +
    labs(
      title    = glue("Top {top_n} States by Foster-Care Maltreatment Rate"),
      subtitle = "Reports per 1,000 child-years (all placement types combined)",
      x = "State FIPS", y = "Rate per 1,000 Child-Years"
    ) +
    theme_dashboard()
}

# ── 15. PLACEMENT-TYPE BREAKDOWN BAR ─────────────────────────────────────────
plot_placement_breakdown <- function(mic_rates) {
  cat("── Plotting placement-type breakdown ──\n")

  ptype <- mic_rates |>
    filter(!is.na(placement_cat)) |>
    group_by(placement_cat) |>
    summarise(
      total     = sum(n_reports, na.rm = TRUE),
      subst     = sum(n_substantiated, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    pivot_longer(cols = c(total, subst),
                 names_to = "measure", values_to = "count") |>
    mutate(measure = if_else(measure == "total", "All Reports", "Substantiated"))

  ggplot(ptype, aes(placement_cat, count, fill = measure)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    scale_fill_manual(values = c("All Reports" = "#41b6c4", "Substantiated" = "#e31a1c"),
                      name = NULL) +
    labs(
      title = "Foster-Care Maltreatment Reports by Placement Type",
      subtitle = "All years combined",
      x = NULL, y = "Number of Reports"
    ) +
    theme_dashboard()
}

# ── 16. RECURRENCE VISUALIZATION ─────────────────────────────────────────────
plot_recurrence <- function(recurrence_df) {
  cat("── Plotting recurrence trends ──\n")

  ggplot(recurrence_df, aes(report_yr, recurrence_rate)) +
    geom_line(colour = "#e31a1c", linewidth = 1.1) +
    geom_point(colour = "#e31a1c", size = 3) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
      title    = "Recurrence of Substantiated Maltreatment in Foster Care",
      subtitle = "% of victims with > 1 substantiated report in same spell",
      x = "Fiscal Year", y = "Recurrence Rate"
    ) +
    theme_dashboard()
}

# ── 17. PERPETRATOR BREAKDOWN BAR ────────────────────────────────────────────
plot_perpetrator <- function(perp_summary) {
  cat("── Plotting perpetrator breakdown ──\n")

  perp_agg <- perp_summary |>
    group_by(perp_label) |>
    summarise(n = sum(n_events, na.rm = TRUE), .groups = "drop") |>
    slice_max(n, n = 10)

  ggplot(perp_agg, aes(reorder(perp_label, n), n)) +
    geom_col(fill = "#fd8d3c", alpha = 0.85) +
    geom_text(aes(label = comma(n)), hjust = -0.15, size = 3) +
    coord_flip(expand = FALSE) +
    labs(
      title = "Maltreatment-in-Care Events by Perpetrator Relationship",
      x = NULL, y = "Number of Events"
    ) +
    theme_dashboard()
}


###############################################################################
#               SECTION E — SAFETY MONITORING RECOMMENDATIONS
###############################################################################

generate_recommendations <- function(mic_rates, recurrence_df) {
  cat("\n── Generating Safety Monitoring Recommendations ──\n")

  # Identify high-risk placement types
  placement_risk <- mic_rates |>
    filter(!is.na(placement_cat)) |>
    group_by(placement_cat) |>
    summarise(rate = sum(n_reports) / sum(child_years) * 1000, .groups = "drop") |>
    arrange(desc(rate))

  highest_type <- placement_risk$placement_cat[1]
  highest_rate <- round(placement_risk$rate[1], 2)

  # Identify states with rising trends (simple linear slope)
  state_trends <- mic_rates |>
    group_by(state_cd, report_yr) |>
    summarise(rate = sum(n_reports) / sum(child_years) * 1000, .groups = "drop") |>
    group_by(state_cd) |>
    filter(n() >= 3) |>
    summarise(
      slope = coef(lm(rate ~ report_yr))[2],
      .groups = "drop"
    ) |>
    filter(slope > 0) |>
    slice_max(slope, n = 5)

  # Average recurrence
  avg_recur <- mean(recurrence_df$recurrence_rate, na.rm = TRUE)

  recs <- glue("
╔══════════════════════════════════════════════════════════════════════╗
║               SAFETY MONITORING RECOMMENDATIONS                     ║
║               for Public Dashboard Implementation                   ║
╚══════════════════════════════════════════════════════════════════════╝

1. KEY METRIC TILES (real-time or quarterly refresh)
   ─────────────────────────────────────────────────
   • Maltreatment-in-care rate per 1,000 child-years (substantiated)
   • Recurrence rate among foster-care victims
     (current avg: {percent(avg_recur, accuracy = 0.1)})
   • Median days to subsequent report within a spell
   • % of investigations completed within 30/60/90 days

2. HIGHEST-RISK PLACEMENT TYPE: {highest_type}
   ──────────────────────────────────────────────
   Current rate: {highest_rate} per 1,000 child-years.
   Recommendation: Flag for enhanced screening frequency, increase
   caseworker visit cadence, and trigger supervisory review when a
   second report is received in {highest_type} placements.

3. RISING-TREND STATES (positive slope in rates)
   ───────────────────────────────────────────────
   {paste(state_trends$state_cd, collapse = ', ')}
   Recommendation: These states should receive technical assistance
   and root-cause analysis. Include in federal CFSR spotlight reviews.

4. DASHBOARD DRILL-DOWN CAPABILITIES
   ───────────────────────────────────
   • Allow filtering by placement type, state, fiscal year,
     maltreatment type (physical, sexual, neglect, emotional).
   • Enable comparison toggling between in-care rates and
     community baseline rates for contextual interpretation.
   • Provide downloadable data underlying each chart.

5. RECURRENCE EARLY-WARNING SYSTEM
   ─────────────────────────────────
   • Alert when a child's second report is received while in
     the same placement spell.
   • Escalation protocol: auto-trigger supervisory staffing,
     consider placement change, and notify CASA/GAL.

6. DATA QUALITY FLAGS
   ────────────────────
   • Monitor linkage match rates (approximate vs exact ID).
   • Flag states with > 20% missing CURPLSET or PER*REL fields.
   • Track year-over-year changes in reporting volume that may
     indicate system changes rather than true incidence shifts.

7. EQUITY LENS
   ─────────────
   • Stratify all rates by child race/ethnicity and age group.
   • Highlight disproportionality in group-home placements.
   • Include disparity ratios on the dashboard.
")

  recs
}


###############################################################################
#               SECTION F — MAIN EXECUTION PIPELINE
###############################################################################

run_pipeline <- function() {

  # ── Data Ingestion ──
  af       <- load_afcars(CONFIG$afcars_path)
  nc       <- load_ncands(CONFIG$ncands_path)
  comm_pop <- load_community_pop(CONFIG$community_pop_path)

  # ── Linkage ──
  linked   <- link_afcars_ncands(af, nc, use_common_id = FALSE)

  # ── Denominators & Rates ──
  denoms    <- calc_fc_denominators(af)
  mic_rates <- calc_mic_rates(linked, denoms)
  comm_rates <- calc_community_rates(nc, comm_pop)

  # ── Recurrence & Perpetrator ──
  recurrence <- calc_recurrence(linked)
  perp_brkdn <- calc_perpetrator_breakdown(linked)

  # ── Save rate tables ──
  write_csv(mic_rates,    file.path(CONFIG$output_dir, "mic_rates_by_placement_state_year.csv"))
  write_csv(comm_rates,   file.path(CONFIG$output_dir, "community_rates_by_state_year.csv"))
  write_csv(recurrence,   file.path(CONFIG$output_dir, "recurrence_summary.csv"))
  write_csv(perp_brkdn,   file.path(CONFIG$output_dir, "perpetrator_breakdown.csv"))
  cat("\n✓ Rate tables saved to:", CONFIG$output_dir, "\n")

  # ── GT Table ──
  gt_tbl <- make_rate_table(mic_rates)
  gtsave(gt_tbl, file.path(CONFIG$output_dir, "rate_table.html"))
  cat("✓ Rate table (HTML) saved\n")

  # ── Plots ──
  p_compare   <- plot_fc_vs_community(mic_rates, comm_rates)
  p_trends    <- plot_trend_lines(mic_rates)
  p_states    <- plot_state_comparison(mic_rates, top_n = 15)
  p_placement <- plot_placement_breakdown(mic_rates)
  p_recur     <- plot_recurrence(recurrence)
  p_perp      <- plot_perpetrator(perp_brkdn)

  ggsave(file.path(CONFIG$output_dir, "fc_vs_community.png"),
         p_compare, width = 12, height = 7, dpi = 300)
  ggsave(file.path(CONFIG$output_dir, "trend_lines.png"),
         p_trends, width = 12, height = 9, dpi = 300)
  ggsave(file.path(CONFIG$output_dir, "state_comparison.png"),
         p_states, width = 10, height = 8, dpi = 300)
  ggsave(file.path(CONFIG$output_dir, "placement_breakdown.png"),
         p_placement, width = 10, height = 6, dpi = 300)
  ggsave(file.path(CONFIG$output_dir, "recurrence_trend.png"),
         p_recur, width = 10, height = 6, dpi = 300)
  ggsave(file.path(CONFIG$output_dir, "perpetrator_breakdown.png"),
         p_perp, width = 10, height = 7, dpi = 300)
  cat("✓ All plots saved\n")

  # ── Recommendations ──
  recs <- generate_recommendations(mic_rates, recurrence)
  writeLines(recs, file.path(CONFIG$output_dir, "safety_recommendations.txt"))
  cat(recs)
  cat("\n✓ Recommendations saved\n")

  # ── Return all objects for interactive use ──
  invisible(list(
    afcars       = af,
    ncands       = nc,
    linked       = linked,
    denominators = denoms,
    mic_rates    = mic_rates,
    comm_rates   = comm_rates,
    recurrence   = recurrence,
    perp_breakdown = perp_brkdn,
    plots = list(
      compare   = p_compare,
      trends    = p_trends,
      states    = p_states,
      placement = p_placement,
      recurrence = p_recur,
      perpetrator = p_perp
    )
  ))
}


###############################################################################
#               SECTION G — SYNTHETIC DATA FOR TESTING
###############################################################################

# Generate synthetic data so the pipeline can be tested end-to-end
# without restricted-use NCANDS/AFCARS files.

generate_synthetic_data <- function(n_fc = 25000, n_ncands = 80000,
                                    years = 2018:2023) {
  cat("\n── Generating synthetic test data ──\n")
  set.seed(42)

  states <- str_pad(c(1,4,6,12,13,17,26,34,36,42,48), 2, pad = "0")

  # --- Synthetic AFCARS ---
  af_syn <- tibble(
    st       = sample(states, n_fc, replace = TRUE,
                      prob = c(.04,.05,.15,.10,.06,.08,.06,.06,.18,.07,.15)),
    childid  = paste0("FC", str_pad(seq_len(n_fc), 7, pad = "0")),
    repdatyr = as.character(sample(years, n_fc, replace = TRUE)),
    latremdt = format(
      as.Date("2017-01-01") + sample(0:2190, n_fc, replace = TRUE), "%Y%m%d"
    ),
    disdt    = NA_character_,
    curplset = as.character(sample(1:5, n_fc, replace = TRUE,
                                   prob = c(.05,.25,.40,.15,.15))),
    agerem   = as.character(sample(0:17, n_fc, replace = TRUE)),
    sex      = as.character(sample(1:2, n_fc, replace = TRUE))
  )
  # Set discharge for ~70% of spells
  has_dis <- sample(n_fc, round(n_fc * 0.7))
  af_syn$disdt[has_dis] <- format(
    as.Date(af_syn$latremdt[has_dis], "%Y%m%d") + sample(30:730, length(has_dis), replace = TRUE),
    "%Y%m%d"
  )

  # --- Synthetic NCANDS ---
  nc_syn <- tibble(
    staterr   = sample(states, n_ncands, replace = TRUE,
                       prob = c(.04,.05,.15,.10,.06,.08,.06,.06,.18,.07,.15)),
    chid      = paste0("NC", str_pad(seq_len(n_ncands), 7, pad = "0")),
    subyr     = as.character(sample(years, n_ncands, replace = TRUE)),
    rptdt     = format(
      as.Date("2017-06-01") + sample(0:2190, n_ncands, replace = TRUE), "%Y%m%d"
    ),
    rptdisp   = sample(c("01","02","03","04","05"), n_ncands, replace = TRUE,
                       prob = c(.18,.07,.05,.55,.15)),
    per1rel   = sample(c("01","02","03","04","05","06","08","09","33","99"),
                       n_ncands, replace = TRUE,
                       prob = c(.30,.15,.04,.03,.02,.01,.10,.03,.02,.30)),
    per2rel   = sample(c("", "01","03","04","99"), n_ncands, replace = TRUE,
                       prob = c(.70,.10,.05,.05,.10)),
    mal1typ   = sample(c("1","2","3","4","8","99"), n_ncands, replace = TRUE,
                       prob = c(.30,.10,.40,.08,.02,.10)),
    chage     = as.character(sample(0:17, n_ncands, replace = TRUE))
  )

  # --- Community population ---
  pop_syn <- expand_grid(state_cd = states, year = years) |>
    mutate(child_pop = round(runif(n(), 200000, 3000000)))

  # Write out
  dir.create("data", showWarnings = FALSE)
  fwrite(af_syn,  "data/afcars_fc.csv")
  fwrite(nc_syn,  "data/ncands_child.csv")
  fwrite(pop_syn, "data/state_child_population.csv")

  cat("  Synthetic AFCARS records:", comma(nrow(af_syn)), "\n")
  cat("  Synthetic NCANDS records:", comma(nrow(nc_syn)), "\n")
  cat("  Saved to data/ directory\n")
}


###############################################################################
#                        SECTION H — RUN
###############################################################################

# ---- Uncomment below to generate synthetic data and run the full pipeline ----
# generate_synthetic_data()
# results <- run_pipeline()

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  Pipeline ready. Usage:\n")
cat("    1. Edit CONFIG paths at top of script\n")
cat("    2. source('maltreatment_foster_care_analysis.R')\n")
cat("    3. generate_synthetic_data()   # optional, for testing\n")
cat("    4. results <- run_pipeline()\n")
cat("══════════════════════════════════════════════════════════════════\n")
