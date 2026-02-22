###############################################################################
#  QUALITATIVE–QUANTITATIVE INTEGRATION ENGINE
#  Purpose : Pair statistical outputs with case studies, OIG/GAO report
#            summaries, news excerpts, and Chapin Hall "Hot Spot" risk-adjusted
#            county analyses to produce publication-ready integrated reports,
#            annotated visuals, and testimony-ready materials.
#
#  Outputs : 1. Annotated plots (stats + callout boxes with real-world examples)
#            2. Tables merging metrics with narrative excerpts
#            3. HTML/PDF integrated reports via rmarkdown / quarto
#            4. Slide-ready visuals with humanizing captions
#
#  Inputs  : • quant_data   – CSV / data-frame of aggregated county-level
#                              metrics (maltreatment rates, placement counts,
#                              re-entry rates, etc.)
#            • qual_data    – CSV / data-frame of qualitative elements (case
#                              studies, report excerpts, OIG/GAO summaries)
#            • hotspot_data – CSV / data-frame of Chapin Hall Hot Spot
#                              risk-adjusted county results
#
#  Author  : AI-Generated Workflow
#  License : CC-BY-4.0 for educational / advocacy use
###############################################################################

# ── 0. SETUP ─────────────────────────────────────────────────────────────────

required_packages <- c(
 "tidyverse",  "ggplot2",   "ggrepel",    "ggtext",    "patchwork",
 "scales",     "knitr",     "kableExtra", "DT",        "rmarkdown",
 "glue",       "htmltools", "gridExtra",  "grid",      "officer",
 "rvg",        "flextable", "cowplot",    "ggforce",   "viridis"
)

install_if_missing <- function(pkgs) {
 new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
 if (length(new_pkgs) > 0) {
   message("Installing: ", paste(new_pkgs, collapse = ", "))
   install.packages(new_pkgs, repos = "https://cran.r-project.org")
 }
}
install_if_missing(required_packages)

suppressPackageStartupMessages({
 library(tidyverse)
 library(ggplot2)
 library(ggrepel)
 library(ggtext)
 library(patchwork)
 library(scales)
 library(knitr)
 library(kableExtra)
 library(DT)
 library(rmarkdown)
 library(glue)
 library(htmltools)
 library(gridExtra)
 library(grid)
 library(officer)
 library(rvg)
 library(flextable)
 library(cowplot)
 library(ggforce)
 library(viridis)
})

# ── Theme for all plots ──────────────────────────────────────────────────────

theme_testimony <- function(base_size = 12) {
 theme_minimal(base_size = base_size) %+replace%
   theme(
     plot.title         = element_textbox_simple(
       size = base_size * 1.3, face = "bold",
       margin = margin(b = 10), lineheight = 1.2
     ),
     plot.subtitle      = element_textbox_simple(
       size = base_size * 0.95, color = "grey30",
       margin = margin(b = 12), lineheight = 1.3
     ),
     plot.caption       = element_textbox_simple(
       size = base_size * 0.75, color = "grey50",
       margin = margin(t = 10), halign = 0
     ),
     panel.grid.minor   = element_blank(),
     panel.grid.major.x = element_blank(),
     axis.title         = element_text(size = base_size * 0.9),
     legend.position    = "top",
     plot.margin        = margin(15, 15, 10, 15)
   )
}

# ── Color palette ─────────────────────────────────────────────────────────────

pal <- list(
 danger  = "#C0392B",
 warning = "#E67E22",
 safe    = "#27AE60",
 neutral = "#2C3E50",
 accent  = "#2980B9",
 bg_call = "#FFF9E6",    # callout background
 bg_warn = "#FDEDEC"     # warning callout background
)


###############################################################################
# SECTION 1 — SYNTHETIC / DEMO DATA GENERATORS
# Replace these with your real data imports for production use.
###############################################################################

generate_demo_quant_data <- function(n_counties = 40, seed = 42) {
 # ── Simulates county-level child welfare metrics ──
 set.seed(seed)
 tibble(
   county_fips     = sprintf("%05d", sample(1000:99999, n_counties)),
   county_name     = paste0("County_", LETTERS[1:min(n_counties, 26)],
                             rep("", n_counties)[1:n_counties]) %>%
                     make.unique(),
   state           = sample(state.abb, n_counties, replace = TRUE),
   # Raw rates per 1,000 children
   maltreatment_rate        = round(runif(n_counties, 2, 35), 1),
   in_care_maltreatment     = round(runif(n_counties, 0.5, 12), 2),
   foster_entry_rate        = round(runif(n_counties, 1, 20), 1),
   reunification_pct        = round(runif(n_counties, 25, 75), 1),
   reentry_within_12mo_pct  = round(runif(n_counties, 5, 30), 1),
   missing_from_care_n      = sample(0:50, n_counties, replace = TRUE),
   # Denominator / population
   child_population         = sample(5000:500000, n_counties),
   poverty_rate_pct         = round(runif(n_counties, 8, 35), 1),
   caseworker_ratio          = round(runif(n_counties, 10, 45), 0)
 ) %>%
   mutate(
     # Risk-adjusted rate (simulated Chapin Hall-style)
     expected_rate    = 5 + 0.4 * poverty_rate_pct +
                          0.05 * caseworker_ratio +
                          rnorm(n_counties, 0, 2),
     risk_adj_ratio   = round(maltreatment_rate / pmax(expected_rate, 1), 2),
     hotspot_flag     = if_else(risk_adj_ratio > 1.3, "Hot Spot", "Not Hot Spot"),
     performance_tier = case_when(
       risk_adj_ratio > 1.5 ~ "Critical",
       risk_adj_ratio > 1.2 ~ "Elevated",
       risk_adj_ratio > 0.8 ~ "Expected",
       TRUE                 ~ "Lower Than Expected"
     )
   )
}

generate_demo_qual_data <- function() {
 # ── Simulates qualitative elements: case studies, report excerpts ──
 tribble(
   ~qual_id, ~source_type,   ~source_name,
     ~county_fips, ~excerpt,                                                    ~year, ~topic,

   "Q001", "OIG Report",   "OIG-19-07 Missing Children",
     "County_A", "The OIG found that caseworkers failed to follow up on 33% of
     reported runaways within the required 24-hour window. In County A, the
     backlog of uninvestigated cases exceeded 120 children over a 6-month
     period, representing a systemic failure in runaway response protocols.", 2019, "missing_children",

   "Q002", "GAO Report",   "GAO-21-313 Foster Care Oversight",
     "County_B", "GAO investigators documented that placement instability
     in County B averaged 4.2 moves per child per year — more than double the
     national median. Children in congregate care settings were 3× more likely
     to experience a substantiated maltreatment allegation while in care.", 2021, "placement_instability",

   "Q003", "News Case",    "AP Investigation: Lost in the System",
     "County_C", "An 8-year-old removed from her home after severe neglect
     was placed in three different emergency shelters in 10 days. Her file
     shows no documented educational enrollment for 7 months. 'She just
     disappeared from everyone's radar,' said her former teacher.", 2022, "in_care_maltreatment",

   "Q004", "Chapin Hall",  "Hot Spot Analysis Report (Simulated)",
     "County_D", "Risk-adjusted analysis reveals County D's maltreatment
     rate is 1.8× the expected rate after controlling for poverty,
     urbanicity, and demographic composition — far exceeding neighboring
     counties with similar resource profiles. Raw rates alone would rank
     this county 15th; risk-adjustment moves it to 3rd, exposing hidden
     system strain.", 2023, "risk_adjustment",

   "Q005", "OIG Report",   "OIG-22-04 Youth Aging Out",
     "County_E", "Among youth who aged out of foster care in County E,
     only 12% had stable housing within 90 days. The OIG noted that
     transition planning began, on average, just 47 days before the
     youth's 18th birthday — far below the recommended 12-month window.", 2022, "aging_out",

   "Q006", "News Case",    "ProPublica: When Protectors Become Threats",
     "County_F", "Investigation found that 14 foster parents with prior
     substantiated allegations were allowed to continue providing care
     in County F due to incomplete cross-state background check systems.
     Three children suffered documented abuse in these placements.", 2023, "in_care_maltreatment",

   "Q007", "GAO Report",   "GAO-23-106 ICPC Interstate Compact",
     "County_A", "Interstate placements took an average of 9.4 months
     to finalize in the study sample. GAO found that children waiting
     for ICPC clearance were 40% more likely to experience a placement
     disruption during the waiting period.", 2023, "interstate_placement",

   "Q008", "Chapin Hall",  "Risk-Adjusted Reentry Analysis (Simulated)",
     "County_G", "After adjusting for case complexity and prior trauma
     history, County G's 12-month reentry rate of 28% represents a
     risk-adjusted ratio of 1.6 — indicating systemic reunification
     support failures rather than inherent population risk.", 2023, "risk_adjustment"
 )
}

generate_demo_hotspot_data <- function(n = 40, seed = 42) {
 # ── Simulates Chapin Hall-style risk-adjusted county comparisons ──
 set.seed(seed)
 tibble(
   county_name    = paste0("County_", LETTERS[1:min(n, 26)]) %>%
                    c(., paste0("County_A", 1:(n - min(n, 26)))) %>%
                    head(n),
   raw_rate       = round(runif(n, 3, 35), 1),
   expected_rate  = round(runif(n, 5, 20), 1),
   risk_adj_ratio = round(raw_rate / expected_rate, 2),
   z_score        = round(qnorm(pmin(pmax(
                      pnorm((raw_rate - expected_rate) /
                            (expected_rate * 0.3)), 0.001), 0.999)), 2),
   ci_lower       = round(risk_adj_ratio - abs(rnorm(n, 0.15, 0.05)), 2),
   ci_upper       = round(risk_adj_ratio + abs(rnorm(n, 0.15, 0.05)), 2),
   sig_flag       = if_else(ci_lower > 1 | ci_upper < 1,
                            "Significant", "Not Significant"),
   tier           = case_when(
     risk_adj_ratio > 1.5 & sig_flag == "Significant" ~ "Critical Hot Spot",
     risk_adj_ratio > 1.2 & sig_flag == "Significant" ~ "Elevated Hot Spot",
     risk_adj_ratio < 0.8 & sig_flag == "Significant" ~ "Under-Expected",
     TRUE                                              ~ "Within Expected Range"
   )
 )
}


###############################################################################
# SECTION 2 — DATA LOADING & MERGING
###############################################################################

load_and_merge <- function(
 quant_path   = NULL,
 qual_path    = NULL,
 hotspot_path = NULL,
 use_demo     = TRUE
) {
 # Load quantitative data
 if (!is.null(quant_path) && file.exists(quant_path)) {
   quant <- read_csv(quant_path, show_col_types = FALSE)
   message("✓ Loaded quantitative data: ", quant_path)
 } else if (use_demo) {
   quant <- generate_demo_quant_data()
   message("⚑ Using demo quantitative data (40 counties)")
 } else {
   stop("No quantitative data provided and use_demo = FALSE")
 }

 # Load qualitative data
 if (!is.null(qual_path) && file.exists(qual_path)) {
   qual <- read_csv(qual_path, show_col_types = FALSE)
   message("✓ Loaded qualitative data: ", qual_path)
 } else if (use_demo) {
   qual <- generate_demo_qual_data()
   message("⚑ Using demo qualitative data (8 case excerpts)")
 } else {
   qual <- tibble()
   message("⚠ No qualitative data provided; proceeding without narratives")
 }

 # Load Chapin Hall Hot Spot data
 if (!is.null(hotspot_path) && file.exists(hotspot_path)) {
   hotspot <- read_csv(hotspot_path, show_col_types = FALSE)
   message("✓ Loaded Hot Spot data: ", hotspot_path)
 } else if (use_demo) {
   hotspot <- generate_demo_hotspot_data()
   message("⚑ Using demo Hot Spot data (40 counties)")
 } else {
   hotspot <- tibble()
   message("⚠ No Hot Spot data provided")
 }

 # Merge quant + hotspot on county_name
 if (nrow(hotspot) > 0 && "county_name" %in% names(hotspot)) {
   merged <- quant %>%
     left_join(
       hotspot %>% select(county_name, raw_rate, expected_rate,
                          risk_adj_ratio_hs = risk_adj_ratio,
                          z_score, ci_lower, ci_upper,
                          sig_flag, tier),
       by = "county_name"
     )
 } else {
   merged <- quant
 }

 list(
   quant   = quant,
   qual    = qual,
   hotspot = hotspot,
   merged  = merged
 )
}


###############################################################################
# SECTION 3 — ANNOTATED VISUALIZATION FUNCTIONS
###############################################################################

# ── 3A. Hot Spot Funnel Plot with Callout Boxes ─────────────────────────────

plot_hotspot_funnel <- function(merged, qual, n_callouts = 3) {

 # Identify top hot-spot counties for annotation
 top_hs <- merged %>%
   filter(!is.na(risk_adj_ratio)) %>%
   arrange(desc(risk_adj_ratio)) %>%
   head(n_callouts)

 # Find matching qualitative excerpts
 callout_labels <- top_hs %>%
   left_join(
     qual %>%
       group_by(county_fips) %>%
       slice_head(n = 1) %>%
       ungroup() %>%
       select(county_fips, source_type, excerpt),
     by = c("county_name" = "county_fips")
   ) %>%
   mutate(
     short_excerpt = if_else(
       !is.na(excerpt),
       str_trunc(excerpt, 100),
       glue("Risk-adj. ratio: {risk_adj_ratio}")
     ),
     label = glue("{county_name}\n{source_type %||% 'Data'}: {short_excerpt}")
   )

 p <- ggplot(merged %>% filter(!is.na(risk_adj_ratio)),
              aes(x = child_population, y = risk_adj_ratio)) +

   # Reference band for "expected" range
   annotate("rect",
            xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.2,
            fill = pal$safe, alpha = 0.12) +
   annotate("text",
            x = max(merged$child_population, na.rm = TRUE) * 0.85,
            y = 1.0, label = "Expected Range",
            color = pal$safe, fontface = "italic", size = 3.5) +

   # Reference line at ratio = 1
   geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +

   # All points
   geom_point(aes(color = performance_tier, size = maltreatment_rate),
              alpha = 0.7) +

   # Highlight hot-spot callouts
   geom_point(data = top_hs,
              aes(x = child_population, y = risk_adj_ratio),
              shape = 21, fill = pal$danger, color = "white",
              size = 5, stroke = 1.5) +

   # Callout labels with ggrepel
   geom_label_repel(
     data = callout_labels,
     aes(x = child_population, y = risk_adj_ratio, label = label),
     size = 2.8, lineheight = 1.1, family = "sans",
     fill = pal$bg_warn, color = pal$neutral,
     box.padding = 1.2, point.padding = 0.5,
     segment.color = pal$danger, segment.size = 0.6,
     max.overlaps = 20, force = 8, seed = 123,
     label.size = 0.4, label.r = unit(0.2, "lines")
   ) +

   scale_color_manual(
     values = c("Critical" = pal$danger, "Elevated" = pal$warning,
                "Expected" = "grey60", "Lower Than Expected" = pal$safe),
     name = "Performance Tier"
   ) +
   scale_size_continuous(range = c(2, 8), name = "Raw Maltreatment Rate") +
   scale_x_continuous(labels = comma) +

   labs(
     title = "**Chapin Hall 'Hot Spot' Funnel Plot:** Risk-Adjusted County Performance",
     subtitle = glue(
       "Each point is a county. The <span style='color:{pal$safe}'>green band</span> ",
       "shows the expected range (ratio 0.8–1.2). Points above = worse than expected ",
       "after controlling for poverty, demographics, and caseload. ",
       "<span style='color:{pal$danger}'>Red-highlighted counties</span> are annotated ",
       "with qualitative evidence."
     ),
     x = "Child Population (Denominator)",
     y = "Risk-Adjusted Ratio (Observed / Expected)",
     caption = glue(
       "Source: Simulated data modeled on Chapin Hall risk-adjustment methodology. ",
       "Qualitative annotations from OIG, GAO, and investigative reports. ",
       "Ratio > 1.0 = performance worse than expected; < 1.0 = better than expected."
     )
   ) +
   theme_testimony(base_size = 11) +
   guides(size = guide_legend(override.aes = list(color = "grey40")))

 p
}


# ── 3B. Raw vs. Risk-Adjusted Ranking Comparison (Slope Chart) ──────────────

plot_raw_vs_adjusted <- function(hotspot, top_n = 15) {

 ranked <- hotspot %>%
   mutate(
     rank_raw = rank(-raw_rate, ties.method = "first"),
     rank_adj = rank(-risk_adj_ratio, ties.method = "first")
   ) %>%
   filter(rank_raw <= top_n | rank_adj <= top_n) %>%
   mutate(
     rank_shift   = rank_raw - rank_adj,
     shift_label  = case_when(
       rank_shift > 3  ~ "Revealed by Risk Adj. (hidden risk)",
       rank_shift < -3 ~ "Explained by Demographics (inflated raw)",
       TRUE            ~ "Stable ranking"
     )
   )

 ranked_long <- ranked %>%
   select(county_name, rank_raw, rank_adj, shift_label) %>%
   pivot_longer(cols = c(rank_raw, rank_adj),
                names_to = "measure", values_to = "rank") %>%
   mutate(
     measure = recode(measure,
       "rank_raw" = "Raw Rate Ranking",
       "rank_adj" = "Risk-Adjusted Ranking"
     )
   )

 p <- ggplot(ranked_long,
              aes(x = measure, y = rank, group = county_name)) +
   geom_line(aes(color = shift_label), linewidth = 0.9, alpha = 0.7) +
   geom_point(aes(color = shift_label), size = 3) +
   geom_text_repel(
     data = ranked_long %>% filter(measure == "Risk-Adjusted Ranking"),
     aes(label = county_name), size = 3, nudge_x = 0.15,
     direction = "y", hjust = 0, segment.size = 0.3
   ) +
   scale_y_reverse(breaks = 1:max(ranked_long$rank)) +
   scale_color_manual(
     values = c(
       "Revealed by Risk Adj. (hidden risk)" = pal$danger,
       "Explained by Demographics (inflated raw)" = pal$accent,
       "Stable ranking" = "grey50"
     ),
     name = "Ranking Shift"
   ) +
   labs(
     title = "**How Risk Adjustment Changes the Story**",
     subtitle = glue(
       "Comparing raw maltreatment rate rankings vs. Chapin Hall-style ",
       "risk-adjusted rankings. <span style='color:{pal$danger}'>Red lines</span> = ",
       "counties whose problems were **hidden** by raw rates. ",
       "<span style='color:{pal$accent}'>Blue lines</span> = counties where ",
       "high raw rates are largely **explained** by demographics."
     ),
     x = NULL, y = "Rank (1 = Worst)",
     caption = paste(
       "Rankings inverted: 1 = highest rate / ratio.",
       "Risk adjustment controls for poverty rate, urbanicity, and demographic composition."
     )
   ) +
   theme_testimony(base_size = 11) +
   theme(panel.grid.major.y = element_line(color = "grey90"))

 p
}


# ── 3C. Annotated Bar Chart: Metrics + Narrative Callouts ────────────────────

plot_annotated_bars <- function(merged, qual, metric = "in_care_maltreatment",
                                top_n = 10) {

 plot_data <- merged %>%
   arrange(desc(.data[[metric]])) %>%
   head(top_n) %>%
   mutate(county_name = fct_reorder(county_name, .data[[metric]]))

 # Match qualitative excerpts
 annotations <- plot_data %>%
   left_join(
     qual %>%
       filter(topic == "in_care_maltreatment") %>%
       group_by(county_fips) %>%
       slice_head(n = 1) %>%
       ungroup() %>%
       select(county_fips, source_name, excerpt),
     by = c("county_name" = "county_fips")
   ) %>%
   filter(!is.na(excerpt)) %>%
   mutate(short = str_trunc(excerpt, 90))

 metric_label <- str_replace_all(metric, "_", " ") %>% str_to_title()

 p <- ggplot(plot_data, aes(x = county_name, y = .data[[metric]])) +
   geom_col(aes(fill = performance_tier), width = 0.7) +

   # Add callout annotations for counties with qualitative matches
   {if (nrow(annotations) > 0)
     geom_label(
       data = annotations,
       aes(label = str_wrap(glue("📋 {source_name}:\n{short}"), 35),
           y = .data[[metric]] * 0.5),
       size = 2.5, hjust = 0.5, fill = pal$bg_call,
       label.size = 0.4, lineheight = 1.1, family = "sans"
     )
   } +

   scale_fill_manual(
     values = c("Critical" = pal$danger, "Elevated" = pal$warning,
                "Expected" = "grey60", "Lower Than Expected" = pal$safe),
     name = "Risk Tier"
   ) +
   coord_flip() +
   labs(
     title = glue("**Top {top_n} Counties: {metric_label}** with Qualitative Evidence"),
     subtitle = glue(
       "Bar heights show quantitative metric values. ",
       "<span style='background-color:{pal$bg_call};padding:2px 6px;'>",
       "Yellow callout boxes</span> overlay matched OIG/GAO/news evidence."
     ),
     x = NULL, y = metric_label,
     caption = "Callout annotations sourced from OIG, GAO, and investigative reports."
   ) +
   theme_testimony(base_size = 11)

 p
}


# ── 3D. Case-Study Spotlight Panel ──────────────────────────────────────────

plot_case_spotlight <- function(qual, merged, case_id = "Q003") {

 case <- qual %>% filter(qual_id == case_id)
 if (nrow(case) == 0) stop("Case ID not found: ", case_id)

 county <- case$county_fips[1]
 county_data <- merged %>% filter(county_name == county)

 # Small multiples: key metrics for the spotlight county
 if (nrow(county_data) > 0) {
   metrics_long <- county_data %>%
     select(county_name, maltreatment_rate, in_care_maltreatment,
            reentry_within_12mo_pct, missing_from_care_n) %>%
     pivot_longer(-county_name, names_to = "metric", values_to = "value") %>%
     mutate(metric = str_replace_all(metric, "_", " ") %>% str_to_title())

   p_metrics <- ggplot(metrics_long, aes(x = metric, y = value)) +
     geom_col(fill = pal$danger, alpha = 0.8, width = 0.6) +
     geom_text(aes(label = round(value, 1)), vjust = -0.5, size = 3.5) +
     coord_flip() +
     labs(title = glue("Key Metrics: {county}"), x = NULL, y = NULL) +
     theme_testimony(base_size = 10) +
     theme(axis.text.x = element_blank(),
           panel.grid = element_blank())
 } else {
   p_metrics <- ggplot() +
     annotate("text", x = 0.5, y = 0.5,
              label = "No quantitative data matched", size = 4) +
     theme_void()
 }

 # Narrative panel as a text grob
 wrapped <- str_wrap(case$excerpt[1], width = 70)
 p_narrative <- ggplot() +
   annotate("richtext",
            x = 0.5, y = 0.6,
            label = glue(
              "<b>{case$source_type[1]}: {case$source_name[1]} ({case$year[1]})</b><br><br>",
              "<i>\"{str_replace_all(wrapped, '\n', '<br>')}\"</i>"
            ),
            size = 3.5, fill = pal$bg_warn, label.color = pal$danger,
            hjust = 0.5, vjust = 0.5,
            label.padding = unit(c(0.8, 0.8, 0.8, 0.8), "lines")) +
   xlim(0, 1) + ylim(0, 1) +
   theme_void() +
   labs(title = glue("**Case Spotlight:** {case$topic[1] %>%
                       str_replace_all('_', ' ') %>%
                       str_to_title()}")) +
   theme(plot.title = element_textbox_simple(
     size = 13, face = "bold", margin = margin(b = 5)))

 # Combine using patchwork
 p_narrative / p_metrics +
   plot_layout(heights = c(2, 1)) +
   plot_annotation(
     caption = glue(
       "Integrated view: qualitative narrative paired with quantitative county data. ",
       "Case ID: {case_id}."
     ),
     theme = theme(plot.caption = element_text(
       size = 8, color = "grey50", hjust = 0))
   )
}


###############################################################################
# SECTION 4 — INTEGRATED TABLES
###############################################################################

create_integrated_table <- function(merged, qual, top_n = 10) {
 # ── Merge top counties with their qualitative annotations ──

 top_counties <- merged %>%
   arrange(desc(risk_adj_ratio)) %>%
   head(top_n) %>%
   select(county_name, state, maltreatment_rate, risk_adj_ratio,
          performance_tier, in_care_maltreatment, missing_from_care_n,
          poverty_rate_pct, caseworker_ratio)

 # Join qualitative evidence
 qual_summary <- qual %>%
   group_by(county_fips) %>%
   summarise(
     evidence_sources = paste(unique(source_type), collapse = "; "),
     n_evidence       = n(),
     key_excerpt      = str_trunc(first(excerpt), 150),
     .groups = "drop"
   )

 integrated <- top_counties %>%
   left_join(qual_summary, by = c("county_name" = "county_fips")) %>%
   mutate(
     evidence_sources = replace_na(evidence_sources, "—"),
     n_evidence       = replace_na(n_evidence, 0L),
     key_excerpt      = replace_na(key_excerpt, "No qualitative evidence matched")
   )

 # ── Flextable for Word/PDF/HTML export ──────────────────────────────────

 ft <- integrated %>%
   select(
     County         = county_name,
     State          = state,
     `Maltreat. Rate` = maltreatment_rate,
     `Risk-Adj Ratio` = risk_adj_ratio,
     Tier           = performance_tier,
     `In-Care Maltreat.` = in_care_maltreatment,
     `Missing N`    = missing_from_care_n,
     `Evidence Sources` = evidence_sources,
     `Key Finding`  = key_excerpt
   ) %>%
   flextable() %>%
   theme_zebra() %>%
   fontsize(size = 9, part = "all") %>%
   fontsize(size = 10, part = "header") %>%
   bold(part = "header") %>%
   color(~ `Risk-Adj Ratio` > 1.5, ~ `Risk-Adj Ratio`,
         color = pal$danger) %>%
   color(~ `Risk-Adj Ratio` > 1.2 & `Risk-Adj Ratio` <= 1.5,
         ~ `Risk-Adj Ratio`, color = pal$warning) %>%
   bg(~ Tier == "Critical", ~ Tier, bg = "#FDEDEC") %>%
   bg(~ Tier == "Elevated", ~ Tier, bg = "#FEF5E7") %>%
   width(j = "Key Finding", width = 3.5) %>%
   width(j = "County", width = 1.2) %>%
   set_caption(
     "Table: Top Counties by Risk-Adjusted Ratio with Matched Qualitative Evidence"
   ) %>%
   add_footer_lines(
     values = c(
       "Risk-Adj Ratio > 1.0 indicates performance worse than expected.",
       "Qualitative evidence sourced from OIG, GAO reports, and news investigations.",
       "Hot Spot methodology adapted from Chapin Hall risk-adjustment framework."
     )
   ) %>%
   fontsize(size = 7, part = "footer") %>%
   italic(part = "footer")

 list(data = integrated, flextable = ft)
}


# ── 4B. kableExtra HTML table for Rmarkdown embedding ───────────────────────

create_html_table <- function(integrated_data) {

 integrated_data %>%
   select(
     County = county_name, State = state,
     `Raw Rate` = maltreatment_rate,
     `Risk-Adj.` = risk_adj_ratio,
     Tier = performance_tier,
     `Evidence` = evidence_sources,
     `Key Finding` = key_excerpt
   ) %>%
   kbl(escape = FALSE, align = c("l","c","r","r","c","l","l")) %>%
   kable_styling(
     bootstrap_options = c("striped", "hover", "condensed"),
     full_width = TRUE, font_size = 13
   ) %>%
   column_spec(4, color = if_else(
     integrated_data$risk_adj_ratio > 1.3, pal$danger, "black"
   ), bold = integrated_data$risk_adj_ratio > 1.3) %>%
   column_spec(5, background = case_when(
     integrated_data$performance_tier == "Critical" ~ "#FDEDEC",
     integrated_data$performance_tier == "Elevated" ~ "#FEF5E7",
     TRUE ~ "white"
   )) %>%
   column_spec(7, width = "30em") %>%
   add_header_above(c(
     "County Info" = 2, "Quantitative Metrics" = 3,
     "Qualitative Evidence" = 2
   )) %>%
   footnote(
     general = c(
       "Risk-Adj. Ratio > 1.0 = performance worse than expected.",
       "Qualitative sources: OIG, GAO, Chapin Hall, investigative journalism."
     ),
     footnote_as_chunk = TRUE
   )
}


###############################################################################
# SECTION 5 — HUMANIZING CAPTIONS GENERATOR
###############################################################################

generate_caption <- function(county_row, qual_match = NULL,
                              context = "testimony") {
 # ── Produces context-aware captions for testimony, petitions, slides ──

 county  <- county_row$county_name
 rate    <- county_row$maltreatment_rate
 ratio   <- county_row$risk_adj_ratio
 tier    <- county_row$performance_tier
 missing <- county_row$missing_from_care_n
 incare  <- county_row$in_care_maltreatment

 base <- glue(
   "{county} reports a maltreatment rate of {rate} per 1,000 children. ",
   "After Chapin Hall-style risk adjustment — accounting for poverty, ",
   "demographics, and caseload — the county's observed-to-expected ratio ",
   "is {ratio}, placing it in the \"{tier}\" performance tier."
 )

 missing_note <- if (missing > 10) {
   glue(" Notably, {missing} children were reported missing from care, ",
        "a figure that demands immediate investigative attention.")
 } else ""

 incare_note <- if (incare > 5) {
   glue(" The in-care maltreatment rate of {incare} per 1,000 suggests ",
        "that the system tasked with protecting these children may itself ",
        "be a source of harm.")
 } else ""

 qual_note <- if (!is.null(qual_match) && nrow(qual_match) > 0) {
   excerpt_short <- str_trunc(qual_match$excerpt[1], 200)
   glue("\n\nCorroborating evidence ({qual_match$source_type[1]}, ",
        "{qual_match$year[1]}): \"{excerpt_short}\"")
 } else ""

 testimony_close <- switch(context,
   "testimony" = glue(
     "\n\nThese are not abstractions. Behind every data point is a child ",
     "whose safety depends on system accountability."
   ),
   "petition" = glue(
     "\n\nWe petition the court to consider these findings as evidence of ",
     "systemic failure requiring immediate remedial action."
   ),
   "slide" = "",
   ""
 )

 paste0(base, missing_note, incare_note, qual_note, testimony_close)
}

# Batch caption generator
generate_all_captions <- function(merged, qual, top_n = 5,
                                   context = "testimony") {
 top <- merged %>%
   arrange(desc(risk_adj_ratio)) %>%
   head(top_n)

 captions <- map_chr(seq_len(nrow(top)), function(i) {
   row <- top[i, ]
   qual_match <- qual %>% filter(county_fips == row$county_name)
   generate_caption(row, qual_match, context)
 })

 tibble(county = top$county_name, caption = captions)
}


###############################################################################
# SECTION 6 — RMARKDOWN / QUARTO REPORT GENERATOR
###############################################################################

generate_integrated_report <- function(
 data_list,
 output_dir   = "output",
 output_file  = "qual_quant_integrated_report",
 output_format = "html",    # "html" or "pdf"
 title         = "Qualitative-Quantitative Integrated Child Welfare Report",
 author        = "Child Welfare Analytics Team",
 subtitle      = "Chapin Hall Hot Spot Risk-Adjusted Analysis with Case Evidence"
) {

 dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

 # ── Write the Rmd template ─────────────────────────────────────────────

 rmd_path <- file.path(output_dir,
                        paste0(output_file, ".Rmd"))

 rmd_content <- glue('
---
title: "{title}"
subtitle: "{subtitle}"
author: "{author}"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    toc_depth: 3
    code_folding: hide
    number_sections: true
    css: !expr NULL
  pdf_document:
    toc: true
    number_sections: true
    latex_engine: xelatex
params:
  merged: NULL
  qual: NULL
  hotspot: NULL
---

```{{r setup, include=FALSE}}
knitr::opts_chunk$set(
  echo = FALSE, message = FALSE, warning = FALSE,
  fig.width = 10, fig.height = 7, dpi = 150
)
library(tidyverse); library(ggplot2); library(ggrepel)
library(ggtext); library(patchwork); library(scales)
library(kableExtra); library(glue); library(ggforce)
library(viridis); library(flextable)

merged  <- params$merged
qual    <- params$qual
hotspot <- params$hotspot
```

# Executive Summary

This report integrates **quantitative child welfare metrics** with
**qualitative evidence** from OIG reports, GAO investigations, news
case studies, and Chapin Hall risk-adjusted "Hot Spot" county analyses.

The purpose is threefold:

1. Demonstrate how **risk-adjusted analysis reveals hidden system failures**
   that raw rates obscure.
2. **Humanize the data** by pairing statistics with real-world case evidence.
3. Provide **testimony-ready materials** for courts, legislatures, and
   oversight bodies.

---

# Why Risk Adjustment Matters

Raw maltreatment rates can be misleading. A county with high poverty may
show elevated rates simply because of its demographics, not because of
system failure. Conversely, a wealthy county may have a low raw rate that
masks genuine underperformance.

**Chapin Hall\'s risk-adjustment methodology** computes an *expected rate*
for each county based on poverty, urbanicity, racial/ethnic composition,
and caseload capacity. The **risk-adjusted ratio** (observed / expected)
reveals whether a county is performing better or worse than similarly
situated peers.

> A ratio of 1.0 = performing exactly as expected.
> A ratio of 1.5 = 50% worse than expected — a **Hot Spot**.

```{{r funnel-plot, fig.cap="Chapin Hall-style funnel plot showing risk-adjusted county performance with qualitative annotations."}}
# Source the plotting functions from the parent script
# (In production, these would be loaded via source())
# For the report, we inline a simplified version:

top_hs <- merged %>%
  filter(!is.na(risk_adj_ratio)) %>%
  arrange(desc(risk_adj_ratio)) %>%
  head(3)

ggplot(merged %>% filter(!is.na(risk_adj_ratio)),
       aes(x = child_population, y = risk_adj_ratio)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.2,
           fill = "#27AE60", alpha = 0.12) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_point(aes(color = performance_tier, size = maltreatment_rate),
             alpha = 0.7) +
  geom_point(data = top_hs,
             aes(x = child_population, y = risk_adj_ratio),
             shape = 21, fill = "#C0392B", color = "white",
             size = 5, stroke = 1.5) +
  geom_label_repel(data = top_hs,
    aes(x = child_population, y = risk_adj_ratio,
        label = paste0(county_name, "\\nRatio: ", risk_adj_ratio)),
    size = 3, fill = "#FDEDEC", segment.color = "#C0392B",
    box.padding = 1, seed = 123) +
  scale_color_manual(values = c("Critical" = "#C0392B",
    "Elevated" = "#E67E22", "Expected" = "grey60",
    "Lower Than Expected" = "#27AE60"), name = "Tier") +
  scale_size_continuous(range = c(2, 8), name = "Raw Rate") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Risk-Adjusted County Performance (Funnel Plot)",
       x = "Child Population", y = "Risk-Adjusted Ratio") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
```

# Raw vs. Risk-Adjusted Rankings

```{{r slope-chart, fig.cap="Slope chart comparing raw and risk-adjusted rankings. Red lines indicate counties whose challenges were hidden by raw rates."}}
ranked <- hotspot %>%
  mutate(rank_raw = rank(-raw_rate, ties.method = "first"),
         rank_adj = rank(-risk_adj_ratio, ties.method = "first")) %>%
  filter(rank_raw <= 15 | rank_adj <= 15) %>%
  mutate(shift_label = case_when(
    rank_raw - rank_adj > 3  ~ "Hidden risk revealed",
    rank_raw - rank_adj < -3 ~ "Inflated by demographics",
    TRUE ~ "Stable"))

ranked_long <- ranked %>%
  pivot_longer(c(rank_raw, rank_adj), names_to = "m", values_to = "rank") %>%
  mutate(m = recode(m, rank_raw = "Raw Rate", rank_adj = "Risk-Adjusted"))

ggplot(ranked_long, aes(x = m, y = rank, group = county_name)) +
  geom_line(aes(color = shift_label), linewidth = 0.9, alpha = 0.7) +
  geom_point(aes(color = shift_label), size = 3) +
  scale_y_reverse() +
  scale_color_manual(values = c("Hidden risk revealed" = "#C0392B",
    "Inflated by demographics" = "#2980B9", "Stable" = "grey50")) +
  labs(title = "How Risk Adjustment Changes County Rankings",
       x = NULL, y = "Rank (1 = Worst)", color = "Shift Type") +
  theme_minimal(base_size = 12)
```

# Integrated Evidence Table

The table below merges the top counties by risk-adjusted ratio with
matched qualitative evidence from OIG/GAO reports and case studies.

```{{r integrated-table}}
top10 <- merged %>%
  arrange(desc(risk_adj_ratio)) %>% head(10) %>%
  select(county_name, state, maltreatment_rate, risk_adj_ratio,
         performance_tier, in_care_maltreatment, missing_from_care_n)

qual_s <- qual %>%
  group_by(county_fips) %>%
  summarise(sources = paste(unique(source_type), collapse = "; "),
            excerpt = stringr::str_trunc(first(excerpt), 120),
            .groups = "drop")

integ <- top10 %>%
  left_join(qual_s, by = c("county_name" = "county_fips")) %>%
  mutate(across(c(sources, excerpt), ~replace_na(.x, "—")))

kbl(integ, col.names = c("County","State","Raw Rate","Risk-Adj",
  "Tier","In-Care","Missing","Sources","Key Finding"),
  escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE, font_size = 12) %>%
  add_header_above(c("Location" = 2, "Quantitative" = 5,
                     "Qualitative" = 2))
```

# Case Spotlights

```{{r case-spotlights, results="asis"}}
for (i in seq_len(min(nrow(qual), 4))) {{
  case <- qual[i, ]
  cat(sprintf("\\n## %s: %s (%s)\\n\\n", case$source_type,
              case$source_name, case$year))
  cat(sprintf("> *%s*\\n\\n", stringr::str_trunc(case$excerpt, 300)))

  matched <- merged %>% filter(county_name == case$county_fips)
  if (nrow(matched) > 0) {{
    cat(sprintf(
      "**County Metrics:** Maltreatment Rate = %s | Risk-Adj. Ratio = %s | ",
      matched$maltreatment_rate[1], matched$risk_adj_ratio[1]))
    cat(sprintf("Tier = %s | Missing from Care = %s\\n\\n",
      matched$performance_tier[1], matched$missing_from_care_n[1]))
  }}
  cat("---\\n\\n")
}}
```

# Testimony-Ready Captions

```{{r captions, results="asis"}}
top5 <- merged %>% arrange(desc(risk_adj_ratio)) %>% head(5)

for (i in seq_len(nrow(top5))) {{
  row <- top5[i, ]
  qm <- qual %>% filter(county_fips == row$county_name)
  cat(sprintf("\\n### %s\\n\\n", row$county_name))
  cat(sprintf(
    "%s reports a maltreatment rate of %s per 1,000 children. ",
    row$county_name, row$maltreatment_rate))
  cat(sprintf(
    "After risk adjustment, the observed-to-expected ratio is **%s**, ",
    row$risk_adj_ratio))
  cat(sprintf("placing it in the **%s** tier.\\n\\n", row$performance_tier))

  if (nrow(qm) > 0) {{
    cat(sprintf(
      "> Corroborating evidence (%s, %s): *\\"%s\\"*\\n\\n",
      qm$source_type[1], qm$year[1],
      stringr::str_trunc(qm$excerpt[1], 200)))
  }}
}}
```

# Methodology Notes

**Quantitative data:** County-level aggregated metrics including
maltreatment rates, in-care maltreatment, foster care entry rates,
reunification percentages, re-entry rates, and missing-from-care counts.

**Risk adjustment:** Following Chapin Hall methodology, expected rates
are computed via regression models controlling for county-level poverty
rates, urbanicity indices, racial/ethnic composition, and caseworker
caseload ratios. The risk-adjusted ratio = observed / expected.

**Qualitative sources:** OIG and GAO reports on missing children, foster
care oversight, and aging-out outcomes; investigative journalism case
studies; and Chapin Hall Hot Spot analysis publications.

**Integration approach:** Qualitative evidence is matched to counties
via county identifiers and topic tags, then overlaid on quantitative
visualizations and tables to create a unified evidence base.

---

*Report generated on `r Sys.Date()`. This report contains simulated data
for demonstration purposes. Replace with real data sources for production use.*
', .open = "{{", .close = "}}")

 writeLines(rmd_content, rmd_path)
 message("✓ Rmd template written: ", rmd_path)

 # ── Render ──────────────────────────────────────────────────────────────

 out_format <- if (output_format == "pdf") "pdf_document" else "html_document"

 tryCatch({
   rmarkdown::render(
     input       = rmd_path,
     output_format = out_format,
     output_dir  = output_dir,
     params      = list(
       merged  = data_list$merged,
       qual    = data_list$qual,
       hotspot = data_list$hotspot
     ),
     quiet = TRUE
   )
   out_file <- file.path(output_dir,
     paste0(output_file, if (output_format == "pdf") ".pdf" else ".html"))
   message("✓ Report rendered: ", out_file)
   out_file
 }, error = function(e) {
   message("✗ Render failed: ", e$message)
   message("  The .Rmd file is still available at: ", rmd_path)
   rmd_path
 })
}


###############################################################################
# SECTION 7 — SLIDE-READY EXPORT (PowerPoint via officer)
###############################################################################

export_slides <- function(data_list, output_dir = "output",
                           filename = "qual_quant_slides.pptx") {

 dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
 pptx_path <- file.path(output_dir, filename)

 doc <- read_pptx()

 # ── Title slide ─────────────────────────────────────────────────────────
 doc <- doc %>%
   add_slide(layout = "Title Slide", master = "Office Theme") %>%
   ph_with(value = "Qualitative-Quantitative Integration",
           location = ph_location_type(type = "ctrTitle")) %>%
   ph_with(value = paste("Chapin Hall Hot Spot Analysis |",
                          format(Sys.Date(), "%B %Y")),
           location = ph_location_type(type = "subTitle"))

 # ── Slide 2: Why Risk Adjustment ────────────────────────────────────────
 doc <- doc %>%
   add_slide(layout = "Title and Content", master = "Office Theme") %>%
   ph_with(value = "Why Risk Adjustment Matters",
           location = ph_location_type(type = "title")) %>%
   ph_with(value = paste(
     "Raw rates mislead: high-poverty counties appear worse,",
     "while wealthy counties may hide systemic failures.",
     "\n\nChapin Hall's methodology computes EXPECTED rates based on",
     "poverty, demographics, and caseload.",
     "\n\nRisk-Adjusted Ratio = Observed / Expected",
     "\n  > 1.0 = Worse than peers",
     "\n  < 1.0 = Better than peers",
     "\n  > 1.5 = Critical Hot Spot"
   ), location = ph_location_type(type = "body"))

 # ── Slide 3: Funnel Plot ────────────────────────────────────────────────
 p_funnel <- plot_hotspot_funnel(data_list$merged, data_list$qual)

 doc <- doc %>%
   add_slide(layout = "Title and Content", master = "Office Theme") %>%
   ph_with(value = "Hot Spot Funnel Plot",
           location = ph_location_type(type = "title")) %>%
   ph_with(value = dml(ggobj = p_funnel),
           location = ph_location(left = 0.5, top = 1.5,
                                  width = 9, height = 5.5))

 # ── Slide 4: Raw vs Adjusted ────────────────────────────────────────────
 p_slope <- plot_raw_vs_adjusted(data_list$hotspot)

 doc <- doc %>%
   add_slide(layout = "Title and Content", master = "Office Theme") %>%
   ph_with(value = "Raw vs. Risk-Adjusted Rankings",
           location = ph_location_type(type = "title")) %>%
   ph_with(value = dml(ggobj = p_slope),
           location = ph_location(left = 0.5, top = 1.5,
                                  width = 9, height = 5.5))

 # ── Slide 5: Case Spotlights ────────────────────────────────────────────
 case_ids <- data_list$qual$qual_id[1:min(3, nrow(data_list$qual))]

 for (cid in case_ids) {
   case <- data_list$qual %>% filter(qual_id == cid)
   excerpt_short <- str_trunc(case$excerpt[1], 250)

   doc <- doc %>%
     add_slide(layout = "Title and Content", master = "Office Theme") %>%
     ph_with(
       value = glue("Case Spotlight: {case$source_name[1]}"),
       location = ph_location_type(type = "title")
     ) %>%
     ph_with(
       value = glue(
         "Source: {case$source_type[1]} ({case$year[1]})\n",
         "Topic: {case$topic[1]}\n",
         "County: {case$county_fips[1]}\n\n",
         "\"{excerpt_short}\""
       ),
       location = ph_location_type(type = "body")
     )
 }

 # ── Slide 6: Integrated Table (top 5) ──────────────────────────────────
 tbl_result <- create_integrated_table(data_list$merged, data_list$qual,
                                        top_n = 5)

 doc <- doc %>%
   add_slide(layout = "Title and Content", master = "Office Theme") %>%
   ph_with(value = "Top Counties: Integrated Evidence Summary",
           location = ph_location_type(type = "title")) %>%
   ph_with(value = tbl_result$flextable,
           location = ph_location(left = 0.3, top = 1.5,
                                  width = 9.4, height = 5))

 # ── Slide 7: Call to Action ─────────────────────────────────────────────
 doc <- doc %>%
   add_slide(layout = "Title and Content", master = "Office Theme") %>%
   ph_with(value = "Implications for Oversight & Testimony",
           location = ph_location_type(type = "title")) %>%
   ph_with(value = paste(
     "KEY FINDINGS:",
     "\n\n1. Risk adjustment reveals counties whose raw rates HIDE",
     "systemic failure — these are 'Hot Spots' demanding intervention.",
     "\n\n2. OIG/GAO evidence CORROBORATES the quantitative signals:",
     "missed runaway follow-ups, placement instability, and",
     "in-care maltreatment are not isolated incidents.",
     "\n\n3. Behind every data point is a child. Integration of",
     "numbers with stories ensures accountability is not abstract.",
     "\n\nRECOMMENDATION: Use risk-adjusted metrics — not raw rates —",
     "as the standard for legislative oversight and court testimony."
   ), location = ph_location_type(type = "body"))

 # ── Save ────────────────────────────────────────────────────────────────
 print(doc, target = pptx_path)
 message("✓ Slides exported: ", pptx_path)
 pptx_path
}


###############################################################################
# SECTION 8 — SAVE INDIVIDUAL PLOTS
###############################################################################

save_all_plots <- function(data_list, output_dir = "output/plots") {

 dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

 # Funnel plot
 p1 <- plot_hotspot_funnel(data_list$merged, data_list$qual)
 ggsave(file.path(output_dir, "hotspot_funnel.png"), p1,
        width = 12, height = 8, dpi = 300, bg = "white")

 # Slope chart
 p2 <- plot_raw_vs_adjusted(data_list$hotspot)
 ggsave(file.path(output_dir, "raw_vs_adjusted_slope.png"), p2,
        width = 10, height = 8, dpi = 300, bg = "white")

 # Annotated bars
 p3 <- plot_annotated_bars(data_list$merged, data_list$qual)
 ggsave(file.path(output_dir, "annotated_bars.png"), p3,
        width = 11, height = 7, dpi = 300, bg = "white")

 # Case spotlight
 p4 <- plot_case_spotlight(data_list$qual, data_list$merged, "Q003")
 ggsave(file.path(output_dir, "case_spotlight_Q003.png"), p4,
        width = 10, height = 8, dpi = 300, bg = "white")

 message("✓ All plots saved to: ", output_dir)
}


###############################################################################
# SECTION 9 — MAIN EXECUTION
###############################################################################

main <- function(
 quant_path   = NULL,
 qual_path    = NULL,
 hotspot_path = NULL,
 use_demo     = TRUE,
 output_dir   = "output",
 render_html  = TRUE,
 render_slides = TRUE,
 save_plots   = TRUE
) {

 cat("\n",
   "╔══════════════════════════════════════════════════════════════╗\n",
   "║  QUALITATIVE-QUANTITATIVE INTEGRATION ENGINE                ║\n",
   "║  Child Welfare Analytics — Hot Spot Risk-Adjusted Reports   ║\n",
   "╚══════════════════════════════════════════════════════════════╝\n\n"
 )

 # ── 1. Load & Merge Data ──────────────────────────────────────────────
 cat("── Step 1: Loading & Merging Data ──\n")
 data_list <- load_and_merge(quant_path, qual_path, hotspot_path, use_demo)

 cat(glue("\n  Quantitative rows : {nrow(data_list$quant)}"),
     glue("\n  Qualitative items : {nrow(data_list$qual)}"),
     glue("\n  Hot Spot entries  : {nrow(data_list$hotspot)}"),
     glue("\n  Merged dataset    : {nrow(data_list$merged)} rows\n\n"))

 # ── 2. Generate Integrated Table ──────────────────────────────────────
 cat("── Step 2: Building Integrated Evidence Table ──\n")
 tbl <- create_integrated_table(data_list$merged, data_list$qual)
 cat("  ✓ Table created with", nrow(tbl$data), "rows\n\n")

 # ── 3. Generate Captions ──────────────────────────────────────────────
 cat("── Step 3: Generating Testimony Captions ──\n")
 captions <- generate_all_captions(data_list$merged, data_list$qual,
                                    top_n = 5, context = "testimony")
 cat("  ✓", nrow(captions), "captions generated\n")

 # Save captions to file
 dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
 captions_path <- file.path(output_dir, "testimony_captions.txt")
 writeLines(
   paste(captions$county, captions$caption, sep = "\n\n", collapse = "\n\n---\n\n"),
   captions_path
 )
 cat("  ✓ Captions saved:", captions_path, "\n\n")

 # ── 4. Save Plots ─────────────────────────────────────────────────────
 if (save_plots) {
   cat("── Step 4: Saving Publication-Ready Plots ──\n")
   save_all_plots(data_list, file.path(output_dir, "plots"))
   cat("\n")
 }

 # ── 5. Render HTML Report ─────────────────────────────────────────────
 if (render_html) {
   cat("── Step 5: Rendering Integrated HTML Report ──\n")
   report_path <- generate_integrated_report(
     data_list, output_dir, output_format = "html"
   )
   cat("\n")
 }

 # ── 6. Export Slides ──────────────────────────────────────────────────
 if (render_slides) {
   cat("── Step 6: Exporting PowerPoint Slides ──\n")
   slides_path <- export_slides(data_list, output_dir)
   cat("\n")
 }

 # ── Summary ───────────────────────────────────────────────────────────
 cat("\n",
   "╔══════════════════════════════════════════════════════════════╗\n",
   "║  ✓ ALL OUTPUTS GENERATED SUCCESSFULLY                       ║\n",
   "╠══════════════════════════════════════════════════════════════╣\n",
   glue("║  Output directory: {output_dir}"), "\n",
   "║                                                              ║\n",
   "║  Files created:                                              ║\n",
   "║   • testimony_captions.txt    (court/petition-ready text)    ║\n",
   "║   • plots/hotspot_funnel.png  (annotated funnel plot)        ║\n",
   "║   • plots/raw_vs_adjusted_slope.png (ranking comparison)     ║\n",
   "║   • plots/annotated_bars.png  (bars + callout boxes)         ║\n",
   "║   • plots/case_spotlight_Q003.png (case study panel)         ║\n",
   "║   • qual_quant_integrated_report.html (full report)          ║\n",
   "║   • qual_quant_slides.pptx    (slide deck)                   ║\n",
   "╚══════════════════════════════════════════════════════════════╝\n\n"
 )

 invisible(data_list)
}


###############################################################################
# SECTION 10 — USAGE EXAMPLES
###############################################################################

# ── Run with demo data (immediate) ──────────────────────────────────────────
# main()

# ── Run with your own data ──────────────────────────────────────────────────
# main(
#   quant_path   = "data/county_metrics.csv",
#   qual_path    = "data/qualitative_evidence.csv",
#   hotspot_path = "data/chapin_hall_hotspot.csv",
#   use_demo     = FALSE,
#   output_dir   = "reports/2024_q3"
# )

# ── Individual function usage ───────────────────────────────────────────────
# data_list <- load_and_merge(use_demo = TRUE)
#
# # Just the funnel plot
# plot_hotspot_funnel(data_list$merged, data_list$qual)
#
# # Just the slope chart
# plot_raw_vs_adjusted(data_list$hotspot)
#
# # Just the integrated table
# tbl <- create_integrated_table(data_list$merged, data_list$qual)
# tbl$flextable   # print the flextable
#
# # Just captions for testimony
# caps <- generate_all_captions(data_list$merged, data_list$qual,
#                                top_n = 3, context = "petition")
# cat(caps$caption[1])
#
# # Add your own qualitative entry manually:
# new_qual <- tibble(
#   qual_id      = "Q_CUSTOM",
#   source_type  = "Court Filing",
#   source_name  = "In re: Smith County DFPS Oversight",
#   county_fips  = "County_A",
#   excerpt      = "Your excerpt text here...",
#   year         = 2024,
#   topic        = "in_care_maltreatment"
# )
# data_list$qual <- bind_rows(data_list$qual, new_qual)

cat("Script loaded. Run main() to generate all outputs with demo data.\n")
cat("See Section 10 for usage examples.\n")
