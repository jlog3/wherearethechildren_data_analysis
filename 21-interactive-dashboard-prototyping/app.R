# =============================================================================
# app.R
# Federal Child Welfare Transparency Dashboard — Prototype
# =============================================================================
# A single-file shinydashboard app with:
#   - Sidebar filters (state, year, age, race, placement)
#   - Overview tab with national KPI cards + combined trend
#   - Dedicated tabs for each of the four focal metrics
#   - Interactive plotly charts, leaflet choropleths, DT tables
#   - Download buttons (CSV data, PNG charts)
#   - Legislative context annotations
#
# Requirements:
#   install.packages(c("shiny","shinydashboard","shinycssloaders",
#     "plotly","leaflet","DT","dplyr","tidyr","lubridate",
#     "sf","tigris","scales","htmltools","networkD3","shinyWidgets"))
#
# Launch:  shiny::runApp(".")
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(htmltools)
library(sf)
library(networkD3)

# =============================================================================
# DATA LOADING
# =============================================================================
# Swap these readRDS() calls with your real data pipeline connections.
# Schema contracts are documented in generate_sample_data.R.
# =============================================================================

pop_df        <- readRDS("data/state_year_population.rds")
substance_agg <- readRDS("data/substance_removals.rds")
substance_ep  <- readRDS("data/substance_removals_episodes.rds")
infant_agg    <- readRDS("data/infant_entries.rds")
infant_ep     <- readRDS("data/infant_entries_episodes.rds")
missing_agg   <- readRDS("data/missing_episodes.rds")
missing_ep    <- readRDS("data/missing_episodes_detail.rds")
maltreat_agg  <- readRDS("data/incare_maltreatment.rds")
maltreat_ep   <- readRDS("data/incare_maltreatment_episodes.rds")
episode_all   <- readRDS("data/episode_level.rds")

# Pre-load US state shapefile for choropleths (cached after first download)
# tigris uses FIPS; our data includes fips column
us_states_sf <- tigris::states(cb = TRUE, year = 2022, class = "sf") %>%
  filter(!STATEFP %in% c("60","66","69","72","78")) %>%   # remove territories
  sf::st_transform(4326)

# Lookup vectors for filter choices
all_states     <- sort(unique(pop_df$state))
all_years      <- sort(unique(pop_df$year))
all_ages       <- c("< 1","1-5","6-12","13-17")
all_races      <- sort(unique(episode_all$race))
all_placements <- sort(unique(episode_all$placement))

# =============================================================================
# HELPER FUNCTIONS (modular — reuse across tabs)
# =============================================================================

#' Filter episode data by sidebar selections
filter_episodes <- function(df, states, yr_range, ages, races, plcmts) {
  df %>%
    filter(
      state     %in% states,
      year      >= yr_range[1] & year <= yr_range[2],
      age_group %in% ages,
      race      %in% races,
      placement %in% plcmts
    )
}

#' Build a national time-series plotly chart
make_trend_plot <- function(df, y_col, y_label, color = "#2c7fb8",
                            annotation_text = NULL, annotation_year = NULL) {
  nat <- df %>%
    group_by(year) %>%
    summarise(value = sum(.data[[y_col]], na.rm = TRUE), .groups = "drop")

  p <- plot_ly(nat, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
               line = list(color = color, width = 3),
               marker = list(color = color, size = 8),
               hovertemplate = paste0("<b>%{x}</b><br>", y_label,
                                      ": %{y:,.0f}<extra></extra>")) %>%
    layout(
      xaxis = list(title = "", dtick = 1, tickformat = "d"),
      yaxis = list(title = y_label, rangemode = "tozero"),
      margin = list(t = 40, b = 40),
      hovermode = "x unified"
    )

  # Optional legislative annotation

if (!is.null(annotation_text) && !is.null(annotation_year)) {
    p <- p %>% layout(annotations = list(list(
      x = annotation_year, y = nat$value[nat$year == annotation_year],
      text = annotation_text, showarrow = TRUE, arrowhead = 2,
      ax = 0, ay = -60, font = list(size = 10, color = "#555"),
      bgcolor = "#fff8dc", bordercolor = "#ccc", borderpad = 4
    )))
  }

  p %>% config(displayModeBar = TRUE, toImageButtonOptions = list(
    format = "png", filename = paste0("trend_", y_col), width = 1200, height = 500
  ))
}

#' Build a state choropleth via leaflet
make_choropleth <- function(agg_df, value_col, legend_title, palette = "YlOrRd") {
  # Summarize to state level (across selected years)
  state_vals <- agg_df %>%
    group_by(state, state_name, fips) %>%
    summarise(value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")

  # Join to shapefile
  map_data <- us_states_sf %>%
    left_join(state_vals, by = c("STATEFP" = "fips"))

  pal <- colorQuantile(palette, domain = map_data$value, n = 7, na.color = "#d9d9d9")

  leaflet(map_data, options = leafletOptions(minZoom = 3, maxZoom = 7)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
    addPolygons(
      fillColor   = ~pal(value),
      fillOpacity = 0.7,
      weight      = 1,
      color       = "#666",
      highlight   = highlightOptions(weight = 3, color = "#333", bringToFront = TRUE),
      label       = ~paste0(state_name, ": ", comma(value)),
      labelOptions = labelOptions(textsize = "13px", direction = "auto")
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = legend_title, opacity = 0.8)
}

#' Build a downloadable DT table
make_dt <- function(df) {
  datatable(
    df,
    extensions = "Buttons",
    options = list(
      dom       = "Bfrtip",
      buttons   = list(
        list(extend = "csv",  filename = "child_welfare_data"),
        list(extend = "excel", filename = "child_welfare_data"),
        list(extend = "copy")
      ),
      pageLength = 15,
      scrollX    = TRUE
    ),
    rownames = FALSE,
    class    = "compact stripe hover"
  ) %>%
    formatStyle(columns = names(df), fontSize = "13px")
}

#' KPI value box helper
kpi_box <- function(value, subtitle, icon_name = "child", color = "blue") {
  valueBox(
    value    = comma(value),
    subtitle = subtitle,
    icon     = icon(icon_name),
    color    = color,
    width    = 3
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  # --- Header ----------------------------------------------------------------
  dashboardHeader(
    title = tags$span(
      tags$img(src = "", height = "0px"),
      "Child Welfare Transparency Dashboard"
    ),
    titleWidth = 380,
    tags$li(class = "dropdown",
            tags$span(style = "color:#fff; padding:15px; font-size:12px;",
                      paste0("Prototype v1.0 | Simulated Data | ",
                             format(Sys.Date(), "%B %d, %Y"))))
  ),

  # --- Sidebar ---------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",             tabName = "overview",  icon = icon("dashboard")),
      menuItem("Substance Removals",   tabName = "substance", icon = icon("flask")),
      menuItem("Infant Entries",       tabName = "infant",    icon = icon("baby")),
      menuItem("Missing Episodes",     tabName = "missing",   icon = icon("search")),
      menuItem("In-Care Maltreatment", tabName = "maltreat",  icon = icon("exclamation-triangle")),
      menuItem("Episode Explorer",     tabName = "explorer",  icon = icon("table")),
      hr(),

      # --- Global filters ---
      pickerInput(
        "sel_states", "States",
        choices  = setNames(all_states, paste0(all_states, " — ", state_lookup[all_states])),
        selected = all_states,
        multiple = TRUE,
        options  = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          selectedTextFormat = "count > 3",
          countSelectedText  = "{0} states selected"
        )
      ),

      sliderInput("sel_years", "Year Range",
                  min = min(all_years), max = max(all_years),
                  value = c(min(all_years), max(all_years)),
                  step = 1, sep = "", ticks = FALSE),

      pickerInput("sel_ages", "Age Group",
                  choices = all_ages, selected = all_ages,
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),

      pickerInput("sel_races", "Race / Ethnicity",
                  choices = all_races, selected = all_races,
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),

      pickerInput("sel_placements", "Placement Type",
                  choices = all_placements, selected = all_placements,
                  multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                                          selectedTextFormat = "count > 2",
                                          countSelectedText  = "{0} types")),

      hr(),
      div(style = "padding: 0 15px; font-size: 11px; color: #999;",
          p("Data: Simulated AFCARS/NCANDS structure."),
          p("Replace with live feeds for production use."),
          p(tags$a(href = "https://www.acf.hhs.gov/cb/data-research",
                   target = "_blank", "ACF Data & Research"))
      )
    )
  ),

  # --- Body ------------------------------------------------------------------
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f7f9fc; }
      .box { border-top: 3px solid #3c8dbc; }
      .annotation-box {
        background: #fff8dc; border-left: 4px solid #f0ad4e;
        padding: 12px 16px; margin: 10px 0 20px 0;
        font-size: 13px; color: #555; border-radius: 4px;
      }
      .annotation-box strong { color: #c9302c; }
      .small-box .inner h3 { font-size: 28px; }
      .info-box-text { font-size: 13px; }
      .nav-tabs-custom > .tab-content { padding: 15px; }
    "))),

    tabItems(

      # =====================================================================
      # TAB: Overview
      # =====================================================================
      tabItem(tabName = "overview",
        fluidRow(
          column(12,
            h2("National Child Welfare Transparency Dashboard",
               style = "margin-top:0;"),
            div(class = "annotation-box",
                icon("info-circle"),
                HTML("<strong>Purpose:</strong> This prototype demonstrates how
                real-time federal dashboards could surface critical child welfare
                metrics for public accountability under FFPSA and proposed
                CAPTA reauthorization. All data shown is <em>simulated</em>
                to match AFCARS/NCANDS structures."))
          )
        ),
        # KPI cards
        fluidRow(
          uiOutput("kpi_substance"),
          uiOutput("kpi_infant"),
          uiOutput("kpi_missing"),
          uiOutput("kpi_maltreat")
        ),
        fluidRow(
          box(title = "Combined National Trends", width = 12,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("overview_trend", height = "400px")),
              div(class = "annotation-box", style = "margin-top:10px;",
                  icon("chart-line"),
                  "Tracking all four metrics together reveals whether
                   system-level interventions (e.g., FFPSA prevention services)
                   are shifting the overall burden of child welfare crises.")
          )
        ),
        fluidRow(
          box(title = "Metric Breakdown by Race/Ethnicity", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("overview_race", height = "350px"))),
          box(title = "Metric Breakdown by Placement Type", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("overview_placement", height = "350px")))
        )
      ),

      # =====================================================================
      # TAB: Substance Removals
      # =====================================================================
      tabItem(tabName = "substance",
        fluidRow(
          column(12,
            h2("Substance-Related Removals"),
            div(class = "annotation-box",
                icon("flask"),
                HTML("<strong>Legislative context:</strong> Live tracking of
                opioid-related removals would enable rapid FFPSA response and
                targeted allocation of prevention services under
                Title IV-E waivers."))
          )
        ),
        fluidRow(
          box(title = "National Trend — Substance Removals", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("sub_trend", height = "380px"))),
          box(title = "By Substance Type", width = 5,
              status = "warning", solidHeader = TRUE,
              withSpinner(plotlyOutput("sub_type_bar", height = "380px")))
        ),
        fluidRow(
          box(title = "State Choropleth — Total Substance Removals", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(leafletOutput("sub_map", height = "420px"))),
          box(title = "Top 15 States by Rate per 100k", width = 5,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("sub_state_bar", height = "420px")))
        ),
        fluidRow(
          box(title = "State-Year Data Table", width = 12, collapsible = TRUE,
              status = "info",
              downloadButton("dl_sub_csv", "Download CSV", class = "btn-sm"),
              br(), br(),
              withSpinner(DTOutput("sub_table")))
        )
      ),

      # =====================================================================
      # TAB: Infant Entries
      # =====================================================================
      tabItem(tabName = "infant",
        fluidRow(
          column(12,
            h2("Infant Foster Care Entries (< 1 Year)"),
            div(class = "annotation-box",
                icon("baby"),
                HTML("<strong>Legislative context:</strong> Monitoring infant
                entries with substance-exposure flags supports the CARA
                (Comprehensive Addiction and Recovery Act) requirement for
                Plans of Safe Care and early intervention referrals."))
          )
        ),
        fluidRow(
          box(title = "National Trend — Infant Entries", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("inf_trend", height = "380px"))),
          box(title = "Neonatal vs. Substance-Exposed", width = 5,
              status = "warning", solidHeader = TRUE,
              withSpinner(plotlyOutput("inf_flags", height = "380px")))
        ),
        fluidRow(
          box(title = "State Choropleth — Infant Entry Rate per 1,000 Under-1",
              width = 7, status = "primary", solidHeader = TRUE,
              withSpinner(leafletOutput("inf_map", height = "420px"))),
          box(title = "Entry Permanency Plan Distribution", width = 5,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("inf_plan_pie", height = "420px")))
        ),
        fluidRow(
          box(title = "State-Year Data Table", width = 12, collapsible = TRUE,
              status = "info",
              downloadButton("dl_inf_csv", "Download CSV", class = "btn-sm"),
              br(), br(),
              withSpinner(DTOutput("inf_table")))
        )
      ),

      # =====================================================================
      # TAB: Missing Episodes
      # =====================================================================
      tabItem(tabName = "missing",
        fluidRow(
          column(12,
            h2("Missing / Runaway / Abducted Episodes"),
            div(class = "annotation-box",
                icon("search"),
                HTML("<strong>Legislative context:</strong> The Preventing Sex
                Trafficking and Strengthening Families Act (P.L. 113-183)
                mandates reporting on missing children in care. Real-time
                dashboards would support immediate intervention and
                anti-trafficking outreach."))
          )
        ),
        fluidRow(
          box(title = "National Trend — Missing Episodes", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("miss_trend", height = "380px"))),
          box(title = "Episode Type Breakdown", width = 5,
              status = "danger", solidHeader = TRUE,
              withSpinner(plotlyOutput("miss_type", height = "380px")))
        ),
        fluidRow(
          box(title = "State Choropleth — Missing Episodes", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(leafletOutput("miss_map", height = "420px"))),
          box(title = "Days Missing Distribution", width = 5,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("miss_days_hist", height = "420px")))
        ),
        fluidRow(
          box(title = "Age × Missing Type Heatmap", width = 12,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("miss_age_heat", height = "300px")))
        ),
        fluidRow(
          box(title = "State-Year Data Table", width = 12, collapsible = TRUE,
              status = "info",
              downloadButton("dl_miss_csv", "Download CSV", class = "btn-sm"),
              br(), br(),
              withSpinner(DTOutput("miss_table")))
        )
      ),

      # =====================================================================
      # TAB: In-Care Maltreatment
      # =====================================================================
      tabItem(tabName = "maltreat",
        fluidRow(
          column(12,
            h2("Maltreatment While in Foster Care"),
            div(class = "annotation-box",
                icon("exclamation-triangle"),
                HTML("<strong>Legislative context:</strong> Federal oversight of
                in-care maltreatment is central to the Child Abuse Prevention
                and Treatment Act (CAPTA). Transparent dashboards would hold
                agencies accountable and drive system reform when children
                are re-victimized in state custody."))
          )
        ),
        fluidRow(
          box(title = "National Trend — In-Care Maltreatment", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("mal_trend", height = "380px"))),
          box(title = "By Maltreatment Type", width = 5,
              status = "danger", solidHeader = TRUE,
              withSpinner(plotlyOutput("mal_type_bar", height = "380px")))
        ),
        fluidRow(
          box(title = "State Choropleth — In-Care Maltreatment", width = 7,
              status = "primary", solidHeader = TRUE,
              withSpinner(leafletOutput("mal_map", height = "420px"))),
          box(title = "Perpetrator Breakdown", width = 5,
              status = "warning", solidHeader = TRUE,
              withSpinner(plotlyOutput("mal_perp", height = "420px")))
        ),
        fluidRow(
          box(title = "Severity × Substantiation", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("mal_severity", height = "300px"))),
          box(title = "Placement-Linked Sankey Flow", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(sankeyNetworkOutput("mal_sankey", height = "300px")))
        ),
        fluidRow(
          box(title = "State-Year Data Table", width = 12, collapsible = TRUE,
              status = "info",
              downloadButton("dl_mal_csv", "Download CSV", class = "btn-sm"),
              br(), br(),
              withSpinner(DTOutput("mal_table")))
        )
      ),

      # =====================================================================
      # TAB: Episode Explorer
      # =====================================================================
      tabItem(tabName = "explorer",
        fluidRow(
          column(12,
            h2("Episode-Level Data Explorer"),
            p("Drill into individual episodes across all four metrics.
               Filters from the sidebar apply. Use the search box and
               column sorting for ad hoc analysis."),
            downloadButton("dl_ep_csv", "Download Filtered Episodes (CSV)",
                           class = "btn-primary btn-sm"),
            br(), br(),
            withSpinner(DTOutput("ep_table"))
          )
        )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- Reactive filtered data ------------------------------------------------

  # Filtered episode-level data (used across tabs)
  ep_filtered <- reactive({
    filter_episodes(episode_all, input$sel_states, input$sel_years,
                    input$sel_ages, input$sel_races, input$sel_placements)
  })

  sub_ep_filt <- reactive({
    filter_episodes(substance_ep, input$sel_states, input$sel_years,
                    input$sel_ages, input$sel_races, input$sel_placements)
  })

  inf_ep_filt <- reactive({
    filter_episodes(infant_ep, input$sel_states, input$sel_years,
                    input$sel_ages, input$sel_races, input$sel_placements)
  })

  miss_ep_filt <- reactive({
    filter_episodes(missing_ep, input$sel_states, input$sel_years,
                    input$sel_ages, input$sel_races, input$sel_placements)
  })

  mal_ep_filt <- reactive({
    filter_episodes(maltreat_ep, input$sel_states, input$sel_years,
                    input$sel_ages, input$sel_races, input$sel_placements)
  })

  # Filtered aggregates (filter by state and year only — aggs don't have
  # demographic breakdowns, but episode-level charts do)
  sub_agg_filt <- reactive({
    substance_agg %>% filter(state %in% input$sel_states,
                             year >= input$sel_years[1],
                             year <= input$sel_years[2])
  })
  inf_agg_filt <- reactive({
    infant_agg %>% filter(state %in% input$sel_states,
                          year >= input$sel_years[1],
                          year <= input$sel_years[2])
  })
  miss_agg_filt <- reactive({
    missing_agg %>% filter(state %in% input$sel_states,
                           year >= input$sel_years[1],
                           year <= input$sel_years[2])
  })
  mal_agg_filt <- reactive({
    maltreat_agg %>% filter(state %in% input$sel_states,
                            year >= input$sel_years[1],
                            year <= input$sel_years[2])
  })

  # =========================================================================
  # OVERVIEW TAB
  # =========================================================================

  output$kpi_substance <- renderUI({
    n <- nrow(sub_ep_filt())
    kpi_box(n, "Substance Removals", "flask", "red")
  })

  output$kpi_infant <- renderUI({
    n <- nrow(inf_ep_filt())
    kpi_box(n, "Infant Entries", "baby", "yellow")
  })

  output$kpi_missing <- renderUI({
    n <- nrow(miss_ep_filt())
    kpi_box(n, "Missing Episodes", "search", "purple")
  })

  output$kpi_maltreat <- renderUI({
    n <- nrow(mal_ep_filt())
    kpi_box(n, "In-Care Maltreatment", "exclamation-triangle", "orange")
  })

  # Combined national trend — all 4 metrics stacked
  output$overview_trend <- renderPlotly({
    df <- ep_filtered() %>%
      group_by(year, metric) %>%
      summarise(n = n(), .groups = "drop")

    colors <- c("Substance Removal" = "#e41a1c",
                "Infant Entry"      = "#ff7f00",
                "Missing Episode"   = "#984ea3",
                "In-Care Maltreatment" = "#377eb8")

    plot_ly(df, x = ~year, y = ~n, color = ~metric, colors = colors,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{x}</b><br>%{y:,.0f}<extra>%{data.name}</extra>") %>%
      layout(
        xaxis     = list(title = "", dtick = 1, tickformat = "d"),
        yaxis     = list(title = "Episode Count", rangemode = "tozero"),
        legend    = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })

  # Race breakdown (all metrics)
  output$overview_race <- renderPlotly({
    df <- ep_filtered() %>%
      count(race, metric) %>%
      group_by(metric) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()

    plot_ly(df, x = ~metric, y = ~pct, color = ~race, type = "bar",
            text = ~paste0(pct, "%"), textposition = "none",
            hovertemplate = "%{x}<br>%{data.name}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = ""),
             yaxis = list(title = "Percent", range = c(0,100)),
             legend = list(orientation = "h", y = -0.2))
  })

  # Placement breakdown
  output$overview_placement <- renderPlotly({
    df <- ep_filtered() %>%
      count(placement, metric) %>%
      group_by(metric) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()

    plot_ly(df, x = ~metric, y = ~pct, color = ~placement, type = "bar",
            hovertemplate = "%{x}<br>%{data.name}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = ""),
             yaxis = list(title = "Percent", range = c(0,100)),
             legend = list(orientation = "h", y = -0.3, font = list(size = 10)))
  })

  # =========================================================================
  # SUBSTANCE REMOVALS TAB
  # =========================================================================

  output$sub_trend <- renderPlotly({
    make_trend_plot(sub_agg_filt(), "n_substance", "Substance Removals",
                    color = "#e41a1c",
                    annotation_text = "FFPSA enacted (2018)",
                    annotation_year = 2018)
  })

  output$sub_type_bar <- renderPlotly({
    df <- sub_ep_filt() %>% count(substance_type, sort = TRUE)
    plot_ly(df, x = ~reorder(substance_type, n), y = ~n, type = "bar",
            marker = list(color = "#e41a1c"),
            hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count")) %>%
      layout(xaxis = list(categoryorder = "total ascending"))
  })

  output$sub_map <- renderLeaflet({
    make_choropleth(sub_agg_filt(), "n_substance", "Total Removals", "YlOrRd")
  })

  output$sub_state_bar <- renderPlotly({
    df <- sub_agg_filt() %>%
      group_by(state, state_name) %>%
      summarise(avg_rate = round(mean(rate_per_100k, na.rm = TRUE), 1),
                .groups = "drop") %>%
      arrange(desc(avg_rate)) %>%
      head(15)

    plot_ly(df, x = ~avg_rate, y = ~reorder(state_name, avg_rate),
            type = "bar", orientation = "h",
            marker = list(color = "#e41a1c"),
            hovertemplate = "%{y}: %{x} per 100k<extra></extra>") %>%
      layout(xaxis = list(title = "Avg Rate per 100k Children"),
             yaxis = list(title = ""),
             margin = list(l = 140))
  })

  output$sub_table <- renderDT({
    make_dt(sub_agg_filt() %>%
              select(state_name, year, n_substance, n_opioid,
                     pct_opioid, rate_per_100k))
  })

  output$dl_sub_csv <- downloadHandler(
    filename = function() paste0("substance_removals_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(sub_agg_filt(), file, row.names = FALSE)
  )

  # =========================================================================
  # INFANT ENTRIES TAB
  # =========================================================================

  output$inf_trend <- renderPlotly({
    make_trend_plot(inf_agg_filt(), "n_infant", "Infant Entries",
                    color = "#ff7f00",
                    annotation_text = "CARA Plans of Safe Care required",
                    annotation_year = 2019)
  })

  output$inf_flags <- renderPlotly({
    df <- inf_agg_filt() %>%
      group_by(year) %>%
      summarise(Neonatal = sum(n_neonatal),
                `Substance Exposed` = sum(n_substance_exp),
                .groups = "drop") %>%
      pivot_longer(-year, names_to = "flag", values_to = "n")

    plot_ly(df, x = ~year, y = ~n, color = ~flag, type = "bar",
            hovertemplate = "%{x}: %{y:,.0f}<extra>%{data.name}</extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "", dtick = 1, tickformat = "d"),
             yaxis = list(title = "Count"))
  })

  output$inf_map <- renderLeaflet({
    # For infant map, use rate
    agg_rate <- inf_agg_filt() %>%
      group_by(state, state_name, fips) %>%
      summarise(rate_per_1000 = round(mean(rate_per_1000, na.rm = TRUE), 1),
                .groups = "drop")
    # Rename for choropleth helper
    make_choropleth(
      agg_rate %>% rename(n_infant = rate_per_1000),
      "n_infant", "Rate per 1,000 Under-1", "Oranges"
    )
  })

  output$inf_plan_pie <- renderPlotly({
    df <- inf_ep_filt() %>% count(plan_at_entry)
    plot_ly(df, labels = ~plan_at_entry, values = ~n, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#ff7f00","#ffbb78","#d62728","#aec7e8"))) %>%
      layout(showlegend = FALSE)
  })

  output$inf_table <- renderDT({
    make_dt(inf_agg_filt() %>%
              select(state_name, year, n_infant, n_neonatal,
                     n_substance_exp, rate_per_1000))
  })

  output$dl_inf_csv <- downloadHandler(
    filename = function() paste0("infant_entries_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(inf_agg_filt(), file, row.names = FALSE)
  )

  # =========================================================================
  # MISSING EPISODES TAB
  # =========================================================================

  output$miss_trend <- renderPlotly({
    make_trend_plot(miss_agg_filt(), "n_episodes", "Missing Episodes",
                    color = "#984ea3",
                    annotation_text = "P.L. 113-183 reporting mandate",
                    annotation_year = 2018)
  })

  output$miss_type <- renderPlotly({
    df <- miss_ep_filt() %>% count(missing_type, sort = TRUE)
    plot_ly(df, labels = ~missing_type, values = ~n, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#984ea3","#b2abd2","#e7d4e8"))) %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", y = -0.1))
  })

  output$miss_map <- renderLeaflet({
    make_choropleth(miss_agg_filt(), "n_episodes", "Missing Episodes", "Purples")
  })

  output$miss_days_hist <- renderPlotly({
    df <- miss_ep_filt()
    plot_ly(df, x = ~days_missing, type = "histogram",
            marker = list(color = "#984ea3", line = list(color = "#fff", width = 0.5)),
            nbinsx = 40,
            hovertemplate = "%{x} days: %{y} episodes<extra></extra>") %>%
      layout(xaxis = list(title = "Days Missing"),
             yaxis = list(title = "Episode Count"))
  })

  output$miss_age_heat <- renderPlotly({
    df <- miss_ep_filt() %>%
      count(age_group, missing_type) %>%
      pivot_wider(names_from = missing_type, values_from = n, values_fill = 0)

    mat_cols <- setdiff(names(df), "age_group")
    plot_ly(z = as.matrix(df[, mat_cols]),
            x = mat_cols, y = df$age_group,
            type = "heatmap", colorscale = "Purples",
            hovertemplate = "%{y}, %{x}: %{z}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Age Group"))
  })

  output$miss_table <- renderDT({
    make_dt(miss_agg_filt() %>%
              select(state_name, year, n_episodes, n_runaway,
                     n_trafficking, median_days, pct_resolved, rate_per_100k))
  })

  output$dl_miss_csv <- downloadHandler(
    filename = function() paste0("missing_episodes_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(miss_agg_filt(), file, row.names = FALSE)
  )

  # =========================================================================
  # IN-CARE MALTREATMENT TAB
  # =========================================================================

  output$mal_trend <- renderPlotly({
    make_trend_plot(mal_agg_filt(), "n_incidents", "In-Care Maltreatment",
                    color = "#377eb8",
                    annotation_text = "CAPTA reauthorization discussions",
                    annotation_year = 2021)
  })

  output$mal_type_bar <- renderPlotly({
    df <- mal_ep_filt() %>% count(maltreatment_type, sort = TRUE)
    plot_ly(df, x = ~reorder(maltreatment_type, n), y = ~n, type = "bar",
            marker = list(color = "#377eb8"),
            hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "", categoryorder = "total ascending"),
             yaxis = list(title = "Count"))
  })

  output$mal_map <- renderLeaflet({
    make_choropleth(mal_agg_filt(), "n_incidents", "In-Care Incidents", "Blues")
  })

  output$mal_perp <- renderPlotly({
    df <- mal_ep_filt() %>% count(perpetrator, sort = TRUE)
    plot_ly(df, labels = ~perpetrator, values = ~n, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = RColorBrewer::brewer.pal(5, "Set2"))) %>%
      layout(showlegend = FALSE)
  })

  output$mal_severity <- renderPlotly({
    df <- mal_ep_filt() %>%
      count(severity, substantiated) %>%
      mutate(substantiated = ifelse(substantiated, "Substantiated", "Unsubstantiated"))

    plot_ly(df, x = ~severity, y = ~n, color = ~substantiated, type = "bar",
            hovertemplate = "%{x}<br>%{data.name}: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Severity"),
             yaxis = list(title = "Count"))
  })

  # Sankey: Placement → Maltreatment Type → Severity
  output$mal_sankey <- renderSankeyNetwork({
    df <- mal_ep_filt()

    # Simplify placements for readability
    df <- df %>%
      mutate(plc_short = case_when(
        grepl("Relative", placement) ~ "Kin Foster",
        grepl("Non-Relative", placement) ~ "Non-Kin Foster",
        grepl("Group", placement) ~ "Group Home",
        grepl("Institution", placement) ~ "Institution",
        TRUE ~ "Other"
      ))

    # Build link tables
    link1 <- df %>% count(plc_short, maltreatment_type) %>%
      rename(source = plc_short, target = maltreatment_type, value = n)
    link2 <- df %>% count(maltreatment_type, severity) %>%
      rename(source = maltreatment_type, target = severity, value = n)

    links <- bind_rows(link1, link2)

    # Node list
    nodes <- data.frame(name = unique(c(links$source, links$target)),
                        stringsAsFactors = FALSE)
    links$source_id <- match(links$source, nodes$name) - 1
    links$target_id <- match(links$target, nodes$name) - 1

    sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "source_id", Target = "target_id", Value = "value",
      NodeID = "name", fontSize = 12, nodeWidth = 25,
      sinksRight = TRUE
    )
  })

  output$mal_table <- renderDT({
    make_dt(mal_agg_filt() %>%
              select(state_name, year, n_incidents, n_substantiated,
                     pct_substantiated, n_severe, rate_per_100k))
  })

  output$dl_mal_csv <- downloadHandler(
    filename = function() paste0("incare_maltreatment_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(mal_agg_filt(), file, row.names = FALSE)
  )

  # =========================================================================
  # EPISODE EXPLORER TAB
  # =========================================================================

  output$ep_table <- renderDT({
    make_dt(ep_filtered() %>%
              select(episode_id, metric, state_name, report_date, year,
                     age_group, race, placement, sex, duration_days) %>%
              arrange(desc(report_date)))
  })

  output$dl_ep_csv <- downloadHandler(
    filename = function() paste0("episodes_filtered_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(ep_filtered(), file, row.names = FALSE)
  )

} # end server


# =============================================================================
# LAUNCH
# =============================================================================

shinyApp(ui = ui, server = server)
