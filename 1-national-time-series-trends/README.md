# National Child Welfare Time-Series Trends — R Workflow

## Overview

This R script (`national_child_welfare_trends.R`) produces national time-series analyses for four focal child welfare metrics from AFCARS and NCANDS data, spanning FY 2000–2025.

## Four Focal Metrics

| # | Metric | Data Source | Derivation |
|---|--------|------------|------------|
| 1 | **Substance-Related Removals** | AFCARS | `DAParent=1 OR DAChild=1 OR AAParent=1 OR AAChild=1` (includes prenatal exposure) |
| 2 | **Infant/Newborn Entries** | AFCARS | `AgeAtLatRem < 1` (age at removal under 1 year) |
| 3 | **Missing/Runaway Episodes** | AFCARS | `CurPlSet=7` (current placement = runaway) OR `DISREASN=7` (discharge reason = runaway) |
| 4 | **In-Care Maltreatment Rate** | NCANDS + AFCARS | NCANDS substantiated reports (`MALLEV=1`) where perpetrator is foster parent (`PER*REL ∈ {03,04,33}`) or child living in foster care (`CHLVNG ∈ {10,11}`), divided by AFCARS children in care |

## Required Directory Structure

```
project/
├── data/
│   ├── afcars/          # Annual AFCARS FC files (.rds or .csv)
│   │   ├── afcars_2000.csv
│   │   ├── afcars_2001.rds
│   │   └── ...
│   ├── ncands/          # NCANDS Child File(s) (.rds or .csv)
│   │   └── ncands_child_2000_2025.csv
│   └── population/      # State-year child population
│       └── child_pop_2000_2025.csv
├── output/              # Created automatically
│   ├── plots/
│   ├── tables/
│   └── animations/
└── national_child_welfare_trends.R
```

## Expected Column Names

### AFCARS Foster Care Files
| Column | Description | Type |
|--------|-------------|------|
| `FY` | Fiscal year (derived from filename if absent) | numeric |
| `STATE` | State FIPS code | numeric |
| `RECNUMBR` / `StFCID` | Unique child-record ID | character |
| `DOB` | Date of birth | date |
| `LatRemDt` | Date of latest removal | date |
| `DAParent` | Drug abuse – parent (0/1) | numeric |
| `DAChild` | Drug abuse – child/prenatal (0/1) | numeric |
| `AAParent` | Alcohol abuse – parent (0/1) | numeric |
| `AAChild` | Alcohol abuse – child (0/1) | numeric |
| `AgeAtLatRem` | Age at latest removal (years) | numeric |
| `CurPlSet` | Current placement setting code | numeric |
| `DISREASN` | Discharge reason code | numeric |
| `TotalRem` | Total number of removals | numeric |

### NCANDS Child File
| Column | Description | Type |
|--------|-------------|------|
| `RptYr` | Report year | numeric |
| `CHAGE` | Child age | numeric |
| `MALLEV` | Maltreatment disposition level (1=substantiated) | numeric |
| `PER1REL`–`PER3REL` | Perpetrator relationship codes | numeric |
| `CHLVNG` | Child living arrangement code | numeric |

### Population File
| Column | Description |
|--------|-------------|
| `FY` / `year` | Year |
| `state_fips` | State FIPS |
| `pop_under1` | Population under age 1 |
| `pop_0_17` | Population ages 0–17 |

## Outputs

### Plots (`output/plots/`)
| File | Description |
|------|-------------|
| `01_four_metric_counts_faceted.png` | 4-panel faceted line chart (counts) |
| `02_rates_per1k_overlay.png` | Overlaid rates per 1,000 with policy annotations |
| `03_substance_sub_breakdown.png` | Stacked area: DA/AA parent/child sub-flags |
| `04_infant_substance_overlap.png` | Infant entries with substance co-occurrence |
| `05_interactive_trends.html` | Plotly interactive faceted chart |
| `06_interactive_rates.html` | Plotly interactive rates chart |
| `07_composite_dashboard.png` | Patchwork 4-panel dashboard |

### Tables (`output/tables/`)
| File | Description |
|------|-------------|
| `national_trends_wide.csv` | Wide-format: all metrics by FY |
| `national_trends_annotated.csv` | Long-format with legislative captions |
| `trend_summary_statistics.csv` | Summary stats + % change + policy captions |

### Animations (`output/animations/`)
| File | Description |
|------|-------------|
| `trends_animated.gif` | gganimate reveal of all four metrics |

## Dependencies

```r
install.packages(c("tidyverse", "lubridate", "plotly", "gganimate",
                   "scales", "patchwork", "glue", "janitor", "gifski"))
```

## Policy Event Annotations

The script overlays dashed vertical lines for key policy milestones:

- **2010** — ACA / Health-care expansion
- **2016** — Peak opioid overdose deaths
- **2018** — Family First Prevention Services Act (FFPSA, P.L. 115-123)
- **2020** — COVID-19 pandemic onset
- **2023** — FFPSA prevention services ramp-up

## Notes

- **Missing data**: The script uses `tidyr::complete()` to fill FY gaps with `NA`, so plots show breaks rather than false interpolation.
- **Fallback population**: If no population CSV is found, approximate Census estimates are generated internally.
- **Column harmonisation**: `janitor::clean_names()` + explicit renaming handles common AFCARS column name variants across file vintages.
- **NCANDS linkage**: Because child-level linkage keys vary by state, Metric 4 uses an aggregate approach (national NCANDS numerator ÷ AFCARS denominator).
