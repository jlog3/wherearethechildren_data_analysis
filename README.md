# Legislative Briefing Roadmap

## Priority Guidance

### Key Legislator Concerns (Lead With These)

- **#9 Economic Costs / ROI** — Legislators think in dollars. Even a placeholder line like "estimated annual cost of system failures" gives fiscal weight.
- **#2 State Variation** — Shows this is a federal problem, not a local one. Justifies federal legislation specifically.
- **#3 Racial Disparities** — Frames transparency as an equity tool, which broadens the coalition (civil rights orgs, CBC, etc.)

### Worth a Mention (Less Critical on Page 1)

- **#8 Policy Precedent** — FFPSA worked → dashboards would too.
- **#11 Underreporting Gaps** — Already implied by the 69% stat.

> **Skip** the predictive modeling, workforce, and cross-system linkage stuff on the one-pager — those are for the full briefing packet, not the door-opener.

### Where to Focus Your Effort

The categories that need the **most original work** (and you should prioritize) are **#9 (economic costs/ROI)** and the **feasibility/implementation details** — those are what staffers will poke at hardest. The data visualization and disparity analyses are compelling but they're also the parts you can generate most efficiently with your R pipeline. The fiscal and "how would this actually work" sections are where legislators decide if a bill is real or aspirational.

> **You don't need all 12 categories finished to have a strong packet.** A focused 8-page document covering 6–7 of them well beats a 15-page document that's thin everywhere.
>
> **Prioritize for v1:** #1 (Scale), #2 (State Variation), #3 (Equity), #5 (Safety Failures), #8 (Policy Precedent), #9 (Costs), #11 (Data Quality Gaps). The predictive modeling and cross-system linkage work can come later as supplemental analyses.

---

## The 12 Analytical Categories

### 1. Scale and National Trends

- **Purpose:** Prove the massive, ongoing scale of the four problems (opioid/substance removals, newborn/infant entries, missing/runaway episodes, in-care maltreatment) to establish urgency.
- **Prompts:** 1 (time-series trends), 15 (projections), 25 (real-time crisis simulation)
- **Key Outputs:** Animated national trend lines, volume estimates (e.g., 50k+ substance cases/year), future projections without intervention.
- **Legislative Hook:** *"Tens of thousands of children affected annually — real-time dashboards would make this visible immediately."*

### 2. State Variation and Performance Gaps

- **Purpose:** Show inconsistent state practices and justify federal standardization.
- **Prompts:** 2 (state rankings & choropleth maps), 22 (high- vs. low-performing benchmarks)
- **Key Outputs:** State maps, ranked tables, coefficient of variation stats.
- **Legislative Hook:** *"Some states report far higher rates than others — national dashboards would drive best-practice sharing."*

### 3. Demographic, Racial, and Equity Disparities

- **Purpose:** Demonstrate disproportionate impacts and frame transparency as an equity tool.
- **Prompts:** 3 (demographic breakdowns), 16 (intersectional analyses), 19 (Tribal/special populations), 24 (synthetic cohort lifetime risks)
- **Key Outputs:** Rate ratios, cumulative risk curves by race/ethnicity (especially AI/AN and Black children), intersection heatmaps.
- **Legislative Hook:** *"Disparities are stark and compounded — public data would enable targeted, equitable responses."*

### 4. High-Risk Populations and Infants/Newborns

- **Purpose:** Spotlight the most vulnerable (newborns with prenatal exposure, substance-affected infants) and intersectional risks.
- **Prompts:** 4 (infant-specific), 16 (intersectional), 19 (Tribal/rural/LGBTQ+)
- **Key Outputs:** Sankey flows for infants, conditional probabilities (e.g., substance-exposed infants who go missing).
- **Legislative Hook:** *"Newborns entering care at birth need immediate visibility for prevention services."*

### 5. System Safety Failures (Missing Children & In-Care Maltreatment)

- **Purpose:** Document risks to children who were supposed to be protected.
- **Prompts:** 6 (missing/runaway details), 7 (maltreatment in foster care), 14 (underreporting audits)
- **Key Outputs:** Episode duration curves, placement-type comparisons, gap analyses vs. OIG audits.
- **Legislative Hook:** *"Children removed for safety should not disappear or face new harm — dashboards would enable rapid response and accountability."*

### 6. Pathways, Flows, and Child Trajectories

- **Purpose:** Reveal how problems move through the system and where interventions can occur.
- **Prompts:** 8 (longitudinal trajectories), 12 (Sankey/alluvial diagrams)
- **Key Outputs:** Interactive pathway diagrams from entry → missing/maltreatment → exit.
- **Legislative Hook:** *"Visualizing system leakage shows exactly where transparency can break harmful cycles."*

### 7. Root Causes, Linkages, and External Drivers

- **Purpose:** Connect child welfare issues to opioids, workforce strain, health, justice, and fatalities.
- **Prompts:** 5 (substance linkages to CDC/ACS data), 13 (cross-system linkages: Medicaid, JJ, fatalities), 17 (workforce correlations)
- **Key Outputs:** Correlation maps, overlap percentages, regression results.
- **Legislative Hook:** *"Better data integration via dashboards would support true cross-agency prevention."*

### 8. Policy Impact and Lessons Learned

- **Purpose:** Evaluate what works (FFPSA, CAPTA, licensing changes) and show the value of data-driven oversight.
- **Prompts:** 11 (pre/post policy evaluations), 22 (comparative benchmarks including international)
- **Key Outputs:** Event-study plots, before/after effect sizes, high-performer comparisons (U.S. states + England/Australia).
- **Legislative Hook:** *"Proven policy wins are limited by poor visibility — real-time dashboards would accelerate and scale successes."*

### 9. Economic Costs and Return on Investment

- **Purpose:** Build the fiscal case for investing in mandated dashboards.
- **Prompts:** 9 (economic costing), 20 (cost-benefit & ROI modeling)
- **Key Outputs:** National/state cost estimates, ROI scenarios, benefit-cost ratios.
- **Legislative Hook:** *"Every dollar in transparency infrastructure could save multiple dollars in placements, searches, and long-term harm."*

### 10. Predictive Insights and Risk Profiling

- **Purpose:** Show how dashboards could move from reactive to proactive.
- **Prompts:** 10 (predictive/risk modeling)
- **Key Outputs:** Variable importance plots, risk score distributions, model performance metrics.
- **Legislative Hook:** *"Public risk dashboards would flag high-risk placements and children in real time."*

### 11. Data Quality, Underreporting, and Transparency Gaps

- **Purpose:** Expose current invisibility problems to argue for real-time public reporting.
- **Prompts:** 14 (underreporting audits), 25 (real-time simulation)
- **Key Outputs:** Reported vs. audited gaps, animated "what live data would have shown" during opioid surges.
- **Legislative Hook:** *"Tens of thousands of cases slip through current delayed systems — mandated live dashboards close this dangerous gap."*

### 12. Visualization, Prototyping, and Storytelling

- **Purpose:** Make the solution tangible and the evidence compelling for non-technical audiences.
- **Prompts:** 21 (Shiny/flexdashboard prototype), 23 (qual-quant integration with case studies & Chapin Hall Hot Spots), 25 (crisis simulations)
- **Key Outputs:** Working dashboard app, annotated reports blending stats + real stories, animated live-feed demos.
- **Legislative Hook:** *"Here is exactly what the proposed public dashboards would look like — clear, actionable, and overdue."*

---

## Master Reference Table: Angles vs. Prompts

| Angle # | Angle Name                    | Primary Prompts   | Secondary Prompts | Best Outputs for Petition/Bill        |
|:-------:|-------------------------------|-------------------|-------------------|---------------------------------------|
| 1       | Scale & National Trends       | 1, 15, 25         | —                 | Trend animations, volume tables       |
| 2       | State Variation               | 2, 22              | —                 | Maps, rankings                        |
| 3       | Equity & Disparities          | 3, 16, 19, 24     | —                 | Lifetime risk curves                  |
| 4       | High-Risk Groups (Infants)    | 4, 16, 19         | —                 | Infant Sankey flows                   |
| 5       | Safety Failures               | 6, 7, 14          | —                 | Missing & maltreatment rates          |
| 6       | System Pathways               | 8, 12              | —                 | Alluvial diagrams                     |
| 7       | Root Causes & Linkages        | 5, 13, 17         | —                 | Cross-system regressions              |
| 8       | Policy Impact                 | 11, 22             | —                 | Pre/post plots                        |
| 9       | Economic & ROI                | 9, 20              | —                 | Cost-benefit tables                   |
| 10      | Predictive Risk               | 10                 | —                 | Risk model visuals                    |
| 11      | Data Gaps & Underreporting    | 14, 25             | —                 | Gap analyses & simulations            |
| 12      | Visualization & Prototyping   | 21, 23, 25        | 12, 22            | Shiny app + integrated reports        |
