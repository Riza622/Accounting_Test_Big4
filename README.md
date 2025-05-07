# Bank Auditors Analysis

This repository contains R scripts for an accounting test analyzing bank–auditor relationships (2005–2015). The analysis covers data cleaning, exploratory summaries, Big4 auditor patterns, regression models, and spatial visualization.

## Project Structure

* `bankauditors.csv` — raw dataset with bank IDs, auditor names, assets, public status, state, and year.
* `EDA_and_Tables.R` — EDA: missing values, unique counts, auditor patterns, summary tables (LaTeX outputs in `include/`).
* `Big4_Patterns_and_Switches.R` — identifies Big4 vs. non-Big4, counts auditor switches, summary statistics.
* `Plots_and_Figures.R` — generates visualizations:

  * Big4 share by public status and trend (PDF: `figure1_combined_big4_share.pdf`)
  * Market share decomposition, asset flows, and state-level maps.
* `Regressions.R` — fits clustered linear and logistic (feglm) models on Big4 propensity; outputs LaTeX table in `include/public_regressions.tex`.

## Dependencies

```r
install.packages(c("dplyr","stringr","ggplot2","scales","gt","patchwork","fixest","modelsummary","maps","sf","tigris"))
```

## Analysis Steps

1. **Load & Inspect Data**: import CSV, check dimensions, missingness, bank and auditor counts.
2. **EDA**: compute full-year banks, disappearance by year, auditor changes, public status changes, assets summary by group.
3. **Auditor Patterns**: detect malformed names, extract Big4 name variants, flag Big4 vs. non-Big4 and count switches.
4. **Visualization**:

   * Bar/line plots of Big4 usage by listing status and year.
   * Asset flow entering/exiting Big4 and market share trends.
   * Geographical maps of assets and Big4 share by state.
5. **Regression Modeling**: estimate propensity to hire Big4 controlling for assets, state, and fixed effects; export LaTeX tables.

## Outputs

* LaTeX tables in `include/` folder for EDA summaries, Big4 patterns, and regression results.
* PDF figures (e.g., `figure1_combined_big4_share.pdf`).
* CSV summary (`bankstate_summary.csv`) with state-level metrics in the working directory.

---

*Riza Saireke — May 2025*
