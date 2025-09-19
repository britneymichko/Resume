# Resume
Portfolio: business analytics (M.S.) and real estate (B.B.A.)—ML models, comps, forecasting, and reporting.


# Real Estate Development & Analysis

Applied Business Analytics + Real Estate project using Excel-based modeling/analysis (valuation, comps, sensitivity).

## What’s inside
- **Workbook:** `data/raw/Real Estate Development & Analysis.xlsx`
- **Outputs:** cleaned tables in `data/processed/` and charts in `figures/`

## How to view
- Open the Excel file and review the summary and model tabs.

## Method highlights (edit these)
- Valuation approach: [e.g., DCF / comps / pro-forma]
- Key drivers: [price/sqft, absorption, cap rate, construction costs]
- Sensitivity: [e.g., ±10% rent, ±50 bps cap → value delta of $X]

## Results (one-liners for recruiters)
- Modeled base-case IRR: **X%**; equity multiple: **Yx**
- Value most sensitive to **[driver]**; ±10% changes move NPV by **$Z**
- Recommended go/no-go threshold met **[yes/no]** under base case

## Data notes
- Data is **anonymized/synthetic** or from public sources.
- Proprietary inputs removed; see `data/README.md` for sources.


##RStudio
# Insurance Demographic Segmentation (Shiny App)

Interactive Shiny dashboard built in **R** for exploring insurance customer demographics and claim patterns.  
Part of my M.S. Applied Business Analytics coursework (BAN 5100: Data Visualization).

## What it does
- Lets the user upload an insurance dataset (CSV).
- Dynamically creates demographic segments based on categorical variables such as:
  - Gender, Education, Marital status, Retirement status
  - Claim type (e.g., Wind/Hail, Theft/Vandalism, Fire/Smoke, etc.)
  - Fraud status, Town size, Primary residence, Deductible level
- Allows selection of key performance variables (e.g., claim amount, income, age).
- Generates multiple interactive visualizations:
  - **Pie chart** of totals or counts by demographic group  
  - **Bar chart** of categorical distributions with % labels  
  - **Jitter plots** to view spread of continuous variables by groups  
  - **Scatterplots** with two categorical variables for coloring  
  - **Income distribution bar plots** by group  

## How to run
1. Open RStudio (with Shiny installed).  
2. Install/load dependencies:
   ```r
   install.packages(c("shiny","ggplot2","dplyr","RColorBrewer","readr"))


# Education, Retirement & Marital Segmentation — Insurance Dataset (R)

Three publication-ready charts analyzing how **education level** relates to **retirement status**, **claim amounts**, and **marital status**.

## What this script produces
1) **Education × Retirement (% stacked/dodged bar)** — percent distribution of education levels within each retired/not-retired group.  
2) **Claim Amounts by Education (column chart)** — total/mean claim amounts by education category.  
3) **Education × Marital Status (% by group)** — percent distribution of education levels within married vs. not married (faceted).

## Files
- `week 5 data visualization.R` — this script 
- `data/insurance.csv` — input CSV (see schema below)

## How to run
1. Put your CSV at `data/insurance.csv`.  
2. Open R / RStudio and run:
   ```r
   install.packages(c("tidyverse","RColorBrewer","scales"))
   source("analysis_education_segments.R")
