# IJC437-coursework-rail-sector-recovery
# Passenger Journeys by Rail Sector (UK, 2015–2025)

Exploratory data science analysis of UK rail passenger journeys by sector using quarterly data (2015 onwards).  
The project examines long-term trends, the impact of COVID-19, and post-pandemic recovery patterns across rail sectors.

## Overview

This repository contains an end-to-end R workflow analysing quarterly passenger journeys across UK rail sectors, including:

- Long-term trend analysis (2015–2025)
- Pre- vs post-COVID comparisons
- Sector-specific recovery patterns indexed to a 2019 baseline
- Regression modelling to quantify COVID and trend effects

The analysis is designed to demonstrate clear data cleaning, visualisation, and statistical modelling practices suitable for academic coursework and portfolio presentation.

## Research Questions

- **RQ1:** How did passenger journeys evolve across rail sectors from 2015 onwards?
- **RQ2:** What was the magnitude of the COVID-19 shock across different rail sectors?
- **RQ3:** How do pre-COVID and post-COVID passenger levels differ by sector?
- **RQ4:** Which rail sectors have recovered most strongly relative to 2019 baseline levels?
- **RQ5:** What is the estimated impact of COVID and long-term trends on passenger journeys after controlling for sector effects?

## Data

- Quarterly UK rail passenger journey data by sector  
- Source: Official UK rail statistics (CSV format)
- Time span: 2015 onwards

Raw data are stored in the `data/` directory and are cleaned within the analysis scripts.


## Key Outputs

- **Figure 1:** Passenger journeys by rail sector (quarterly, 2015+) with COVID onset marked  
- **Figure 2:** Pre-COVID vs post-COVID passenger journeys by sector (boxplots)  
- **Figure 3:** Sector recovery trajectories indexed to 2019 = 100  
- **Regression table:** Estimated effects of long-term trend, COVID period, and sector  
- **Summary tables:** COVID impact and recovery ratios by sector  

All outputs are automatically saved to the `output/` folder.

## How to Run the Analysis

1. Install required R packages:
   ```r
   install.packages(c("ggplot2", "viridis", "dplyr", "broom"))
2. Clone or download this repository.

3. Set your working directory to the project root.

4. Run the analysis script:

          source("scripts/ijc437_rail_analysis.R")


All figures and tables will be generated automatically.

