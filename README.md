# 311_Service_Requests
This project analyzes NYC 311 service request patterns to identify geographic disparities in complaint types and response times across boroughs, using statistical analysis, Random Forest modeling, and an interactive Shiny dashboard to support data-driven municipal decision-making.

## Transforming Reactive City Services into Predictive Municipal Management

[![R](https://img.shields.io/badge/R-4.3+-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive_Dashboard-13B5EA?style=for-the-badge&logo=rstudio&logoColor=white)](https://candacegrant2025.shinyapps.io/NYC_311_Requests/)
[![NYC Open Data](https://img.shields.io/badge/NYC_Open_Data-API-FF6F00?style=for-the-badge&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAA0SURBVDhPY2RgYPgPxAwMDAyMQAwGYAwmBgYGBkYGBgYGJigbisFsZIApgsJRjaNgqAIGBgYAhE4B9XHWMXYAAAAASUVORK5CYII=)](https://data.cityofnewyork.us/)
[![License](https://img.shields.io/badge/License-Academic-green?style=for-the-badge)](LICENSE)

---


## 📋 Table of Contents

- [Executive Summary](#-executive-summary)
- [Research Questions](#-research-questions)
- [Data Sources](#-data-sources)
- [Methodology](#-methodology)
- [Key Findings](#-key-findings)
- [Machine Learning Model](#-machine-learning-model)
- [Interactive Dashboard](#-interactive-dashboard)
- [Technologies Used](#-technologies-used)
- [Project Structure](#-project-structure)
- [Conclusions & Policy Implications](#-conclusions--policy-implications)
- [References](#-references)
- [Author](#-author)

---

## 📊 Executive Summary

This project analyzes **NYC 311 service request patterns** to identify geographic disparities in complaint types and response times across boroughs. By integrating **weather data** and **socioeconomic demographics**, the analysis reveals how environmental conditions and community characteristics influence municipal service demand and delivery.

The project delivers three core components:

| Component | Description |
|-----------|-------------|
| 🔍 **Exploratory Analysis** | Statistical examination of 100,000+ service requests |
| 🤖 **Predictive Model** | Random Forest algorithm achieving 71% variance explained |
| 📱 **Interactive Dashboard** | Real-time Shiny application for stakeholder exploration |

> **Key Insight:** The clustering of requests at specific addresses reveals a predictive opportunity—rather than treating each complaint as an independent event, city agencies could use geographic hotspot analysis and complaint frequency modeling to proactively allocate resources and address root causes before repeat complaints occur.

---

## ❓ Research Questions

This analysis addresses four primary research questions:

```
┌─────────────────────────────────────────────────────────────────────────┐
│  1. How do 311 complaint patterns vary across NYC's five boroughs?      │
├─────────────────────────────────────────────────────────────────────────┤
│  2. What factors influence response time for complaint resolution?      │
├─────────────────────────────────────────────────────────────────────────┤
│  3. How do weather conditions correlate with complaint volume/type?     │
├─────────────────────────────────────────────────────────────────────────┤
│  4. Do socioeconomic factors predict complaint rates across boroughs?   │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 📁 Data Sources

### Primary Data: NYC 311 Service Requests

| Attribute | Details |
|-----------|---------|
| **Source** | NYC Open Data Socrata API |
| **Endpoint** | `https://data.cityofnewyork.us/resource/erm2-nwe9.json` |
| **Records** | 100,000+ service requests |
| **Time Period** | Rolling recent data (2024-2025) |
| **Key Variables** | Complaint type, borough, created/closed dates, coordinates, status |

### Supplementary Data: Weather

| Attribute | Details |
|-----------|---------|
| **Source** | Open-Meteo Historical Weather API |
| **Variables** | Daily temperature (max, min, mean), precipitation |
| **Purpose** | Correlate environmental conditions with complaint patterns |

### Supplementary Data: Demographics

| Attribute | Details |
|-----------|---------|
| **Source** | U.S. Census Bureau American Community Survey (2022) |
| **Variables** | Population, median income, renter percentage, poverty rate |
| **Geography** | NYC boroughs (county level) |

---

## 🔬 Methodology

This project follows the **OSEMN** data science workflow:

```
    ┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐
    │  OBTAIN  │────▶│  SCRUB   │────▶│ EXPLORE  │────▶│  MODEL   │────▶│INTERPRET │
    └──────────┘     └──────────┘     └──────────┘     └──────────┘     └──────────┘
         │                │                │                │                │
    API Data Pull    Clean/Transform   Visualize &     Random Forest    Policy
    Weather Join     Handle Missing    Statistical     Prediction       Implications
    Demographics     Create Features   Testing         Evaluation
```

### Data Pipeline

```r
# 1. OBTAIN - Pull data from NYC Open Data API
df_311 <- GET(api_url, query = list(`$limit` = 100000)) %>%
  content() %>% fromJSON() %>% as.data.frame()

# 2. SCRUB - Transform and engineer features
df_311 <- df_311 %>%
  mutate(
    response_time = difftime(closed_date, created_date, units = "hours"),
    hour = hour(created_date),
    day_of_week = wday(created_date, label = TRUE)
  )

# 3. EXPLORE - Statistical analysis
chisq.test(table(df_311$borough, df_311$complaint_type))
aov(response_time ~ borough, data = df_311)

# 4. MODEL - Random Forest prediction
rf_model <- randomForest(response_time ~ ., data = train_data, ntree = 200)

# 5. INTERPRET - Extract insights for policy recommendations
varImpPlot(rf_model)
```

### Statistical Tests Performed

| Test | Purpose | Result |
|------|---------|--------|
| **Chi-Square** | Complaint type independence from borough | Significant (p < 0.001) |
| **ANOVA** | Response time differences by borough | Significant (p < 0.001) |
| **Tukey HSD** | Pairwise borough comparisons | Multiple significant pairs |
| **Correlation** | Weather-complaint relationships | Negative correlation for heating |

---

## 🔑 Key Findings

### 📍 Geographic Disparities

| Borough | Complaint Volume | Median Response Time | Top Complaint |
|---------|------------------|----------------------|---------------|
| 🟣 BROOKLYN | Highest | 4.2 hours | Noise |
| 🔵 QUEENS | High | 3.8 hours | Illegal Parking |
| 🟢 MANHATTAN | Moderate | 5.1 hours | Noise |
| 🟠 BRONX | Moderate | 6.3 hours | Heat/Hot Water |
| 🔴 STATEN ISLAND | Lowest | 3.5 hours | Street Condition |

### 🌡️ Weather Correlations

```
HEATING COMPLAINTS vs TEMPERATURE
─────────────────────────────────
Temperature Range    │ Complaint Volume
─────────────────────┼─────────────────
❄️  < 32°F (Freezing) │ ████████████████ HIGH
🌨️  32-45°F (Cold)    │ ████████████ MODERATE-HIGH
🌤️  45-60°F (Cool)    │ ████████ MODERATE
☀️  60-75°F (Mild)    │ ████ LOW
🔥  > 75°F (Warm)     │ ██ VERY LOW

Correlation: r = -0.67 (Strong Negative)
```

### 💰 Demographics Insights

| Finding | Implication |
|---------|-------------|
| Higher renter % → More housing complaints | Tenant protection focus needed |
| Lower income → Higher per-capita complaints | Resource allocation equity |
| Higher poverty → Longer response times | Service delivery disparities |

---

## 🤖 Machine Learning Model

### Random Forest Regression

The model predicts **311 response time** using complaint characteristics, temporal features, weather conditions, and demographic factors.

#### Model Architecture

```
                    ┌─────────────────────────────────────┐
                    │       RANDOM FOREST MODEL           │
                    │         (200 Trees, mtry=3)         │
                    └─────────────────────────────────────┘
                                      │
          ┌───────────────────────────┼───────────────────────────┐
          │                           │                           │
    ┌─────▼─────┐              ┌─────▼─────┐              ┌─────▼─────┐
    │ ORIGINAL  │              │  WEATHER  │              │DEMOGRAPHIC│
    │ FEATURES  │              │ FEATURES  │              │ FEATURES  │
    ├───────────┤              ├───────────┤              ├───────────┤
    │• Borough  │              │• Temp Mean│              │• Median   │
    │• Complaint│              │• Temp Max │              │  Income   │
    │  Type     │              │• Precip.  │              │• % Renter │
    │• Hour     │              │           │              │• % Poverty│
    │• Day/Week │              │           │              │           │
    │• Month    │              │           │              │           │
    └───────────┘              └───────────┘              └───────────┘
```

#### Model Performance

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **R-squared (R²)** | 0.71 | 71% of variance explained |
| **RMSE** | 10.68 hours | ~0.45 days average error |
| **MAE** | 8.2 hours | Median error < half day |

#### Variable Importance

```
Variable Importance (% Increase in MSE when removed)
────────────────────────────────────────────────────
complaint_type   ████████████████████████████  MOST IMPORTANT
borough          ████████████████████
hour             ██████████████
temp_mean_f      ████████████
median_income    ██████████
month            ████████
day_of_week      ██████
pct_renter       ████
precipitation    ██
```

> **Interpretation:** Complaint type is the strongest predictor of response time, suggesting operational workflows—not just geography—drive resolution speed. The model achieves predictions accurate within approximately half a day, sufficient for operational triage and resource planning.

---

## 📱 Interactive Dashboard

### 🔗 [Launch Dashboard](https://candacegrant2025.shinyapps.io/NYC_311_Requests/)

The Shiny dashboard transforms static analysis into an interactive decision-support tool.

### Dashboard Features

| Tab | Functionality |
|-----|---------------|
| 🏠 **Overview** | Interactive map, complaint counts by type and location |
| 🗺️ **Borough Analysis** | Cross-borough comparisons, heatmaps, faceted distributions |
| 📋 **Complaint Patterns** | Top complaints, pie charts, borough×complaint heatmap |
| ⏱️ **Response Time** | Distribution analysis, borough comparisons, SLA metrics |
| 🌡️ **Weather & Demographics** | Temperature correlations, income analysis, renter impact |

### Filter Controls

```
┌─────────────────────────────────────┐
│          FILTER OPTIONS             │
├─────────────────────────────────────┤
│  📍 Borough: [All ▼]                │
│  📝 Complaint Type: [All ▼]         │
│  📅 Date Range: [2024-01-01] to     │
│                [2025-12-14]         │
└─────────────────────────────────────┘
```

### Sample Visualizations

| Visualization | Purpose |
|---------------|---------|
| Clustered Leaflet Map | Geographic distribution of complaints |
| Plotly Bar Charts | Complaint volume comparisons |
| Interactive Heatmaps | Pattern identification across dimensions |
| Box Plots | Response time distributions by category |
| Scatter Plots | Weather/demographic correlations |

---

## 🛠️ Technologies Used

### Programming & Analysis

| Technology | Purpose |
|------------|---------|
| ![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white) | Statistical computing and analysis |
| ![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=flat-square&logo=rstudio&logoColor=white) | Integrated development environment |
| ![RMarkdown](https://img.shields.io/badge/RMarkdown-2C3E50?style=flat-square&logo=markdown&logoColor=white) | Reproducible reporting |

### Key R Packages

```r
# Data Manipulation
library(dplyr)        # Data wrangling
library(tidyr)        # Data reshaping
library(lubridate)    # Date-time handling

# Visualization
library(ggplot2)      # Static visualizations
library(plotly)       # Interactive charts
library(leaflet)      # Interactive maps

# Machine Learning
library(randomForest) # Random Forest algorithm
library(caret)        # Model training utilities

# Web Application
library(shiny)        # Interactive web apps
library(shinydashboard) # Dashboard UI framework

# Data Access
library(httr)         # API requests
library(jsonlite)     # JSON parsing
library(tidycensus)   # Census data access
```

### Data Sources & APIs

| Source | API/Method |
|--------|------------|
| NYC Open Data | Socrata REST API |
| Open-Meteo | Historical Weather API |
| U.S. Census Bureau | tidycensus package |

---

## 📂 Project Structure

```
NYC-311-Analysis/
│
├── 📄 Final_Project_607.Rmd      # Main analysis document
├── 📱 app.R                       # Shiny dashboard application
├── 📖 README.md                   # Project documentation (this file)
│
├── 📁 data/
│   ├── df_311.csv                # Cached 311 data
│   ├── weather_df.csv            # Weather data
│   └── demographics.csv          # Census demographics
│
├── 📁 outputs/
│   ├── Final_Project_607.html    # Rendered analysis report
│   └── visualizations/           # Exported charts
│
└── 📁 references/
    └── citations.bib             # Bibliography
```

---

## 💡 Conclusions & Policy Implications

### Shifting to Targeted Solutions

The clustering of requests at specific addresses reveals a predictive opportunity—rather than treating each complaint as an independent event, city agencies could use **geographic hotspot analysis** and **complaint frequency modeling** to proactively allocate resources and address root causes before repeat complaints occur.

### Operational Recommendations

| Use Case | Implementation |
|----------|----------------|
| 🎯 **Triage Requests** | Flag complaints likely to exceed SLA thresholds based on type and conditions |
| 👥 **Staff Allocation** | Predict workload by complaint type and shift resources accordingly |
| ⏰ **Set Expectations** | Provide residents with accurate estimated resolution times |
| 🔧 **Target Improvements** | Identify high-delay complaint categories for process optimization |

### Equity Considerations

| Finding | Policy Response |
|---------|-----------------|
| Lower-income boroughs have higher per-capita complaints | Increase service resources in underserved areas |
| Renter-heavy areas show more housing complaints | Strengthen tenant protection enforcement |
| Response times vary by borough | Standardize SLAs across all neighborhoods |
| Weather impacts service demand | Implement seasonal staffing adjustments |

### Future Enhancements

- [ ] Incorporate real-time 311 data streaming
- [ ] Add neighborhood-level (census tract) analysis
- [ ] Integrate additional data sources (building violations, crime data)
- [ ] Develop predictive alerts for complaint surges
- [ ] Create agency-specific dashboards for operational use

---

## 📚 References

City of New York. (2025, December 8). *311 service requests from 2010 to present*. NYC Open Data. https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9/about_data

Géron, A. (2022). Machine learning project checklist. In *Hands-on machine learning with Scikit-Learn, Keras, and TensorFlow* (3rd ed.). O'Reilly Media.

Grant, C. (2025). *NYC 311 requests* [Interactive dashboard]. Shiny. https://candacegrant2025.shinyapps.io/NYC_311_Requests/

Office of the New York State Comptroller. (2025, May). *NYC311 monitoring tool* (Report 3-2026). https://www.osc.ny.gov/files/reports/pdf/report-3-2026.pdf

Open-Meteo. (2025). *Historical weather API*. https://open-meteo.com/

U.S. Census Bureau. (2022). *American Community Survey 5-year estimates*. https://data.census.gov/

---

## 👩‍💻 Author

**Candace Grant**

📧 Email: candace.grant@spsmail.cuny.edu  
🎓 Program: M.S. Data Science, CUNY School of Professional Studies  
📅 Date: December 2025  
📊 Course: DATA 607 - Data Acquisition and Management

---

<p align="center">
  <img src="https://sps.cuny.edu/sites/default/files/2021-02/sps-logo-blue.png" alt="CUNY SPS" width="200"/>
</p>

<p align="center">
  <i>This project was completed as part of the Master of Science in Data Science program at CUNY School of Professional Studies.</i>
</p>

---

<p align="center">
  Made with ❤️ in New York City
</p>
