# =============================================================================
# NYC Electricity Consumption Analysis Dashboard
# DATA 606 Final Project - Candace Grant
# A Portfolio-Quality Shiny Application
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(bslib)

# =============================================================================
# CUSTOM THEME & STYLING
# =============================================================================

# Custom CSS for a sophisticated, modern aesthetic
custom_css <- "
/* Import Google Fonts - Distinctive typography */
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;600;700&family=Source+Sans+Pro:wght@300;400;600;700&family=JetBrains+Mono:wght@400;500&display=swap');

/* Root variables for consistent theming */
:root {
  --primary-dark: #1a1a2e;
  --primary-medium: #16213e;
  --accent-coral: #e94560;
  --accent-gold: #f5a623;
  --accent-teal: #00d9c0;
  --accent-purple: #7b68ee;
  --text-light: #eaeaea;
  --text-muted: #a0a0a0;
  --card-bg: rgba(255, 255, 255, 0.03);
  --card-border: rgba(255, 255, 255, 0.08);
  --gradient-1: linear-gradient(135deg, #e94560 0%, #f5a623 100%);
  --gradient-2: linear-gradient(135deg, #00d9c0 0%, #7b68ee 100%);
  --gradient-3: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
}

/* Body styling */
body {
  font-family: 'Source Sans Pro', sans-serif;
  font-size: 16px;
  background: linear-gradient(135deg, #0f0f1a 0%, #1a1a2e 50%, #16213e 100%);
  color: var(--text-light);
  min-height: 100vh;
}

/* Dashboard header */
.main-header .logo {
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  font-size: 20px;
  background: var(--gradient-1) !important;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.main-header .navbar {
  background: rgba(26, 26, 46, 0.95) !important;
  backdrop-filter: blur(10px);
  border-bottom: 1px solid var(--card-border);
}

/* Sidebar styling */
.main-sidebar {
  background: linear-gradient(180deg, #1a1a2e 0%, #0f0f1a 100%) !important;
  border-right: 1px solid var(--card-border);
}

.sidebar-menu > li > a {
  font-family: 'Source Sans Pro', sans-serif;
  font-weight: 600;
  color: var(--text-muted) !important;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
}

.sidebar-menu > li > a:hover,
.sidebar-menu > li.active > a {
  background: rgba(233, 69, 96, 0.1) !important;
  color: var(--accent-coral) !important;
  border-left: 3px solid var(--accent-coral);
}

.sidebar-menu > li > a > .fa {
  color: var(--accent-teal);
}

/* Content wrapper */
.content-wrapper {
  background: transparent !important;
}

/* Box styling */
.box {
  background: var(--card-bg);
  border: 1px solid var(--card-border);
  border-radius: 16px;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
  backdrop-filter: blur(10px);
}

.box-header {
  border-bottom: 1px solid var(--card-border);
  padding: 20px 25px;
}

.box-title {
  font-family: 'Playfair Display', serif;
  font-weight: 600;
  font-size: 1.3rem;
  color: var(--text-light);
}

.box-body {
  padding: 25px;
}

/* Value boxes */
.small-box {
  border-radius: 16px;
  overflow: hidden;
  border: 1px solid var(--card-border);
  transition: transform 0.3s ease, box-shadow 0.3s ease;
}

.small-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 40px rgba(233, 69, 96, 0.2);
}

.small-box.bg-aqua {
  background: linear-gradient(135deg, #00d9c0 0%, #00b4a0 100%) !important;
}

.small-box.bg-yellow {
  background: linear-gradient(135deg, #f5a623 0%, #e09000 100%) !important;
}

.small-box.bg-red {
  background: linear-gradient(135deg, #e94560 0%, #c73050 100%) !important;
}

.small-box.bg-purple {
  background: linear-gradient(135deg, #7b68ee 0%, #6050d0 100%) !important;
}

.small-box h3 {
  font-family: 'JetBrains Mono', monospace;
  font-size: 2.2rem;
  font-weight: 500;
}

.small-box p {
  font-family: 'Source Sans Pro', sans-serif;
  font-weight: 600;
  font-size: 1rem;
  text-transform: uppercase;
  letter-spacing: 1px;
}

/* Tab styling */
.nav-tabs {
  border-bottom: 1px solid var(--card-border);
  margin-bottom: 20px;
}

.nav-tabs > li > a {
  font-family: 'Source Sans Pro', sans-serif;
  font-weight: 600;
  color: var(--text-muted);
  border: none;
  border-bottom: 3px solid transparent;
  background: transparent;
  padding: 12px 24px;
  transition: all 0.3s ease;
}

.nav-tabs > li > a:hover {
  background: rgba(233, 69, 96, 0.1);
  color: var(--accent-coral);
  border-color: transparent;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background: transparent;
  color: var(--accent-coral);
  border: none;
  border-bottom: 3px solid var(--accent-coral);
}

/* Tables */
.dataTables_wrapper {
  color: var(--text-light);
}

table.dataTable {
  border-collapse: collapse !important;
}

table.dataTable thead th {
  font-family: 'Source Sans Pro', sans-serif;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-size: 0.85rem;
  color: var(--accent-teal);
  border-bottom: 2px solid var(--accent-teal) !important;
  padding: 15px 10px;
}

table.dataTable tbody td {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.9rem;
  padding: 12px 10px;
  border-bottom: 1px solid var(--card-border);
  color: var(--text-light);
}

table.dataTable tbody tr:hover {
  background: rgba(233, 69, 96, 0.1) !important;
}

/* Select inputs */
.selectize-input {
  background: var(--primary-medium) !important;
  border: 1px solid var(--card-border) !important;
  border-radius: 8px !important;
  color: var(--text-light) !important;
  font-family: 'Source Sans Pro', sans-serif;
}

.selectize-dropdown {
  background: var(--primary-dark) !important;
  border: 1px solid var(--card-border) !important;
  border-radius: 8px !important;
}

.selectize-dropdown-content .option {
  color: var(--text-light);
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: var(--accent-coral) !important;
}

/* Picker input (shinyWidgets) */
.picker .btn {
  background: var(--primary-medium) !important;
  border: 1px solid var(--card-border) !important;
  color: var(--text-light) !important;
  border-radius: 8px !important;
}

/* Action buttons */
.btn-primary {
  background: var(--gradient-1) !important;
  border: none !important;
  border-radius: 8px !important;
  font-family: 'Source Sans Pro', sans-serif;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  padding: 12px 24px;
  transition: all 0.3s ease;
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 20px rgba(233, 69, 96, 0.4);
}

/* Statistical output styling */
.stat-output {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.9rem;
  background: rgba(0, 0, 0, 0.3);
  border-radius: 8px;
  padding: 20px;
  border-left: 4px solid var(--accent-teal);
  overflow-x: auto;
  white-space: pre-wrap;
}

/* Hypothesis text */
.hypothesis-box {
  background: rgba(123, 104, 238, 0.1);
  border: 1px solid rgba(123, 104, 238, 0.3);
  border-radius: 12px;
  padding: 20px;
  margin-bottom: 20px;
}

.hypothesis-box h4 {
  font-family: 'Playfair Display', serif;
  color: var(--accent-purple);
  margin-bottom: 15px;
}

.hypothesis-box p {
  font-family: 'Source Sans Pro', sans-serif;
  line-height: 1.8;
  margin-bottom: 10px;
}

/* Decision box */
.decision-box {
  background: rgba(233, 69, 96, 0.1);
  border: 1px solid rgba(233, 69, 96, 0.3);
  border-radius: 12px;
  padding: 20px;
  margin-top: 20px;
}

.decision-box.significant {
  border-color: var(--accent-teal);
  background: rgba(0, 217, 192, 0.1);
}

/* Info cards */
.info-card {
  background: var(--card-bg);
  border: 1px solid var(--card-border);
  border-radius: 12px;
  padding: 25px;
  margin-bottom: 20px;
  transition: all 0.3s ease;
}

.info-card:hover {
  border-color: var(--accent-coral);
  box-shadow: 0 8px 30px rgba(233, 69, 96, 0.15);
}

.info-card h4 {
  font-family: 'Playfair Display', serif;
  color: var(--accent-gold);
  margin-bottom: 15px;
}

/* Abstract section */
.abstract-text {
  font-family: 'Source Sans Pro', sans-serif;
  font-size: 1.1rem;
  line-height: 1.9;
  color: var(--text-light);
  text-align: justify;
}

/* Section headers */
.section-header {
  font-family: 'Playfair Display', serif;
  font-size: 2rem;
  font-weight: 700;
  background: var(--gradient-1);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  margin-bottom: 30px;
  padding-bottom: 15px;
  border-bottom: 1px solid var(--card-border);
}

/* Plotly styling adjustments */
.plotly {
  border-radius: 12px;
  overflow: hidden;
}

/* Loading spinner */
.shiny-busy {
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  z-index: 9999;
}

/* Scrollbar styling */
::-webkit-scrollbar {
  width: 8px;
  height: 8px;
}

::-webkit-scrollbar-track {
  background: var(--primary-dark);
}

::-webkit-scrollbar-thumb {
  background: var(--accent-coral);
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: var(--accent-gold);
}

/* Footer */
.footer-text {

  font-family: 'Source Sans Pro', sans-serif;
  color: var(--text-muted);
  text-align: center;
  padding: 20px;
  font-size: 0.9rem;
}
"

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "black",
  
  # Header
  dashboardHeader(
    title = "NYC Energy Analytics",
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("ANOVA Analysis", tabName = "anova", icon = icon("flask")),
      menuItem("Regression", tabName = "regression", icon = icon("chart-line")),
      menuItem("Conclusions", tabName = "conclusions", icon = icon("flag-checkered")),
      
      hr(),
      
      # Filters
      div(style = "padding: 15px;",
          h4(style = "color: #a0a0a0; font-family: 'Source Sans Pro', sans-serif; margin-bottom: 15px;", 
             "FILTERS"),
          
          pickerInput(
            inputId = "borough_filter",
            label = "Select Boroughs:",
            choices = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
            selected = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2"
            )
          ),
          
          pickerInput(
            inputId = "season_filter",
            label = "Select Seasons:",
            choices = c("Winter", "Spring", "Summer", "Fall"),
            selected = c("Winter", "Spring", "Summer", "Fall"),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE
            )
          )
      )
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # =======================================================================
      # OVERVIEW TAB
      # =======================================================================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "NYC Electricity Consumption Analysis"),
                 div(class = "abstract-text", style = "margin-bottom: 40px;",
                     p("This study investigates whether temporal and geographical factors predict electricity 
                charges across New York City boroughs using data from the NYC Open Data Electric 
                Consumption and Cost dataset. The dataset, sourced from the New York City Housing 
                Authority (NYCHA), contains 539,000 electric consumption records spanning 2010 
                through May 2025."),
                     p("Four statistical methods were employed: one-way ANOVA for borough effect, one-way 
                ANOVA for season effect, two-way ANOVA with interaction, and multiple linear regression. 
                Results indicate that both borough and season are statistically significant predictors 
                of electricity charges, with Staten Island exhibiting the highest mean charges and 
                summer months showing charges approximately $2,800 higher than winter months.")
                 )
          )
        ),
        
        # Value Boxes
        fluidRow(
          valueBoxOutput("total_records", width = 3),
          valueBoxOutput("avg_charge", width = 3),
          valueBoxOutput("total_boroughs", width = 3),
          valueBoxOutput("date_range", width = 3)
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Research Question",
                   width = NULL,
                   solidHeader = FALSE,
                   div(class = "info-card",
                       h4(icon("question-circle"), " Primary Question"),
                       p(style = "font-size: 1.1rem; line-height: 1.8;",
                         "What temporal and geographical factors best predict electricity charges 
                  across NYC boroughs?"),
                       hr(),
                       h4(icon("bullseye"), " Variables"),
                       tags$ul(
                         tags$li(strong("Response Variable:"), " Current monthly charges ($)"),
                         tags$li(strong("Explanatory Variables:"), " Borough (geographic), Season (temporal)")
                       )
                   )
                 )
          ),
          column(6,
                 box(
                   title = "Key Findings",
                   width = NULL,
                   solidHeader = FALSE,
                   div(class = "info-card",
                       h4(icon("lightbulb"), " Summary of Results"),
                       tags$ul(style = "font-size: 1rem; line-height: 2;",
                               tags$li("Both borough and season are ", strong("statistically significant"), " predictors (p < 0.001)"),
                               tags$li("Staten Island has ", strong("highest"), " mean charges across all boroughs"),
                               tags$li("Summer charges are ", strong("~$2,800 higher"), " than winter"),
                               tags$li("Significant ", strong("interaction effect"), " between borough and season"),
                               tags$li("Model R² ≈ 2% — other factors explain most variation")
                       )
                   )
                 )
          )
        )
      ),
      
      # =======================================================================
      # DATA EXPLORER TAB
      # =======================================================================
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "Data Explorer")
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Dataset Preview",
                   width = NULL,
                   solidHeader = FALSE,
                   DTOutput("data_table")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Summary Statistics by Borough",
                   width = NULL,
                   solidHeader = FALSE,
                   DTOutput("borough_summary")
                 )
          ),
          column(6,
                 box(
                   title = "Summary Statistics by Season",
                   width = NULL,
                   solidHeader = FALSE,
                   DTOutput("season_summary")
                 )
          )
        )
      ),
      
      # =======================================================================
      # VISUALIZATIONS TAB
      # =======================================================================
      tabItem(
        tabName = "visualizations",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "Interactive Visualizations")
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Current Charges Distribution by Borough",
                   width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput("boxplot_borough", height = "400px")
                 )
          ),
          column(6,
                 box(
                   title = "Mean Charges by Borough",
                   width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput("barplot_borough", height = "400px")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Mean Charges by Season",
                   width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput("barplot_season", height = "400px")
                 )
          ),
          column(6,
                 box(
                   title = "Borough × Season Interaction",
                   width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput("interaction_plot", height = "400px")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Heatmap: Mean Charges by Borough and Season",
                   width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput("heatmap", height = "450px")
                 )
          )
        )
      ),
      
      # =======================================================================
      # ANOVA TAB
      # =======================================================================
      tabItem(
        tabName = "anova",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "Analysis of Variance (ANOVA)")
          )
        ),
        
        # Borough ANOVA
        fluidRow(
          column(12,
                 box(
                   title = "One-Way ANOVA: Effect of Borough on Current Charges",
                   width = NULL,
                   solidHeader = FALSE,
                   
                   div(class = "hypothesis-box",
                       h4(icon("file-alt"), " Hypothesis Test"),
                       p(strong("Null Hypothesis (H₀):"), " There is no difference in mean current charges among NYC boroughs."),
                       p(strong("Alternative Hypothesis (H₁):"), " At least one borough has a significantly different mean current charge."),
                       p(strong("Significance Level:"), " α = 0.05")
                   ),
                   
                   h4("ANOVA Results:", style = "color: #00d9c0; margin-top: 25px;"),
                   verbatimTextOutput("anova_borough_output"),
                   
                   uiOutput("anova_borough_decision"),
                   
                   h4("Post-Hoc Analysis: Tukey's HSD", style = "color: #f5a623; margin-top: 25px;"),
                   verbatimTextOutput("tukey_borough_output")
                 )
          )
        ),
        
        # Season ANOVA
        fluidRow(
          column(12,
                 box(
                   title = "One-Way ANOVA: Effect of Season on Current Charges",
                   width = NULL,
                   solidHeader = FALSE,
                   
                   div(class = "hypothesis-box",
                       h4(icon("file-alt"), " Hypothesis Test"),
                       p(strong("Null Hypothesis (H₀):"), " There is no difference in mean current charges among seasons."),
                       p(strong("Alternative Hypothesis (H₁):"), " At least one season has a significantly different mean current charge."),
                       p(strong("Significance Level:"), " α = 0.05")
                   ),
                   
                   h4("ANOVA Results:", style = "color: #00d9c0; margin-top: 25px;"),
                   verbatimTextOutput("anova_season_output"),
                   
                   uiOutput("anova_season_decision"),
                   
                   h4("Post-Hoc Analysis: Tukey's HSD", style = "color: #f5a623; margin-top: 25px;"),
                   verbatimTextOutput("tukey_season_output")
                 )
          )
        ),
        
        # Two-Way ANOVA
        fluidRow(
          column(12,
                 box(
                   title = "Two-Way ANOVA: Borough × Season Interaction",
                   width = NULL,
                   solidHeader = FALSE,
                   
                   div(class = "hypothesis-box",
                       h4(icon("file-alt"), " Hypothesis Tests"),
                       p(strong("1. Main Effect of Borough")),
                       p("H₀: No difference in mean charges among boroughs | H₁: At least one borough differs"),
                       p(strong("2. Main Effect of Season")),
                       p("H₀: No difference in mean charges among seasons | H₁: At least one season differs"),
                       p(strong("3. Interaction Effect (Borough × Season)")),
                       p("H₀: The effect of season on charges is the same across all boroughs"),
                       p("H₁: The effect of season on charges differs by borough"),
                       p(strong("Significance Level:"), " α = 0.05")
                   ),
                   
                   h4("Two-Way ANOVA Results:", style = "color: #00d9c0; margin-top: 25px;"),
                   verbatimTextOutput("anova_twoway_output"),
                   
                   uiOutput("anova_twoway_decision")
                 )
          )
        )
      ),
      
      # =======================================================================
      # REGRESSION TAB
      # =======================================================================
      tabItem(
        tabName = "regression",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "Multiple Linear Regression")
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Model Specification",
                   width = NULL,
                   solidHeader = FALSE,
                   
                   div(class = "info-card",
                       h4(icon("cogs"), " Model Details"),
                       p(strong("Dependent Variable:"), " current_charges (monthly electricity charges in $)"),
                       p(strong("Independent Variables:"), " borough (categorical), season (categorical)"),
                       p(strong("Model Equation:")),
                       p(code("current_charges = β₀ + β₁(borough) + β₂(season) + ε"), 
                         style = "font-family: 'JetBrains Mono', monospace; font-size: 1.1rem;")
                   )
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Regression Output",
                   width = NULL,
                   solidHeader = FALSE,
                   verbatimTextOutput("regression_output")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "95% Confidence Intervals",
                   width = NULL,
                   solidHeader = FALSE,
                   verbatimTextOutput("conf_int_output")
                 )
          ),
          column(6,
                 box(
                   title = "Model Interpretation",
                   width = NULL,
                   solidHeader = FALSE,
                   uiOutput("regression_interpretation")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Diagnostic Plots",
                   width = NULL,
                   solidHeader = FALSE,
                   plotOutput("diagnostic_plots", height = "500px")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Effect Size Analysis",
                   width = NULL,
                   solidHeader = FALSE,
                   verbatimTextOutput("effect_size_output")
                 )
          )
        )
      ),
      
      # =======================================================================
      # CONCLUSIONS TAB
      # =======================================================================
      tabItem(
        tabName = "conclusions",
        
        fluidRow(
          column(12,
                 h1(class = "section-header", "Conclusions & Implications")
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Summary of Statistical Tests",
                   width = NULL,
                   solidHeader = FALSE,
                   DTOutput("summary_table")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Key Findings",
                   width = NULL,
                   solidHeader = FALSE,
                   div(class = "info-card",
                       h4(icon("check-circle"), style = "color: #00d9c0;", " Confirmed Results"),
                       tags$ul(style = "font-size: 1rem; line-height: 2;",
                               tags$li("Both borough and season are ", strong("statistically significant"), 
                                       " predictors of electricity charges (p < 0.001)"),
                               tags$li("Staten Island has ", strong("substantially higher"), 
                                       " charges than all other boroughs (~$10,000 higher)"),
                               tags$li("Summer months show charges approximately ", strong("$2,800 higher"), 
                                       " than winter months"),
                               tags$li("The ", strong("interaction effect"), " is significant, meaning seasonal 
                          patterns vary by borough"),
                               tags$li("Combined model explains approximately ", strong("2%"), 
                                       " of variance (R² ≈ 0.02)")
                       )
                   )
                 )
          ),
          column(6,
                 box(
                   title = "Limitations",
                   width = NULL,
                   solidHeader = FALSE,
                   div(class = "info-card",
                       h4(icon("exclamation-triangle"), style = "color: #f5a623;", " Study Limitations"),
                       tags$ul(style = "font-size: 1rem; line-height: 2;",
                               tags$li("Low R-squared indicates borough and season are ", strong("weak predictors"), 
                                       " on their own"),
                               tags$li("Dataset represents only ", strong("NYCHA properties"), 
                                       ", not all NYC buildings"),
                               tags$li("API limit of ", strong("50,000 rows"), 
                                       " represents a subset of 539,000 total records"),
                               tags$li("Observational study — ", strong("cannot establish causation"), 
                                       ", only associations"),
                               tags$li("Other factors (building size, rate class, usage type) likely explain more variance")
                       )
                   )
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Practical Implications",
                   width = NULL,
                   solidHeader = FALSE,
                   div(class = "info-card",
                       h4(icon("lightbulb"), style = "color: #7b68ee;", " Why This Matters"),
                       p(style = "font-size: 1.05rem; line-height: 1.9;",
                         "Understanding the geographic and temporal drivers of electricity costs supports 
                  better resource planning for housing authorities, informs policy decisions about 
                  energy assistance programs, and helps identify which communities face the highest 
                  energy burdens. The finding that Staten Island has significantly higher charges 
                  warrants further investigation into the underlying causes—whether due to building 
                  characteristics, infrastructure differences, or rate structures."),
                       p(style = "font-size: 1.05rem; line-height: 1.9;",
                         "The significant summer spike across all boroughs suggests that targeted energy 
                  efficiency programs focused on cooling costs could benefit NYCHA residents, 
                  particularly in boroughs showing the largest seasonal variation.")
                   )
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 div(class = "footer-text",
                     p("DATA 606 Final Project | Candace Grant | CUNY School of Professional Studies"),
                     p("Data Source: NYC Open Data - Electric Consumption and Cost (2010 - May 2025)")
                 )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # DATA LOADING AND PROCESSING
  # ===========================================================================
  
  # Load data reactively
  data <- reactive({
    # Show loading notification
    showNotification("Loading data from NYC Open Data API...", type = "message", duration = 3)
    
    # Pull data from API
    response <- GET(
      "https://data.cityofnewyork.us/resource/jr24-e7cr.json",
      query = list(`$limit` = 50000)
    )
    
    nyc_energy <- fromJSON(content(response, "text"))
    nyc_energy_data <- as.data.frame(nyc_energy)
    
    # Clean and process
    nyc_energy_clean <- nyc_energy_data %>%
      mutate(
        consumption_kwh = as.numeric(consumption_kwh),
        consumption_kw = as.numeric(consumption_kw),
        current_charges = as.numeric(current_charges),
        kwh_charges = as.numeric(kwh_charges),
        kw_charges = as.numeric(kw_charges),
        other_charges = as.numeric(other_charges),
        days = as.numeric(days),
        revenue_month = as.Date(paste0(revenue_month, "-01")),
        service_start_date = as.POSIXct(service_start_date, format = "%Y-%m-%dT%H:%M:%S"),
        service_end_date = as.POSIXct(service_end_date, format = "%Y-%m-%dT%H:%M:%S")
      ) %>%
      filter(!is.na(consumption_kwh)) %>%
      mutate(
        month = month(revenue_month),
        season = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall",
          TRUE ~ NA_character_
        ),
        season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
      )
    
    nyc_energy_clean
  })
  
  # Filtered data based on user selection
  filtered_data <- reactive({
    req(input$borough_filter, input$season_filter)
    
    data() %>%
      filter(
        borough %in% input$borough_filter,
        season %in% input$season_filter
      ) %>%
      mutate(
        borough = factor(borough),
        season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
      )
  })
  
  # ===========================================================================
  # VALUE BOXES
  # ===========================================================================
  
  output$total_records <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()), big.mark = ","),
      "Total Records",
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  output$avg_charge <- renderValueBox({
    avg <- mean(filtered_data()$current_charges, na.rm = TRUE)
    valueBox(
      paste0("$", format(round(avg, 2), big.mark = ",")),
      "Average Charge",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$total_boroughs <- renderValueBox({
    valueBox(
      length(unique(filtered_data()$borough)),
      "Boroughs Selected",
      icon = icon("map-marker-alt"),
      color = "red"
    )
  })
  
  output$date_range <- renderValueBox({
    dates <- range(filtered_data()$revenue_month, na.rm = TRUE)
    valueBox(
      paste(format(dates[1], "%Y"), "-", format(dates[2], "%Y")),
      "Date Range",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  # ===========================================================================
  # DATA EXPLORER
  # ===========================================================================
  
  output$data_table <- renderDT({
    filtered_data() %>%
      select(borough, season, current_charges, consumption_kwh, revenue_month, 
             development_name, funding_source) %>%
      head(500) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#1a1a2e', 'color': '#00d9c0'});",
            "}"
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      ) %>%
      formatCurrency(columns = c("current_charges"), currency = "$", digits = 2) %>%
      formatRound(columns = c("consumption_kwh"), digits = 0)
  })
  
  output$borough_summary <- renderDT({
    filtered_data() %>%
      group_by(Borough = borough) %>%
      summarise(
        N = n(),
        `Mean ($)` = round(mean(current_charges, na.rm = TRUE), 2),
        `SD ($)` = round(sd(current_charges, na.rm = TRUE), 2),
        `Median ($)` = round(median(current_charges, na.rm = TRUE), 2)
      ) %>%
      datatable(
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE
      ) %>%
      formatCurrency(columns = c("Mean ($)", "SD ($)", "Median ($)"), currency = "$")
  })
  
  output$season_summary <- renderDT({
    filtered_data() %>%
      group_by(Season = season) %>%
      summarise(
        N = n(),
        `Mean ($)` = round(mean(current_charges, na.rm = TRUE), 2),
        `SD ($)` = round(sd(current_charges, na.rm = TRUE), 2),
        `Median ($)` = round(median(current_charges, na.rm = TRUE), 2)
      ) %>%
      datatable(
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE
      ) %>%
      formatCurrency(columns = c("Mean ($)", "SD ($)", "Median ($)"), currency = "$")
  })
  
  # ===========================================================================
  # VISUALIZATIONS
  # ===========================================================================
  
  # Color palette
  borough_colors <- c(
    "BRONX" = "#e94560",
    "BROOKLYN" = "#00d9c0", 
    "MANHATTAN" = "#f5a623",
    "QUEENS" = "#7b68ee",
    "STATEN ISLAND" = "#ff6b9d"
  )
  
  season_colors <- c(
    "Winter" = "#64b5f6",
    "Spring" = "#81c784",
    "Summer" = "#ffb74d",
    "Fall" = "#e57373"
  )
  
  output$boxplot_borough <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = borough, y = current_charges, fill = borough)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8) +
      scale_fill_manual(values = borough_colors) +
      coord_cartesian(ylim = quantile(filtered_data()$current_charges, c(0.05, 0.95), na.rm = TRUE)) +
      labs(x = "", y = "Current Charges ($)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.1)"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#eaeaea", size = 10),
        axis.title = element_text(color = "#eaeaea", size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#eaeaea')
      )
  })
  
  output$barplot_borough <- renderPlotly({
    borough_means <- filtered_data() %>%
      group_by(borough) %>%
      summarise(mean_charges = mean(current_charges, na.rm = TRUE)) %>%
      arrange(desc(mean_charges))
    
    p <- ggplot(borough_means, aes(x = reorder(borough, mean_charges), 
                                   y = mean_charges, fill = borough)) +
      geom_bar(stat = "identity", alpha = 0.9) +
      scale_fill_manual(values = borough_colors) +
      coord_flip() +
      labs(x = "", y = "Mean Current Charges ($)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.1)"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#eaeaea", size = 10),
        axis.title = element_text(color = "#eaeaea", size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#eaeaea')
      )
  })
  
  output$barplot_season <- renderPlotly({
    season_means <- filtered_data() %>%
      group_by(season) %>%
      summarise(mean_charges = mean(current_charges, na.rm = TRUE))
    
    p <- ggplot(season_means, aes(x = season, y = mean_charges, fill = season)) +
      geom_bar(stat = "identity", alpha = 0.9) +
      scale_fill_manual(values = season_colors) +
      labs(x = "", y = "Mean Current Charges ($)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.1)"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#eaeaea", size = 10),
        axis.title = element_text(color = "#eaeaea", size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#eaeaea')
      )
  })
  
  output$interaction_plot <- renderPlotly({
    interaction_data <- filtered_data() %>%
      group_by(borough, season) %>%
      summarise(mean_charges = mean(current_charges, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(interaction_data, aes(x = season, y = mean_charges, 
                                      color = borough, group = borough)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = borough_colors) +
      labs(x = "", y = "Mean Current Charges ($)", color = "Borough") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.1)"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#eaeaea", size = 10),
        axis.title = element_text(color = "#eaeaea", size = 12),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(color = "#eaeaea"),
        legend.title = element_text(color = "#eaeaea")
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#eaeaea'),
        legend = list(bgcolor = 'rgba(0,0,0,0)')
      )
  })
  
  output$heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(borough, season) %>%
      summarise(mean_charges = mean(current_charges, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(heatmap_data, aes(x = season, y = borough, fill = mean_charges)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = paste0("$", format(round(mean_charges, 0), big.mark = ","))),
                color = "white", fontface = "bold", size = 4) +
      scale_fill_gradient2(
        low = "#00d9c0", 
        mid = "#7b68ee", 
        high = "#e94560",
        midpoint = median(heatmap_data$mean_charges),
        name = "Mean Charges ($)"
      ) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.text = element_text(color = "#eaeaea", size = 12),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(color = "#eaeaea"),
        legend.title = element_text(color = "#eaeaea")
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#eaeaea')
      )
  })
  
  # ===========================================================================
  # ANOVA ANALYSIS
  # ===========================================================================
  
  # Borough ANOVA
  anova_borough <- reactive({
    aov(current_charges ~ borough, data = filtered_data())
  })
  
  output$anova_borough_output <- renderPrint({
    summary(anova_borough())
  })
  
  output$anova_borough_decision <- renderUI({
    anova_summary <- summary(anova_borough())
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    f_stat <- anova_summary[[1]]$`F value`[1]
    
    if (p_value < 0.05) {
      div(class = "decision-box significant",
          h4(icon("check-circle"), style = "color: #00d9c0;", " Decision: REJECT H₀"),
          p(paste0("F-statistic: ", round(f_stat, 2))),
          p(paste0("P-value: ", format(p_value, scientific = TRUE, digits = 4))),
          p("Since p-value < 0.05, there is statistically significant evidence that mean 
           current charges differ among NYC boroughs.")
      )
    } else {
      div(class = "decision-box",
          h4(icon("times-circle"), style = "color: #e94560;", " Decision: FAIL TO REJECT H₀"),
          p(paste0("F-statistic: ", round(f_stat, 2))),
          p(paste0("P-value: ", format(p_value, scientific = TRUE, digits = 4))),
          p("Since p-value ≥ 0.05, there is insufficient evidence to conclude that mean 
           current charges differ among NYC boroughs.")
      )
    }
  })
  
  output$tukey_borough_output <- renderPrint({
    TukeyHSD(anova_borough())
  })
  
  # Season ANOVA
  anova_season <- reactive({
    aov(current_charges ~ season, data = filtered_data())
  })
  
  output$anova_season_output <- renderPrint({
    summary(anova_season())
  })
  
  output$anova_season_decision <- renderUI({
    anova_summary <- summary(anova_season())
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    f_stat <- anova_summary[[1]]$`F value`[1]
    
    if (p_value < 0.05) {
      div(class = "decision-box significant",
          h4(icon("check-circle"), style = "color: #00d9c0;", " Decision: REJECT H₀"),
          p(paste0("F-statistic: ", round(f_stat, 2))),
          p(paste0("P-value: ", format(p_value, scientific = TRUE, digits = 4))),
          p("Since p-value < 0.05, there is statistically significant evidence that mean 
           current charges differ among seasons.")
      )
    } else {
      div(class = "decision-box",
          h4(icon("times-circle"), style = "color: #e94560;", " Decision: FAIL TO REJECT H₀"),
          p(paste0("F-statistic: ", round(f_stat, 2))),
          p(paste0("P-value: ", format(p_value, scientific = TRUE, digits = 4))),
          p("Since p-value ≥ 0.05, there is insufficient evidence to conclude that mean 
           current charges differ among seasons.")
      )
    }
  })
  
  output$tukey_season_output <- renderPrint({
    TukeyHSD(anova_season())
  })
  
  # Two-Way ANOVA
  anova_twoway <- reactive({
    aov(current_charges ~ borough * season, data = filtered_data())
  })
  
  output$anova_twoway_output <- renderPrint({
    summary(anova_twoway())
  })
  
  output$anova_twoway_decision <- renderUI({
    anova_summary <- summary(anova_twoway())
    
    p_borough <- anova_summary[[1]]$`Pr(>F)`[1]
    p_season <- anova_summary[[1]]$`Pr(>F)`[2]
    p_interaction <- anova_summary[[1]]$`Pr(>F)`[3]
    
    div(
      div(class = if(p_borough < 0.05) "decision-box significant" else "decision-box",
          h4("1. Borough Main Effect"),
          p(paste0("P-value: ", format(p_borough, scientific = TRUE, digits = 3))),
          p(if(p_borough < 0.05) "✓ SIGNIFICANT - Borough affects charges" else "✗ Not significant")
      ),
      div(class = if(p_season < 0.05) "decision-box significant" else "decision-box",
          h4("2. Season Main Effect"),
          p(paste0("P-value: ", format(p_season, scientific = TRUE, digits = 3))),
          p(if(p_season < 0.05) "✓ SIGNIFICANT - Season affects charges" else "✗ Not significant")
      ),
      div(class = if(p_interaction < 0.05) "decision-box significant" else "decision-box",
          h4("3. Borough × Season Interaction"),
          p(paste0("P-value: ", format(p_interaction, scientific = TRUE, digits = 3))),
          p(if(p_interaction < 0.05) 
            "✓ SIGNIFICANT - The effect of season depends on borough" 
            else 
              "✗ Not significant - Seasonal effect is consistent across boroughs")
      )
    )
  })
  
  # ===========================================================================
  # REGRESSION
  # ===========================================================================
  
  reg_model <- reactive({
    lm(current_charges ~ borough + season, data = filtered_data())
  })
  
  output$regression_output <- renderPrint({
    summary(reg_model())
  })
  
  output$conf_int_output <- renderPrint({
    confint(reg_model())
  })
  
  output$regression_interpretation <- renderUI({
    reg_summary <- summary(reg_model())
    r_sq <- round(reg_summary$r.squared, 4)
    adj_r_sq <- round(reg_summary$adj.r.squared, 4)
    
    div(class = "info-card",
        h4(icon("chart-pie"), " R-Squared Interpretation"),
        p(strong("R² = "), r_sq),
        p(strong("Adjusted R² = "), adj_r_sq),
        hr(),
        p(paste0("Approximately ", round(r_sq * 100, 2), "% of the variance in current 
               electricity charges is explained by borough and season combined.")),
        p(style = "color: #f5a623;",
          "Note: While statistically significant, this indicates that other factors 
        (building size, consumption patterns, rate class) explain the majority of 
        variation in electricity charges.")
    )
  })
  
  output$diagnostic_plots <- renderPlot({
    par(mfrow = c(2, 2), bg = "transparent", fg = "#eaeaea", 
        col.axis = "#eaeaea", col.lab = "#eaeaea", col.main = "#eaeaea")
    plot(reg_model(), col = "#e94560", pch = 16)
  }, bg = "transparent")
  
  output$effect_size_output <- renderPrint({
    # Calculate eta-squared
    anova_b <- summary(anova_borough())
    anova_s <- summary(anova_season())
    
    ss_borough <- anova_b[[1]]$`Sum Sq`[1]
    ss_total_borough <- sum(anova_b[[1]]$`Sum Sq`)
    eta_sq_borough <- ss_borough / ss_total_borough
    
    ss_season <- anova_s[[1]]$`Sum Sq`[1]
    ss_total_season <- sum(anova_s[[1]]$`Sum Sq`)
    eta_sq_season <- ss_season / ss_total_season
    
    cat("ETA-SQUARED (η²) - Proportion of Variance Explained:\n")
    cat("====================================================\n\n")
    cat("Borough effect: η² =", round(eta_sq_borough, 4), 
        "(", round(eta_sq_borough * 100, 2), "% of variance)\n")
    cat("Season effect:  η² =", round(eta_sq_season, 4), 
        "(", round(eta_sq_season * 100, 2), "% of variance)\n\n")
    cat("Interpretation guidelines:\n")
    cat("  Small effect:  η² ≈ 0.01\n")
    cat("  Medium effect: η² ≈ 0.06\n")
    cat("  Large effect:  η² ≈ 0.14\n")
  })
  
  # ===========================================================================
  # SUMMARY TABLE
  # ===========================================================================
  
  output$summary_table <- renderDT({
    anova_b <- summary(anova_borough())
    anova_s <- summary(anova_season())
    anova_tw <- summary(anova_twoway())
    reg_sum <- summary(reg_model())
    
    summary_df <- data.frame(
      Test = c("One-Way ANOVA (Borough)", 
               "One-Way ANOVA (Season)", 
               "Two-Way ANOVA (Borough)", 
               "Two-Way ANOVA (Season)",
               "Two-Way ANOVA (Interaction)",
               "Multiple Regression"),
      `F Statistic` = c(
        round(anova_b[[1]]$`F value`[1], 2),
        round(anova_s[[1]]$`F value`[1], 2),
        round(anova_tw[[1]]$`F value`[1], 2),
        round(anova_tw[[1]]$`F value`[2], 2),
        round(anova_tw[[1]]$`F value`[3], 2),
        round(reg_sum$fstatistic[1], 2)
      ),
      `P Value` = c(
        format(anova_b[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 3),
        format(anova_s[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 3),
        format(anova_tw[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 3),
        format(anova_tw[[1]]$`Pr(>F)`[2], scientific = TRUE, digits = 3),
        format(anova_tw[[1]]$`Pr(>F)`[3], scientific = TRUE, digits = 3),
        "< 2.2e-16"
      ),
      Significant = c(
        ifelse(anova_b[[1]]$`Pr(>F)`[1] < 0.05, "Yes ✓", "No"),
        ifelse(anova_s[[1]]$`Pr(>F)`[1] < 0.05, "Yes ✓", "No"),
        ifelse(anova_tw[[1]]$`Pr(>F)`[1] < 0.05, "Yes ✓", "No"),
        ifelse(anova_tw[[1]]$`Pr(>F)`[2] < 0.05, "Yes ✓", "No"),
        ifelse(anova_tw[[1]]$`Pr(>F)`[3] < 0.05, "Yes ✓", "No"),
        "Yes ✓"
      ),
      check.names = FALSE
    )
    
    datatable(
      summary_df,
      options = list(
        dom = 't',
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#1a1a2e', 'color': '#00d9c0'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = 'cell-border'
    )
  })
}

# =============================================================================
# RUN THE APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
