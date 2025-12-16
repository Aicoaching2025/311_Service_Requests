# 311 Shiny App - WITH WEATHER & DEMOGRAPHICS ANALYSIS

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(tidycensus)

# ============================================
# DATA LOADING
# ============================================

# --- 311 Data ---
api_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.json"

query_params <- list(
  `$limit` = 50000,
  `$order` = "created_date DESC"
)

response <- GET(api_url, query = query_params)

df_311 <- content(response, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  as.data.frame() %>%
  mutate(
    created_date = as.POSIXct(created_date, format = "%Y-%m-%dT%H:%M:%S"),
    closed_date = as.POSIXct(closed_date, format = "%Y-%m-%dT%H:%M:%S"),
    date_only = as.Date(created_date),
    month = floor_date(date_only, "month"),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    hour = hour(created_date),
    day_of_week = wday(created_date, label = TRUE, abbr = FALSE),
    month_name = month(created_date, label = TRUE, abbr = FALSE),
    response_time = as.numeric(difftime(closed_date, created_date, units = "hours"))
  )

# --- Weather Data ---
start_date <- min(df_311$date_only, na.rm = TRUE)
end_date <- max(df_311$date_only, na.rm = TRUE)

weather_url <- "https://archive-api.open-meteo.com/v1/archive"

weather_params <- list(
  latitude = 40.7128,
  longitude = -74.0060,
  start_date = as.character(start_date),
  end_date = as.character(end_date),
  daily = "temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum",
  timezone = "America/New_York"
)

weather_response <- GET(weather_url, query = weather_params)

if (status_code(weather_response) == 200) {
  weather_json <- fromJSON(content(weather_response, "text", encoding = "UTF-8"))
  
  weather_df <- data.frame(
    date = as.Date(weather_json$daily$time),
    temp_max_f = weather_json$daily$temperature_2m_max * 9/5 + 32,
    temp_min_f = weather_json$daily$temperature_2m_min * 9/5 + 32,
    temp_mean_f = weather_json$daily$temperature_2m_mean * 9/5 + 32,
    precipitation_mm = weather_json$daily$precipitation_sum
  )
  
  # Join weather to 311 data
  df_311 <- df_311 %>%
    left_join(weather_df, by = c("date_only" = "date"))
}

# --- Demographics Data ---
nyc_demographics <- data.frame(
  borough = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
  population = c(1472654, 2736074, 1694263, 2405464, 495747),
  median_income = c(43726, 67572, 93651, 75748, 85381),
  pct_renter = c(80.2, 69.8, 75.6, 54.3, 35.8),
  pct_poverty = c(26.8, 19.2, 15.6, 11.4, 10.2)
)

# Join demographics to 311 data
df_311 <- df_311 %>%
  left_join(nyc_demographics, by = "borough")

# ============================================
# SCRUB DATA
# ============================================

df_311 <- df_311 %>%
  distinct() %>%
  mutate(
    borough = toupper(trimws(borough)),
    complaint_type = toupper(trimws(complaint_type)),
    borough = if_else(borough %in% c("", "UNSPECIFIED", "NA"), NA_character_, borough)
  ) %>%
  filter(is.na(closed_date) | closed_date >= created_date) %>%
  filter(created_date <= Sys.time()) %>%
  mutate(
    response_time = if_else(response_time < 0, NA_real_, response_time)
  )

# ============================================
# UI
# ============================================
ui <- dashboardPage(
  dashboardHeader(title = "NYC 311 Monitoring Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Borough Analysis", tabName = "borough", icon = icon("map")),
      menuItem("Complaint Patterns", tabName = "complaints", icon = icon("exclamation-circle")),
      menuItem("Response Time Analysis", tabName = "response", icon = icon("hourglass-half")),
      menuItem("Weather & Demographics", tabName = "correlations", icon = icon("chart-line"))
    ),
    
    hr(),
    h4("  Filters", style = "padding-left: 15px;"),
    
    selectInput("borough", "Select Borough:",
                choices = c("All", unique(df_311$borough[!is.na(df_311$borough) & df_311$borough != "Unspecified"])),
                selected = "All"),
    
    selectInput("complaint", "Complaint Type:",
                choices = c("All", names(sort(table(df_311$complaint_type), decreasing = TRUE)[1:25])),
                selected = "All"),
    
    dateRangeInput("dates", "Date Range:",
                   start = min(df_311$date_only, na.rm = TRUE),
                   end = max(df_311$date_only, na.rm = TRUE),
                   format = "yyyy-mm-dd",
                   separator = " to ")
  ),
  
  dashboardBody(
    tags$style(HTML("
      .small-box h3 { font-size: 20px; }
      .box-header { font-weight: bold; }
      .shiny-date-input { width: 100%; }
      .input-daterange input { font-size: 12px; }
      .input-daterange { width: 100%; }
    ")),
    
    tabItems(
      
      # ---- TAB 1: OVERVIEW ----
      tabItem(tabName = "overview",
              h2("NYC 311 Service Requests: Overview"),
              
              fluidRow(
                valueBoxOutput("total_complaints", width = 4),
                valueBoxOutput("top_complaint", width = 4),
                valueBoxOutput("avg_response", width = 4)
              ),
              
              fluidRow(
                box(title = "Complaint Locations", 
                    width = 12, 
                    leafletOutput("map", height = 500),
                    footer = "Click markers to see complaint details. Use filters to narrow by borough and complaint type.")
              ),
              
              fluidRow(
                box(title = "Complaint Count by Type", 
                    width = 6, 
                    DTOutput("complaint_count_table", height = 350)),
                box(title = "Complaint Count by Location (Address)", 
                    width = 6, 
                    DTOutput("location_count_table", height = 350))
              )
      ),
      
      # ---- TAB 2: BOROUGH ANALYSIS ----
      tabItem(tabName = "borough",
              h2("Borough-Level Analysis"),
              p("Examining how service requests and response times vary across NYC's five boroughs."),
              
              fluidRow(
                box(title = "Complaint Volume by Borough",
                    width = 6,
                    plotlyOutput("borough_volume_plot", height = 350)),
                box(title = "Top Complaint Types by Borough",
                    width = 6,
                    plotlyOutput("borough_complaint_heatmap", height = 350))
              ),
              
              fluidRow(
                box(title = "Borough Comparison: Key Metrics",
                    width = 12,
                    DTOutput("borough_summary_table"))
              ),
              
              fluidRow(
                box(title = "Complaint Distribution Within Each Borough",
                    width = 12,
                    plotlyOutput("borough_facet_plot", height = 500))
              )
      ),
      
      # ---- TAB 3: COMPLAINT PATTERNS ----
      tabItem(tabName = "complaints",
              h2("Complaint Type Analysis"),
              p("Understanding which types of complaints are most common and how they vary by location."),
              
              fluidRow(
                box(title = "Top 15 Complaint Types",
                    width = 6,
                    plotlyOutput("top_complaints_plot", height = 400)),
                box(title = "Complaint Type Distribution",
                    width = 6,
                    plotlyOutput("complaint_pie", height = 400))
              ),
              
              fluidRow(
                box(title = "Complaint Types by Borough (Heatmap)",
                    width = 12,
                    plotlyOutput("complaint_borough_heatmap", height = 450))
              ),
              
              fluidRow(
                box(title = "Complaint Type Statistics",
                    width = 12,
                    DTOutput("complaint_stats_table"))
              )
      ),
      
      # ---- TAB 4: RESPONSE TIME ANALYSIS ----
      tabItem(tabName = "response",
              h2("Response Time Analysis"),
              p("How quickly are 311 complaints resolved? Are there disparities across boroughs or complaint types?"),
              
              fluidRow(
                valueBoxOutput("median_response", width = 4),
                valueBoxOutput("pct_resolved_24h", width = 4),
                valueBoxOutput("pct_resolved_week", width = 4)
              ),
              
              fluidRow(
                box(title = "Response Time Distribution",
                    width = 6,
                    plotlyOutput("response_histogram", height = 350),
                    footer = "Distribution of resolution times (capped at 500 hours for visibility)."),
                box(title = "Response Time by Borough",
                    width = 6,
                    plotlyOutput("response_borough_box", height = 350),
                    footer = "Comparing resolution times across boroughs.")
              ),
              
              fluidRow(
                box(title = "Average Response Time by Complaint Type (Top 15)",
                    width = 12,
                    plotlyOutput("response_complaint_plot", height = 400))
              ),
              
              fluidRow(
                box(title = "Response Time Summary by Borough",
                    width = 6,
                    DTOutput("response_borough_table")),
                box(title = "Response Time Summary by Complaint Type",
                    width = 6,
                    DTOutput("response_complaint_table"))
              )
      ),
      
      # ---- TAB 5: WEATHER & DEMOGRAPHICS CORRELATIONS ----
      tabItem(tabName = "correlations",
              h2("Weather & Demographics Analysis"),
              p("Exploring how weather conditions and socioeconomic factors correlate with 311 complaints."),
              
              # Weather Section
              h3("Weather Correlations", style = "margin-top: 20px;"),
              
              fluidRow(
                box(title = "Complaint Types by Temperature Range",
                    width = 6,
                    plotlyOutput("temp_complaint_heatmap", height = 400),
                    footer = "How complaint patterns shift with temperature"),
                box(title = "Daily Complaints vs Temperature",
                    width = 6,
                    plotlyOutput("complaints_vs_temp", height = 400),
                    footer = "Total daily complaints compared to mean temperature")
              ),
              
              fluidRow(
                box(title = "Heating Complaints by Temperature Range",
                    width = 6,
                    plotlyOutput("heating_vs_temp", height = 350),
                    footer = "Heat/Hot Water complaints increase in cold weather"),
                box(title = "Noise Complaints by Temperature Range",
                    width = 6,
                    plotlyOutput("noise_vs_temp", height = 350),
                    footer = "Do noise complaints rise in warm weather?")
              ),
              
              # Demographics Section
              h3("Demographics Correlations", style = "margin-top: 30px;"),
              
              fluidRow(
                box(title = "Housing Complaints vs Renter Percentage",
                    width = 6,
                    plotlyOutput("housing_vs_renter", height = 400),
                    footer = "Housing-related complaints in renter-heavy boroughs"),
                box(title = "Complaints per Capita vs Median Income",
                    width = 6,
                    plotlyOutput("complaints_vs_income", height = 400),
                    footer = "Do lower-income boroughs file more complaints?")
              ),
              
              fluidRow(
                box(title = "Demographics Summary by Borough",
                    width = 12,
                    DTOutput("demographics_table"))
              )
      )
      
    ) # End tabItems
  )
)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  # ---- Reactive: Filtered Data ----
  filtered_data <- reactive({
    data <- df_311 %>%
      filter(date_only >= input$dates[1] & date_only <= input$dates[2])
    
    if (input$borough != "All") {
      data <- data %>% filter(borough == input$borough)
    }
    
    if (input$complaint != "All") {
      data <- data %>% filter(complaint_type == input$complaint)
    }
    
    data
  })
  
  # Filtered response data (valid response times only)
  filtered_response <- reactive({
    filtered_data() %>%
      filter(!is.na(response_time) & response_time > 0 & response_time < 720)
  })
  
  # ============================================
  # TAB 1: OVERVIEW OUTPUTS
  # ============================================
  
  output$total_complaints <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()), big.mark = ","),
      "Total Complaints",
      icon = icon("phone"),
      color = "blue"
    )
  })
  
  output$top_complaint <- renderValueBox({
    top <- filtered_data() %>%
      count(complaint_type, sort = TRUE) %>%
      slice(1) %>%
      pull(complaint_type)
    
    valueBox(
      ifelse(length(top) > 0, top, "N/A"),
      "Top Complaint Type",
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  output$avg_response <- renderValueBox({
    avg_hrs <- filtered_response() %>%
      pull(response_time) %>%
      median(na.rm = TRUE)
    
    valueBox(
      ifelse(!is.na(avg_hrs), paste(round(avg_hrs, 1), "hrs"), "N/A"),
      "Median Response Time",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  output$map <- renderLeaflet({
    map_data <- filtered_data() %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      head(1000)
    
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = -73.95, lat = 40.75, zoom = 11) %>%
      addMarkers(
        ~longitude, ~latitude,
        popup = ~paste(
          "<b>Complaint Type:</b>", complaint_type, "<br>",
          "<b>Borough:</b>", borough, "<br>",
          "<b>Address:</b>", incident_address, "<br>",
          "<b>Date:</b>", as.character(created_date), "<br>",
          "<b>Status:</b>", status
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$complaint_count_table <- renderDT({
    filtered_data() %>%
      count(complaint_type, sort = TRUE) %>%
      rename(`Complaint Type` = complaint_type, `Count` = n) %>%
      head(15) %>%
      datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE
      )
  })
  
  output$location_count_table <- renderDT({
    filtered_data() %>%
      filter(!is.na(incident_address) & incident_address != "") %>%
      count(incident_address, borough, sort = TRUE) %>%
      rename(Address = incident_address, Borough = borough, Count = n) %>%
      head(15) %>%
      datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE
      )
  })
  
  # ============================================
  # TAB 2: BOROUGH ANALYSIS OUTPUTS
  # ============================================
  
  output$borough_volume_plot <- renderPlotly({
    borough_data <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      count(borough, sort = TRUE)
    
    plot_ly(borough_data, x = ~reorder(borough, -n), y = ~n, type = "bar",
            marker = list(color = c("#8B5CF6", "#3B82F6", "#10B981", "#F59E0B", "#EF4444")),
            text = ~format(n, big.mark = ","), textposition = "outside") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Number of Complaints"),
             showlegend = FALSE)
  })
  
  output$borough_complaint_heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      count(borough, complaint_type) %>%
      group_by(borough) %>%
      slice_max(n, n = 5) %>%
      ungroup()
    
    plot_ly(heatmap_data, x = ~borough, y = ~complaint_type, z = ~n,
            type = "heatmap", colors = "Blues") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", tickfont = list(size = 10)))
  })
  
  output$borough_summary_table <- renderDT({
    filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      group_by(Borough = borough) %>%
      summarise(
        `Total Complaints` = n(),
        `Unique Complaint Types` = n_distinct(complaint_type),
        `Top Complaint` = names(sort(table(complaint_type), decreasing = TRUE))[1],
        `Avg Response (hrs)` = round(mean(response_time[response_time > 0 & response_time < 720], na.rm = TRUE), 1)
      ) %>%
      arrange(desc(`Total Complaints`)) %>%
      datatable(options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
  
  output$borough_facet_plot <- renderPlotly({
    facet_data <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      count(borough, complaint_type) %>%
      group_by(borough) %>%
      slice_max(n, n = 5) %>%
      ungroup()
    
    p <- ggplot(facet_data, aes(x = reorder(complaint_type, n), y = n, fill = borough)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~borough, scales = "free_y", ncol = 3) +
      labs(x = "", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 8))
    
    ggplotly(p, height = 500)
  })
  
  # ============================================
  # TAB 3: COMPLAINT PATTERNS OUTPUTS
  # ============================================
  
  output$top_complaints_plot <- renderPlotly({
    complaint_data <- filtered_data() %>%
      count(complaint_type, sort = TRUE) %>%
      head(15)
    
    plot_ly(complaint_data, x = ~n, y = ~reorder(complaint_type, n), 
            type = "bar", orientation = "h",
            marker = list(color = "steelblue")) %>%
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Number of Complaints"))
  })
  
  output$complaint_pie <- renderPlotly({
    pie_data <- filtered_data() %>%
      count(complaint_type, sort = TRUE) %>%
      head(8) %>%
      mutate(complaint_type = ifelse(row_number() > 7, "Other", complaint_type))
    
    plot_ly(pie_data, labels = ~complaint_type, values = ~n, type = "pie",
            textinfo = "percent", hoverinfo = "label+value") %>%
      layout(showlegend = TRUE)
  })
  
  output$complaint_borough_heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      count(complaint_type, borough) %>%
      filter(complaint_type %in% names(sort(table(filtered_data()$complaint_type), decreasing = TRUE)[1:15]))
    
    plot_ly(heatmap_data, x = ~borough, y = ~complaint_type, z = ~n,
            type = "heatmap", colors = "YlOrRd") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", tickfont = list(size = 10)))
  })
  
  output$complaint_stats_table <- renderDT({
    filtered_data() %>%
      count(complaint_type, sort = TRUE) %>%
      head(20) %>%
      mutate(
        Percentage = paste0(round(n / sum(n) * 100, 1), "%"),
        `Cumulative %` = paste0(round(cumsum(n) / sum(n) * 100, 1), "%")
      ) %>%
      rename(`Complaint Type` = complaint_type, Count = n) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ============================================
  # TAB 4: RESPONSE TIME ANALYSIS OUTPUTS
  # ============================================
  
  output$median_response <- renderValueBox({
    med <- filtered_response() %>%
      pull(response_time) %>%
      median(na.rm = TRUE)
    
    valueBox(
      paste(round(med, 1), "hrs"),
      "Median Response Time",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$pct_resolved_24h <- renderValueBox({
    pct <- filtered_response() %>%
      summarise(pct = mean(response_time <= 24, na.rm = TRUE) * 100) %>%
      pull(pct)
    
    valueBox(
      paste0(round(pct, 1), "%"),
      "Resolved Within 24 Hours",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$pct_resolved_week <- renderValueBox({
    pct <- filtered_response() %>%
      summarise(pct = mean(response_time <= 168, na.rm = TRUE) * 100) %>%
      pull(pct)
    
    valueBox(
      paste0(round(pct, 1), "%"),
      "Resolved Within 1 Week",
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$response_histogram <- renderPlotly({
    hist_data <- filtered_response() %>%
      filter(response_time <= 500)
    
    plot_ly(hist_data, x = ~response_time, type = "histogram", 
            nbinsx = 50, marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Response Time (Hours)"),
             yaxis = list(title = "Frequency"))
  })
  
  output$response_borough_box <- renderPlotly({
    box_data <- filtered_response() %>%
      filter(!is.na(borough) & borough != "Unspecified")
    
    plot_ly(box_data, x = ~borough, y = ~response_time, type = "box",
            color = ~borough) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Response Time (Hours)", range = c(0, 200)),
             showlegend = FALSE)
  })
  
  output$response_complaint_plot <- renderPlotly({
    response_complaint <- filtered_response() %>%
      group_by(complaint_type) %>%
      summarise(
        avg_response = mean(response_time, na.rm = TRUE),
        count = n()
      ) %>%
      filter(count >= 10) %>%
      arrange(desc(avg_response)) %>%
      head(15)
    
    plot_ly(response_complaint, x = ~avg_response, y = ~reorder(complaint_type, avg_response),
            type = "bar", orientation = "h",
            marker = list(color = "#EF4444")) %>%
      layout(xaxis = list(title = "Average Response Time (Hours)"),
             yaxis = list(title = ""))
  })
  
  output$response_borough_table <- renderDT({
    filtered_response() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      group_by(Borough = borough) %>%
      summarise(
        `Count` = n(),
        `Mean (hrs)` = round(mean(response_time, na.rm = TRUE), 1),
        `Median (hrs)` = round(median(response_time, na.rm = TRUE), 1),
        `Std Dev` = round(sd(response_time, na.rm = TRUE), 1),
        `% < 24hrs` = paste0(round(mean(response_time <= 24) * 100, 1), "%")
      ) %>%
      arrange(desc(`Mean (hrs)`)) %>%
      datatable(options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
  
  output$response_complaint_table <- renderDT({
    filtered_response() %>%
      group_by(`Complaint Type` = complaint_type) %>%
      summarise(
        Count = n(),
        `Mean (hrs)` = round(mean(response_time, na.rm = TRUE), 1),
        `Median (hrs)` = round(median(response_time, na.rm = TRUE), 1)
      ) %>%
      filter(Count >= 10) %>%
      arrange(desc(`Mean (hrs)`)) %>%
      head(15) %>%
      datatable(options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # ============================================
  # TAB 5: WEATHER & DEMOGRAPHICS OUTPUTS
  # ============================================
  
  # --- Weather Visualizations ---
  
  # Complaint Types by Temperature Range (Heatmap)
  output$temp_complaint_heatmap <- renderPlotly({
    temp_data <- filtered_data() %>%
      filter(!is.na(temp_mean_f)) %>%
      mutate(temp_range = cut(temp_mean_f, 
                              breaks = c(0, 40, 55, 70, 85, 100),
                              labels = c("<40°F", "40-55°F", "55-70°F", "70-85°F", ">85°F"))) %>%
      filter(!is.na(temp_range)) %>%
      filter(complaint_type %in% names(sort(table(filtered_data()$complaint_type), decreasing = TRUE)[1:10])) %>%
      count(complaint_type, temp_range)
    
    plot_ly(temp_data, x = ~temp_range, y = ~complaint_type, z = ~n,
            type = "heatmap", colors = "YlOrRd") %>%
      layout(xaxis = list(title = "Temperature Range"),
             yaxis = list(title = "", tickfont = list(size = 10)))
  })
  
  # Daily Complaints vs Temperature (Scatter)
  output$complaints_vs_temp <- renderPlotly({
    daily_temp <- filtered_data() %>%
      filter(!is.na(temp_mean_f)) %>%
      group_by(date_only, temp_mean_f) %>%
      summarise(complaints = n(), .groups = "drop")
    
    plot_ly(daily_temp, x = ~temp_mean_f, y = ~complaints, type = "scatter", mode = "markers",
            marker = list(color = "steelblue", opacity = 0.5)) %>%
      layout(xaxis = list(title = "Mean Temperature (°F)"),
             yaxis = list(title = "Daily Complaints"))
  })
  
  # Heating Complaints by Temperature Range (Bar)
  output$heating_vs_temp <- renderPlotly({
    heating_data <- filtered_data() %>%
      filter(complaint_type == "HEAT/HOT WATER" & !is.na(temp_mean_f)) %>%
      mutate(temp_range = cut(temp_mean_f, 
                              breaks = c(0, 32, 45, 60, 75, 100),
                              labels = c("<32°F", "32-45°F", "45-60°F", "60-75°F", ">75°F"))) %>%
      filter(!is.na(temp_range)) %>%
      count(temp_range)
    
    plot_ly(heating_data, x = ~temp_range, y = ~n, type = "bar",
            marker = list(color = c("darkblue", "steelblue", "skyblue", "khaki", "orange"))) %>%
      layout(xaxis = list(title = "Temperature Range"),
             yaxis = list(title = "Number of Heating Complaints"))
  })
  
  # Noise Complaints by Temperature Range (Bar)
  output$noise_vs_temp <- renderPlotly({
    noise_data <- filtered_data() %>%
      filter(grepl("Noise", complaint_type, ignore.case = TRUE) & !is.na(temp_mean_f)) %>%
      mutate(temp_range = cut(temp_mean_f, 
                              breaks = c(0, 40, 55, 70, 85, 100),
                              labels = c("<40°F", "40-55°F", "55-70°F", "70-85°F", ">85°F"))) %>%
      filter(!is.na(temp_range)) %>%
      count(temp_range)
    
    plot_ly(noise_data, x = ~temp_range, y = ~n, type = "bar",
            marker = list(color = c("lightblue", "skyblue", "khaki", "orange", "red"))) %>%
      layout(xaxis = list(title = "Temperature Range"),
             yaxis = list(title = "Number of Noise Complaints"))
  })
  
  # --- Demographics Visualizations ---
  
  # Housing Complaints vs Renter Percentage
  output$housing_vs_renter <- renderPlotly({
    housing_data <- filtered_data() %>%
      filter(complaint_type %in% c("HEAT/HOT WATER", "PLUMBING", "WATER SYSTEM", "UNSANITARY CONDITION")) %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      group_by(borough, pct_renter, population) %>%
      summarise(housing_complaints = n(), .groups = "drop") %>%
      mutate(housing_per_1000 = housing_complaints / population * 1000)
    
    plot_ly(housing_data, x = ~pct_renter, y = ~housing_per_1000, 
            type = "scatter", mode = "markers+text",
            marker = list(size = 15, color = "orange"),
            text = ~borough, textposition = "top center") %>%
      layout(xaxis = list(title = "Percent Renter-Occupied Housing"),
             yaxis = list(title = "Housing Complaints per 1,000 Residents"))
  })
  
  # Complaints per Capita vs Median Income
  output$complaints_vs_income <- renderPlotly({
    income_data <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      group_by(borough, population, median_income) %>%
      summarise(total_complaints = n(), .groups = "drop") %>%
      mutate(complaints_per_1000 = total_complaints / population * 1000)
    
    plot_ly(income_data, x = ~median_income, y = ~complaints_per_1000, 
            type = "scatter", mode = "markers+text",
            marker = list(size = 15, color = "steelblue"),
            text = ~borough, textposition = "top center") %>%
      layout(xaxis = list(title = "Median Household Income ($)", tickformat = "$,.0f"),
             yaxis = list(title = "Complaints per 1,000 Residents"))
  })
  
  # Demographics Summary Table
  output$demographics_table <- renderDT({
    demo_summary <- filtered_data() %>%
      filter(!is.na(borough) & borough != "Unspecified") %>%
      group_by(Borough = borough) %>%
      summarise(
        `Total Complaints` = n(),
        .groups = "drop"
      ) %>%
      left_join(nyc_demographics, by = c("Borough" = "borough")) %>%
      mutate(
        `Complaints per 1,000` = round(`Total Complaints` / population * 1000, 1),
        `Median Income` = paste0("$", format(median_income, big.mark = ",")),
        `% Renters` = paste0(round(pct_renter, 1), "%"),
        `% Poverty` = paste0(round(pct_poverty, 1), "%")
      ) %>%
      select(Borough, `Total Complaints`, `Complaints per 1,000`, 
             `Median Income`, `% Renters`, `% Poverty`)
    
    datatable(demo_summary, options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
}

# ============================================
# RUN APP
# ============================================
shinyApp(ui, server)
