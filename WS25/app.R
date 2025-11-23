library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(DT)
library(plotly)
library(scales)
library(bslib)
library(survival)

# --- Data Loading ---------
# Load pre-processed data
patient_demo <- readRDS("full_dataset.rds")
scored_data <- readRDS("shiny_data.rds")

# --- Data Preparation ----------
# Prepare Master Dataset for General Analysis
master_data <- patient_demo %>%
  mutate(
    # Date parsing (ensure they are Dates)
    date_surgery = as.Date(date_surgery),
    date_birth = as.Date(date_birth),
    
    # Joint Label
    joint_label = case_when(
      str_detect(affected_joint, "(?i)Hip") ~ "Hip",
      str_detect(affected_joint, "(?i)Knee") ~ "Knee",
      TRUE ~ "Other"
    ),
    
    # Procedure Stage
    procedure_stage = ifelse(str_detect(op_type, "(?i)Revision"), "Revision", "Primary"),
    
    # ASA Grouping (Fix string parsing)
    asa_clean = str_extract(pat_asa_surgery, "\\d+"), # Extract number
    asa_label = case_when(
      asa_clean == "1" ~ "ASA I",
      asa_clean == "2" ~ "ASA II",
      asa_clean == "3" ~ "ASA III",
      asa_clean %in% c("4", "5") ~ "ASA IV/V",
      TRUE ~ "Unknown"
    ),
    
    # BMI Calculation (if not already valid)
    bmi = ifelse(is.na(bmi_surgery), bmi_enrollment, bmi_surgery),
    
    # Hospital Name (Clean up)
    hospital = str_to_title(str_trim(hospital_of_surgery_surgery)),
    hospital = ifelse(is.na(hospital) | hospital == "", "Unknown", hospital),
    
    # Manufacturer (Handle NAs)
    manufacturer = case_when(
      !is.na(stem_manufucturer) & stem_manufucturer != "" ~ stem_manufucturer,
      !is.na(implant_manufacturer_surgery) & implant_manufacturer_surgery != "" ~ str_trim(implant_manufacturer_surgery),
      TRUE ~ "Unknown"
    ),
    
    # Time variables
    year = year(date_surgery),
    month = floor_date(date_surgery, "month"),
    week = floor_date(date_surgery, "week")
  )

# --- Helper Functions ----------

# Generate Benchmark Data for Funnel Plot (Simulated)
generate_benchmark_data <- function(knh_data) {
  # Calculate KNH stats
  knh_vol <- nrow(knh_data)
  knh_rev_rate <- mean(knh_data$procedure_stage == "Revision", na.rm = TRUE) * 100
  
  # Simulate 50 other hospitals
  set.seed(123)
  n_hospitals <- 50
  volumes <- round(rlnorm(n_hospitals, meanlog = 5, sdlog = 1)) # Log-normal distribution for volume
  
  # Revision rate depends slightly on volume (volume-outcome relationship) + noise
  base_rate <- 5 # 5% average
  rates <- base_rate + rnorm(n_hospitals, 0, 2) - (log(volumes) * 0.5)
  rates <- pmax(0.5, rates) # Minimum 0.5%
  
  bench_data <- data.frame(
    hospital = paste("Hospital", 1:n_hospitals),
    volume = volumes,
    revision_rate = rates,
    is_knh = FALSE
  )
  
  # Add KNH (From the "real" simulated Data)
  knh_entry <- data.frame(
    hospital = "Kenyatta National Hospital",
    volume = knh_vol,
    revision_rate = knh_rev_rate,
    is_knh = TRUE
  )
  
  bind_rows(bench_data, knh_entry)
}

# --- UI Definition ------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Kenya Arthroplasty Registry",
    # Theme Selector in Header
    tags$li(class = "dropdown",
            style = "padding: 8px;",
            selectInput("theme_selector", NULL, 
                        choices = c("Standard", "Vibrant", "Dark Mode", "Nature", "Pastel", "Ocean"), 
                        selected = "Standard", width = "150px")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quality Control", tabName = "quality", icon = icon("shield-alt")),
      menuItem("Operational Volume", tabName = "operations", icon = icon("chart-bar")),
      menuItem("Complications", tabName = "complications", icon = icon("exclamation-triangle")),
      hr(),
      h4("Global Filters", style = "padding-left: 15px; color: #b8c7ce;"),
      selectInput("filter_joint", "Joint:", choices = c("All", "Hip", "Knee", "Other"), selected = "All"),
      
      # Age Filter: Plotly Histogram + Range Slider
      div(style = "padding: 0 15px;",
          p("Age Distribution (Select Range):", style = "color: #b8c7ce; font-size: 12px; margin-bottom: 5px;"),
          plotlyOutput("age_hist_slider", height = "120px")
      ),
      
      selectInput("filter_year_global", "Year:", choices = c("All", sort(unique(master_data$year))), selected = "All")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box.box-solid.box-primary>.box-header { background: #fff; color: #333; }
        .box.box-solid.box-primary { border: 1px solid #d2d6de; }
        .small-box { box-shadow: 0 1px 1px rgba(0,0,0,0.1); }
        /* Adjust header dropdown alignment */
        .navbar-custom-menu .navbar-nav>li { float: left; }
        .navbar-custom-menu .navbar-nav { float: right; }
      "))
    ),
    
    tabItems(
      # --- Tab 1: Quality Control ---
      tabItem(tabName = "quality",
        fluidRow(
          box(width = 12, title = "Hospital Performance: Revision Rate Funnel Plot", status = "primary", solidHeader = TRUE,
              plotlyOutput("funnel_plot", height = "500px"),
              p("Comparison of KNH (Red Diamond) against National Benchmark (Simulated).")
          )
        ),
        fluidRow(
          box(width = 12, title = "Case Concentration Score (CCS) & Trends", status = "info", solidHeader = TRUE,
              column(12, selectInput("ccs_manufacturer", "Select Manufacturer:", choices = NULL, width = "300px")),
              column(4, plotlyOutput("ccs_gauge_plot", height = "300px")), # Gauge
              column(8, plotlyOutput("ccs_trend_plot", height = "300px"))  # Trend
          )
        )
      ),
      
      # --- Tab 2: Operational Volume ---
      tabItem(tabName = "operations",
        fluidRow(
          valueBoxOutput("vol_total_box", width = 3),
          valueBoxOutput("vol_primary_box", width = 3),
          valueBoxOutput("vol_rev_box", width = 3),
          valueBoxOutput("avg_age_box", width = 3)
        ),
        fluidRow(
          box(width = 8, title = "Volume Trends", status = "primary",
              div(style="display:inline-block; vertical-align:top;", 
                  selectInput("vol_time_filter", "Time Scale:", 
                              choices = c("Week", "Month", "Year", "All Time"), selected = "Month", width = "150px")),
              plotlyOutput("volume_trend_plot", height = "400px")
          ),
          box(width = 4, title = "ASA Score Distribution", status = "primary",
              plotlyOutput("asa_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Top Diagnoses (Treemap)", status = "primary",
              plotlyOutput("top_diagnoses_plot", height = "600px")
          )
        )
      ),
      
      # --- Tab 3: Complications ---
      tabItem(tabName = "complications",
        fluidRow(
          box(width = 12, title = "Surgical Complications Survival Analysis", status = "danger",
              p("Kaplan-Meier survival curve showing probability of complication-free survival (Time to Revision)."),
              plotlyOutput("complication_survival_plot", height = "500px")
          )
        ),
        fluidRow(
          box(width = 6, title = "Reasons for Early Revision (<2 Years)", status = "warning",
              plotlyOutput("early_revision_reasons", height = "400px")
          ),
          box(width = 6, title = "Risk of Complication Over Time", status = "warning",
              selectInput("comp_time_filter", "Time Scale:", choices = c("Week", "Month", "Year"), selected = "Month"),
              plotlyOutput("complication_risk_plot", height = "400px")
          )
        )
      )
    )
  )
)

# --- Server Logic --------------
server <- function(input, output, session) {
  
  ## --- Reactive Theme --------
  current_theme <- reactive({
    switch(input$theme_selector,
           "Standard" = list(primary = "steelblue", secondary = "lightblue", accent = "red", text = "black", bg = "white"),
           "Vibrant" = list(primary = "#9b59b6", secondary = "#e74c3c", accent = "#f1c40f", text = "#2c3e50", bg = "#ecf0f1"),
           "Dark Mode" = list(primary = "#3498db", secondary = "#95a5a6", accent = "#e74c3c", text = "white", bg = "#2c3e50"),
           "Nature" = list(primary = "#27ae60", secondary = "#8e44ad", accent = "#d35400", text = "#2c3e50", bg = "#fdfefe"),
           "Pastel" = list(primary = "#ffb3ba", secondary = "#baffc9", accent = "#bae1ff", text = "#555555", bg = "#fffdf9"),
           "Ocean" = list(primary = "#006994", secondary = "#00a8cc", accent = "#ff7f50", text = "#003366", bg = "#e0f7fa")
    )
  })
  
  ## --- Reactive Values for Age Filter ---------
  age_range <- reactiveVal(c(15, 85))
  
  observeEvent(event_data("plotly_relayout", source = "age_hist"), {
    d <- event_data("plotly_relayout", source = "age_hist")
    if (!is.null(d[["xaxis.range[0]"]])) {
      age_range(c(d[["xaxis.range[0]"]], d[["xaxis.range[1]"]]))
    } else {
      age_range(c(15, 85))
    }
  })
  
  ## --- Reactive Data Filtering ----------
  filtered_data <- reactive({
    data <- master_data
    
    if (input$filter_joint != "All") {
      if (input$filter_joint == "Other") {
        data <- data %>% filter(joint_label == "Other")
      } else {
        data <- data %>% filter(joint_label == input$filter_joint)
      }
    }
    
    rng <- age_range()
    data <- data %>% filter(age >= rng[1] & age <= rng[2])
    
    if (input$filter_year_global != "All") {
      data <- data %>% filter(year == as.numeric(input$filter_year_global))
    }
    
    data
  })
  
  ## Sidebar Age Histogram (High Contrast)-----------
  output$age_hist_slider <- renderPlotly({
    data <- master_data
    if (input$filter_joint != "All") {
       if (input$filter_joint == "Other") {
        data <- data %>% filter(joint_label == "Other")
      } else {
        data <- data %>% filter(joint_label == input$filter_joint)
      }
    }
    if (input$filter_year_global != "All") {
      data <- data %>% filter(year == as.numeric(input$filter_year_global))
    }
    
    # High contrast color for sidebar (Cyan/Bright Blue)
    contrast_color <- "#00c0ef" 
    
    plot_ly(data, x = ~age, type = "histogram", marker = list(color = contrast_color), source = "age_hist") %>%
      layout(
        xaxis = list(title = "", rangeslider = list(visible = TRUE), range = c(15, 85), tickfont = list(color = "white")),
        yaxis = list(title = "", showticklabels = FALSE),
        margin = list(l=0, r=0, t=0, b=20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  ## --- Tab 1: Quality Control ----------
  ### Funnel plot-----------
  
  output$funnel_plot <- renderPlotly({
    knh_dat <- filtered_data()
    bench <- generate_benchmark_data(knh_dat)
    theme <- current_theme()
    
    avg_rate <- mean(bench$revision_rate) / 100
    bench <- bench %>%
      mutate(
        se = sqrt(avg_rate * (1 - avg_rate) / volume),
        lower_95 = pmax(0, (avg_rate - 1.96 * se) * 100),
        upper_95 = (avg_rate + 1.96 * se) * 100,
        lower_99 = pmax(0, (avg_rate - 3.09 * se) * 100),
        upper_99 = (avg_rate + 3.09 * se) * 100
      )
    
    p <- ggplot(bench, aes(x = volume, y = revision_rate)) +
      geom_point(data = subset(bench, !is_knh), color = "lightgrey", alpha = 0.5, size = 2) +
      geom_line(aes(y = upper_95), linetype = "dashed", color = "orange") +
      geom_line(aes(y = upper_99), linetype = "dashed", color = "red") +
      geom_line(aes(y = lower_95), linetype = "dashed", color = "orange") +
      geom_hline(yintercept = avg_rate * 100, color = "blue", alpha = 0.5) +
      geom_point(data = subset(bench, is_knh), color = theme$accent, size = 6, shape = 18, 
                 aes(text = paste(hospital, "<br>Vol:", volume, "<br>Rate:", round(revision_rate, 2), "%"))) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Procedure Volume", y = "Revision Rate (%)", title = "Funnel Plot: Revision Rates") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
    
    ggplotly(p, tooltip = "text")
  })
  
  observe({
    manufs <- unique(master_data$manufacturer)
    manufs <- manufs[manufs != "Unknown"]
    updateSelectInput(session, "ccs_manufacturer", choices = manufs, selected = manufs[1])
  })
  
  ### CCS Gauge Plot (Enhanced with Color Steps)----------
  output$ccs_gauge_plot <- renderPlotly({
    req(input$ccs_manufacturer)
    data <- filtered_data()
    theme <- current_theme()
    
    total_vol <- nrow(data)
    manuf_vol <- sum(data$manufacturer == input$ccs_manufacturer, na.rm = TRUE)
    share <- ifelse(total_vol > 0, (manuf_vol / total_vol) * 100, 0)
    
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = share,
      title = list(text = "Market Share (%)", font = list(color = theme$text)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(NULL, 100), tickfont = list(color = theme$text)),
        bar = list(color = theme$primary),
        steps = list(
          list(range = c(0, 20), color = "green"),   # Low (Green)
          list(range = c(20, 40), color = "yellow"), # Medium (Yellow)
          list(range = c(40, 100), color = "red")    # High (Red)
        ),
        bgcolor = theme$bg
      )
    ) %>% layout(margin = list(l=20, r=20, t=50, b=20), paper_bgcolor = theme$bg)
  })
  
  ### CCS Trend Plot-------
  output$ccs_trend_plot <- renderPlotly({
    req(input$ccs_manufacturer)
    data <- filtered_data()
    theme <- current_theme()
    
    trend_data <- data %>%
      mutate(date_unit = floor_date(date_surgery, "month")) %>% # Changed to Month
      group_by(date_unit) %>%
      summarise(
        total_vol = n(),
        manuf_vol = sum(manufacturer == input$ccs_manufacturer, na.rm = TRUE),
        share = manuf_vol / total_vol
      ) %>%
      filter(!is.na(date_unit))
    
    p <- ggplot(trend_data, aes(x = date_unit, y = share)) +
      geom_line(color = theme$primary) +
      geom_area(fill = theme$primary, alpha = 0.3) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = "Trend Over Time", x = "Date", y = "Share") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
    
    ggplotly(p)
  })
  
  ##--- Tab 2: Operational Volume ---------
  
  output$vol_total_box <- renderValueBox({ valueBox(nrow(filtered_data()), "Total Procedures", icon = icon("list"), color = "aqua") })
  output$vol_primary_box <- renderValueBox({ valueBox(sum(filtered_data()$procedure_stage == "Primary"), "Primary", icon = icon("plus"), color = "green") })
  output$vol_rev_box <- renderValueBox({ valueBox(sum(filtered_data()$procedure_stage == "Revision"), "Revisions", icon = icon("redo"), color = "red") })
  output$avg_age_box <- renderValueBox({ valueBox(round(mean(filtered_data()$age, na.rm=TRUE), 1), "Avg Age", icon = icon("user"), color = "purple") })
  
  output$volume_trend_plot <- renderPlotly({
    data <- filtered_data()
    theme <- current_theme()
    t_unit <- switch(input$vol_time_filter, "Week" = "week", "Month" = "month", "Year" = "year", "All Time" = "all")
    
    if (t_unit == "all") {
      plot_data <- data %>% summarise(n = n()) %>% mutate(time = "All Time")
      p <- ggplot(plot_data, aes(x = time, y = n)) + geom_col(fill = theme$primary)
    } else {
      plot_data <- data %>%
        mutate(time = floor_date(date_surgery, t_unit)) %>%
        count(time)
      p <- ggplot(plot_data, aes(x = time, y = n)) +
        geom_line(color = theme$primary) + geom_point(color = theme$primary) +
        labs(x = "Date", y = "Cases") + theme_minimal()
    }
    
    p <- p + theme(plot.background = element_rect(fill = theme$bg, color = NA),
                   text = element_text(color = theme$text),
                   axis.text = element_text(color = theme$text))
    ggplotly(p)
  })
  
  output$top_diagnoses_plot <- renderPlotly({
    data <- filtered_data() %>% count(diagnosis) %>% filter(!is.na(diagnosis))
    theme <- current_theme()
    
    plot_ly(data, labels = ~diagnosis, parents = NA, values = ~n, type = 'treemap',
            textinfo = "label+value+percent parent", source = "treemap_source") %>% # Added source
      layout(title = list(text = "Diagnoses Distribution", font = list(color = theme$text)),
             paper_bgcolor = theme$bg)
  })
  
  # Treemap Drill-down Observer
  observeEvent(event_data("plotly_click", source = "treemap_source"), {
    clickData <- event_data("plotly_click", source = "treemap_source")
    if (is.null(clickData)) return()
    
    # Extract diagnosis label (Treemap click data structure can vary, usually 'label' or 'pointNumber' mapping)
    # For treemap, 'label' should be present if set in plot_ly
    diag_name <- clickData$label
    
    if (is.null(diag_name)) return()
    
    showModal(modalDialog(
      title = paste("Trend for:", diag_name),
      plotlyOutput("diagnosis_trend_plot"),
      easyClose = TRUE,
      footer = NULL,
      size = "l" # Large modal
    ))
    
    output$diagnosis_trend_plot <- renderPlotly({
      theme <- current_theme()
      data <- filtered_data() %>%
        filter(diagnosis == diag_name) %>%
        mutate(date_unit = floor_date(date_surgery, "month")) %>%
        count(date_unit)
      
      p <- ggplot(data, aes(x = date_unit, y = n)) +
        geom_line(color = theme$accent) +
        geom_point(color = theme$accent) +
        labs(x = "Date", y = "Cases", title = paste("Monthly Cases:", diag_name)) +
        theme_minimal() +
        theme(plot.background = element_rect(fill = theme$bg, color = NA),
              text = element_text(color = theme$text),
              axis.text = element_text(color = theme$text))
      
      ggplotly(p)
    })
  })
  
  output$asa_plot <- renderPlotly({
    data <- filtered_data() %>%
      filter(asa_label != "Unknown") %>%
      count(asa_label)
    theme <- current_theme()
    
    p <- ggplot(data, aes(x = asa_label, y = n, fill = asa_label)) +
      geom_col() +
      scale_fill_brewer(palette = "Blues") + # Could be thematic, but Blues is safe
      labs(title = "ASA Score Distribution", x = "ASA Score", y = "Count") +
      theme_minimal() + 
      theme(legend.position = "none",
            plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
      
    ggplotly(p)
  })
  
  ## --- Tab 3: Complications --------
  
  output$complication_survival_plot <- renderPlotly({
    data <- filtered_data()
    theme <- current_theme()
    
    data_surv <- data %>%
      mutate(
        status = ifelse(procedure_stage == "Revision", 1, 0),
        time_days = ifelse(status == 1, 
                           as.numeric(date_surgery - as.Date(date_last_surgery_surgery)),
                           as.numeric(Sys.Date() - date_surgery))
      ) %>%
      filter(time_days > 0 & !is.na(time_days)) %>%
      mutate(time_years = time_days / 365.25)
    
    fit <- survfit(Surv(time_years, status) ~ joint_label, data = data_surv)
    
    if (is.null(fit$strata)) {
      surv_df <- data.frame(time = fit$time, surv = fit$surv, strata = "All")
    } else {
      surv_df <- data.frame(
        time = fit$time,
        surv = fit$surv,
        strata = rep(names(fit$strata), fit$strata)
      )
    }
    
    p <- ggplot(surv_df, aes(x = time, y = surv, color = strata)) +
      geom_step() +
      labs(x = "Years", y = "Survival Probability", color = "Joint") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
    
    ggplotly(p)
  })
  
  output$early_revision_reasons <- renderPlotly({
    data <- filtered_data() %>%
      filter(procedure_stage == "Revision")
    theme <- current_theme()
      
    if("date_last_surgery_surgery" %in% names(data)) {
       data <- data %>%
         mutate(days_diff = as.numeric(date_surgery - as.Date(date_last_surgery_surgery))) %>%
         filter(days_diff < 730)
    }
    
    data <- data %>% count(diagnosis) %>% top_n(10, n)
    
    p <- ggplot(data, aes(x = reorder(diagnosis, n), y = n)) +
      geom_col(fill = theme$secondary) + coord_flip() +
      labs(title = "Reasons for Early Revision", x = "Reason", y = "Count") + 
      theme_minimal() +
      theme(plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
    ggplotly(p)
  })
  
  output$complication_risk_plot <- renderPlotly({
    data <- filtered_data()
    theme <- current_theme()
    t_unit <- switch(input$comp_time_filter, "Week" = "week", "Month" = "month", "Year" = "year")
    
    plot_data <- data %>%
      mutate(time = floor_date(date_surgery, t_unit)) %>%
      group_by(time) %>%
      summarise(risk = mean(procedure_stage == "Revision") * 100)
    
    p <- ggplot(plot_data, aes(x = time, y = risk)) +
      geom_line(color = theme$accent) + 
      labs(x = "Date", y = "Risk (%)") + 
      theme_minimal() +
      theme(plot.background = element_rect(fill = theme$bg, color = NA),
            text = element_text(color = theme$text),
            axis.text = element_text(color = theme$text))
    ggplotly(p)
  })
}

shinyApp(ui, server)