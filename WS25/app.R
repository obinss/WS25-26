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

# --- Data Loading ---
# Load pre-processed data
patient_demo <- readRDS("full_dataset.rds")
scored_data <- readRDS("shiny_data.rds")

# --- Data Preparation ---
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

# --- Helper Functions ---

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
  
  # Add KNH (Real Data)
  knh_entry <- data.frame(
    hospital = "Kenyatta National Hospital",
    volume = knh_vol,
    revision_rate = knh_rev_rate,
    is_knh = TRUE
  )
  
  bind_rows(bench_data, knh_entry)
}

# --- UI Definition ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Kenya Arthroplasty Registry"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quality Control", tabName = "quality", icon = icon("shield-alt")),
      menuItem("Operational Volume", tabName = "operations", icon = icon("chart-bar")),
      menuItem("Complications", tabName = "complications", icon = icon("exclamation-triangle")),
      menuItem("PROMs", tabName = "proms", icon = icon("heartbeat")),
      hr(),
      h4("Global Filters", style = "padding-left: 15px; color: #b8c7ce;"),
      selectInput("filter_joint", "Joint:", choices = c("All", "Hip", "Knee", "Other"), selected = "All"),
      
      # Age Filter with Plotly Histogram + Range Slider
      div(style = "padding: 0 15px;",
          p("Age Distribution (Select Range):", style = "color: #b8c7ce; font-size: 12px;"),
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
              column(4, selectInput("ccs_manufacturer", "Select Manufacturer:", choices = NULL)), 
              column(8, plotlyOutput("ccs_trend_plot", height = "400px"))
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
          box(width = 4, title = "Procedures per Theatre", status = "primary",
              plotlyOutput("theatre_volume_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 6, title = "Top Diagnoses", status = "primary",
              plotlyOutput("top_diagnoses_plot", height = "500px") # Treemap
          ),
          box(width = 6, title = "ASA Score & BMI Distribution", status = "primary",
              plotlyOutput("asa_bmi_plot", height = "500px") # Boxplot
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
      ),
      
      # --- Tab 4: PROMs ---
      tabItem(tabName = "proms",
        fluidRow(
          box(width = 12, title = "SF-36 Domains (Pre-op vs Post-op)", status = "success",
              plotlyOutput("sf36_domains_plot", height = "500px")
          )
        ),
        fluidRow(
          box(width = 6, title = "Pain Improvement (Pre vs Post)", status = "primary",
              plotlyOutput("pain_bubble_plot", height = "400px")
          ),
          box(width = 6, title = "Correlation: Function vs Outcome", status = "primary",
              selectInput("corr_filter_type", "Procedure Type:", choices = c("All", "Primary", "Revision"), selected = "All"),
              plotlyOutput("correlation_plot", height = "400px")
          )
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive Values for Age Filter ---
  # Initialize with full range
  age_range <- reactiveVal(c(15, 85))
  
  # Observe Plotly Selection
  observeEvent(event_data("plotly_relayout", source = "age_hist"), {
    d <- event_data("plotly_relayout", source = "age_hist")
    if (!is.null(d[["xaxis.range[0]"]])) {
      age_range(c(d[["xaxis.range[0]"]], d[["xaxis.range[1]"]]))
    } else {
      # Reset if double-click or autoscale
      age_range(c(15, 85))
    }
  })
  
  # --- Reactive Data Filtering ---
  filtered_data <- reactive({
    data <- master_data
    
    # Joint Filter
    if (input$filter_joint != "All") {
      if (input$filter_joint == "Other") {
        data <- data %>% filter(joint_label == "Other")
      } else {
        data <- data %>% filter(joint_label == input$filter_joint)
      }
    }
    
    # Age Filter (from Plotly Slider)
    rng <- age_range()
    data <- data %>% filter(age >= rng[1] & age <= rng[2])
    
    # Global Year Filter (Apply unless overridden by specific chart logic)
    if (input$filter_year_global != "All") {
      data <- data %>% filter(year == as.numeric(input$filter_year_global))
    }
    
    data
  })
  
  # Sidebar Age Histogram with Range Slider
  output$age_hist_slider <- renderPlotly({
    # Use master_data filtered by Joint/Year only for the background context
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
    
    plot_ly(data, x = ~age, type = "histogram", marker = list(color = "#3c8dbc"), source = "age_hist") %>%
      layout(
        xaxis = list(title = "", rangeslider = list(visible = TRUE), range = c(15, 85)),
        yaxis = list(title = "", showticklabels = FALSE),
        margin = list(l=0, r=0, t=0, b=20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # --- Tab 1: Quality Control ---
  
  output$funnel_plot <- renderPlotly({
    # Use simulated benchmark data combined with actual KNH data
    knh_dat <- filtered_data()
    bench <- generate_benchmark_data(knh_dat)
    
    # Calculate Control Limits
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
      # Simulated Hospitals (Background)
      geom_point(data = subset(bench, !is_knh), color = "lightgrey", alpha = 0.5, size = 2) +
      # Control Limits
      geom_line(aes(y = upper_95), linetype = "dashed", color = "orange") +
      geom_line(aes(y = upper_99), linetype = "dashed", color = "red") +
      geom_line(aes(y = lower_95), linetype = "dashed", color = "orange") +
      geom_hline(yintercept = avg_rate * 100, color = "blue", alpha = 0.5) +
      # KNH (Foreground, Large, Red Diamond)
      geom_point(data = subset(bench, is_knh), color = "red", size = 6, shape = 18, 
                 aes(text = paste(hospital, "<br>Vol:", volume, "<br>Rate:", round(revision_rate, 2), "%"))) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Procedure Volume", y = "Revision Rate (%)", title = "Funnel Plot: Revision Rates") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Update Manufacturer Choices (Exclude Unknown)
  observe({
    manufs <- unique(master_data$manufacturer)
    manufs <- manufs[manufs != "Unknown"]
    updateSelectInput(session, "ccs_manufacturer", choices = manufs, selected = manufs[1])
  })
  
  output$ccs_trend_plot <- renderPlotly({
    req(input$ccs_manufacturer)
    data <- filtered_data()
    
    # Calculate share over time (Quarterly)
    trend_data <- data %>%
      mutate(quarter = floor_date(date_surgery, "quarter")) %>%
      group_by(quarter) %>%
      summarise(
        total_vol = n(),
        manuf_vol = sum(manufacturer == input$ccs_manufacturer, na.rm = TRUE),
        share = manuf_vol / total_vol
      ) %>%
      filter(!is.na(quarter))
    
    p <- ggplot(trend_data, aes(x = quarter, y = share)) +
      geom_line(color = "steelblue") +
      geom_area(fill = "steelblue", alpha = 0.3) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = paste("Market Share Trend:", input$ccs_manufacturer), x = "Date", y = "Share") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- Tab 2: Operational Volume ---
  
  output$vol_total_box <- renderValueBox({ valueBox(nrow(filtered_data()), "Total Procedures", icon = icon("list"), color = "aqua") })
  output$vol_primary_box <- renderValueBox({ valueBox(sum(filtered_data()$procedure_stage == "Primary"), "Primary", icon = icon("plus"), color = "green") })
  output$vol_rev_box <- renderValueBox({ valueBox(sum(filtered_data()$procedure_stage == "Revision"), "Revisions", icon = icon("redo"), color = "red") })
  output$avg_age_box <- renderValueBox({ valueBox(round(mean(filtered_data()$age, na.rm=TRUE), 1), "Avg Age", icon = icon("user"), color = "purple") })
  
  output$volume_trend_plot <- renderPlotly({
    data <- filtered_data()
    t_unit <- switch(input$vol_time_filter, "Week" = "week", "Month" = "month", "Year" = "year", "All Time" = "all")
    
    if (t_unit == "all") {
      plot_data <- data %>% summarise(n = n()) %>% mutate(time = "All Time")
      p <- ggplot(plot_data, aes(x = time, y = n)) + geom_col(fill = "steelblue")
    } else {
      plot_data <- data %>%
        mutate(time = floor_date(date_surgery, t_unit)) %>%
        count(time)
      p <- ggplot(plot_data, aes(x = time, y = n)) +
        geom_line(color = "steelblue") + geom_point(color = "steelblue") +
        labs(x = "Date", y = "Cases") + theme_minimal()
    }
    ggplotly(p)
  })
  
  output$theatre_volume_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(pat_thet_knh_surgery) %>%
      filter(!is.na(pat_thet_knh_surgery)) %>%
      top_n(10, n)
    
    p <- ggplot(data, aes(x = reorder(pat_thet_knh_surgery, n), y = n)) +
      geom_col(fill = "#00c0ef") +
      coord_flip() +
      labs(x = "Theatre", y = "Procedures") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$top_diagnoses_plot <- renderPlotly({
    data <- filtered_data() %>% count(diagnosis) %>% filter(!is.na(diagnosis))
    
    # Treemap (All diagnoses)
    plot_ly(data, labels = ~diagnosis, parents = NA, values = ~n, type = 'treemap',
            textinfo = "label+value+percent parent") %>%
      layout(title = "Diagnoses Distribution")
  })
  
  output$asa_bmi_plot <- renderPlotly({
    data <- filtered_data() %>%
      filter(asa_label != "Unknown" & !is.na(bmi))
    
    # Boxplot for BMI by ASA
    plot_ly(data, x = ~asa_label, y = ~bmi, type = "box", color = ~asa_label) %>%
      layout(title = "BMI Distribution by ASA Score",
             xaxis = list(title = "ASA Score"),
             yaxis = list(title = "BMI"))
  })
  
  # --- Tab 3: Complications ---
  
  output$complication_survival_plot <- renderPlotly({
    data <- filtered_data()
    
    # Survival Logic: Time to Revision
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
    
    # Robust Data Extraction
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
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$early_revision_reasons <- renderPlotly({
    data <- filtered_data() %>%
      filter(procedure_stage == "Revision")
      
    if("date_last_surgery_surgery" %in% names(data)) {
       data <- data %>%
         mutate(days_diff = as.numeric(date_surgery - as.Date(date_last_surgery_surgery))) %>%
         filter(days_diff < 730)
    }
    
    data <- data %>% count(diagnosis) %>% top_n(10, n)
    
    p <- ggplot(data, aes(x = reorder(diagnosis, n), y = n)) +
      geom_col(fill = "salmon") + coord_flip() +
      labs(title = "Reasons for Early Revision", x = "Reason", y = "Count") + theme_minimal()
    ggplotly(p)
  })
  
  output$complication_risk_plot <- renderPlotly({
    data <- filtered_data()
    t_unit <- switch(input$comp_time_filter, "Week" = "week", "Month" = "month", "Year" = "year")
    
    plot_data <- data %>%
      mutate(time = floor_date(date_surgery, t_unit)) %>%
      group_by(time) %>%
      summarise(risk = mean(procedure_stage == "Revision") * 100)
    
    p <- ggplot(plot_data, aes(x = time, y = risk)) +
      geom_line(color = "red") + labs(x = "Date", y = "Risk (%)") + theme_minimal()
    ggplotly(p)
  })
  
  # --- Tab 4: PROMs ---
  
  output$sf36_domains_plot <- renderPlotly({
    ids <- filtered_data()$registry_id
    proms <- scored_data %>%
      filter(registry_id %in% ids) %>%
      select(visit_name, ends_with("Score")) %>%
      pivot_longer(cols = ends_with("Score"), names_to = "Domain", values_to = "Score") %>%
      group_by(visit_name, Domain) %>%
      summarise(Avg_Score = mean(Score, na.rm = TRUE)) %>%
      filter(visit_name %in% c("enrollment", "fu_1yr"))
    
    p <- ggplot(proms, aes(x = Domain, y = Avg_Score, fill = visit_name)) +
      geom_col(position = "dodge") +
      labs(y = "Average Score (0-100)", fill = "Timepoint") + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$pain_bubble_plot <- renderPlotly({
    ids <- filtered_data()$registry_id
    pain_data <- scored_data %>%
      filter(registry_id %in% ids) %>%
      select(registry_id, visit_name, BP_Score) %>%
      filter(visit_name %in% c("enrollment", "fu_1yr")) %>%
      pivot_wider(names_from = visit_name, values_from = BP_Score) %>%
      filter(!is.na(enrollment) & !is.na(fu_1yr)) %>%
      count(enrollment, fu_1yr)
    
    p <- ggplot(pain_data, aes(x = enrollment, y = fu_1yr, size = n)) +
      geom_point(alpha = 0.6, color = "purple") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(x = "Pre-op BP Score", y = "1-Year Post-op BP Score", size = "Count") + theme_minimal()
    ggplotly(p)
  })
  
  output$correlation_plot <- renderPlotly({
    ids <- filtered_data()$registry_id
    corr_data <- scored_data %>%
      filter(registry_id %in% ids, visit_name == "enrollment") %>%
      inner_join(select(master_data, registry_id, procedure_stage), by = "registry_id") %>%
      select(PF_Score, BP_Score, procedure_stage) %>%
      filter(!is.na(PF_Score) & !is.na(BP_Score))
    
    if (input$corr_filter_type != "All") {
      corr_data <- corr_data %>% filter(procedure_stage == input$corr_filter_type)
    }
    
    p <- ggplot(corr_data, aes(x = PF_Score, y = BP_Score)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(x = "Physical Function (PF)", y = "Bodily Pain (BP)") + theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui, server)