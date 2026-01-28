# Arthroplasty Registry Dashboard 
## Purpose
The dashboard is a clinical quality monitoring and operational analytics platform. It tracks joint replacement surgeries at Kenyatta National Hospital in Kenya. It helps monitor surgical quality and patient outcomes, comparing hospital performance against national benchmarks. Users can easily identify trends and tracks implant performance by manufacturer. All this supports evidence-based clinical decisions and quality improvement.

## Use Case
Kenyatta National Hospital faces overcrowding in orthopedic wards. Bed occupancy often exceeds 140%. This is a result from patients bypassing lower-level facilities for arthroplasty and trauma care, and the high concentration of specialist arthroplasty surgeons in the Hospital. There are also high revision rates resulting from infections, dislocations, and fractures. These issues stem from the perioperative experience. This strains resources even at a facility that handles complex cases.
-	Orthopedic surgeons need to track personal outcomes and revision patterns for their patients. Without real time analytics, they miss performance drifts amid rising volumes. 
-	Hospital quality officers lack benchmarking tools against the set national standards of 1 to 2 percent early revisions, delaying action on outliers like infections in high-risk patients. 
-	Administrators struggle with opaque metrics for staffing high risk cases, theatre allocation and performance and workload on the staff, often getting the required data too late to impact the current decisions.
-	Procurement officers risk depending too much on one supplier with over 40 percent market share. At present, the hospital relies heavily on a single implant supplier, with over 70% market share. This raises costs without having proper performance data. 
-	Patients face unmonitored complications, poor follow up, and low mobility in low resource settings. This leads to readmissions and lost trust. 
The dashboard enables targeted monitoring, and use should help reduce revisions through early detection and optimized resource allocation and use to achieve efficiency. 


## Target Users
The dashboard serves four primary user groups.
**Orthopedic Surgeons & Surgical Teams** use the dashboard to monitor their surgical outcomes, revision rates, and complication patterns, enabling them to identify areas for technique improvement and track performance trends over time.
**Hospital Quality & Safety Officers** conduct quality assurance reviews and benchmark against national standards, ensuring the hospital meets quality targets and identifying outlier performance that requires intervention.
**Hospital Administrators & Department Heads** review operational metrics including procedure volumes, resource utilization, and high-risk patient percentages to make informed decisions about resource allocation, staffing, and strategic planning.
**Implant Procurement Officers** monitor manufacturer market share and implant performance through Case Concentration Scores (CCS) to make informed procurement decisions based on performance data and market trends.


## Solution
The registry was started in 2024. I used the mock data from the exported csv for this platform and started by analyzing the raw file. The file was too large and unprocessed. Static reports lacked fast querying, interactive filtering, and statistical benchmarking. Base R plots offered no interactivity. 
Data preprocessing came next. The data_pipeline.R script chunked the CSV into .rds files. Full_dataset.rds holds demographics and surgery data. Shiny_data.rds contains scored outcomes. It computes CCS as manufacturer volume over total volume times 100. It also calculates survival times and aggregations for gauges and trends. Load times dropped from minutes to seconds. The scored data was to be used for the functional outcomes, but this will be implemented later.
Global filters connect all tabs. Joint type uses a select input. Age histogram allows drag select. Timeline slider picks date ranges. These update Quality Control, Operational Volume, and Complications tabs dynamically. Age filters layer on top of timeline and joint for cohort views.
I added Kaplan Meier curves by joint using survfit. Treemap clicks open modals despite Plotly warnings. Value boxes show primaries, revisions, and high-risk percent. The reset button clears all inputs and server states.
These selected charts and filters fit right into the workflows of all stakeholders in the registry. There is clear use case for each, and feedback was obtained from the team on the functionality.

## Required Features & Functionality
The dashboard is organized into **three tabs**, each addressing specific needs.

### **Tab 1: Quality Control**
This focuses on benchmarking hospital performance and monitoring implant manufacturer concentration.
**Hospital Performance: Revision Rate Funnel Plot** - This displays KNH's revision rate plotted against 50 simulated hospital benchmarks, with statistical control limits shown as dashed orange and red lines representing 95% and 99% confidence intervals. The average national revision rate (blue horizontal line) demonstrating the volume-outcome relationship where larger hospitals typically have lower revision rates. The plot is fully interactive and responds to all global filters including joint type, age, and changes in the selected timeline.
**Case Concentration Score (CCS) & Trends** - The CCS section provides two visualizations. The gauge chart displays current market share percentage for a selected implant manufacturer with color-coded zones: green (0-20%) low concentration and good diversity, yellow (20-40%) medium concentration, and red (40-100%) high concentration and potential monopoly risk. The trend chart plots historical manufacturer market share over time monthly. Users can select manufacturers from the dropdown menu to monitor whether the hospital is becoming too dependent on a single supplier.

### **Tab 2: Operational Volume**
This tab provides operational metrics and volume analytics.
**Summary Value Boxes** - Four key metrics are displayed: Total Procedures (total number of surgeries), Primary (first-time joint replacements), Revisions (repeat procedures on previously replaced joints), and High-Risk percentage (patients with ASA score III or higher). These boxes provide a quick snapshot of operational activity and tracking high-risk patient volumes for resource planning.
**Volume Trends Chart** - The volume trends chart displays a time series of procedure volumes aggregated by selected time scale with optional gender filtering. This helps identify patterns or trends in surgical volume and plan staffing and resource allocation based on volume predictions.
**Top Diagnoses Treemap** - The interactive treemap visualizes the most common diagnoses and indications for surgery, where box size represents relative frequency. A key interactive feature allows users to click on any diagnosis to open a modal showing the trend over time for that specific diagnosis. To return to the full treemap you need to click the selected box again.

### **Tab 3: Complications**
This tab focuses on adverse outcomes and complication analysis.
**Surgical Complications Survival Analysis** - The survival analysis displays Kaplan-Meier survival curves showing the probability of complication-free survival stratified by joint type. This visualization enables comparison of long-term outcomes between different joint types.
**Early Revision Reasons** - This horizontal bar chart displays the top 10 reasons for early revisions within 2 years of primary surgery. It helps identify the most common causes of early surgical failure and target quality improvement efforts.
**Complication Risk Trends** - The complication risk chart shows time series of revision rate trends with selectable time scales to monitor if complication rates are increasing or decreasing over time and assess the impact of quality improvement initiatives.

## Required Filters
All visualizations respond dynamically to the global filters located in the sidebar.
**Joint Type Filter** - Select All, Hip, Knee, or Other to focus analysis on specific joint types.
**Age Distribution Histogram with Range Selection** - Interactive histogram showing patient age distribution. Click and drag to select an age range, and all charts will filter to show only patients within that age range. The histogram correctly responds to other global filters (Timeline, Joint) while maintaining its own selection capability.
**Timeline Slider** - Date range selector with month-year format. Drag handles to adjust start and end dates to focus analysis on specific time periods.
All these filters are connected by logic to ensure users get precisely the data visualisation they need.
**Reset Filters Button** - Red button at the bottom of the sidebar that instantly resets all filters to default values. The data filtering is cleared immediately, though visual selection on the histogram may persist for a few seconds.

## Implementation and challenges
I built data_pipeline.R first. It loads the raw CSV and handles missing values like imputed survival times. It splits data by variables and saves RDS files. The app server runs this if files are missing. I processed the data separately with the script data_pipeline.R. This creates .rds files for faster loading oof the application. The app runs this script automatically if the .rds files are missing from the working diorectory. The reason for doing this is the original CSV from the REDCap registry was too heavy for my computer and splitting it into smaller chunks as .rds files was more manageable.
The UI uses navbarPage with sidebar filters. Joint type is a selectInput. Dates use sliderInput. Age histogram uses plotlyOutput with dragmode select and event callbacks.
Server logic creates reactive data subsets. It requires and filters by joint, dates, and age range from histogram clicks. Then it renders plots. Funnel plot uses ggplot with rbinom simulated benchmarks. Gauge colors green below 20 percent, yellow 20 to 40, red above. Trends switched from dygraphs to Plotly. Survival uses survminer ggsurvplot. Treemap uses plot_ly type treemap with modalDialog on click.
Age histogram reactivity proved tough. AI code failed integration multiple times. I fixed it with proxy_plotly and independent filter layers. Heavy data crashed local Shiny. RDS files solved this.
Plotly_click warnings persisted despite event_register. Clicks still function so I kept it. Reset uses lapply to set inputs to NULL. Histogram visual selection lags slightly due to DOM persistence. Here is the discussion: <https://github.com/plotly/plotly.R/issues/1528>
I deployed to shinyapps.io after suppressing noncritical warnings. I performed iterative testing checking filter consistency across tabs like monthly CCS trends by manufacturer dropdown.

