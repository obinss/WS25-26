# Arthroplasty Registry Dashboard - Report on the creation and usage
## Purpose
The **Arthroplasty Registry Dashboard** is a comprehensive clinical quality monitoring and operational analytics platform designed to track, analyze, and provide crucial information for decisionmaking to improve outcomes for joint replacement surgeries (arthroplasty procedures) performed at Kenyatta National Hospital in Kenya. This dashboard serves as a surgical registry analytics platform that enables monitoring surgical quality and patient outcomes over time, compare hospital performance against national benchmarks, identify trends in procedure volumes and complication rates, track manufacturer-specific implant performance, analyze patient demographics and risk factors, and support evidence-based clinical decision-making and quality improvement initiatives.
I have reworked the dashboard with fixes for warnings (but some get through as demonstrated below), reactivity, and a reset button for all the filters to improve the functionality of the dashboard.  I had to work on the data separately using the data cleaning script data_pipeline.R, that generate a .rds file to make the application faster. I have included code to run this script in the application in case the .rds files are missing. The csv file is a mirror of what we get from the registry platform on REDCap and was quite heavy for my computer. I decided to work on it and split it into bite sized chunks, hence the .rds files.

# Why the hospital admin perspective is important - Who will use the dashboard

The hospital's perspective is crucial, as it is where surgeons and patients interact. Hospitals aim to provide high-quality healthcare at reasonable costs, integrating the volume of patients into their considerations. Additionally, hospitals or departments have an interest in ensuring that patients remember the institution where they were treated successfully. This encourages them to return for any necessary future care, including reasons beyond a prosthetic joint replacement. 
The registry can therefor be perceived as an instrument for quality control, for the implants used and for the entire process. This can range from the preoperative consultation to the procedures in the operating room, as well as the postoperative follow-up. Personal recommendations from satisfied patients are the very best advertising for hospitals and related medical institutions. As institutions providing healthcare in today's competitive environment, hospitals are also very keen to uphold their reputation, and the registry is an invaluable tool for this purpose.

## Target Users

The dashboard serves five primary user groups, each with distinct analytical needs.
**Orthopedic Surgeons & Surgical Teams** use the dashboard to monitor their surgical outcomes, revision rates, and complication patterns, enabling them to identify areas for technique improvement and track performance trends over time.
**Hospital Quality & Safety Officers** conduct quality assurance reviews and benchmark against national standards, ensuring the hospital meets quality targets and identifying outlier performance that requires intervention.
**Hospital Administrators & Department Heads** review operational metrics including procedure volumes, resource utilization, and high-risk patient percentages to make informed decisions about resource allocation, staffing, and strategic planning.
**Clinical Researchers & Registry Coordinators** analyze registry data for research purposes and generate insights on arthroplasty outcomes, contributing to evidence-based medicine and publishing findings on implant performance and surgical techniques.
**Implant Procurement Officers** monitor manufacturer market share and implant performance through Case Concentration Scores (CCS) to make informed procurement decisions based on performance data and market trends.
# Key Features & Functionality

The dashboard is organized into **three main tabs**, each addressing specific analytical needs.

## **Tab 1: Quality Control**

This tab focuses on benchmarking hospital performance and monitoring implant manufacturer concentration.

**Hospital Performance: Revision Rate Funnel Plot** - The funnel plot displays KNH's revision rate (red diamond) plotted against 50 simulated hospital benchmarks, with statistical control limits shown as dashed orange and red lines representing 95% and 99% confidence intervals. The average national revision rate appears as a blue horizontal line, demonstrating the volume-outcome relationship where larger hospitals typically have lower revision rates. To interpret the plot, identify whether KNH performs within expected ranges (between control limits). If the red diamond falls above the upper control limits, it indicates higher-than-expected revision rates requiring investigation. The plot is fully interactive and responds to all global filters including joint type, age, and changes in the selected timeline.

**Case Concentration Score (CCS) & Trends** - The CCS section provides two visualizations. The gauge chart displays current market share percentage for a selected implant manufacturer with color-coded zones: green (0-20%) indicates low concentration and good diversity, yellow (20-40%) shows medium concentration, and red (40-100%) warns of high concentration and potential monopoly risk. The trend chart plots historical manufacturer market share over time on a monthly basis. Users can select manufacturers from the dropdown menu to monitor whether the hospital is becoming too dependent on a single supplier.

## **Tab 2: Operational Volume**

This tab provides operational metrics and volume analytics.

**Summary Value Boxes** - Four key metrics are displayed: Total Procedures (total number of arthroplasty cases), Primary (first-time joint replacements), Revisions (repeat procedures on previously replaced joints), and High Risk percentage (patients with ASA score III or higher). These boxes provide a quick snapshot of operational activity and help track high-risk patient volumes for resource planning.

**Volume Trends Chart** - The volume trends chart displays a time series of procedure volumes aggregated by selected time scale (Week/Month/Year/All Time) with optional gender filtering. This visualization helps identify seasonal patterns or trends in surgical volume and plan staffing and resource allocation based on volume predictions.

**Top Diagnoses Treemap** - The interactive treemap visualizes the most common diagnoses and indications for surgery, where box size represents relative frequency. A key interactive feature allows users to click on any diagnosis to open a modal showing the trend over time for that specific diagnosis. To return to the full treemap you need to click the selected box again.

## **Tab 3: Complications**

This tab focuses on adverse outcomes and complication analysis.

**Surgical Complications Survival Analysis** - The survival analysis displays Kaplan-Meier survival curves showing the probability of complication-free survival (time to revision) stratified by joint type (Hip/Knee/Other). This visualization enables comparison of long-term outcomes between different joint types.

**Early Revision Reasons** - This horizontal bar chart displays the top 10 reasons for early revisions within 2 years of primary surgery, focusing on acute complications requiring quick intervention. It helps identify the most common causes of early surgical failure and target quality improvement efforts.

**Complication Risk Trends** - The complication risk chart shows time series of revision rate trends with selectable time scales (Week/Month/Year) to monitor if complication rates are increasing or decreasing over time and assess the impact of quality improvement initiatives.

# Required Global Filters

All visualizations respond dynamically to the global filters located in the sidebar.

**Joint Type Filter** - Select All, Hip, Knee, or Other to focus analysis on specific joint types.

**Age Distribution Histogram with Range Selection** - Interactive histogram showing patient age distribution. Click and drag to select an age range, and all charts will filter to show only patients within that age range. The histogram correctly responds to other global filters (Timeline, Joint) while maintaining its own selection capability.

**Timeline Slider** - Date range selector with month-year format. Drag handles to adjust start and end dates to focus analysis on specific time periods.

All these filters are connected by logic to ensure users get precisely the data visualisation they need.

**Reset Filters Button** - Red button at the bottom of the sidebar that instantly resets all filters to default values. The data filtering is cleared immediately, though visual selection on the histogram may persist for a few seconds.

# Some Experimental Changes

## **Age Histogram for the filter with Reactivity**

I particularly enjoyed working on this as it was one of the most challenging sections for the app. I had to use AI to help with the generation of this but everytime the integration would fail or result in code that doesnt work.

```         
-   The Age Histogram now correctly responds to **Global Filters** (Timeline, Joint) while maintaining its own selection capability.
	-   	This also serves as the age range selector filter
```

## **Reset Filter Button**:
-   Now this button correctly resets **all** filters (data filtering is cleared, though visual selection on histogram may persist for a few seconds).

# Still getting a warning

This warning has persisted since I opted to use plotly to make the plots more reactive and not just static .png images that have to be picked from the server every time.

```         
Warning: The 'plotly_click' event tied a source ID of 'treemap_source' is not registered. In order to obtain this event data, please add `event_register(p, 'plotly_click')` to the plot (`p`) that you wish to obtain event data from.
```

I found a more indepth discussion on the warning here: <https://github.com/plotly/plotly.R/issues/1528>

------------------------------------------------------------------------

## Typical User Workflows

**Quality Review Meeting (Monthly)** - Quality & Safety Officers open the dashboard at the start of meetings, review the funnel plot to ensure KNH is within control limits, check survival curves for concerning trends, review early revision reasons, set timeline filter to "Last Month" to focus on recent cases, and generate discussion points for quality improvement initiatives.

**Manufacturer Performance Review (Quarterly)** - Implant Procurement Officers navigate to the Quality Control Tab, use the CCS section to cycle through each supplier, note market share percentages and trends, identify if any manufacturer has excessive concentration (\>40%), filter by joint type to analyze hip versus knee implant preferences, and document findings for procurement committee meetings.

**Surgical Volume Planning (Weekly)** - Department Administrators navigate to the Operational Volume Tab, review value boxes for current week/month totals, set Volume Trends to "Week" view, check for upcoming capacity issues or unusual dips in volume, review High Risk percentage to anticipate need for intensive care beds, and use the Top Diagnoses treemap to understand case mix.

**Research Data Exploration** - Clinical Researchers apply specific filters (joint type, age range, timeline), analyze survival curves for outcomes, click on specific diagnoses in the treemap to see temporal trends, and use filters to create cohort-specific analyses.

------------------------------------------------------------------------

## Technical Details

**Data Files** - The application loads two pre-processed RDS files: `full_dataset.rds` (complete patient demographic and surgical data) and `shiny_data.rds` (scored outcomes data).

**Key Variables Tracked** - Demographics (age, gender, BMI), clinical variables (ASA score, diagnosis, affected joint), surgical details (operation type, date of surgery, surgeon, hospital), implant information (manufacturer, stem type), outcomes (revision status, date of revision, reasons for revision), and follow-up data (date last seen, survival time).

**Statistical Methods** - The Funnel Plot uses binomial proportion confidence intervals (95% and 99%) based on procedure volume. Survival Analysis employs the Kaplan-Meier estimator for time-to-revision curves. The CCS (Case Concentration Score) is calculated as Market share percentage = (Manufacturer Volume / Total Volume) × 100.

------------------------------------------------------------------------

## Key Benefits

**For Clinical Quality** - The dashboard enables evidence-based identification of performance outliers through funnel plot analysis, provides an early warning system for increasing complication rates, and ensures transparent benchmarking against national standards.

**For Operations** - Operational benefits include real-time volume monitoring and trend analysis, resource planning based on case mix and risk profiles, and data-driven procurement decisions informed by manufacturer performance metrics.

**For Research** - Researchers gain access to comprehensive registry data for outcomes research, interactive exploration of patient cohorts through filtering capabilities, and temporal trend analysis capabilities across multiple dimensions.

**For Patient Safety** - Patient safety improvements are achieved through systematic tracking of complications and adverse events, identification of high-risk patient populations requiring additional support, and continuous quality improvement through data transparency.
