# Arthroplasty Registry Dashboard – Report

The Arthroplasty Registry Dashboard is a clinical quality and operational analytics tool for joint replacement surgery at Kenyatta National Hospital (KNH). It tracks outcomes, complications, volumes, and implant performance to support evidence-based decision-making and quality improvement. Data come from the REDCap arthroplasty registry and are pre-processed via a data_pipeline.R script into lighter .rds files to improve speed.

# Purpose and Users

The registry functions as a quality-control instrument across the full care pathway (preoperative, intraoperative, postoperative) and supports hospital reputation and patient satisfaction.

Primary user groups are:
- Orthopedic surgeons: monitor outcomes, revision rates, and complication patterns.
- Quality and safety officers: benchmark against standards and identify outliers.
- Hospital administrators: follow volumes, high-risk case mix, and resource use.
- Clinical researchers: conduct outcomes research and generate publishable insights.
- Implant procurement officers: monitor manufacturer market share to inform purchasing.

# Dashboard Structure

The dashboard has three main tabs plus global filters (joint type, age, timeline, reset) that update all visualizations.

**Tab 1 – Quality Control**
- Revision Rate Funnel Plot: KNH's revision rate against 50 simulated benchmarks with 95%/99% control limits and national average line. Values above upper limits trigger review. Fully interactive.
- Case Concentration Score (CCS): Gauge chart shows manufacturer market share with zones (green 0–20%, yellow 20–40%, red 40–100%). Monthly trend chart flags emerging supplier dependence.

**Tab 2 – Operational Volume**
- Summary Value Boxes: Total Procedures, Primary, Revisions, High-Risk percentage (ASA III+).
- Volume Trends Chart: Time series by week/month/year with gender filter for capacity planning.
- Top Diagnoses Treemap: Interactive; clicking a diagnosis shows its trend over time. Click once on a diagnosis to view the trendlines, then click twice to go back to the original treemap.

**Tab 3 – Complications** 
- Survival Analysis: Kaplan–Meier curves of time to revision by joint type (Hip, Knee, Other).
- Early Revision Reasons: Top 10 reasons for revisions within 2 years of primary surgery.
- Complication Risk Trends: Revision-rate time series to monitor improvement over time.

**Global Filters**
- Joint Type (All, Hip, Knee, Other), Age Histogram with drag selection, Timeline Slider, Reset Button.

# Warnings and Challenges Encountered

**Data size and performance**
The original CSV export from REDCap was too large and caused performance issues. The solution was to create a separate data_pipeline.R script that cleans and splits data into smaller .rds files. Code was added to regenerate these files automatically if missing.

**Age histogram reactivity**
Ensuring the age histogram responded correctly to global filters (timeline, joint type) while also serving as an interactive age-range selector was one of the most challenging parts of the project. AI assistance was used to generate code, but integration frequently failed or produced non-functional code. Multiple iterations were required to achieve correct behaviour.

**Reset filter functionality**
The reset button needed to clear all filters simultaneously. After rework, data filtering now clears immediately, though the visual selection on the histogram may persist for a few seconds before updating.

**Plotly warning (unresolved)**
A persistent warning appears when using Plotly for the interactive treemap:
> Warning: The 'plotly_click' event tied a source ID of 'treemap_source' is not registered. In order to obtain this event data, please add `event_register(p, 'plotly_click')` to the plot (`p`) that you wish to obtain event data from.

This warning does not prevent functionality but has not been fully resolved. A more detailed discussion is available on GitHub (plotly/plotly.R issue #1528). The warning emerged when moving from static .png images to reactive Plotly graphics to improve user interactivity.

# Typical Workflows

- **Monthly quality meetings** - Review funnel plots, survival curves, early revision reasons; filter to last month; identify quality issues.
- **Quarterly manufacturer review** - Use CCS and trends to detect high (>40%) market share; inform procurement decisions.
- **Weekly volume planning** - Track current volumes and high-risk percentages to plan beds, theatres, and ICU capacity.
- **Research exploration** - Apply filters, inspect survival curves and diagnosis trends, build cohort-specific datasets.
