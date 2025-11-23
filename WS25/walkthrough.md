# Arthroplasty Registry Dashboard - Final Version

I have finalized the dashboard with the interactive Treemap Drill-down feature.

## Final Updates

### 1. Treemap Drill-down
- **Interaction**: Clicking on any diagnosis rectangle in the Treemap now opens a **Modal Dialog**.
- **Content**: The modal displays a **Trend Line Plot** showing the number of cases for that specific diagnosis over time (Monthly).
- **Benefit**: Allows detailed exploration of specific diagnoses without cluttering the main dashboard.

### 2. Previous Fixes & Enhancements
- **CCS Trend**: Fixed to show full year data (Monthly aggregation).
- **Theme Selector**: Located in the Header.
- **CCS Gauge**: Green/Yellow/Red color steps.
- **Age Slider**: High-contrast histogram.

## How to Run
```r
library(shiny)
runApp('app.R')
```
