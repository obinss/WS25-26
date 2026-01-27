---
editor_options: 
  markdown: 
    wrap: 72
---

# Shiny App Submission

## Overview

This Shiny app is optimized for performance and reproducibility. It uses
`renv` for dependency management. The app is available from this live
link <https://xp31jg-obinss.shinyapps.io/WS25-26/>. The Github
repository is available at <https://github.com/obinss/WS25-26.git>.

## How to Run

1.  **Open the project** in RStudio.
2.  **Run `app.R`**:
    -   The app includes a self-checking logic to install essential
        packages (`shiny`, `dplyr`, etc.) automatically if missing.
    -   It acts as the single entry point for the professor/grader.

## Data Regeneration (Optional)

If you need to regenerate the data from the source
(`arthroplasty_registry_comprehensive_data.csv`): 1. Ensure you have the
full environment. Run: `r     renv::restore()` This will install the
data processing dependencies (like `tidyverse`) that are not loaded by
the app itself. 2. Run `source("data_pipeline.R")`.

## Files

-   `app.R`: Main application file.
-   `renv.lock`: Lockfile ensuring exact package versions.
-   `data_pipeline.R` & `sf36_scoring.R`: Scripts for data processing.
