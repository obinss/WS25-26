# 1. LOAD NECESSARY PACKAGES ------
# (Ensure you have run 'install.packages("pacman")' once before)
pacman::p_load(
  tidyverse,  # For data manipulation (dplyr, tidyr, readr, etc.)
  parsedate,  # For flexible date parsing
  janitor,    # For cleaning names
  lubridate,  # For date functions
  shiny,      # For the app
  shinydashboard, # For the app layout
  plotly,     # For interactive plots
  sf,         # For maps
  leaflet     # For maps
)

# 2. SOURCE THE SCORING FUNCTION ------
# This script must be in the same directory
source("sf36_scoring.R") # added on the visit column as visit_name, and date to not be dropped

# 3. DEFINE HELPER FUNCTION ------
# Extracts the leading number from strings like "1- Excellent"
extract_score <- function(x) {
  score <- str_extract(x, "^[0-9]+")
  return(as.numeric(score))
}

# 4. LOAD & CLEAN MAIN DATA ------
arthroplasty <- read_csv("arthroplasty_registry_comprehensive_data.csv")

arthroplasty_std <- arthroplasty %>%
  clean_names() %>% 
  rename(date_birth = dob,
         pat_umr = pat_umr_1,
         first_name = pt_first,
         last_name = pt_last,
         age = pt_age,
         gender = pt_gender,
         county = pat_county,
         diagnosis = pat_diag,
         phone = pat_phone,
         email = pat_email,
         education = pat_education,
         employment = pat_employment,
         employment_sector = pat_employment_sector,
         income = pat_income,
         mode_payment = pat_mod_pay,
         affected_joint = pat_joint_problem,
         laterality = pat_laterality,
         duration_arthropathy = pat_duration_arthropathy,
         allergies = pat_allergies,
         date_enrollment = event_date_enrollment,
         date_surgery = event_date_surgery,
         date_fu_6wk = event_date_fu_6wk,
         date_fu_3mo = event_date_fu_3mo,
         date_fu_6mo = event_date_fu_6mo,
         date_fu_1yr = event_date_fu_1yr,
         date_fu_2yr = event_date_fu_2yr,
         date_fu_6yr = event_date_fu_6yr,
         surgery_position = pat_postion_surgery) %>% 
  select(registry_id, pat_umr, 
         first_name, last_name, 
         age, gender, county, 
         diagnosis, affected_joint, op_type, # <-- Ensured op_type is here
         laterality, duration_arthropathy, 
         date_enrollment, everything()
  ) %>% 
  mutate(
    gender = str_to_title(gender),
    county = str_to_title(county),
    employment = str_to_title(employment),
    first_name = str_to_title(first_name),
    last_name = str_to_title(last_name),
    pat_umr = toupper(pat_umr),
    email = tolower(email),
    op_type = str_to_title(op_type) # Clean op_type
  ) %>%
  mutate(
    income = str_replace(income, "Ksh", "KSh"),
    pat_umr = str_replace(pat_umr, "_DUPE", ""),
    registry_id = str_replace(registry_id, "_DUPE", ""),
    affected_joint = str_to_sentence(affected_joint),
    diagnosis = str_to_sentence(diagnosis)
  ) %>%
  mutate(
    across(c(date_enrollment, date_birth, date_fu_6wk, date_fu_3mo, 
             date_fu_6mo, date_fu_1yr, date_fu_2yr, date_fu_6yr, 
             date_surgery), 
           parse_date)
  ) %>%
  mutate(across(where(is.character), trimws)) %>% 
  mutate(
    across(starts_with("pat_weight_post"), as.numeric),
    across(starts_with("pat_height_post"), as.numeric),
    registry_id = as.numeric(registry_id)
  ) %>% 
  mutate(
    days_to_surgery = date_surgery - date_enrollment,
    bmi_enrollment = pat_weight_post_enrollment / (pat_height_post_enrollment/100)^2,
    bmi_surgery = pat_weight_post_surgery / (pat_height_post_surgery/100)^2,
    days_to_surgery = if_else(days_to_surgery < 0, NA, days_to_surgery)
  ) %>%
  mutate(
    cup_manufucturer = str_extract(pat_cup_label_surgery, "^[^ ]+"),
    head_manufucturer = str_extract(pat_head_label_surgery, "^[^ ]+"),
    liner_manufucturer = str_extract(pat_lin_label_surgery, "^[^ ]+"),
    cement_manufucturer = str_extract(pat_cem_label_surgery, "^[^ ]+"),
    stem_manufucturer = str_extract(pat_stem_label_surgery, "^[^ ]+"),
    meniscus_manufucturer = str_extract(mens_comp_surgery, "^[^ ]+"),
    femoral_manufucturer = str_extract(fem_comp_surgery, "^[^ ]+"),
    tibial_manufucturer = str_extract(tib_comp_surgery, "^[^ ]+"),
    patella_manufucturer = str_extract(pat_comp_surgery, "^[^ ]+")
  ) %>%
  distinct()

# 5. OPTIMIZED SF-36 CLEANING & SCORING ------
visit_levels <- c("enrollment", "surgery", "fu_6wk", "fu_3mo", 
                  "fu_6mo", "fu_1yr", "fu_2yr", "fu_6yr")

data_for_scoring <- arthroplasty_std %>%
  mutate(across(starts_with("sf36_"), extract_score)) %>%
  select(registry_id, starts_with("sf36_"), starts_with("date_")) %>% # add dates
  pivot_longer(
    cols = starts_with("sf36_"),  # make sure to retain dates for the filters
    names_to = c("question", "visit_name"),
    names_pattern = "^(sf36_\\d+)_(.*)$",
    values_to = "score"
  ) %>%
  filter(!is.na(score)) %>%
  pivot_wider(
    names_from = "question",
    values_from = "score"
  ) %>%
  mutate(visit_name = factor(visit_name, levels = visit_levels)) %>%
  arrange(registry_id, visit_name)

## now score the data
scored_data <- score_sf36(data_for_scoring) 

scored_data$age <- year(as.period(interval(start=scored_data$date_birth, end=Sys.Date())))
# 6. PREPARE FINAL SHINY DATASET ------
# Get unique demographic data per patient
patient_demo <- arthroplasty_std %>%
  select(
    registry_id, age, gender, county, employment, 
    income, date_birth, diagnosis, affected_joint, laterality,
    op_type, # missed in the first versions
    pat_height_post_enrollment, pat_weight_post_enrollment, bmi_enrollment, referral,
    mode_payment, refferal_fac, physio_preop, physio_postop_surgery, cci_total,
    surgery_position, pat_pain_scale_enrollment, pat_thrombo_proph_surgery,
    pat_thet_knh_surgery, contains("date"), contains("hhs_total_score_"), contains("pat_sbp_"),
    contains("pat_dbp_"), contains("pat_pulse_"), pat_asa_surgery, pat_min_tech_surgery,
    pat_bone_graft_surgery, contains("kss_total"), 
    # Add implant manufacturers for the Implant tab
    cup_manufucturer, head_manufucturer, liner_manufucturer, 
    cement_manufucturer, stem_manufucturer, meniscus_manufucturer,
    femoral_manufucturer, tibial_manufucturer, patella_manufucturer
  ) %>%
  mutate(
    diagnosis = recode(diagnosis, 
                       "Chronic trauma" = "Chronic Trauma",
                       "Congenital dislocation/ddh" = "Congenital Dislocation (DDH)",
                       "Dislocation/subluxation" = "Dislocation (Subluxation)",
                       "Implant fracture" = "Implant Fracture",
                       "Inflammatory arthropathy" = "Inflammatory Arthropathy",
                       "Lysis of stem" = "Lysis of Stem",
                       "Previous infection" = "Previous Infection",
                       "Rheumatoid arthritis" = "Rheumatoid Arthritis",
                       "Skeletal dysplasia" = "Skeletal Dysplasia",
                       "Spontaneous osteonecrosis of the knee" = "Spontaneous Osteonecrosis (Knee)",
                       "Sufe" = "SUFE",
                       "Trauma acute- hip fracture" = "Acute Trauma (Hip Fracture)",
                       "Unexplained pain" = "Unexplained Pain",
                       "Wear of acetabular component" = "Acetabular Component Wear",
                       "Active infection" = "Active Infection",
                       "Avascular necrosis" = "Avascular Necrosis",
                       "Aseptic loosening of stem" = "Aseptic Loosening Stem",
                       "Aseptic loosening of socket" = "Aseptic Loosening Socket"
      
    )
  ) %>% 
  distinct(registry_id, .keep_all = TRUE) # Ensure demo data is unique per patient

# Join the scores (one row per patient-visit) with the demographics (one row per patient)-----
# terrible idea, wont make sense if we can recreate smaller sets and use them 
# wont this make the application slower??



# 7. SAVE FINAL OPTIMIZED DATA ------
saveRDS(scored_data, "shiny_data.rds")
saveRDS(patient_demo, "patient_demo.rds")
saveRDS(arthroplasty_std, "full_dataset.rds")

