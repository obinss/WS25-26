# install.packages("tidyverse") # Uncomment this line if you don't have tidyverse already installed
library(tidyverse)

# Define the scoring function------
score_sf36 <- function(data) {
  
  ## Define item lists for each scale -------- 
  # make sure the names below correspond to the names of the columns in the
  # wide format version of the dataset to be analysed
  pf_items <- c("sf36_3", "sf36_4", "sf36_5", "sf36_6", "sf36_7", 
                "sf36_8", "sf36_9", "sf36_10", "sf36_11", "sf36_12")
  rp_items <- c("sf36_13", "sf36_14", "sf36_15", "sf36_16")
  bp_items <- c("sf36_21", "sf36_22")
  gh_items <- c("sf36_1", "sf36_33", "sf36_34", "sf36_35", "sf36_36")
  vt_items <- c("sf36_23", "sf36_25", "sf36_27", "sf36_29")
  sf_items <- c("sf36_20", "sf36_31")
  mh_items <- c("sf36_24", "sf36_26", "sf36_28", "sf36_30", "sf36_32")
  re_items <- c("sf36_17", "sf36_18", "sf36_19")
  
  ## Check if all required columns exist ---------
  all_items <- c(pf_items, rp_items, bp_items, gh_items, vt_items, 
                 sf_items, mh_items, re_items)
  missing_cols <- setdiff(all_items, names(data))
  
  if(length(missing_cols) > 0) {
    warning_msg <- paste("The following required SF-36 columns are 
                         MISSING from the data:",
                         paste(missing_cols, collapse=", "))
    warning(warning_msg)
    
    ### Add missing columns as NA so the script doesn't break ---------
    # allow the script to progress and generate results for the sections with
    # complete records
    for(col in missing_cols) {
      data[[col]] <- NA
    }
  }
  
  # Recode items as relevant per the SF36 scoring manual---------
  # https://www.rand.org/health/surveys/mos/36-item-short-form/scoring.html
  # This section creates new, recoded item columns based on the RAND manual
  # before averaging them into scale scores
  
  scored_data <- data %>%
    # Use rowwise() to perform calculations on each row independently 
    # hence the need to ensure the data is in the wide format
    rowwise() %>%
    mutate(
      
      ## Physical Functioning (PF) -------
      # Items 3-12: Scale 1-3. No recoding needed. 10 items required
      # note that 1 is 0% hence the calculation
      # Lowest = 1, Highest = 3, Range = 2
      pf_valid_count = sum(!is.na(c_across(all_of(pf_items)))),
      pf_raw = mean(c_across(all_of(pf_items)), na.rm = TRUE),
      PF_Score = ifelse(pf_valid_count >= 5, ((pf_raw - 1) / 2) * 100, NA),
      
      ## Role-Physical (RP) ---------
      # denotes role limitations due to physical health, 4 items
      # Items 13-16: Scale 1-2. No recoding needed. 1 == 0%, 2 == 100%
      # Lowest = 1, Highest = 2, Range = 1
      rp_valid_count = sum(!is.na(c_across(all_of(rp_items)))),
      rp_raw = mean(c_across(all_of(rp_items)), na.rm = TRUE),
      RP_Score = ifelse(rp_valid_count >= 2, ((rp_raw - 1) / 1) * 100, NA),
      
      ## Bodily Pain (BP) --------
      # 2 items only, recoded differently
      # Item 21: Scale 1-6 (Recoded 1-6) where 1 == 100% and 6 == 0% decrements of 20
      # Item 22: Scale 1-5 (Recoded 5,4,3,2,1)  where 1 == 100%, decrements 25
      bp1_r = 7 - sf36_21, # 1=6, 2=5, 3=4, 4=3, 5=2, 6=1. # reversed the order as the higher the score, the worse limitation
      bp2_r = 6 - sf36_22, # 1=5, 2=4, 3=3, 4=2, 5=1  # math is neat, eh?
      bp_valid_count = sum(!is.na(c(bp1_r, bp2_r))),
      bp_raw = mean(c(bp1_r, bp2_r), na.rm = TRUE),
      # Lowest mean = mean(1,1) = 1. Highest mean = mean(6,5) = 5.5. Range = 4.5
      BP_Score = ifelse(bp_valid_count >= 1, ((bp_raw - 1) / 4.5) * 100, NA),
      
      ## General Health (GH) --------
      # Items 33, 35: Scale 1-5 (Recoded 5,4,3,2,1)
      # Items 1, 34, 36: Scale 1-5 (Recoded 1-5)
      gh1_r = 6 - sf36_1, # 1=5, 2=4, 3=3, 4=2, 5=1. # 1 == 100%, 5 == 0%
      gh2_r = sf36_33, # 1=1, 2=2, 3=3, 4=4, 5=5 
      gh3_r = 6 - sf36_34, # 1=5, 2=4, 3=3, 4=2, 5=1 # 1 == 100%, 5 == 0%
      gh4_r = sf36_35, # 1=1, 2=2, 3=3, 4=4, 5=5 
      gh5_r = 6 - sf36_36, # 1=5, 2=4, 3=3, 4=2, 5=1 # 1 == 100%, 5 == 0%
      gh_valid_count = sum(!is.na(c(gh1_r, gh2_r, gh3_r, gh4_r, gh5_r))),
      gh_raw = mean(c(gh1_r, gh2_r, gh3_r, gh4_r, gh5_r), na.rm = TRUE),
      # Lowest = 1, Highest = 5, Range = 4
      GH_Score = ifelse(gh_valid_count >= 3, ((gh_raw - 1) / 4) * 100, NA),
      
      ## Vitality (VT) --------
      # energy and fatigue
      # Items 23, 27 (Recoded 5,4,3,2,1,0) as 1 == 100%, 6 == 0%
      # 29, 31: Scale 1-6 where 1 == 0%, 6 == 100%, recode to 0 - 5
      vt1_r = recode(sf36_23, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, `6`=0),
      vt2_r = recode(sf36_29, `1`=0, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5),
      vt3_r = recode(sf36_27, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, `6`=0),
      vt4_r = recode(sf36_31, `1`=0, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5),
      vt_valid_count = sum(!is.na(c(vt1_r, vt2_r, vt3_r, vt4_r))),
      vt_raw = mean(c(vt1_r, vt2_r, vt3_r, vt4_r), na.rm = TRUE),
      # Lowest = 0, Highest = 5, Range = 5
      VT_Score = ifelse(vt_valid_count >= 2, (vt_raw / 5) * 100, NA),
      
      ## Social Functioning (SF) ----------
      # Items 20 Scale 1-5 (Recoded 5,4,3,2,1)
      # Item 32 Scale 1-5 no recoding as 1 == 0% and 5 == 100%
      # This handles the '6' in sf36_31 by turning it into NA.
      sf1_r = recode(sf36_20, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1), 
      sf2_r = sf36_32,
      sf_valid_count = sum(!is.na(c(sf1_r, sf2_r))),
      sf_raw = mean(c(sf1_r, sf2_r), na.rm = TRUE),
      # Lowest = 1, Highest = 5, Range = 4
      SF_Score = ifelse(sf_valid_count >= 1, ((sf_raw - 1) / 4) * 100, NA),
      
      ## Role-Emotional (RE) ---------
      # role limitations due to emotional problems
      # Items 17-19: Scale 1-2. No recoding needed.
      # Lowest = 1, Highest = 2, Range = 1
      re_valid_count = sum(!is.na(c_across(all_of(re_items)))),
      re_raw = mean(c_across(all_of(re_items)), na.rm = TRUE),
      RE_Score = ifelse(re_valid_count >= 2, ((re_raw - 1) / 1) * 100, NA),
      
      ## Mental Health (MH) ----------
      # emotional wllbeing assessment
      # Items 24, 25, 26, 28, 30
      # 26, 30 scale scale 1-6 (recode 5:0)
      # 24, 25, 28 scale 1:6, (recode 0:5)
      # Lowest = 1, Highest = 5, Range = 4
      mh1_r = recode(sf36_24, `1`=0, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5),
      mh2_r = recode(sf36_25, `1`=0, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5),
      mh3_r = recode(sf36_26, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, `6`=0),
      mh4_r = recode(sf36_28, `1`=0, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5),
      mh5_r = recode(sf36_30, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, `6`=0),
      mh_valid_count = sum(!is.na(c(mh1_r, mh2_r, mh3_r, mh4_r, mh5_r))),
      mh_raw = mean(c(mh1_r, mh2_r, mh3_r, mh4_r, mh5_r), na.rm = TRUE),
      # Lowest = 0, Highest = 5, Range = 5
      MH_Score = ifelse(mh_valid_count >= 2, (mh_raw / 5) * 100, NA),
    ) %>%
    # Ungroup to return to a standard data frame-------
    ungroup() %>%
    # Select only the original ID and the new scale scores
    select(
      # Keep registry_id if it exists
      any_of("registry_id"),
      starts_with("date_"),
      visit_name,
      PF_Score,
      RP_Score,
      BP_Score,
      GH_Score,
      VT_Score,
      SF_Score,
      RE_Score,
      MH_Score
    )
  
  return(scored_data)
}
