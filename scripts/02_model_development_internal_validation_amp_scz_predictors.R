# Set language to English for messages and warnings
Sys.setenv(LANG = "en")

#### PREPARE A SOCIODEM-ANCHORED DATASET ####

library(tidyverse)   # dplyr, tidyr, readr, etc.
library(readr)
library(dplyr)
library(writexl)
library(predRupdate)

#---------------------------
# 1) Load reference dataset
#---------------------------
data_socdem <- read_delim(
  "datasets/SOCIODEM_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)

#---------------------------------------------
# 2) Load PPS baseline (for item-level vars)
#    -> derive all PPS-based predictors here,
#    -> then left_join into SOCIODEM later
#---------------------------------------------
PPS_PRONET <- read_delim(
  "datasets/PPS_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)

# --- Transformations on PPS_PRONET ---

# Ages (use numeric fields that exist; do NOT overwrite with dob*)
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    participant_age = as.numeric(interview_age) / 12,
    father_age      = as.numeric(chrpps_fage),
    mother_age      = as.numeric(chrpps_mage)
  )

# Parental ages relative to participant
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    paternal_age = father_age - participant_age,
    maternal_age = mother_age - participant_age
  )

# Reverse-coded CTQ items
PPS_PRONET$chrpps_loved_rev    <- 6 - PPS_PRONET$chrpps_loved
PPS_PRONET$chrpps_special_rev  <- 6 - PPS_PRONET$chrpps_special
PPS_PRONET$chrpps_care_rev     <- 6 - PPS_PRONET$chrpps_care
PPS_PRONET$chrpps_closefam_rev <- 6 - PPS_PRONET$chrpps_closefam
PPS_PRONET$chrpps_support_rev  <- 6 - PPS_PRONET$chrpps_support
PPS_PRONET$chrpps_protect_rev  <- 6 - PPS_PRONET$chrpps_protect
PPS_PRONET$chrpps_docr_rev     <- 6 - PPS_PRONET$chrpps_docr

# Reverse code TEPS (anhedonia) items
PPS_PRONET[, c(188, 189, 191:205)] <- 7 - PPS_PRONET[, c(188, 189, 191:205)]

# Reverse code NSCQ (neighborhood quality) – example subset specified
PPS_PRONET[, c(158, 159, 160)] <- 5 - PPS_PRONET[, c(158, 159, 160)]

# Reverse code DGLS (loneliness) – example subset specified
PPS_PRONET[, c(206, 210, 211)] <- 4 - PPS_PRONET[, c(206, 210, 211)]

# Handedness: NA only if all four are NA; else 0/1 by mean threshold
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    handedness = case_when(
      is.na(rowMeans(PPS_PRONET[, c("chrpps_writing","chrpps_throwing","chrpps_toothbrush","chrpps_spoon")], na.rm = TRUE)) ~ NA_real_,
      rowMeans(PPS_PRONET[, c("chrpps_writing","chrpps_throwing","chrpps_toothbrush","chrpps_spoon")], na.rm = TRUE) > 3 ~ 0,
      TRUE ~ 1
    )
  )

# Adult life events – NA only if ALL sixmo items are NA
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    adult_life_events = case_when(
      if_all(starts_with("chrpps_sixmo___"), is.na) ~ NA_real_,
      chrpps_sixmo___15 == 1 ~ 0,
      TRUE ~ 1
    )
  )

# Childhood trauma + 5 CTQ subscales – NA only if ALL items in the set are NA
ct_cols <- c(
  "chrpps_lazy","chrpps_born","chrpps_hate","chrpps_hurt",
  "chrpps_emoab","chrpps_doc","chrpps_bruise","chrpps_belt",
  "chrpps_physab","chrpps_beat","chrpps_touch","chrpps_threat",
  "chrpps_sexual","chrpps_molest","chrpps_sexab",
  "chrpps_loved_rev","chrpps_special_rev","chrpps_care_rev",
  "chrpps_closefam_rev","chrpps_support_rev",
  "chrpps_hunger","chrpps_protect_rev","chrpps_pardrunk","chrpps_dirty",
  "chrpps_docr_rev"
)

PPS_PRONET <- PPS_PRONET %>%
  mutate(
    childhood_trauma_sum = rowSums(across(all_of(ct_cols)), na.rm = TRUE),
    childhood_trauma = case_when(
      if_all(all_of(ct_cols), is.na) ~ NA_real_,
      childhood_trauma_sum > 0       ~ childhood_trauma_sum,
      TRUE                           ~ 0
    )
  ) %>%
  select(-childhood_trauma_sum)

# CTQ subscales (same NA rule: NA only if ALL items in the subscale are NA)
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    # Emotional abuse
    ea_sum = rowSums(across(c(chrpps_lazy, chrpps_born, chrpps_hate, chrpps_hurt, chrpps_emoab)), na.rm = TRUE),
    emotional_abuse = case_when(
      if_all(c(chrpps_lazy, chrpps_born, chrpps_hate, chrpps_hurt, chrpps_emoab), is.na) ~ NA_real_,
      ea_sum > 0 ~ ea_sum,
      TRUE ~ 0
    ),
    # Physical abuse
    pa_sum = rowSums(across(c(chrpps_doc, chrpps_bruise, chrpps_belt, chrpps_physab, chrpps_beat)), na.rm = TRUE),
    physical_abuse = case_when(
      if_all(c(chrpps_doc, chrpps_bruise, chrpps_belt, chrpps_physab, chrpps_beat), is.na) ~ NA_real_,
      pa_sum > 0 ~ pa_sum,
      TRUE ~ 0
    ),
    # Sexual abuse
    sa_sum = rowSums(across(c(chrpps_touch, chrpps_threat, chrpps_sexual, chrpps_molest, chrpps_sexab)), na.rm = TRUE),
    sexual_abuse = case_when(
      if_all(c(chrpps_touch, chrpps_threat, chrpps_sexual, chrpps_molest, chrpps_sexab), is.na) ~ NA_real_,
      sa_sum > 0 ~ sa_sum,
      TRUE ~ 0
    ),
    # Emotional neglect
    en_sum = rowSums(across(c(chrpps_loved_rev, chrpps_special_rev, chrpps_care_rev, chrpps_closefam_rev, chrpps_support_rev)), na.rm = TRUE),
    emotional_neglect = case_when(
      if_all(c(chrpps_loved_rev, chrpps_special_rev, chrpps_care_rev, chrpps_closefam_rev, chrpps_support_rev), is.na) ~ NA_real_,
      en_sum > 0 ~ en_sum,
      TRUE ~ 0
    ),
    # Physical neglect
    pn_sum = rowSums(across(c(chrpps_hunger, chrpps_protect_rev, chrpps_pardrunk, chrpps_dirty, chrpps_docr_rev)), na.rm = TRUE),
    physical_neglect = case_when(
      if_all(c(chrpps_hunger, chrpps_protect_rev, chrpps_pardrunk, chrpps_dirty, chrpps_docr_rev), is.na) ~ NA_real_,
      pn_sum > 0 ~ pn_sum,
      TRUE ~ 0
    )
  ) %>%
  select(-ea_sum, -pa_sum, -sa_sum, -en_sum, -pn_sum)

# Anhedonia, Neighborhood Quality, Loneliness – NA only if ALL items in each set are NA
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    anhedonia_sum = rowSums(across(188:205), na.rm = TRUE),
    anhedonia = case_when(
      if_all(188:205, is.na) ~ NA_real_,
      anhedonia_sum > 0      ~ anhedonia_sum,
      TRUE                   ~ 0
    ),
    nbh_sum = rowSums(across(151:162), na.rm = TRUE),
    neighborhood_quality = case_when(
      if_all(151:162, is.na) ~ NA_real_,
      nbh_sum > 0            ~ nbh_sum,
      TRUE                   ~ 0
    ),
    lon_sum = rowSums(across(206:211), na.rm = TRUE),
    loneliness = case_when(
      if_all(206:211, is.na) ~ NA_real_,
      lon_sum > 0            ~ lon_sum,
      TRUE                   ~ 0
    )
  ) %>%
  select(-anhedonia_sum, -nbh_sum, -lon_sum)

# PPS-derived frame to merge into SOCIODEM
pps_derived <- PPS_PRONET %>%
  select(
    src_subject_id, subjectkey,
    participant_age, handedness,
    paternal_age,
    adult_life_events,
    childhood_trauma, emotional_abuse, physical_abuse, sexual_abuse,
    emotional_neglect, physical_neglect,
    anhedonia, neighborhood_quality, loneliness,
    chrpps_fcob
  )

#------------------------------------------------------
# 3) Derive SOCIODEM-based variables inside SOCIODEM
#------------------------------------------------------

# Gender 
data_socdem <- data_socdem %>%
  mutate(
    gender = case_when(
      chrdemo_gender_identity___1 == 1 ~ "Male",
      chrdemo_gender_identity___2 == 1 ~ "Female",
      chrdemo_gender_identity___4 == 1 ~ "Non-binary",
      chrdemo_gender_identity____99 == 1 | chrdemo_gender_identity___99 == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  )

# Ethnicity (more granular; keep as-is)
cols <- paste0("chrdemo_racial_back___", 1:8)
labels <- c(
  "Indigenous_Aboriginal",                 # 1
  "East_Asian",                            # 2
  "Southeast_Asia",                        # 3
  "South_Asian",                           # 4
  "Black",                                 # 5
  "West_Central_Asia_or_Middle_Eastern",   # 6
  "White",                                 # 7
  "Pacific_Islander"                       # 8
)
n_selected <- rowSums(data_socdem[, cols] == 1, na.rm = TRUE)
which_one <- apply(data_socdem[, cols] == 1, 1, function(x) {
  if (sum(x, na.rm = TRUE) == 1) which(x) else NA_integer_
})
data_socdem$ethnicity <- NA_character_
single <- !is.na(which_one)
data_socdem$ethnicity[single] <- labels[which_one[single]]
data_socdem$ethnicity[n_selected >= 2] <- "Mixed"

# Merge CHR and HC age variables (convert from months to years)
data_socdem <- data_socdem %>%
  mutate(
    age = coalesce(chrdemo_age_mos_chr, chrdemo_age_mos_hc) / 12
  )

#---------------------------------------------
# 4) Merge everything INTO SOCIODEM (left_join)
#---------------------------------------------
SOCIODEM_enriched <- data_socdem %>%
  left_join(pps_derived, by = "src_subject_id")

#---------------------------------------------
# 5) Parents' severe mental illness (FIGS) – four variables
#---------------------------------------------
data_figs1 <- read_delim(
  "datasets/FIGS_baseline1.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)
data_figs2 <- read_delim(
  "datasets/FIGS_baseline2.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)
data_figs <- bind_rows(data_figs1, data_figs2)

# Keep first two rows per subject
rows_to_keep <- unlist(lapply(seq(1, nrow(data_figs), by = 15), function(x) c(x, x + 1)))
data_figs_simpl <- data_figs[rows_to_keep, ] %>%
  select(subjectkey, src_subject_id,
         chrfigs_fam_ddx, chrfigs_fam_mdx, chrfigs_fam_pdx, chrfigs_fam_apdx, chrfigs_fam_napdx)

# tri_pair helper (1 if any 1–3; 0 if all 0/9; NA if both NA)
tri_pair <- function(a, b) {
  vals <- c(a, b)
  if (all(is.na(vals))) return(NA_integer_)
  if (any(vals %in% 1:3, na.rm = TRUE)) return(1L)
  0L
}
nth2 <- function(x, n) if (length(x) >= n) x[n] else NA_integer_

# Summarise per subject
figs_simpl_final <- data_figs_simpl %>%
  arrange(src_subject_id) %>%
  group_by(src_subject_id) %>%
  summarise(
    mother_psychosis            = tri_pair(nth2(chrfigs_fam_pdx, 1),  nth2(chrfigs_fam_apdx, 1)),
    mother_other_mental_illness = tri_pair(nth2(chrfigs_fam_ddx, 1),  nth2(chrfigs_fam_mdx, 1)),
    father_psychosis            = tri_pair(nth2(chrfigs_fam_pdx, 2),  nth2(chrfigs_fam_apdx, 2)),
    father_other_mental_illness = tri_pair(nth2(chrfigs_fam_ddx, 2),  nth2(chrfigs_fam_mdx, 2)),
    .groups = "drop"
  ) %>%
  mutate(
    parent_smi = case_when(
      rowSums(across(c(mother_psychosis, mother_other_mental_illness,
                       father_psychosis, father_other_mental_illness),
                     ~ !is.na(.))) == 0 ~ NA_real_,
      rowSums(across(c(mother_psychosis, mother_other_mental_illness,
                       father_psychosis, father_other_mental_illness),
                     ~ . == 1), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )
  )

SOCIODEM_enriched <- SOCIODEM_enriched %>%
  left_join(figs_simpl_final, by = "src_subject_id")


#---------------------------------------------
# 6) Tobacco & cannabis (ASSIST) – KEEP NA if no ASSIST row
#---------------------------------------------
library(readr) # just to ensure `parse_number` is available


# Prepare ASSIST with proper NA handling and parsing


data_assist <- read_delim(
  "datasets/ASSIST_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    chrassist_whoassist_often1 = parse_number(as.character(chrassist_whoassist_often1)),
    chrassist_whoassist_often3 = parse_number(as.character(chrassist_whoassist_often3)),
    tobacco_daily = as.integer(coalesce(chrassist_whoassist_often1 == 6, FALSE)),
    cannabis_daily = as.integer(coalesce(chrassist_whoassist_often3 == 6, FALSE))
  ) %>%
  select(src_subject_id, tobacco_daily, cannabis_daily)


# LEFT JOIN without overwriting NAs
SOCIODEM_enriched <- SOCIODEM_enriched %>%
  left_join(data_assist, by = "src_subject_id")

#---------------------------------------------
# 7) Premorbid IQ (WRAT-5), Current IQ (WASI-2/WISC-V/WAIS-IV)
#---------------------------------------------
premorbid_iq_wrat5 <- read_delim(
  "datasets/Premorbid_IQ_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  select(src_subject_id, pm_iq_wrat5 = chrpreiq_standard_score)

premorbid_iq_wasi2 <- read_delim(
  "datasets/WASI2_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  select(src_subject_id, pm_iq_wasi2 = chriq_fsiq)

premorbid_iq_wiscv <- read_delim(
  "datasets/WISCV_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  select(src_subject_id, pm_iq_wiscv = chriq_fsiq)

premorbid_iq_waisiv <- read_delim(
  "datasets/WAISIV_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  select(src_subject_id, pm_iq_waisiv = chriq_fsiq)

SOCIODEM_enriched <- SOCIODEM_enriched %>%
  left_join(premorbid_iq_wrat5, by = "src_subject_id") %>%
  left_join(premorbid_iq_wasi2, by = "src_subject_id") %>%
  left_join(premorbid_iq_wiscv, by = "src_subject_id") %>%
  left_join(premorbid_iq_waisiv, by = "src_subject_id") %>%
  mutate(pm_iq_wasi2 = ifelse(pm_iq_wasi2 %in% c(-900, "-900"), NA, pm_iq_wasi2)) %>%
  mutate(premorbid_iq = coalesce(pm_iq_wrat5, pm_iq_wasi2)) %>%
  select(-pm_iq_wiscv, -pm_iq_waisiv)

#---------------------------------------------
# 8) Childhood social withdrawal (Premorbid AS)
#---------------------------------------------
premorbid_as <- read_delim(
  "datasets/Premorbid_AS_month1.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  select(src_subject_id, childhood_social_withdrawal = chrpas_pmod_child1)

SOCIODEM_enriched <- SOCIODEM_enriched %>%
  left_join(premorbid_as, by = "src_subject_id")

#-------------------------------------------------------------
# 9) Migrant flags & parental socioeconomic status
#-------------------------------------------------------------
SOCIODEM_enriched <- SOCIODEM_enriched %>%
  mutate(
    
    # North African migrants
    north_african_first_gen_migrant = case_when(
      chrdemo_country_situation %in% c(2,3) &
        chrdemo_cob %in% c(3,52,99,118,180) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    
    north_african_second_gen_migrant = case_when(
      chrdemo_country_situation == 4 &
        chrpps_fcob %in% c(3,52,98,117,179) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Migrants excluding North Africa
    first_gen_migrant = case_when(
      chrdemo_country_situation %in% c(2,3) &
        !(chrdemo_cob %in% c(3,52,99,118,180)) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    
    second_gen_migrant = case_when(
      chrdemo_country_situation == 4 &
        !(chrpps_fcob %in% c(3,52,98,117,179)) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    
    parent_ses = case_when(
      is.na(chrdemo_parent_occupation) & is.na(chrdemo_parent_occupation2) ~ NA_real_,
      TRUE ~ if_else(
        pmin(chrdemo_parent_occupation, chrdemo_parent_occupation2, na.rm = TRUE) <= 6, 0, 1
      )
    )
  )

#---------------------------------------------
# 10) CHR vs HC (INCEXC)
#---------------------------------------------
data_incexc <- read_delim(
  "datasets/INCEXC_criteria.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
) %>%
  select(src_subject_id, chrcrit_part)

SOCIODEM_enriched <- SOCIODEM_enriched %>%
  left_join(data_incexc, by = "src_subject_id") %>%
  mutate(
    chr_status = case_when(
      is.na(chrcrit_part) ~ NA_real_,
      chrcrit_part == 2   ~ 0,      # HC
      TRUE                ~ as.numeric(chrcrit_part)
    )
  ) %>%
  select(-chrcrit_part)

#---------------------------------------------
# 11) Final selection (keeps ALL SOCIODEM rows)
#---------------------------------------------
PPS_PRONET_clean_newpred <- SOCIODEM_enriched %>%
  select(
    src_subject_id,
    age, gender, handedness, ethnicity,
    first_gen_migrant, second_gen_migrant,
    north_african_first_gen_migrant, north_african_second_gen_migrant,
    paternal_age, parent_ses,
    mother_psychosis, mother_other_mental_illness,
    father_psychosis, father_other_mental_illness,                         
    adult_life_events, tobacco_daily, cannabis_daily, emotional_abuse, physical_abuse, sexual_abuse,
    emotional_neglect, physical_neglect, anhedonia,
    neighborhood_quality, loneliness, premorbid_iq,
    childhood_social_withdrawal,
    chr_status
  )

# inspect missingness
missing_summary <- PPS_PRONET_clean_newpred %>%
  select(-src_subject_id) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingPercent") %>%
  arrange(desc(MissingPercent))

write_xlsx(missing_summary, "missigness_RNCV_AMPSCZ.xlsx")

print(missing_summary)

#---------------------------------------------
# 12) Ethnicity reference = "White"
#---------------------------------------------
PPS_PRONET_clean_newpred <- PPS_PRONET_clean_newpred %>%
  mutate(
    ethnicity = factor(ethnicity,
                       levels = c("White", "Black", "East_Asian", "Southeast_Asia",
                                  "South_Asian", "West_Central_Asia_or_Middle_Eastern",
                                  "Indigenous_Aboriginal", "Pacific_Islander", "Mixed"),
                       ordered = FALSE)
  )

#---------------------------------------------
# 12) Save final dataset
#---------------------------------------------
write_csv(PPS_PRONET_clean_newpred, "PPS_PRONET_clean_newpred.csv")


#### REPEATED NESTED CROSS-VALIDATION ####

# =============================================================================
# PPS Repeated Nested Cross-Validation (EU-GEI-aligned), annotated
# =============================================================================
# Concept: This script implements the EU-GEI detection pipeline (environmental PPS),
# using LASSO logistic regression with repeated nested CV:
#   - Outer CV -> unbiased generalisation estimate (holdout test per split)
#   - Inner CV -> λ tuning on training only (no leakage)
#   - Non-leaky preprocessing (NZV filtering + imputation) always fit on train,
#     applied to test where appropriate
#   - Optional class rebalancing (SMOTE) inside inner CV folds
#   - Discrimination + calibration metrics aggregated across outer splits

# -------------------------
# 0) Packages, paths & parameters
# -------------------------
suppressPackageStartupMessages({
  library(tidyverse)   # wrangling
  library(caret)       # resampling, metrics, nearZeroVar, SMOTE hook
  library(glmnet)      # LASSO logistic regression
  library(predRupdate) # calibration
  library(pROC)        # ROC/AUC
  library(missForest)  # random-forest imputation
  library(Hmisc)       # rcorr.cens (C-index)
  library(writexl)     # export CV results
  library(dcurves)     # decision curve analysis
  library(openxlsx)    # DCA tables
})

data_path <- "PRONET_PPS_Processing/PPS_PRONET_clean_newpred.csv"

# Repeated nested CV design
outerFolds   <- 5 
outerRepeats <- 10 
innerFolds   <- 5
innerRepeats <- 10 

set.seed(231) # reproducibility
lambda_grid <- seq(0.001, 0.1, length.out = 100)
use_smote   <- FALSE  # EU-GEI used SMOTE; disable unless needed

# DCA prevalence assumptions
prev_clinical <- 0.192
prev_general  <- 0.02

# -------------------------
# 1) Load data
# -------------------------
# EU locales: ";" sep, "," decimal -> use read.csv2
PPS_PRONET_clean_newpred <- read.csv2(data_path, stringsAsFactors = FALSE, dec = ",")

# # ============================================================
# # CHECK: HC and CHR availability per site (for LOSO-CV)
# # ============================================================
# 
# site_check <- PPS_PRONET_clean_newpred %>%
#   mutate(
#     site = substr(src_subject_id, 1, 2),
#     chr  = factor(chr_status, levels = c(0, 1), labels = c("HC", "CHR"))
#   ) %>%
#   group_by(site, chr) %>%
#   summarise(N = n(), .groups = "drop") %>%
#   tidyr::pivot_wider(
#     names_from = chr,
#     values_from = N,
#     values_fill = 0
#   ) %>%
#   mutate(
#     total = HC + CHR,
#     valid_for_LOSO = (HC > 0 & CHR > 0)
#   ) %>%
#   arrange(total)
# 
# print(site_check)


# -------------------------
# 2) Preprocess variable classes (global type hygiene; no leakage)
# -------------------------

# 2.1 IDs to drop
id_cols <- intersect(c("src_subject_id","subjectkey","subject_id","id"),
                     names(PPS_PRONET_clean_newpred))
if (length(id_cols)) PPS_PRONET_clean_newpred[id_cols] <- list(NULL)

# 2.2 Outcome factor with positive class "CHR"
PPS_PRONET_clean_newpred$chr <- factor(
  PPS_PRONET_clean_newpred$chr_status,
  levels = c(0, 1),
  labels = c("HC", "CHR")
)

# 2.3 Type standardisation
bin_vars <- c(
  "handedness", "first_gen_migrant", "second_gen_migrant",
  "north_african_first_gen_migrant", "north_african_second_gen_migrant",
  "parent_ses", "mother_psychosis", "mother_other_mental_illness",
  "father_psychosis", "father_other_mental_illness",
  "adult_life_events", "tobacco_daily", "cannabis_daily"
)
cat_vars <- c("gender","ethnicity")
num_vars <- c(
  "age", "paternal_age", "emotional_abuse", "physical_abuse", "sexual_abuse",
  "emotional_neglect", "physical_neglect", "loneliness",
  "neighborhood_quality", "anhedonia", "premorbid_iq", "childhood_social_withdrawal"
)

for (v in intersect(bin_vars, names(PPS_PRONET_clean_newpred))) {
  PPS_PRONET_clean_newpred[[v]] <- factor(PPS_PRONET_clean_newpred[[v]], levels = c(0,1))
}
for (v in intersect(cat_vars, names(PPS_PRONET_clean_newpred))) {
  PPS_PRONET_clean_newpred[[v]] <- as.factor(PPS_PRONET_clean_newpred[[v]])
}
for (v in intersect(num_vars, names(PPS_PRONET_clean_newpred))) {
  PPS_PRONET_clean_newpred[[v]] <- suppressWarnings(as.numeric(PPS_PRONET_clean_newpred[[v]]))
}

# 2.4 Build modeling frame (predictors + outcome) - prepare for subject-level BOOTSTRAP
outcome_var <- "chr"
pred_vars   <- setdiff(names(PPS_PRONET_clean_newpred), c("chr_status", outcome_var))
df <- PPS_PRONET_clean_newpred[, c(pred_vars, outcome_var), drop = FALSE]
df[[outcome_var]] <- factor(df[[outcome_var]], levels = c("HC","CHR"))
# ---- create stable subject ID for CV tracking ----
df$subject_index <- seq_len(nrow(df))


# -------------------------
# 3) Helper functions (metrics + encoding)
# -------------------------
cindex_summary <- function(data, lev = NULL, model = NULL) {
  if (is.null(lev)) lev <- levels(data$obs)
  pos_level <- lev[2]
  pred_prob <- data[[pos_level]]
  obs_num   <- ifelse(data$obs == pos_level, 1, 0)
  cindex    <- as.numeric(Hmisc::rcorr.cens(pred_prob, obs_num)[1]) # Harrell's C
  c(C = cindex)
}

ici_stat <- function(observed, predicted) {
  observed  <- as.numeric(observed)
  predicted <- pmin(pmax(predicted, 1e-6), 1 - 1e-6)
  lo <- loess(observed ~ predicted)
  p_cal <- predict(lo, newdata = data.frame(predicted = predicted))
  mean(abs(p_cal - predicted)) # ICI
}

safe_model_matrix <- function(df) {
  MM <- as.data.frame(model.matrix(~ . - 1, df))
  colnames(MM) <- make.names(colnames(MM), unique = TRUE)
  MM
}

# -------------------------
# 4) Repeated Nested CV driver (EU-GEI logic) — stores out-of-fold preds
# -------------------------
LR_repeated_nested_cv <- function(combined_df,
                                  outerFolds, outerRepeats,
                                  innerFolds, innerRepeats,
                                  lambda_grid,
                                  seed = 231,
                                  use_smote = NULL) {
  
  if (is.null(use_smote)) use_smote <- FALSE 
  
  set.seed(seed)
  seeds <- sample(1:10000, outerRepeats)
  
  c_stat_nested      <- tibble()
  calibration_slopes <- tibble()
  preds_store        <- tibble()
  dropped_vars_log   <- tibble()
  
  for (outer_rep in seq_len(outerRepeats)) {
    message("Outer Repeat: ", outer_rep)
    set.seed(seeds[outer_rep])
    
    # Outer splits: stratified folds on outcome
    outer_folds <- createFolds(combined_df[[outcome_var]],
                               k = outerFolds,
                               list = TRUE, returnTrain = TRUE)
    
    for (i in seq_along(outer_folds)) {
      message("  Outer Fold: ", i)
      
      # Outer train/test
      train_idx  <- outer_folds[[i]]
      train_base <- combined_df[train_idx, , drop = FALSE]
      test_base  <- combined_df[-train_idx, , drop = FALSE]
      
      # ---- Drop predictors with high missingness in OUTER-TRAIN (>20%), mirror to TEST
      drop_threshold <- 0.20
      preds_only <- setdiff(names(train_base), outcome_var)
      miss_rate  <- sapply(train_base[, preds_only, drop = FALSE], function(x) mean(is.na(x)))
      drop_vars  <- names(miss_rate)[miss_rate > drop_threshold]
      
      if (length(drop_vars)) {
        dropped_vars_log <- bind_rows(
          dropped_vars_log,
          tibble(OuterRepeat = outer_rep,
                 Fold        = i,
                 variable    = drop_vars,
                 missing_pct = as.numeric(miss_rate[drop_vars]))
        )
        message("Dropped (> ", 100*drop_threshold, "% missing) in OuterRepeat ", outer_rep,
                ", Fold ", i, ": ", paste(drop_vars, collapse = ", "))
      }
      
      keep_vars  <- setdiff(names(train_base), drop_vars)
      train_base <- train_base[, keep_vars, drop = FALSE]
      test_base  <- test_base[, intersect(names(test_base), keep_vars), drop = FALSE]
      
      # --- NZV filtering (fit on predictors only, exclude outcome) ---
      nzv <- nearZeroVar(train_base[, setdiff(names(train_base), c(outcome_var,"subject_index")), drop = FALSE],
                         saveMetrics = TRUE)
      keep_cols  <- c(rownames(nzv)[!nzv$nzv], outcome_var, "subject_index")
      train_base <- train_base[, keep_cols, drop = FALSE]
      test_base  <- test_base[, intersect(names(test_base), keep_cols), drop = FALSE]
      
      # ----- SINGLE IMPUTATION (missForest) -----
      set.seed(1000 + outer_rep*10 + i)
      
      y_train_vec  <- train_base[[outcome_var]]
      X_train_base <- train_base[, setdiff(names(train_base), c(outcome_var,"subject_index")), drop = FALSE]
      y_test_vec   <- test_base[[outcome_var]]
      X_test_base  <- test_base[, setdiff(names(test_base), c(outcome_var,"subject_index")), drop = FALSE]
      
      imp_train_mf <- missForest(X_train_base)
      X_train_imp  <- as.data.frame(imp_train_mf$ximp)
      imp_test_mf  <- missForest(X_test_base)
      X_test_imp   <- as.data.frame(imp_test_mf$ximp)
      
      train_imp_once <- cbind(X_train_imp, setNames(data.frame(y_train_vec), outcome_var))
      test_imp       <- cbind(X_test_imp,  setNames(data.frame(y_test_vec),  outcome_var))
      train_imp_once[[outcome_var]] <- factor(train_imp_once[[outcome_var]], levels = levels(combined_df[[outcome_var]]))
      test_imp[[outcome_var]]       <- factor(test_imp[[outcome_var]],       levels = levels(combined_df[[outcome_var]]))
      
      # ----- Inner loop: λ tuning on outer-train only -----
      inner_results <- vector("list", innerRepeats)
      for (inner_rep in seq_len(innerRepeats)) {
        message("    Inner Repeat: ", inner_rep)
        set.seed(seeds[outer_rep] + inner_rep)
        
        X_train <- safe_model_matrix(train_imp_once %>% dplyr::select(-dplyr::all_of(outcome_var)))
        y_train <- factor(train_imp_once[[outcome_var]])
        
        ctrl <- trainControl(
          method = "cv",
          number = innerFolds,
          summaryFunction = cindex_summary,
          classProbs = TRUE,
          sampling = if (use_smote) "smote" else NULL
        )
        tune_grid  <- expand.grid(alpha = 1, lambda = lambda_grid)
        inner_model <- train(
          x = X_train, y = y_train,
          method = "glmnet",
          trControl = ctrl,
          metric = "C",
          tuneGrid = tune_grid,
          family = "binomial"
        )
        if (!all(is.na(inner_model$results$C))) {
          inner_results[[inner_rep]] <- tibble(
            C = inner_model$results$C,
            lambda = inner_model$bestTune$lambda,
            repeat_number = inner_rep
          )
        }
      }
      
      # Aggregate λ across inner repeats
      all_inner <- bind_rows(inner_results)
      if (nrow(all_inner) == 0) stop("No inner results collected; check data and settings.")
      best_lambda <- mean(all_inner$lambda, na.rm = TRUE)
      
      # Design matrices; align columns between train/test (add missing dummies as 0)
      X_train_final <- safe_model_matrix(train_imp_once %>% select(-all_of(outcome_var)))
      X_test_final  <- safe_model_matrix(test_imp       %>% select(-all_of(outcome_var)))
      common_cols <- union(colnames(X_train_final), colnames(X_test_final))
      add_missing <- function(M, cols) {
        miss <- setdiff(cols, colnames(M))
        if (length(miss) > 0) M[, miss] <- 0
        M[, cols, drop = FALSE]
      }
      X_train_final <- add_missing(X_train_final, common_cols)
      X_test_final  <- add_missing(X_test_final,  common_cols)
      
      y_train_final <- factor(train_imp_once[[outcome_var]], levels = levels(df[[outcome_var]]))
      y_test        <- factor(test_imp[[outcome_var]],       levels = levels(df[[outcome_var]]))
      
      # LASSO fit at tuned λ (binomial, standardised)
      fit_final <- glmnet(
        x = as.matrix(X_train_final),
        y = as.numeric(y_train_final == levels(y_train_final)[2]),
        alpha = 1, lambda = best_lambda, family = "binomial", standardize = TRUE
      )
      
      # Predict on outer-test
      prob_test <- as.numeric(predict(fit_final, newx = as.matrix(X_test_final), type = "response"))
      prob_test <- pmin(pmax(prob_test, 1e-6), 1 - 1e-6)
      y_num     <- as.integer(y_test == levels(y_test)[2])  # 0/1
      
      # Calibration on OUTER-TEST
      calibration <- data.frame(
        observed = y_num,
        predicted = prob_test
      )
      
      logistic_calibration <- predRupdate::pred_val_probs(
        binary_outcome = calibration$observed,
        Prob = calibration$predicted,
        cal_plot = FALSE
      )
      
      cal_intercept <- logistic_calibration$CalInt
      cal_slope     <- logistic_calibration$CalSlope
      
      brier <- mean((y_num - prob_test)^2)
      ici   <- ici_stat(y_num, prob_test)
      
      calibration_slopes <- bind_rows(
        calibration_slopes,
        tibble(
          fold = i,
          OuterRepeat = outer_rep,
          intercept = cal_intercept,
          slope = cal_slope,
          brier = brier,
          ICI = ici
        )
      )
      
      # Discrimination on OUTER-TEST
      C_test <- as.numeric(Hmisc::rcorr.cens(prob_test, y_num)[1])
      
      # Threshold from OUTER-TRAIN (Youden)
      prob_train <- as.numeric(predict(fit_final, newx = as.matrix(X_train_final), type = "response"))
      roc_train  <- pROC::roc(response = y_train_final, predictor = prob_train, quiet = TRUE)
      thr <- as.numeric(pROC::coords(roc_train, "best", best.method = "youden", ret = "threshold"))
      pred_test <- factor(ifelse(prob_test >= thr, "CHR", "HC"), levels = c("HC","CHR"))
      cm <- caret::confusionMatrix(data = pred_test, reference = y_test, positive = "CHR")
      
      c_stat_nested <- bind_rows(c_stat_nested, tibble(
        C_test = C_test,
        Fold = i,
        OuterRepeat = outer_rep,
        n_train = nrow(train_imp_once),
        events_train = sum(y_train_final == levels(y_train_final)[2]),
        n_test = length(y_test),
        events_test = sum(y_test == levels(y_test)[2]),
        balanced_accuracy = cm$byClass["Balanced Accuracy"],
        sensitivity = cm$byClass["Sensitivity"],
        specificity = cm$byClass["Specificity"],
        ppv = cm$byClass["Pos Pred Value"],
        npv = cm$byClass["Neg Pred Value"],
        precision = cm$byClass["Precision"],
        recall = cm$byClass["Recall"],
        f1 = cm$byClass["F1"]
      ))
      
      # ---- Store out-of-fold test probs and observed labels (for DCA) ----
      preds_store <- bind_rows(
        preds_store,
        tibble(
          OuterRepeat = outer_rep,
          Fold        = i,
          subject_index  = test_base$subject_index,
          obs         = y_num,      # 0/1 on outer-test
          pred        = prob_test   # predicted probability of CHR
        )
      )
    } # outer folds
  }   # outer repeats
  
  list(
    c_stat_nested      = c_stat_nested,
    calibration_slopes = calibration_slopes,
    preds              = preds_store,
    dropped_vars_log   = dropped_vars_log
  )
}

# -------------------------
# 5) Run the pipeline
# -------------------------
results_PPS <- LR_repeated_nested_cv(
  combined_df  = df,
  outerFolds   = outerFolds,
  outerRepeats = outerRepeats,
  innerFolds   = innerFolds,
  innerRepeats = innerRepeats,
  lambda_grid  = lambda_grid,
  seed         = 231,
  use_smote    = use_smote
)


# -------------------------
# 6) Decision Curve Analysis (DCA) from RN-CV results (corrected)
# -------------------------

# Guard
if (!"preds" %in% names(results_PPS) || nrow(results_PPS$preds) == 0) {
  stop("results_PPS$preds is empty. Re-run RN-CV with preds storage enabled.")
}

# Prepare individual-level observed/predicted from outer folds
pred_df <- results_PPS$preds %>%
  group_by(subject_index) %>%
  summarise(
    obs  = first(obs),
    pred = mean(pred),
    .groups = "drop"
  ) %>%
  filter(!is.na(obs) & !is.na(pred))

# -------------------------
# Diagnostic: predicted probabilities used in DCA (+ CALIBRATION PLOT)
# -------------------------

# Summary stats overall and by class
cat("📌 Summary of predicted probabilities (all):\n")
print(summary(pred_df$pred))

cat("\n📌 Summary by observed class (0=HC, 1=CHR):\n")
print(tapply(pred_df$pred, pred_df$obs, summary))

# Calibration LOESS plot
p_calib_loess <- ggplot(pred_df, aes(x = pred, y = obs)) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = expression("Refined PPS model"^c),
    x = "Predicted probability (CHR)",
    y = "Observed CHR rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    axis.title.x = element_text(size = 14),
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

print(p_calib_loess)

# Export plot
ggsave("Calibration_LOESS_RNCV_AMPSCZ.png", p_calib_loess, width = 7, height = 6, dpi = 300)
message("✅ Exported: Calibration_LOESS_RNCV_AMPSCZ.png")


# Helper: compute DCA with specified prevalence
compute_dca <- function(obs, pred, thresholds, prevalence = NULL) {
  prev_use <- if (is.null(prevalence)) mean(obs == 1, na.rm = TRUE) else prevalence
  
  dca_tbl <- dcurves::dca(
    obs ~ pred,
    data = tibble(obs = obs, pred = pred),
    thresholds = thresholds,
    prevalence = prev_use
  ) %>% as_tibble()
  
  # Rename model curve to PPS
  pps <- dca_tbl %>% filter(label == "pred") %>%
    transmute(label = "PPS", threshold, net_benefit)
  
  thr <- sort(unique(thresholds))
  baseline <- tibble(
    label = rep(c("Treat None", "Treat All"), each = length(thr)),
    threshold = rep(thr, 2),
    net_benefit = c(
      rep(0, length(thr)),
      prev_use - (1 - prev_use) * (thr / (1 - thr))
    )
  )
  
  bind_rows(baseline, pps) %>%
    mutate(std_net_benefit = net_benefit / prev_use)
}

# Thresholds and prevalence values
thr_clin <- seq(0, 0.50, by = 0.005)
thr_gen  <- seq(0, 0.03, by = 0.001)

# Clinical population DCA
prev_clinical <- 0.192
dca_clinical  <- compute_dca(pred_df$obs, pred_df$pred, thr_clin, prevalence = prev_clinical)

# General population DCA
prev_general <- 0.02
dca_general  <- compute_dca(pred_df$obs, pred_df$pred, thr_gen, prevalence = prev_general)

# Plot helpers
plot_dca <- function(dca_tbl, title) {
  ggplot(dca_tbl, aes(x = threshold, y = net_benefit, color = label)) +
    geom_line(linewidth = 1) +
    labs(title = title, x = "Threshold probability", y = "Net benefit", color = "Strategy") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_color_manual(values = c("Treat None" = "black", "Treat All" = "grey50", "PPS" = "darkorange"))
}

# Generate and save plots
p_clin <- plot_dca(dca_clinical, "DCA — Clinical Population (AMPSCZ)") +
  coord_cartesian(ylim = c(0, 0.10))
p_gen  <- plot_dca(dca_general,  "DCA — General Population (AMPSCZ)") +
  coord_cartesian(ylim = c(-0.005, 0.02))

ggsave("DCA_RNCV_AMPSCZ_clinical.png", p_clin, bg = "white", width = 6.5, height = 5, dpi = 300)
ggsave("DCA_RNCV_AMPSCZ_general.png",  p_gen,  bg = "white", width = 6.5, height = 5, dpi = 300)

# Save tables
wb <- createWorkbook()
addWorksheet(wb, "Clinical")
addWorksheet(wb, "General")
writeData(wb, "Clinical", dca_clinical)
writeData(wb, "General",  dca_general)
saveWorkbook(wb, "DCA_RNCV_tables_AMPSCZ.xlsx", overwrite = TRUE)
message("Saved: DCA_RNCV_AMPSCZ_clinical.png, DCA_RNCV_AMPSCZ_general.png, DCA_RNCV_tables_AMPSCZ.xlsx")


# -------------------------
# 7) Summaries (means + 95%CIs across outer splits)
# -------------------------
ci_95 <- function(x) {
  m  <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  c(lower = m - 1.96 * se, upper = m + 1.96 * se)
}

class_df <- results_PPS$c_stat_nested %>%
  transmute(
    OuterRepeat, Fold,
    C = C_test,
    BalancedAccuracy = as.numeric(balanced_accuracy),
    Sensitivity = as.numeric(sensitivity),
    Specificity = as.numeric(specificity),
    PPV = as.numeric(ppv),
    NPV = as.numeric(npv),
    Precision = as.numeric(precision),
    Recall = as.numeric(recall),
    F1 = as.numeric(f1)
  )

calib_df <- results_PPS$calibration_slopes %>%
  transmute(
    OuterRepeat, fold,
    Intercept = as.numeric(intercept),
    Slope = as.numeric(slope),
    Brier = as.numeric(brier),
    ICI = as.numeric(ICI)
  )

summarise_with_ci <- function(df, cols) {
  df %>%
    summarise(across(all_of(cols), list(
      mean   = ~ mean(.x, na.rm = TRUE),
      ci_low = ~ ci_95(.x)[1],
      ci_high= ~ ci_95(.x)[2]
    ), .names = "{.col}_{.fn}"))
}

class_summary <- summarise_with_ci(class_df, c("C", "BalancedAccuracy", "Sensitivity", "Specificity", "PPV", "NPV", "Precision", "Recall", "F1"))
calib_summary <- summarise_with_ci(calib_df, c("Intercept", "Slope", "Brier", "ICI"))

prep_long_summary <- function(df, metric_names) {
  out <- tibble(Metric = metric_names, Mean = NA_real_, CI_95 = NA_character_)
  for (k in seq_along(metric_names)) {
    m <- metric_names[k]
    out$Mean[k]  <- as.numeric(df[[paste0(m, "_mean")]])
    lo <- as.numeric(df[[paste0(m, "_ci_low")]])
    hi <- as.numeric(df[[paste0(m, "_ci_high")]])
    out$CI_95[k] <- sprintf("(%.3f–%.3f)", lo, hi)
  }
  out
}

class_summary_long <- prep_long_summary(class_summary, c("C", "BalancedAccuracy", "Sensitivity", "Specificity", "PPV", "NPV", "Precision", "Recall", "F1"))
calib_summary_long <- prep_long_summary(calib_summary, c("Intercept", "Slope", "Brier", "ICI"))

#-------------------------------------
# BOOTSTRAPPED 95%CI
#-------------------------------------
# ============================================================
# Bootstrap confidence intervals using subject-level resampling
# ============================================================

library(dplyr)
library(pROC)
library(caret)
library(writexl)

# collapse repeated predictions per subject
boot_df <- results_PPS$preds %>%
  group_by(subject_index) %>%
  summarise(
    obs  = first(obs),
    pred = mean(pred),
    .groups = "drop"
  )

set.seed(123)

B <- 2000
n <- nrow(boot_df)

# storage
C_boot  <- numeric(B)
BA_boot <- numeric(B)
SEN_boot <- numeric(B)
SPEC_boot <- numeric(B)
PPV_boot <- numeric(B)
NPV_boot <- numeric(B)

Intercept_boot <- numeric(B)
Slope_boot <- numeric(B)
Brier_boot <- numeric(B)
ICI_boot <- numeric(B)

for (b in seq_len(B)) {
  
  idx <- sample(seq_len(n), size = n, replace = TRUE)
  d <- boot_df[idx, ]
  
  # ---------- C-index ----------
  C_boot[b] <- as.numeric(pROC::roc(d$obs, d$pred, quiet = TRUE)$auc)
  
  # ---------- Classification metrics ----------
  pred_class <- ifelse(d$pred >= 0.5, 1, 0)
  
  cm <- caret::confusionMatrix(
    factor(pred_class, levels = c(0,1)),
    factor(d$obs, levels = c(0,1)),
    positive = "1"
  )
  
  BA_boot[b]   <- cm$byClass["Balanced Accuracy"]
  SEN_boot[b]  <- cm$byClass["Sensitivity"]
  SPEC_boot[b] <- cm$byClass["Specificity"]
  PPV_boot[b]  <- cm$byClass["Pos Pred Value"]
  NPV_boot[b]  <- cm$byClass["Neg Pred Value"]
  
  # ---------- Calibration ----------
  
  p <- pmin(pmax(d$pred, 1e-6), 1 - 1e-6)
  y <- d$obs
  boot_cal <- predRupdate::pred_val_probs(
    binary_outcome = y,
    Prob = p,
    cal_plot = FALSE
  )
  
  Intercept_boot[b] <- boot_cal$CalInt
  Slope_boot[b]     <- boot_cal$CalSlope
  
  Brier_boot[b] <- mean((y - p)^2)
  
  ICI_boot[b] <- ici_stat(y, p)
}

# helper function
boot_summary <- function(x){
  q <- quantile(x, c(0.025, 0.975), na.rm = TRUE)
  c(
    Mean = mean(x, na.rm = TRUE),
    CI_low = unname(q[1]),
    CI_high = unname(q[2])
  )
}

# classification metrics
class_boot_summary <- rbind(
  C = boot_summary(C_boot),
  BalancedAccuracy = boot_summary(BA_boot),
  Sensitivity = boot_summary(SEN_boot),
  Specificity = boot_summary(SPEC_boot),
  PPV = boot_summary(PPV_boot),
  NPV = boot_summary(NPV_boot)
)

# calibration metrics
calib_boot_summary <- rbind(
  Intercept = boot_summary(Intercept_boot),
  Slope = boot_summary(Slope_boot),
  Brier = boot_summary(Brier_boot),
  ICI = boot_summary(ICI_boot)
)

class_boot_summary <- as.data.frame(class_boot_summary)
colnames(class_boot_summary) <- c("Mean","CI_low","CI_high")
class_boot_summary$Metric <- rownames(class_boot_summary)
rownames(class_boot_summary) <- NULL

calib_boot_summary <- as.data.frame(calib_boot_summary)
colnames(calib_boot_summary) <- c("Mean","CI_low","CI_high")
calib_boot_summary$Metric <- rownames(calib_boot_summary)
rownames(calib_boot_summary) <- NULL

# reorder columns
class_boot_summary <- class_boot_summary %>%
  select(Metric, Mean, CI_low, CI_high)

calib_boot_summary <- calib_boot_summary %>%
  select(Metric, Mean, CI_low, CI_high)

# ============================================================
# Export new Excel file
# ============================================================

excel_path_boot <- "PPS_RNCV_AMPSCZ_results_bootstrap.xlsx"

write_xlsx(
  list(
    "PerSplit_Classification" = results_PPS$c_stat_nested,
    "PerSplit_Calibration" = results_PPS$calibration_slopes,
    "Bootstrap_Summary_Classification" = class_boot_summary,
    "Bootstrap_Summary_Calibration" = calib_boot_summary,
    "Dropped_Variables_Log" = results_PPS$dropped_vars_log
  ),
  path = excel_path_boot
)

message("Exported: ", normalizePath(excel_path_boot))


# ============================================================
# SUBGROUP PERFORMANCE: REFINED PPS MODEL
# Using aggregated out-of-fold predictions from repeated nested CV
# ============================================================

# # Join subgroup variables to subject-level aggregated RN-CV predictions
# subgroup_df_refined <- boot_df %>%
#   left_join(
#     df %>%
#       select(subject_index, gender, ethnicity),
#     by = "subject_index"
#   ) %>%
#   mutate(
#     gender = as.character(gender),
#     ethnicity = as.character(ethnicity)
#   )
# 
# # Define subgroup levels from the data
# gender_levels_refined <- subgroup_df_refined %>%
#   filter(!is.na(gender)) %>%
#   distinct(gender) %>%
#   arrange(gender) %>%
#   pull(gender)
# 
# ethnicity_levels_refined <- subgroup_df_refined %>%
#   filter(!is.na(ethnicity)) %>%
#   distinct(ethnicity) %>%
#   arrange(ethnicity) %>%
#   pull(ethnicity)
# 
# # Helper function for one metric calculation
# compute_metrics_once <- function(d, threshold = 0.5) {
#   
#   # Return NA if only one outcome class is present
#   if (length(unique(d$obs)) < 2) {
#     return(tibble(
#       C = NA_real_,
#       BalancedAccuracy = NA_real_,
#       Sensitivity = NA_real_,
#       Specificity = NA_real_,
#       PPV = NA_real_,
#       NPV = NA_real_,
#       Intercept = NA_real_,
#       Slope = NA_real_,
#       Brier = NA_real_,
#       ICI = NA_real_
#     ))
#   }
#   
#   # C-index / AUC
#   C_val <- tryCatch(
#     as.numeric(pROC::roc(d$obs, d$pred, quiet = TRUE)$auc),
#     error = function(e) NA_real_
#   )
#   
#   # Classification metrics
#   pred_class <- ifelse(d$pred >= threshold, 1, 0)
#   
#   cm <- tryCatch(
#     caret::confusionMatrix(
#       factor(pred_class, levels = c(0, 1)),
#       factor(d$obs, levels = c(0, 1)),
#       positive = "1"
#     ),
#     error = function(e) NULL
#   )
#   
#   if (is.null(cm)) {
#     BA_val <- SEN_val <- SPEC_val <- PPV_val <- NPV_val <- NA_real_
#   } else {
#     BA_val   <- as.numeric(cm$byClass["Balanced Accuracy"])
#     SEN_val  <- as.numeric(cm$byClass["Sensitivity"])
#     SPEC_val <- as.numeric(cm$byClass["Specificity"])
#     PPV_val  <- as.numeric(cm$byClass["Pos Pred Value"])
#     NPV_val  <- as.numeric(cm$byClass["Neg Pred Value"])
#   }
#   
#   # Calibration metrics
#   p <- pmin(pmax(d$pred, 1e-6), 1 - 1e-6)
#   y <- d$obs
#   
#   cal <- tryCatch(
#     predRupdate::pred_val_probs(
#       binary_outcome = y,
#       Prob = p,
#       cal_plot = FALSE
#     ),
#     error = function(e) NULL
#   )
#   
#   if (is.null(cal)) {
#     Int_val <- Slope_val <- NA_real_
#   } else {
#     Int_val   <- cal$CalInt
#     Slope_val <- cal$CalSlope
#   }
#   
#   Brier_val <- mean((y - p)^2)
#   
#   ICI_val <- tryCatch(
#     ici_stat(y, p),
#     error = function(e) NA_real_
#   )
#   
#   tibble(
#     C = C_val,
#     BalancedAccuracy = BA_val,
#     Sensitivity = SEN_val,
#     Specificity = SPEC_val,
#     PPV = PPV_val,
#     NPV = NPV_val,
#     Intercept = Int_val,
#     Slope = Slope_val,
#     Brier = Brier_val,
#     ICI = ICI_val
#   )
# }
# 
# # Helper function for bootstrapped subgroup performance
# compute_subgroup_boot <- function(data, subgroup_var, subgroup_value,
#                                   model_label = "Refined PPS model",
#                                   threshold = 0.5,
#                                   B = 2000) {
#   
#   d <- data %>%
#     filter(.data[[subgroup_var]] == subgroup_value) %>%
#     filter(!is.na(obs), !is.na(pred))
#   
#   n_sub   <- nrow(d)
#   n_chr   <- sum(d$obs == 1, na.rm = TRUE)
#   n_ctrl  <- sum(d$obs == 0, na.rm = TRUE)
#   
#   # If subgroup does not contain both CHR and controls, return empty estimates
#   if (length(unique(d$obs)) < 2) {
#     return(tibble(
#       Model = model_label,
#       Subgroup_variable = subgroup_var,
#       Subgroup = subgroup_value,
#       N = n_sub,
#       N_CHR = n_chr,
#       N_control = n_ctrl,
#       Metric = c("C", "BalancedAccuracy", "Sensitivity", "Specificity",
#                  "PPV", "NPV", "Intercept", "Slope", "Brier", "ICI"),
#       Estimate = NA_real_,
#       Lower_95CI = NA_real_,
#       Upper_95CI = NA_real_
#     ))
#   }
#   
#   # Point estimates in the original subgroup
#   point_est <- compute_metrics_once(d, threshold = threshold)
#   
#   # Bootstrap CIs by resampling participants within subgroup
#   boot_mat <- vector("list", B)
#   
#   for (b in seq_len(B)) {
#     idx <- sample(seq_len(n_sub), size = n_sub, replace = TRUE)
#     db <- d[idx, ]
#     boot_mat[[b]] <- compute_metrics_once(db, threshold = threshold)
#   }
#   
#   boot_df_sub <- bind_rows(boot_mat)
#   
#   metrics <- names(point_est)
#   
#   tibble(
#     Model = model_label,
#     Subgroup_variable = subgroup_var,
#     Subgroup = subgroup_value,
#     N = n_sub,
#     N_CHR = n_chr,
#     N_control = n_ctrl,
#     Metric = metrics,
#     Estimate = as.numeric(point_est[1, metrics]),
#     Lower_95CI = sapply(metrics, function(m) quantile(boot_df_sub[[m]], 0.025, na.rm = TRUE)),
#     Upper_95CI = sapply(metrics, function(m) quantile(boot_df_sub[[m]], 0.975, na.rm = TRUE))
#   )
# }
# 
# # Run subgroup analyses
# set.seed(123)
# 
# B_sub <- 2000
# 
# subgroup_refined_gender <- map_dfr(
#   gender_levels_refined,
#   ~ compute_subgroup_boot(
#     data = subgroup_df_refined,
#     subgroup_var = "gender",
#     subgroup_value = .x,
#     model_label = "Refined PPS model",
#     threshold = 0.5,
#     B = B_sub
#   )
# )
# 
# subgroup_refined_ethnicity <- map_dfr(
#   ethnicity_levels_refined,
#   ~ compute_subgroup_boot(
#     data = subgroup_df_refined,
#     subgroup_var = "ethnicity",
#     subgroup_value = .x,
#     model_label = "Refined PPS model",
#     threshold = 0.5,
#     B = B_sub
#   )
# )
# 
# subgroup_performance_refined <- bind_rows(
#   subgroup_refined_gender,
#   subgroup_refined_ethnicity
# )
# 
# # Export subgroup results
# write_xlsx(
#   list(
#     "Refined_Subgroup_Performance" = subgroup_performance_refined
#   ),
#   path = "subgroup_performance_refined_PPS.xlsx"
# )
# 
# message("Exported: subgroup_performance_refined_PPS.xlsx")


#--------------------------
#### Harrell's C final plot (BOOTSTRAP CI + 50 CV points)
#--------------------------

library(ggplot2)

# extract the 50 CV estimates
c_df <- results_PPS$c_stat_nested %>%
  dplyr::transmute(C = C_test) %>%
  mutate(Model = "RNCV")

# bootstrap CI summary
c_boot_summary <- data.frame(
  Model = "RNCV",
  Mean  = mean(C_boot),
  Lower = unname(quantile(C_boot, 0.025)),
  Upper = unname(quantile(C_boot, 0.975))
)

p_cindex_boot <- ggplot() +
  
  # 50 CV points
  geom_jitter(
    data = c_df,
    aes(x = Model, y = C),
    width = 0.05,
    alpha = 0.5,
    color = "steelblue"
  ) +
  
  # bootstrap mean
  geom_point(
    data = c_boot_summary,
    aes(x = Model, y = Mean),
    color = "darkorange",
    size = 4
  ) +
  
  # bootstrap CI
  geom_errorbar(
    data = c_boot_summary,
    aes(x = Model, ymin = Lower, ymax = Upper),
    width = 0.1,
    color = "black",
    linewidth = 1
  ) +
  
  labs(
    y = "Mean and 95% CI",
    x = "Harrell's C",
    title = expression("Refined PPS model"^c)
  ) +
  
  coord_cartesian(ylim = c(0.65, 0.95)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_blank()
  )

ggsave("Cindex_RNCV_AMPSCZ_bootstrap.png", p_cindex_boot, width = 5, height = 5, dpi = 300)