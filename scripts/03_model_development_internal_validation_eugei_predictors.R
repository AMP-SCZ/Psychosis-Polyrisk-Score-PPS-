# Set language to English for messages and warnings
Sys.setenv(LANG = "en")

########### AMP-SCZ DATASET PREPARATION ###################

library(tidyverse)   # For data wrangling
library(readr)
library(dplyr)
library(missForest)  # For imputation

# Set print options to display all columns and rows (careful with huge tibbles)
options(tibble.width = Inf)      # Show all columns
options(dplyr.print_max = Inf)   # Show all rows


# ------------------------------------------------------------------------------
# 1) LOAD BASE (LEFT) DATA: SOCIODEM_baseline
#    This is now the reference dataset; everything else will be left-joined to it
# ------------------------------------------------------------------------------

socdem <- read_delim(
  "datasets/SOCIODEM_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)

# Derive gender within SOCIODEM (same logic as before)
socdem <- socdem %>%
  mutate(
    gender = case_when(
      chrdemo_gender_identity___1 == 1 ~ "Male",
      chrdemo_gender_identity___2 == 1 ~ "Female",
      chrdemo_gender_identity___4 == 1 ~ "Non-binary",
      chrdemo_gender_identity____99 == 1 | chrdemo_gender_identity___99 == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  )

# Derive ethnicity within SOCIODEM (same logic as before)
socdem <- socdem %>%
  mutate(
    ethnicity = case_when(
      chrdemo_racial_back___7 == 1 &
        rowSums(across(c(chrdemo_racial_back___1, chrdemo_racial_back___2, chrdemo_racial_back___3,
                         chrdemo_racial_back___4, chrdemo_racial_back___5, chrdemo_racial_back___6,
                         chrdemo_racial_back___8), ~ . == 1), na.rm = TRUE) == 0 ~ "White",
      chrdemo_racial_back___5 == 1 &
        rowSums(across(c(chrdemo_racial_back___1, chrdemo_racial_back___2, chrdemo_racial_back___3,
                         chrdemo_racial_back___4, chrdemo_racial_back___6, chrdemo_racial_back___7,
                         chrdemo_racial_back___8), ~ . == 1), na.rm = TRUE) == 0 ~ "Black",
      rowSums(across(c(chrdemo_racial_back___2, chrdemo_racial_back___3,
                       chrdemo_racial_back___4, chrdemo_racial_back___6), ~ . == 1), na.rm = TRUE) == 1 &
        rowSums(across(c(chrdemo_racial_back___1, chrdemo_racial_back___5,
                         chrdemo_racial_back___7, chrdemo_racial_back___8), ~ . == 1), na.rm = TRUE) == 0 ~ "Asian",
      rowSums(across(c(chrdemo_racial_back___1, chrdemo_racial_back___2, chrdemo_racial_back___3,
                       chrdemo_racial_back___4, chrdemo_racial_back___5, chrdemo_racial_back___6,
                       chrdemo_racial_back___7, chrdemo_racial_back___8), ~ . == 1), na.rm = TRUE) > 1 ~ "Mixed",
      (chrdemo_racial_back___1 == 1 | chrdemo_racial_back___8 == 1) &
        rowSums(across(c(chrdemo_racial_back___2, chrdemo_racial_back___3, chrdemo_racial_back___4,
                         chrdemo_racial_back___5, chrdemo_racial_back___6, chrdemo_racial_back___7), ~ . == 1), na.rm = TRUE) == 0 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    ethnicity = case_when(
      ethnicity %in% c("White", "Black") ~ ethnicity,
      ethnicity %in% c("Asian", "Mixed", "Other") ~ "Other",
      TRUE ~ NA_character_
    ),
    ethnicity = factor(ethnicity, levels = c("White", "Black", "Other"))
  )

# First/second generation migrant flags (within SOCIODEM)
socdem <- socdem %>%
  mutate(
    first_gen_migrant = case_when(
      chrdemo_country_situation %in% c(2,3) &
        !(chrdemo_cob %in% c(3,52,99,118,180)) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    second_gen_migrant = case_when(
      chrdemo_country_situation == 4 &
        !(chrdemo_cob %in% c(3,52,99,118,180)) ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    )
  )

# Merge CHR and HC age variables (convert from months to years)
socdem <- socdem %>%
  mutate(
    age = coalesce(chrdemo_age_mos_chr, chrdemo_age_mos_hc) / 12
  )

# ------------------------------------------------------------------------------
# 2) LOAD PPS_baseline AND DERIVE PPS VARIABLES
#    We compute everything exactly as before, then left-join into SOCIODEM.
# ------------------------------------------------------------------------------

PPS_PRONET <- read_delim(
  "datasets/PPS_baseline.csv",
  delim = ";",
  na = c("-3", "-300", "900", "-900")
)

# Create participant & father "age" variables (same as before)
PPS_PRONET$dob <- round(PPS_PRONET$interview_age / 12, 8)      # participant_age (years)
PPS_PRONET$dob_father <- round(PPS_PRONET$chrpps_fage, 8)      # father_age (years)

# Reverse-code CTQ items (same columns)
PPS_PRONET$chrpps_loved_rev   <- 6 - PPS_PRONET$chrpps_loved
PPS_PRONET$chrpps_special_rev <- 6 - PPS_PRONET$chrpps_special
PPS_PRONET$chrpps_care_rev    <- 6 - PPS_PRONET$chrpps_care
PPS_PRONET$chrpps_closefam_rev<- 6 - PPS_PRONET$chrpps_closefam
PPS_PRONET$chrpps_support_rev <- 6 - PPS_PRONET$chrpps_support
PPS_PRONET$chrpps_protect_rev <- 6 - PPS_PRONET$chrpps_protect
PPS_PRONET$chrpps_docr_rev    <- 6 - PPS_PRONET$chrpps_docr

# Reverse-code TEPS items (same index selection as before)
PPS_PRONET[, c(188, 189, 191:205)] <- 7 - PPS_PRONET[, c(188, 189, 191:205)]

# Handedness: NA only if all four are NA; else 0/1 by mean threshold
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    handedness = case_when(
      is.na(rowMeans(PPS_PRONET[, c("chrpps_writing","chrpps_throwing","chrpps_toothbrush","chrpps_spoon")], na.rm = TRUE)) ~ NA_real_,
      rowMeans(PPS_PRONET[, c("chrpps_writing","chrpps_throwing","chrpps_toothbrush","chrpps_spoon")], na.rm = TRUE) > 3 ~ 0,
      TRUE ~ 1
    )
  )

# Compute paternal age 
PPS_PRONET <- PPS_PRONET %>%
  rename(participant_age = dob,
         father_age = dob_father) %>%
  mutate(
    paternal_age = father_age - participant_age)

# Adult life events (same logic)
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    adult_life_events = case_when(
      if_all(starts_with("chrpps_sixmo___"), is.na) ~ NA_real_,
      chrpps_sixmo___15 == 1 ~ 0,
      TRUE ~ 1
    )
  )

# Childhood trauma (same logic)
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
      if_all(all_of(ct_cols), is.na) ~ NA_real_,      # all missing
      childhood_trauma_sum > 0       ~ childhood_trauma_sum,
      TRUE                           ~ 0
    )
  ) %>%
  select(-childhood_trauma_sum)


# Anhedonia (same logic)
PPS_PRONET <- PPS_PRONET %>%
  mutate(
    anhedonia_sum = rowSums(across(188:205), na.rm = TRUE),
    anhedonia = case_when(
      if_all(188:205, is.na) ~ NA_real_,
      anhedonia_sum > 0      ~ anhedonia_sum,
      TRUE                   ~ 0
    )
  ) %>% select(-anhedonia_sum)


# ------------------------------------------------------------------------------
# 3) FIGS derived variables
# ------------------------------------------------------------------------------
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
  select(
    subjectkey, src_subject_id,
    chrfigs_fam_ddx, chrfigs_fam_mdx, chrfigs_fam_pdx, chrfigs_fam_apdx, chrfigs_fam_napdx
  )

tri_pair <- function(a, b) {
  vals <- c(a, b)
  if (all(is.na(vals))) return(NA_integer_)
  if (any(vals %in% 1:3, na.rm = TRUE)) return(1L)
  0L
}

# Safe nth for older dplyr
nth2 <- function(x, n) if (length(x) >= n) x[n] else NA_integer_

# Four FIGS flags + combined parent_smi (NA only if all four flags are NA)
data_figs_simpl <- data_figs_simpl %>%
  arrange(src_subject_id) %>%
  group_by(src_subject_id) %>%
  summarise(
    mother_psychosis            = tri_pair(nth2(chrfigs_fam_pdx, 1), nth2(chrfigs_fam_apdx, 1)),
    mother_other_mental_illness = tri_pair(nth2(chrfigs_fam_ddx, 1), nth2(chrfigs_fam_mdx, 1)),
    father_psychosis            = tri_pair(nth2(chrfigs_fam_pdx, 2), nth2(chrfigs_fam_apdx, 2)),
    father_other_mental_illness = tri_pair(nth2(chrfigs_fam_ddx, 2), nth2(chrfigs_fam_mdx, 2)),
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
  ) %>%
  select(src_subject_id, mother_psychosis, mother_other_mental_illness,
         father_psychosis, father_other_mental_illness, parent_smi)

# ------------------------------------------------------------------------------
# 3) ASSIST derived variables  (KEEP NA if no ASSIST row)
# ------------------------------------------------------------------------------
library(readr)  # for parse_number

assist <- read_delim(
  "datasets/ASSIST_baseline.csv",
  delim = ";",
  na = c("-3","-300","900","-900"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    chrassist_whoassist_often1 = parse_number(as.character(chrassist_whoassist_often1)),
    chrassist_whoassist_often3 = parse_number(as.character(chrassist_whoassist_often3)),
    tobacco_daily      = as.integer(coalesce(chrassist_whoassist_often1 == 6, FALSE)),
    heavy_cannabis_use = as.integer(coalesce(chrassist_whoassist_often3 %in% c(4,6), FALSE))
  ) %>%
  select(src_subject_id, tobacco_daily, heavy_cannabis_use)

# ------------------------------------------------------------------------------
# 4) PPS-derived mini table (PPS only; do NOT merge FIGS/ASSIST here)
# ------------------------------------------------------------------------------
pps_derived <- PPS_PRONET %>%
  select(
    src_subject_id,
    participant_age, father_age, paternal_age, handedness,
    adult_life_events, childhood_trauma, anhedonia,
    chrpps_fcob
  )

# ------------------------------------------------------------------------------
# 5) SOCIODEM-anchored joins
# ------------------------------------------------------------------------------
# SOCIODEM + PPS
df <- socdem %>%
  left_join(pps_derived, by = "src_subject_id")

# Join FIGS + ASSIST directly to SOCIODEM-anchored df (keep NA if no row)
df <- df %>%
  left_join(data_figs_simpl, by = "src_subject_id") %>%
  left_join(assist, by = "src_subject_id")


# First and second generation migrant variables
df <- df %>%
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
    
    # General migrants excluding North Africa
    first_gen_migrant = case_when(
      chrdemo_country_situation %in% c(2,3) &
        north_african_first_gen_migrant == 0 ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    ),
    
    second_gen_migrant = case_when(
      chrdemo_country_situation == 4 &
        north_african_second_gen_migrant == 0 ~ 1,
      is.na(chrdemo_country_situation) ~ NA_real_,
      TRUE ~ 0
    )
    
  )

# Parental SES (min-of-two rule; NA only if both missing)
df <- df %>%
  mutate(
    parent_ses = case_when(
      is.na(chrdemo_parent_occupation) & is.na(chrdemo_parent_occupation2) ~ NA_real_,
      TRUE ~ if_else(
        pmin(chrdemo_parent_occupation, chrdemo_parent_occupation2, na.rm = TRUE) <= 6, 0, 1
      )
    )
  )

# 10) CHR vs HC (INCEXC)
data_incexc <- read_delim("datasets/INCEXC_criteria.csv", delim = ";", na = c("-3", "-300", "900", "-900"))

df <- df %>%
  left_join(data_incexc %>% select(src_subject_id, chrcrit_part), by = "src_subject_id") %>%
  rename(chr_status = chrcrit_part) %>%
  mutate(chr_status = if_else(chr_status == 2, 0, chr_status))

# ------------------------------------------------------------------------------
# 6) Clean table 
# ------------------------------------------------------------------------------
PPS_PRONET_clean_oldpred <- df %>%
  transmute(
    src_subject_id,
    age,
    gender,
    ethnicity,
    first_gen_migrant,
    second_gen_migrant,
    north_african_first_gen_migrant,
    north_african_second_gen_migrant,
    handedness,
    parent_smi,
    adult_life_events,
    tobacco_daily,
    heavy_cannabis_use,
    childhood_trauma,
    anhedonia,
    parent_ses,
    chr_status
  )

# inspect missingness
missing_summary <- PPS_PRONET_clean_oldpred %>%
  select(-src_subject_id) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingPercent") %>%
  arrange(desc(MissingPercent))

write_xlsx(missing_summary, "missigness_RNCV_EUGEI.xlsx")

print(missing_summary)



write_csv(PPS_PRONET_clean_oldpred, "PPS_PRONET_clean_oldpred.csv")



#### REPEATED NESTED CROSS-VALIDATION (with DCA) ####

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
  library(tidyverse) # wrangling
  library(caret)     # resampling, metrics, nearZeroVar, SMOTE hook
  library(glmnet)    # LASSO logistic regression
  library(predRupdate)# calibration
  library(pROC)      # ROC/AUC
  library(missForest)# random-forest imputation
  library(Hmisc)     # rcorr.cens (C-index)
  library(writexl)   # export CV results
  library(dcurves)   # decision curve analysis
  library(openxlsx)  # DCA tables
})

data_path <- "PRONET_PPS_Processing/PPS_PRONET_clean_oldpred.csv" # change if needed

# Repeated nested CV design
outerFolds   <- 5 
outerRepeats <- 10 
innerFolds   <- 5
innerRepeats <- 10 

set.seed(231) # reproducibility
lambda_grid <- seq(0.001, 0.1, length.out = 100)
use_smote   <- FALSE  # EU-GEI used SMOTE; disable for speed if desired

# -------------------------
# 1) Load data
# -------------------------
# EU locales: ";" separator, "," decimal mark -> use read.csv2
PPS_PRONET_clean_oldpred <- read.csv2(data_path, stringsAsFactors = FALSE, dec = ",")

# -------------------------
# 2) Preprocess variable classes (global type hygiene; no leakage)
# -------------------------

# 2.1 IDs to drop
id_cols <- intersect(c("src_subject_id","subjectkey","subject_id","id"),
                     names(PPS_PRONET_clean_oldpred))
if (length(id_cols)) PPS_PRONET_clean_oldpred[id_cols] <- list(NULL)

# 2.2 Outcome factor with positive class "CHR"
PPS_PRONET_clean_oldpred$chr <- factor(
  PPS_PRONET_clean_oldpred$chr_status,
  levels = c(0, 1),
  labels = c("HC", "CHR")
)

# 2.3 Type standardisation
bin_vars <- c(
  "first_gen_migrant","second_gen_migrant", "north_african_first_gen_migrant",
  "north_african_second_gen_migrant", "handedness",
  "parent_ses","parent_smi",
  "adult_life_events","tobacco_daily","heavy_cannabis_use"
)
cat_vars <- c("gender","ethnicity")
num_vars <- character(0)

for (v in intersect(bin_vars, names(PPS_PRONET_clean_oldpred))) {
  PPS_PRONET_clean_oldpred[[v]] <- factor(PPS_PRONET_clean_oldpred[[v]], levels = c(0,1))
}
for (v in intersect(cat_vars, names(PPS_PRONET_clean_oldpred))) {
  PPS_PRONET_clean_oldpred[[v]] <- as.factor(PPS_PRONET_clean_oldpred[[v]])
}
PPS_PRONET_clean_oldpred$ethnicity <- relevel(
  PPS_PRONET_clean_oldpred$ethnicity,
  ref = "White"
)
for (v in intersect(num_vars, names(PPS_PRONET_clean_oldpred))) {
  PPS_PRONET_clean_oldpred[[v]] <- suppressWarnings(as.numeric(PPS_PRONET_clean_oldpred[[v]]))
}

# --- Combined gender–age predictor: Male aged 25–35 = 1, else 0
PPS_PRONET_clean_oldpred$male_25_35 <- ifelse(
  PPS_PRONET_clean_oldpred$gender == "Male" &
    PPS_PRONET_clean_oldpred$age >= 25 &
    PPS_PRONET_clean_oldpred$age <= 35,
  1, 0
)

PPS_PRONET_clean_oldpred$male_25_35 <- factor(
  PPS_PRONET_clean_oldpred$male_25_35,
  levels = c(0, 1)
)

# 2.4 Build modeling frame (predictors + outcome)
outcome_var <- "chr"
pred_vars   <- setdiff(names(PPS_PRONET_clean_oldpred), c("chr_status", outcome_var, "age", "gender"))
df <- PPS_PRONET_clean_oldpred[, c(pred_vars, outcome_var), drop = FALSE]
df[[outcome_var]] <- factor(df[[outcome_var]], levels = c("HC","CHR"))
# ---- create stable subject ID for bootstrap ----
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
      
      # Split predictors / outcome for TRAIN/TEST
      y_train_vec  <- train_base[[outcome_var]]
      X_train_base <- train_base[, setdiff(names(train_base), c(outcome_var,"subject_index")), drop = FALSE]
      
      y_test_vec   <- test_base[[outcome_var]]
      X_test_base  <- test_base[, setdiff(names(test_base), c(outcome_var,"subject_index")), drop = FALSE]
      
      # Impute TRAIN predictors
      imp_train_mf <- missForest(X_train_base)
      X_train_imp  <- as.data.frame(imp_train_mf$ximp)
      # Impute TEST predictors (separately — non-leaky)
      imp_test_mf  <- missForest(X_test_base)
      X_test_imp   <- as.data.frame(imp_test_mf$ximp)
      
      # Bind back the unchanged outcomes
      train_imp_once <- cbind(X_train_imp, setNames(data.frame(y_train_vec), outcome_var))
      test_imp       <- cbind(X_test_imp,  setNames(data.frame(y_test_vec),  outcome_var))
      
      # ------------------------------------------------------------------------------
      # Binarize childhood trauma and anhedonia (EU-GEI cutoffs)
      # ------------------------------------------------------------------------------
      
      train_imp_once$childhood_trauma <- ifelse(train_imp_once$childhood_trauma > 55, 1, 0)
      train_imp_once$anhedonia        <- ifelse(train_imp_once$anhedonia > 35, 1, 0)
      
      test_imp$childhood_trauma <- ifelse(test_imp$childhood_trauma > 55, 1, 0)
      test_imp$anhedonia        <- ifelse(test_imp$anhedonia > 35, 1, 0)

      
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
      
      # Aggregate λ across inner repeats (EU-GEI averaged best λ)
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
      prob_test <- pmin(pmax(prob_test, 1e-6), 1 - 1e-6)  # clip
      y_num     <- as.integer(y_test == levels(y_test)[2]) # 0/1
      
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
          subject_index = test_base$subject_index,
          obs         = y_num,
          pred        = prob_test
        )
      )
    } # end outer folds
  }   # end outer repeats
  
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
# 6) Decision Curve Analysis (DCA) from RN-CV results (individual-level)
# -------------------------


# Ensure predictions and observed values exist
if (!"preds" %in% names(results_PPS) || nrow(results_PPS$preds) == 0) {
  stop("results_PPS$preds is empty. Re-run RN-CV with preds storage enabled.")
}


# Extract outer-test predictions
pred_df <- results_PPS$preds %>%
  group_by(subject_index) %>%
  summarise(
    obs  = first(obs),
    pred = mean(pred),
    .groups = "drop"
  ) %>%
  filter(!is.na(obs) & !is.na(pred))

# -------------------------
# 🔍 Diagnostic: predicted probabilities used in DCA (+ CLIBRATION PLOT)
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
    title = expression("Retrained PPS original model"^b),
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

# Export plot
ggsave("Calibration_LOESS_RNCV_EUGEI.png", p_calib_loess, width = 7, height = 6, dpi = 300)
message("✅ Exported: Calibration_LOESS_RNCV_EUGEI_png")


# DCA computation
compute_dca <- function(obs, pred, thresholds, prevalence) {
  prev <- if (is.null(prevalence)) mean(obs == 1, na.rm = TRUE) else prevalence
  dca_tbl <- dcurves::dca(
    obs ~ pred,
    data = tibble(obs = obs, pred = pred),
    thresholds = thresholds,
    prevalence = prev
  ) %>% as_tibble()
  
  
  pps <- dca_tbl %>% filter(label == "pred") %>%
    transmute(label = "PPS", threshold, net_benefit)
  
  
  baseline <- tibble(
    label = rep(c("Treat None", "Treat All"), each = length(thresholds)),
    threshold = rep(thresholds, 2),
    net_benefit = c(
      rep(0, length(thresholds)),
      prev - (1 - prev) * (thresholds / (1 - thresholds))
    )
  )
  
  
  bind_rows(baseline, pps) %>%
    mutate(std_net_benefit = net_benefit / prev)
}


# Clinical and general thresholds
thr_clinical <- seq(0, 0.50, by = 0.005)
thr_general <- seq(0, 0.03, by = 0.001)


# Run DCA
clinical_dca <- compute_dca(pred_df$obs, pred_df$pred, thr_clinical, prevalence = 0.192)
general_dca <- compute_dca(pred_df$obs, pred_df$pred, thr_general, prevalence = 0.02)


# Plotting helpers
plot_dca <- function(dca_tbl, title) {
  ggplot(dca_tbl, aes(x = threshold, y = net_benefit, color = label)) +
    geom_line(linewidth = 1) +
    labs(title = title, x = "Threshold Probability", y = "Net Benefit", color = "Strategy") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    scale_color_manual(values = c("Treat None" = "black", "Treat All" = "gray50", "PPS" = "steelblue"))
}


# Plot and save
p_clinical <- plot_dca(clinical_dca, "DCA — Clinical Population") +
  coord_cartesian(ylim = c(0, 0.10))


p_general <- plot_dca(general_dca, "DCA — General Population") +
  coord_cartesian(ylim = c(-0.005, 0.02))


ggsave("DCA_RNCV_clinical.png", p_clinical, width = 7, height = 5, dpi = 300)
ggsave("DCA_RNCV_general.png", p_general, width = 7, height = 5, dpi = 300)


# Save to Excel
wb <- createWorkbook()
addWorksheet(wb, "Clinical")
addWorksheet(wb, "General")
writeData(wb, sheet = "Clinical", clinical_dca)
writeData(wb, sheet = "General", general_dca)
saveWorkbook(wb, "DCA_RNCV_tables_EUGEI.xlsx", overwrite = TRUE)


message("Exported: DCA_RNCV_clinical.png, DCA_RNCV_general.png, DCA_RNCV_tables_EUGEI.xlsx")

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
    cal_plot = FALSE)
  
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

excel_path_boot <- "PPS_RNCV_EUGEI_results_bootstrap.xlsx"

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


#--------------------------
#### Harrell's C final plot (BOOTSTRAP CI + 50 CV points)
#--------------------------

# extract CV estimates
c_df <- results_PPS$c_stat_nested %>%
  dplyr::transmute(C = C_test) %>%
  mutate(Model = "RNCV")

# bootstrap CI
c_boot_summary <- data.frame(
  Model = "RNCV",
  Mean  = mean(C_boot),
  Lower = unname(quantile(C_boot, 0.025)),
  Upper = unname(quantile(C_boot, 0.975))
)

p_cindex_boot <- ggplot() +
  
  geom_jitter(
    data = c_df,
    aes(x = Model, y = C),
    width = 0.05,
    alpha = 0.5,
    color = "steelblue"
  ) +
  
  geom_point(
    data = c_boot_summary,
    aes(x = Model, y = Mean),
    color = "darkorange",
    size = 4
  ) +
  
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
    title = expression("Retrained PPS original model"^b)
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

ggsave("Cindex_RNCV_EUGEI_bootstrap.png", p_cindex_boot, width = 5, height = 5, dpi = 300)