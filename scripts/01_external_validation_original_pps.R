# Set language to English for messages and warnings
Sys.setenv(LANG = "en")

########### AMP-SCZ DATASET PREPARATION ###################

library(tidyverse)   # For data wrangling
library(readr)
library(dplyr)
library(missForest)  # For imputation
library(writexl)

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
    heavy_cannabis_use = as.integer(coalesce(chrassist_whoassist_often3 %in% c(4, 6), FALSE))
  ) %>%
  select(src_subject_id, tobacco_daily, heavy_cannabis_use)

# ------------------------------------------------------------------------------
# 4) PPS-derived mini table (PPS only; do NOT merge FIGS/ASSIST here)
# ------------------------------------------------------------------------------
pps_derived <- PPS_PRONET %>%
  select(
    src_subject_id,
    participant_age, father_age, paternal_age,
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

# Migrant indicators (North Africa and non)
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

# ------------------------------------------------------------------------------
# 6) Clean table 
# ------------------------------------------------------------------------------
PPS_PRONET_clean <- df %>%
  transmute(
    src_subject_id,
    age,
    gender,
    ethnicity,
    first_gen_migrant,
    second_gen_migrant,
    north_african_first_gen_migrant,
    north_african_second_gen_migrant,
    parent_smi,
    adult_life_events,
    tobacco_daily,
    heavy_cannabis_use,
    childhood_trauma,
    anhedonia,
    parent_ses
  )


# ------------------------------------------------------------------------------
# 7) MISSINGNESS + IMPUTATION (unchanged, now on SOCIODEM-anchored table)
# ------------------------------------------------------------------------------
missing_summary <- PPS_PRONET_clean %>%
  select(-src_subject_id) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingPercent") %>%
  arrange(desc(MissingPercent))

write_xlsx(missing_summary, "missigness_external_validation.xlsx")

print(missing_summary)


# Set classes before missForest
PPS_PRONET_clean <- PPS_PRONET_clean %>%
  mutate(
    gender    = factor(gender, levels = c("Male", "Female", "Non-binary", "Other")),
    ethnicity = factor(ethnicity, levels = c("White", "Black", "Other"))
  )

bin_vars <- c("first_gen_migrant", "second_gen_migrant", "north_african_second_gen_migrant",
              "parent_ses", "parent_smi",
              "adult_life_events",
              "tobacco_daily", "heavy_cannabis_use")

PPS_PRONET_clean[bin_vars] <- lapply(PPS_PRONET_clean[bin_vars], function(x) {
  factor(x, levels = c(0, 1))
})

cont_vars <- c("age", "childhood_trauma", "anhedonia")

# Convert only those known to be continuous to numeric (safe if already numeric)
for (v in intersect(cont_vars, names(PPS_PRONET_clean))) {
  PPS_PRONET_clean[[v]] <- as.numeric(PPS_PRONET_clean[[v]])
}

# Build imputation input without the ID
imp_input <- PPS_PRONET_clean %>% select(-src_subject_id)

set.seed(123)
imp_res <- missForest(as.data.frame(imp_input))

# Recombine ID and cast binaries back to numeric 0/1
PPS_PRONET_clean_imp <- dplyr::bind_cols(
  PPS_PRONET_clean %>% dplyr::select(src_subject_id),
  tibble::as_tibble(imp_res$ximp)
)

PPS_PRONET_clean_imp[bin_vars] <- lapply(PPS_PRONET_clean_imp[bin_vars], function(x) {
  as.integer(as.character(x))
})


# ------------------------------------------------------------------------------
# 8) PPS SCORE 
# ------------------------------------------------------------------------------

# 8) PPS SCORE  – using EU-GEI coefficients
PPS_PRONET_clean_imp <- PPS_PRONET_clean_imp %>%
  mutate(
    PPS_score_oldpred = -6.516721e-01 +
      6.657729e-02  * ifelse(gender == "Male" & age >= 25 & age <= 35, 1, 0) +
      -1.721190e-01  * ifelse(ethnicity == "Other", 1, 0) +
      3.725069e-01  * ifelse(ethnicity == "White", 1, 0) +
      5.647518e-16  * north_african_first_gen_migrant +     # FirstGenImmigrantNA1
      -9.327723e-02  * first_gen_migrant +                  # FirstGenImmigrant1
      1.025347e+00  * north_african_second_gen_migrant +    # SecondGenImmigrantNA1
      7.709335e-02  * second_gen_migrant +                  # SecondGenImmigrant1
      6.252665e-01  * parent_ses +                          # ParentalSES1
      7.848041e-01  * parent_smi +                          # Parental_SMI1
      4.013517e-01  * adult_life_events +                   # AdultLifeEvents1
      6.674957e-01  * heavy_cannabis_use +                  # Cannabis1
      1.485927e+00  * ifelse(childhood_trauma > 55, 1, 0) + # ChildhoodTrauma1
      1.992040e+00  * ifelse(anhedonia > 35, 1, 0) +        # Anhedonia1
      4.323031e-01  * ifelse(tobacco_daily > 0, 1, 0)       # Tobacco1
  )


# ------------------------------------------------------------------------------
# 9) CHR vs HC FLAG (unchanged, but left-joined into SOCIODEM-based table)
# ------------------------------------------------------------------------------

data_incexc <- read_delim("datasets/INCEXC_criteria.csv", delim = ";", na = c("-3", "-300", "900", "-900"))

PPS_PRONET_clean_imp <- PPS_PRONET_clean_imp %>%
  left_join(data_incexc %>% select(src_subject_id, chrcrit_part), by = "src_subject_id") %>%
  rename(chr_status = chrcrit_part) %>%
  mutate(chr_status = if_else(chr_status == 2, 0, chr_status))

# ------------------------------------------------------------------------------
# 10) SAVE
# ------------------------------------------------------------------------------

write_csv(PPS_PRONET_clean_imp, "PPS_PRONET_clean_imp.csv")


################ EXTERNAL VALIDATION ######################

# ---- Libraries ----
library(tidyverse)
library(dplyr)
library(caret)
library(pROC)
library(dcurves)
library(ggplot2)
library(tibble)
library(Metrics)
library(Hmisc)
library(DescTools)
library(boot)
library(openxlsx)
library(readxl)
library(readr)

# ---- import PPS_PRONET_clean_imp dataset
PPS_PRONET_clean_imp <- read_delim(
  "PRONET_PPS_Processing/PPS_PRONET_clean_imp.csv",
  delim = ";",
  locale = locale(decimal_mark = ",")
)

# --- openxlsx tempdir workaround (prevents the Windows temp-dir error) ---
options("openxlsx.tempdir" = tempdir())

# -------------------------
# Data assumptions:
# - PPS_PRONET_clean_imp exists in memory
# - chr_status coded 0/1 (HC/CHR)
# - PPS_score_oldpred is the EU-GEI linear predictor (logit); 
# -------------------------

##### CHR vs HC #####

# Keep a factor (for confusion matrices) and numeric (for calibration) version
observed_fac <- factor(as.character(PPS_PRONET_clean_imp$chr_status), levels = c("0","1"))
y_num        <- as.integer(as.character(PPS_PRONET_clean_imp$chr_status))  # 0/1

# Predicted probability from the fixed EU-GEI coefficients (original model)
PPS_PRONET_clean_imp$risk <- 1 / (1 + exp(-PPS_PRONET_clean_imp$PPS_score_oldpred))
PPS_PRONET_clean_imp$risk <- pmin(pmax(PPS_PRONET_clean_imp$risk, 1e-4), 0.9999)  # clip

# ----- Threshold grid metrics (original model) -----
thresholds <- seq(0, 1, by = 0.01)

results_orig_thresholds <- map_dfr(
  thresholds,
  ~{
    predicted_labels_tmp <- factor(ifelse(PPS_PRONET_clean_imp$risk >= .x, "1", "0"),
                                   levels = c("0","1"))
    cm <- caret::confusionMatrix(predicted_labels_tmp, observed_fac, positive = "1")
    tibble(
      Threshold   = .x,
      BAC         = as.numeric(cm$byClass["Balanced Accuracy"]),
      Sensitivity = as.numeric(cm$byClass["Sensitivity"]),
      Specificity = as.numeric(cm$byClass["Specificity"]),
      PPV         = as.numeric(cm$byClass["Pos Pred Value"]),
      NPV         = as.numeric(cm$byClass["Neg Pred Value"])
    )
  }
)

# ----- Recalibration (logistic calibration of the linear predictor) -----
# This fits: logit(P(Y=1)) = a + b * PPS_score_oldpred
recal_model <- glm(y_num ~ PPS_score_oldpred, data = PPS_PRONET_clean_imp, family = binomial())
recalibrated_probs <- predict(recal_model, type = "response")
recalibrated_probs <- pmin(pmax(recalibrated_probs, 1e-4), 0.9999)




# -----------------------------
# Compare original vs. recalibrated probabilities (DIAGNOSIC CHECK + CALIBRATION PLOT)
# -----------------------------

# Summary stats by class (0 = HC, 1 = CHR)
cat("📌 Original probabilities by class:\n")
print(tapply(PPS_PRONET_clean_imp$risk, PPS_PRONET_clean_imp$chr_status, summary))

cat("\n📌 Recalibrated probabilities by class:\n")
print(tapply(recalibrated_probs, PPS_PRONET_clean_imp$chr_status, summary))

# Helper function to create LOESS calibration plot
plot_loess_calib <- function(pred, obs_num, title) {
  ggplot(tibble(pred = pred, obs = obs_num),
         aes(x = pred, y = obs)) +
    geom_smooth(method = "loess", se = TRUE, color = "steelblue", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="black") +
    labs(title = title,
         x = "Predicted probability (CHR)",
         y = "Observed CHR rate") +
    theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    axis.title.x = element_text(size = 14),
  ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}

# Create both calibration plots
p_orig  <- plot_loess_calib(PPS_PRONET_clean_imp$risk, y_num,
                            "External Validation (original model)")
p_recal <- plot_loess_calib(recalibrated_probs, y_num,
                            expression("PPS original model (recalibrated) - external validation"^a))

# Export to PNG
ggsave("Calibration_LOESS_original.png",  p_orig,  width = 7, height = 6, dpi = 300)
ggsave("Calibration_LOESS_recalibrated.png", p_recal, width = 7, height = 6, dpi = 300)




# AUC + 95% CI for recalibrated probabilities
roc_recal <- pROC::roc(
  response  = observed_fac,
  predictor = recalibrated_probs,
  levels    = c("0","1"),
  direction = "<",
  quiet     = TRUE
)
auc_recal <- as.numeric(pROC::auc(roc_recal))
ci_recal  <- pROC::ci.auc(roc_recal, conf.level = 0.95)
print(paste("Harrell's C (AUC) after recalibration =", round(auc_recal, 3)))
print(ci_recal)

# ----- Threshold grid metrics (recalibrated model) -----
results_recal_thresholds <- map_dfr(
  thresholds,
  ~{
    predicted_labels_tmp <- factor(ifelse(recalibrated_probs >= .x, "1", "0"),
                                   levels = c("0","1"))
    cm <- caret::confusionMatrix(predicted_labels_tmp, observed_fac, positive = "1")
    tibble(
      Threshold   = .x,
      BAC         = as.numeric(cm$byClass["Balanced Accuracy"]),
      Sensitivity = as.numeric(cm$byClass["Sensitivity"]),
      Specificity = as.numeric(cm$byClass["Specificity"]),
      PPV         = as.numeric(cm$byClass["Pos Pred Value"]),
      NPV         = as.numeric(cm$byClass["Neg Pred Value"])
    )
  }
)

# ----- Export the threshold grids -----
wb1 <- createWorkbook()
addWorksheet(wb1, "original model")
writeData(wb1, "original model", results_orig_thresholds)

addWorksheet(wb1, "recalibrated model")
writeData(wb1, "recalibrated model", results_recal_thresholds)

saveWorkbook(wb1, file.path(getwd(), "all_threshold_metrics.xlsx"), overwrite = TRUE)
message("Saved: ", normalizePath(file.path(getwd(), "all_threshold_metrics.xlsx")))


#### Compute metrics at the BEST THRESHOLDS (manual) + 95% CIs via BCa bootstrap ####

# --- Manual thresholds ---
thres_orig  <- 0.9   # original model
thres_recal <- 0.77   # recalibrated model

# --- Bootstrap function returning point estimates + 95% CIs (BCa) ---
boot_metrics <- function(obs_factor, obs_num, pred_probs, threshold, R = 2000, type = "bca") {
  set.seed(202)  # reproducible bootstrap
  
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    predicted <- factor(ifelse(d$pred >= threshold, "1", "0"), levels = c("0","1"))
    cm <- caret::confusionMatrix(predicted, d$obs_fac, positive = "1")
    
    BAC <- cm$byClass["Balanced Accuracy"]
    Se  <- cm$byClass["Sensitivity"]
    Sp  <- cm$byClass["Specificity"]
    PPV <- cm$byClass["Pos Pred Value"]
    NPV <- cm$byClass["Neg Pred Value"]
    
    Brier <- DescTools::BrierScore(d$obs_num, d$pred)
    
    # Calibration intercept (offset) and slope
    intercept <- coef(glm(d$obs_num ~ offset(qlogis(d$pred)), family = binomial()))
    slope     <- coef(glm(d$obs_num ~ qlogis(d$pred),        family = binomial()))[2]
    
    # ICI (mean absolute calibration error; simple approximation)
    ICI <- mean(abs(d$obs_num - d$pred))
    
    # Harrell's C
    Cindex <- as.numeric(Hmisc::rcorr.cens(d$pred, d$obs_num)[1])
    
    c(BAC, Se, Sp, PPV, NPV, Brier, intercept, slope, ICI, Cindex)
  }
  
  df <- data.frame(obs_fac = obs_factor, obs_num = obs_num, pred = pred_probs)
  boot_obj <- boot(df, boot_fun, R = R, strata = df$obs_num) # stratified by outcome to avoid all-0 or all-1 resamples
  
  metric_names <- c("Balanced Accuracy","Sensitivity","Specificity","PPV","NPV",
                    "Brier Score","Calibration Intercept","Calibration Slope",
                    "ICI","Harrell's C")
  
  # Point estimates = bootstrap means
  est <- colMeans(boot_obj$t, na.rm = TRUE)
  
  # 95% CIs (BCa; fallback NA if not available)
  ci_low  <- ci_high <- numeric(length(est))
  for (j in seq_along(est)) {
    ci <- try(boot.ci(boot_obj, type = type, index = j), silent = TRUE)
    if (inherits(ci, "try-error") || is.null(ci)) {
      ci_low[j]  <- NA
      ci_high[j] <- NA
    } else if (type == "bca" && !is.null(ci$bca)) {
      ci_low[j]  <- ci$bca[4]
      ci_high[j] <- ci$bca[5]
    } else if (type == "perc" && !is.null(ci$percent)) {
      ci_low[j]  <- ci$percent[4]
      ci_high[j] <- ci$percent[5]
    } else {
      ci_low[j]  <- NA
      ci_high[j] <- NA
    }
  }
  
  tibble(
    Metric     = metric_names,
    Estimate   = est,
    Lower_95CI = ci_low,
    Upper_95CI = ci_high
  )
}

# --- Run bootstrap for Original and Recalibrated at the best thresholds ---
metrics_orig  <- boot_metrics(observed_fac, y_num, PPS_PRONET_clean_imp$risk, thres_orig,  R = 2000, type = "perc")
metrics_recal <- boot_metrics(observed_fac, y_num, recalibrated_probs,        thres_recal, R = 2000, type = "perc")

# # -------------------------------------------------------------------------
# # SUBGROUP PERFORMANCE: EXTERNAL VALIDATION BY GENDER AND ETHNICITY
# # -------------------------------------------------------------------------
# 
# # Add predictions and outcome labels to the dataset
# subgroup_data_ext <- PPS_PRONET_clean_imp %>%
#   mutate(
#     obs_fac = factor(as.character(chr_status), levels = c("0", "1")),
#     obs_num = as.integer(as.character(chr_status)),
#     risk_original = risk,
#     risk_recalibrated = recalibrated_probs,
#     gender = as.character(gender),
#     ethnicity = as.character(ethnicity)
#   )
# 
# # Helper function: compute bootstrapped performance within one subgroup
# compute_subgroup_metrics <- function(data, subgroup_var, subgroup_value,
#                                      pred_var, threshold, model_label,
#                                      R = 2000) {
#   
#   d <- data %>%
#     filter(.data[[subgroup_var]] == subgroup_value) %>%
#     filter(!is.na(obs_num), !is.na(.data[[pred_var]]))
#   
#   # If subgroup does not contain both CHR and CC, metrics cannot be computed
#   if (length(unique(d$obs_num)) < 2) {
#     return(tibble(
#       Model = model_label,
#       Subgroup_variable = subgroup_var,
#       Subgroup = subgroup_value,
#       N = nrow(d),
#       N_CHR = sum(d$obs_num == 1, na.rm = TRUE),
#       N_CC = sum(d$obs_num == 0, na.rm = TRUE),
#       Metric = NA_character_,
#       Estimate = NA_real_,
#       Lower_95CI = NA_real_,
#       Upper_95CI = NA_real_
#     ))
#   }
#   
#   res <- boot_metrics(
#     obs_factor = d$obs_fac,
#     obs_num = d$obs_num,
#     pred_probs = d[[pred_var]],
#     threshold = threshold,
#     R = R,
#     type = "perc"
#   )
#   
#   res %>%
#     mutate(
#       Model = model_label,
#       Subgroup_variable = subgroup_var,
#       Subgroup = subgroup_value,
#       N = nrow(d),
#       N_CHR = sum(d$obs_num == 1, na.rm = TRUE),
#       N_CC = sum(d$obs_num == 0, na.rm = TRUE),
#       .before = Metric
#     )
# }
# 
# # Define subgroup levels
# gender_levels_ext <- c("Male", "Female")
# ethnicity_levels_ext <- c("White", "Black", "Other")
# 
# # Original external validation model, before recalibration
# subgroup_orig_gender <- map_dfr(
#   gender_levels_ext,
#   ~ compute_subgroup_metrics(
#     data = subgroup_data_ext,
#     subgroup_var = "gender",
#     subgroup_value = .x,
#     pred_var = "risk_original",
#     threshold = thres_orig,
#     model_label = "Externally validated PPS original model",
#     R = 2000
#   )
# )
# 
# subgroup_orig_ethnicity <- map_dfr(
#   ethnicity_levels_ext,
#   ~ compute_subgroup_metrics(
#     data = subgroup_data_ext,
#     subgroup_var = "ethnicity",
#     subgroup_value = .x,
#     pred_var = "risk_original",
#     threshold = thres_orig,
#     model_label = "Externally validated PPS original model",
#     R = 2000
#   )
# )
# 
# # Recalibrated external validation model
# subgroup_recal_gender <- map_dfr(
#   gender_levels_ext,
#   ~ compute_subgroup_metrics(
#     data = subgroup_data_ext,
#     subgroup_var = "gender",
#     subgroup_value = .x,
#     pred_var = "risk_recalibrated",
#     threshold = thres_recal,
#     model_label = "Recalibrated PPS original model",
#     R = 2000
#   )
# )
# 
# subgroup_recal_ethnicity <- map_dfr(
#   ethnicity_levels_ext,
#   ~ compute_subgroup_metrics(
#     data = subgroup_data_ext,
#     subgroup_var = "ethnicity",
#     subgroup_value = .x,
#     pred_var = "risk_recalibrated",
#     threshold = thres_recal,
#     model_label = "Recalibrated PPS original model",
#     R = 2000
#   )
# )
# 
# # Combine all subgroup results
# subgroup_performance_external <- bind_rows(
#   subgroup_orig_gender,
#   subgroup_orig_ethnicity,
#   subgroup_recal_gender,
#   subgroup_recal_ethnicity
# )
# 
# # Save subgroup results
# wb_subgroups <- createWorkbook()
# 
# addWorksheet(wb_subgroups, "External subgroup metrics")
# writeData(wb_subgroups, "External subgroup metrics", subgroup_performance_external)
# 
# saveWorkbook(
#   wb_subgroups,
#   file.path(getwd(), "subgroup_performance_external_validation.xlsx"),
#   overwrite = TRUE
# )
# 
# message("Saved: subgroup_performance_external_validation.xlsx")


# --- Export to Excel ---
wb2 <- createWorkbook()
addWorksheet(wb2, "Original Model (0.90)")
addWorksheet(wb2, "Recalibrated Model (0.77)")
writeData(wb2, "Original Model (0.90)",    metrics_orig)
writeData(wb2, "Recalibrated Model (0.77)", metrics_recal)
saveWorkbook(wb2, file.path(getwd(), "best_thresholds_results.xlsx"), overwrite = TRUE)
message("Saved: ", normalizePath(file.path(getwd(), "best_thresholds_results.xlsx")))



############### Decision Curve Analysis (DCA): Clinical & General ##################

library(tidyverse)
library(dcurves)
library(openxlsx)

# ---- Data ----
# ---- Data (using recalibrated predictions) ----
observed  <- as.integer(as.character(PPS_PRONET_clean_imp$chr_status))    # 0/1
predicted <- recalibrated_probs                                           # PPS recalibrated probabilities

# ---- Helper to compute DCA ----
compute_dca <- function(obs, pred, thresholds, prevalence = NULL) {
  prev_use <- if (is.null(prevalence)) mean(obs == 1, na.rm = TRUE) else prevalence
  
  dca_tbl <- dcurves::dca(
    obs ~ pred,
    data = tibble(obs = obs, pred = pred),
    thresholds = thresholds,
    prevalence = prev_use
  ) %>% as_tibble()
  
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

# ---- Compute DCA ----
# Clinical population: uses empirical prevalence
thr_clin <- seq(0, 0.50, by = 0.005)
dca_clinical <- compute_dca(observed, predicted, thr_clin, prevalence = 0.192)

# General population: assumed 2% prevalence
thr_gen <- seq(0, 0.03, 0.001)
dca_general <- compute_dca(observed, predicted, thr_gen, prevalence = 0.02)

# ---- Plot function ----
plot_dca <- function(dca_tbl, title) {
  ggplot(dca_tbl, aes(x = threshold, y = net_benefit, color = label)) +
    geom_line(linewidth = 1) +
    labs(
      title = title,
      x = "Threshold probability",
      y = "Net benefit",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_color_manual(values = c("Treat None" = "black", "Treat All" = "grey50", "PPS" = "steelblue"))
}

# ---- Make and save plots ----
p_clin <- plot_dca(dca_clinical, "DCA — Clinical Population") +
  coord_cartesian(ylim = c(0, 0.10))
p_gen  <- plot_dca(dca_general,  "DCA — General Population") +
  coord_cartesian(ylim = c(-0.005, 0.02)) # zoom Y-axis for visibility

ggsave("DCA_clinical.png", p_clin, bg = "white", width = 6.5, height = 5, dpi = 300)
ggsave("DCA_general.png",  p_gen,  bg = "white", width = 6.5, height = 5, dpi = 300)

# ---- Save Excel tables ----
wb <- createWorkbook()
addWorksheet(wb, "Clinical")
addWorksheet(wb, "General")
writeData(wb, "Clinical", dca_clinical)
writeData(wb, "General",  dca_general)
saveWorkbook(wb, "DCA_tables.xlsx", overwrite = TRUE)

message("Saved: DCA_clinical.png, DCA_general.png, DCA_tables.xlsx")



############### Association with symptoms and functioning ########

library(ggplot2)
library(readxl)
library(dplyr)

# --- Load dataset and treat special codes as NA ---
file_path <- file.path(getwd(), "datasets", "psychs_sofas_simple.xlsx")
psychs_sofas_simple <- read_excel(file_path, na = c("", "NA", "-3", "-300", "900", "-900"))

# Ensure numeric columns
psychs_sofas_simple <- psychs_sofas_simple %>%
  mutate(
    psychs_pos_tot_final = as.numeric(psychs_pos_tot_final),
    chrsofas_currscore_final = as.numeric(chrsofas_currscore_final)
  )

# --- Merge with PPS dataset ---
PPS_PRONET_clean_imp <- PPS_PRONET_clean_imp %>%
  left_join(
    psychs_sofas_simple %>%
      select(src_subject_id, psychs_pos_tot_final, chrsofas_currscore_final),
    by = "src_subject_id"
  )

# --- Spearman correlations (exclude missing) ---
spearman_sympt <- cor.test(
  ~ PPS_score_oldpred + psychs_pos_tot_final,
  data = PPS_PRONET_clean_imp,
  method = "spearman",
  use = "complete.obs"
)
spearman_func <- cor.test(
  ~ PPS_score_oldpred + chrsofas_currscore_final,
  data = PPS_PRONET_clean_imp,
  method = "spearman",
  use = "complete.obs"
)

rho_sympt <- round(spearman_sympt$estimate, 3)
rho_func  <- round(spearman_func$estimate, 3)

# --- Plot 1: Association with symptom severity ---
p_sympt <- ggplot(PPS_PRONET_clean_imp, aes(x = PPS_score_oldpred, y = psychs_pos_tot_final)) +
  geom_point(alpha = 0.6, size = 0.5, color = "gray") +
  geom_smooth(method = "lm", color = "steelblue") +
  annotate("text", x = Inf, y = Inf, label = paste0("ρ = ", rho_sympt),
           hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold") +
  labs(
    x = "PPS old score",
    y = "Positive symptom severity"
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggsave("association_with_symptoms_PPSold.png", p_sympt, width = 6.5, height = 5, dpi = 300, bg = "white")

# --- Plot 2: Association with functioning ---
p_func <- ggplot(PPS_PRONET_clean_imp, aes(x = PPS_score_oldpred, y = chrsofas_currscore_final)) +
  geom_point(alpha = 0.6, size = 0.5, color = "gray") +
  geom_smooth(method = "lm", color = "seagreen4") +
  annotate("text", x = Inf, y = Inf, label = paste0("ρ = ", rho_func),
           hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold") +
  labs(
    x = "PPS old score",
    y = "Socio-occupational functioning"
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggsave("association_with_functioning_PPSold.png", p_func, width = 6.5, height = 5, dpi = 300, bg = "white")


# --- Pearson correlations + summary table ---
pearson_sympt <- cor.test(
  ~ PPS_score_oldpred + psychs_pos_tot_final,
  data = PPS_PRONET_clean_imp,
  method = "pearson",
  use = "complete.obs"
)
pearson_func <- cor.test(
  ~ PPS_score_oldpred + chrsofas_currscore_final,
  data = PPS_PRONET_clean_imp,
  method = "pearson",
  use = "complete.obs"
)

cor_summary <- data.frame(
  Comparison  = c("PPS vs Symptom Severity", "PPS vs Functioning",
                  "PPS vs Symptom Severity", "PPS vs Functioning"),
  Method      = c("Spearman", "Spearman", "Pearson", "Pearson"),
  Correlation = c(spearman_sympt$estimate, spearman_func$estimate,
                  pearson_sympt$estimate, pearson_func$estimate),
  P_value     = c(spearman_sympt$p.value, spearman_func$p.value,
                  pearson_sympt$p.value, pearson_func$p.value),
  N           = c(spearman_sympt$parameter + 2, spearman_func$parameter + 2,
                  pearson_sympt$parameter + 2, pearson_func$parameter + 2)
)

write.csv(cor_summary, file.path(getwd(), "PPS_correlation_summary.csv"), row.names = FALSE)
message("Saved: association_with_symptoms.png, association_with_functioning.png, PPS_correlation_summary.csv")


#--------------------------
#### Harrell's C — External Validation
#--------------------------
library(ggplot2)
library(dplyr)

# Use the AUC (Harrell's C) and its CI already computed earlier
ext_c_summary <- tibble(
  Model = "External Validation",
  mean_C = auc_recal,
  lower  = ci_recal[1],
  upper  = ci_recal[3]
)

# Create a minimal data frame with a single value for plotting consistency
ext_c_df <- tibble(
  Model = "External Validation",
  C = auc_recal
)

# Plot
p_cindex_external <- ggplot(ext_c_df, aes(x = Model, y = C)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 3) +
  geom_point(data = ext_c_summary, aes(x = Model, y = mean_C),
             color = "darkorange", size = 3, inherit.aes = FALSE) +
  geom_errorbar(data = ext_c_summary,
                aes(x = Model, ymin = lower, ymax = upper),
                width = 0.1, color = "black", linewidth = 1, inherit.aes = FALSE) +
  labs(
    y = "Mean and 95% CI",
    x = "Harrell's C",
    title = expression("PPS original model (recalibrated) - external validation"^a)
  ) +
  coord_cartesian(ylim = c(0.65, 0.95)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_blank()
  )

# Save
ggsave("Cindex_External_Validation.png", p_cindex_external, width = 5, height = 5, dpi = 300, bg = "white")
message("✅ Saved: Cindex_External_Validation.png")
