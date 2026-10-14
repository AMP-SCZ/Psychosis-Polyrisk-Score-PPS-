# Set language to English for messages and warnings
Sys.setenv(LANG = "en")

#### 5-FOLD CV FOR FINAL LASSO MODEL TO GET PPS SCORES ####
suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(caret)
  library(missForest)
  library(writexl)
})

# -------------------------
# 0) Paths & parameters
# -------------------------
data_path <- "PRONET_PPS_Processing/PPS_PRONET_clean_newpred.csv"
set.seed(231)
n_lambda   <- 100
outerFolds <- 5

# -------------------------
# 1) Load & preprocess (types BEFORE imputation)
# -------------------------
PPS <- read.csv2(data_path, stringsAsFactors = FALSE, dec = ",")

# Drop IDs
id_cols <- intersect(c("src_subject_id","subjectkey","subject_id","id"), names(PPS))
if (length(id_cols)) PPS[id_cols] <- list(NULL)

# Categorical (multi-level) as factors
factor_candidates <- intersect(c("gender","ethnicity"), names(PPS))
for (v in factor_candidates) {
  PPS[[v]] <- as.factor(PPS[[v]])
  if (v == "ethnicity" && "White" %in% levels(PPS[[v]])) {
    PPS[[v]] <- relevel(PPS[[v]], ref = "White")
  }
}

# Continuous scales as numeric
scale_vars <- intersect(c(
  "age","emotional_abuse","physical_abuse","sexual_abuse",
  "emotional_neglect","physical_neglect","loneliness",
  "neighborhood_quality","anhedonia","premorbid_iq","childhood_social_withdrawal"
), names(PPS))
if (length(scale_vars)) PPS[scale_vars] <- lapply(PPS[scale_vars], as.numeric)

# Binary vars -> FACTORS (so missForest classifies, not regresses)
bin_vars <- intersect(c(
  "handedness","first_gen_migrant","second_gen_migrant",
  "north_african_first_gen_migrant","north_african_second_gen_migrant",
  "paternal_age","parent_ses","mother_psychosis","mother_other_mental_illness",
  "father_psychosis","father_other_mental_illness",
  "adult_life_events","tobacco_daily","cannabis_daily"
), names(PPS))
if (length(bin_vars)) {
  PPS[bin_vars] <- lapply(PPS[bin_vars], function(z) factor(z, levels = c(0,1)))
}

# Outcome
if (!"chr" %in% names(PPS)) {
  PPS$chr <- factor(PPS$chr_status, levels = c(0,1), labels = c("HC","CHR"))
}
outcome_var <- "chr"
pred_vars   <- setdiff(names(PPS), c(outcome_var,"chr_status"))

# Keep predictors + outcome; clean names
df <- PPS[, c(pred_vars, outcome_var)]
names(df) <- make.names(names(df), unique = TRUE)

# -------------------------
# 2) Drop >20% missing (predictors only)
# -------------------------
miss_rate <- sapply(df[, pred_vars, drop = FALSE], function(x) mean(is.na(x)))
drop_vars <- names(miss_rate)[miss_rate > 0.20]
if (length(drop_vars)) {
  message("Dropped for >20% missing: ", paste(drop_vars, collapse = ", "))
  df <- df[, setdiff(names(df), drop_vars), drop = FALSE]
}
kept_vars <- setdiff(names(df), outcome_var)

# Align bin_vars list to columns that survived dropping and name cleaning
bin_vars_kept <- intersect(make.names(bin_vars, unique = TRUE), kept_vars)

# -------------------------
# 3) Impute with missForest
#    (binary factors stay factors; others as set above)
# -------------------------
X <- df[, kept_vars, drop = FALSE]
imp <- missForest(X)
X_imp <- as.data.frame(imp$ximp)

# Convert binary factors back to integer 0/1
if (length(bin_vars_kept)) {
  X_imp[bin_vars_kept] <- lapply(X_imp[bin_vars_kept], function(z) {
    if (is.factor(z)) z <- as.character(z)
    as.integer(as.character(z))
  })
}

full_df <- cbind(X_imp, chr = df[[outcome_var]])

# -------------------------
# 4) 5-fold CV to pick lambdas
# -------------------------
folds <- createFolds(full_df$chr, k = outerFolds, returnTrain = TRUE)
lambda_grid <- exp(seq(log(0.001), log(0.1), length.out = n_lambda))

best_lambdas <- numeric(length(folds))
for (i in seq_along(folds)) {
  message("Fold ", i)
  train_idx <- folds[[i]]
  
  X_train <- model.matrix(~ . - 1, full_df[train_idx, kept_vars, drop = FALSE])
  y_train <- full_df$chr[train_idx]
  
  cvfit <- cv.glmnet(
    x = X_train, y = y_train,
    family = "binomial", alpha = 1,
    lambda = lambda_grid,
    nfolds = 5,
    type.measure = "deviance",
    standardize = TRUE  
  )
  best_lambdas[i] <- cvfit$lambda.min
}

mean_lambda <- mean(best_lambdas, na.rm = TRUE)
message("Mean selected lambda: ", signif(mean_lambda, 4))

# -------------------------
# 5) Final fit on full data
# -------------------------
X_full <- model.matrix(~ . - 1, full_df[, kept_vars, drop = FALSE])
y_full <- full_df$chr

final_fit <- glmnet(
  x = X_full, y = y_full,
  family = "binomial", alpha = 1,
  lambda = mean_lambda,
  standardize = TRUE  
)

coef_final <- as.matrix(coef(final_fit))
coef_tbl <- tibble(
  Predictor   = rownames(coef_final),
  Coefficient = as.numeric(coef_final)
)

# -------------------------
# 5b) Standardize coefficients (CLEAN VERSION)
# -------------------------

coef_no_intercept <- coef_tbl %>%
  filter(Predictor != "(Intercept)")

# Keep only continuous variables that actually survived preprocessing
scale_vars_kept <- intersect(scale_vars, colnames(X_imp))

sd_cont <- sapply(X_imp[scale_vars_kept], sd, na.rm = TRUE)

# Function to extract base variable name
get_base_var <- function(name) {
  sub("^(.*?)[^a-zA-Z0-9_].*$", "\\1", name)
}

# Build standardized coefficients
std_coef <- coef_no_intercept %>%
  mutate(
    base_var = sapply(Predictor, get_base_var),
    is_continuous = base_var %in% scale_vars_kept,
    SD = sd_cont[base_var],
    Coefficient_standardized = ifelse(
      is_continuous,
      Coefficient * (2 * SD),
      Coefficient   # leave all non-continuous unchanged
    )
  )

# Add intercept back
intercept_row <- coef_tbl %>%
  filter(Predictor == "(Intercept)") %>%
  mutate(Coefficient_standardized = Coefficient)

coef_comparison_tbl <- bind_rows(intercept_row, std_coef) %>%
  select(
    Predictor,
    Coefficient_raw = Coefficient,
    Coefficient_standardized
  ) %>%
  filter(Coefficient_raw != 0 | Predictor == "(Intercept)")

# Raw non-zero coefficients
kept_coef_tbl <- coef_tbl %>% filter(Coefficient != 0)

# -------------------------
# 6) Export coefficients
# -------------------------
drop_tbl <- tibble(Variable = drop_vars, Reason = ">20% missing")
shrink_tbl <- coef_tbl %>%
  filter(Predictor != "(Intercept)" & Coefficient == 0) %>%
  transmute(Variable = Predictor, Reason = "Shrunk_to_zero")

write_xlsx(
  list(
    "Final_Coefficients_Raw" = kept_coef_tbl,
    "Coefficients_Standardized" = coef_comparison_tbl,
    "Dropped_or_Shrunk"  = bind_rows(drop_tbl, shrink_tbl)
  ),
  path = "Final_LASSO_5fold_coeff_newpred.xlsx"
)
message("Exported: ", normalizePath("Final_LASSO_5fold_coeff_newpred.xlsx"))

# -------------------------
# 7) Compute PPS_score_newpred (linear predictor)
# -------------------------

# Extract coefficients
coef_vec <- as.matrix(coef(final_fit))
coef_vec <- coef_vec[coef_vec[,1] != 0, , drop = FALSE]
coef_vec <- setNames(as.numeric(coef_vec[,1]), rownames(coef_vec))

intercept <- coef_vec["(Intercept)"]
coef_vec  <- coef_vec[names(coef_vec) != "(Intercept)"]

# Recreate the same design matrix used by glmnet
X_mm <- model.matrix(~ . - 1, X_imp)

# Align predictors with coefficients
X_use <- X_mm[, intersect(colnames(X_mm), names(coef_vec)), drop = FALSE]

# Compute PPS score
pps_score <- intercept + as.matrix(X_use) %*% coef_vec[colnames(X_use)]

final_df <- cbind(X_imp, chr = df$chr, PPS_score_newpred = as.numeric(pps_score))

# Add back IDs (only src_subject_id is available)
raw <- read.csv2(data_path, stringsAsFactors = FALSE, dec = ",")
if ("src_subject_id" %in% names(raw)) {
  final_df <- cbind(raw["src_subject_id"], final_df)
}

write.csv(final_df,
            "PPS_PRONET_clean_newpred_PPSscore.csv",
            sep = ";",
            dec = ".",
            row.names = FALSE)

# #### Ehtincity sensitivty analysis by site ####
# df <- PPS_PRONET_clean_newpred_PPSscore_imp
# 
# df$site <- substr(df$src_subject_id, 1, 2)
# 
# df %>%
#   count(site, chr) %>%
#   pivot_wider(names_from = chr, values_from = n, values_fill = 0) %>%
#   mutate(total = HC + CHR,
#          chr_ratio = CHR / total) %>%
#   arrange(site) %>%
#   print(n = Inf)
# 
# df %>%
#   filter(ethnicity %in% c("East_Asian", "South_Asian")) %>%
#   count(site, ethnicity) %>%
#   arrange(desc(n))

# ############### ASSOCIATION w/ symptoms and functioning ########
# 
# library(readxl)
# library(dplyr)
# library(ggplot2)
# 
# # --- Load psychs/SOFAS and treat special codes as NA ---
# file_path <- file.path(getwd(), "datasets", "psychs_sofas_simple.xlsx")
# psychs_sofas_simple <- read_excel(file_path, na = c("", "NA", "-3", "-300", "900", "-900")) %>%
#   mutate(
#     psychs_pos_tot_final   = as.numeric(psychs_pos_tot_final),
#     chrsofas_currscore_final = as.numeric(chrsofas_currscore_final)
#   )
# 
# # --- Load PPS (comma decimals) ---
# PPS_PRONET_clean_newpred_PPSscore_imp <- read.csv2(
#   file.path(getwd(), "PRONET_PPS_processing", "PPS_PRONET_clean_newpred_PPSscore.csv"),
#   stringsAsFactors = FALSE,
#   dec = ","
# )
# 
# # Ensure PPS score is numeric (guard against character from locale)
# PPS_PRONET_clean_newpred_PPSscore_imp <- PPS_PRONET_clean_newpred_PPSscore_imp %>%
#   mutate(PPS_score_newpred = as.numeric(PPS_score_newpred))
# 
# # --- Merge with psychs/SOFAS ---
# PPS_PRONET_clean_newpred_PPSscore_imp <- PPS_PRONET_clean_newpred_PPSscore_imp %>%
#   left_join(
#     psychs_sofas_simple %>%
#       select(src_subject_id, psychs_pos_tot_final, chrsofas_currscore_final),
#     by = "src_subject_id"
#   )
# 
# # --- Export to Excel in current working directory ---
# writexl::write_xlsx(
#   PPS_PRONET_clean_newpred_PPSscore_imp,
#   path = file.path(getwd(), "PPS_PRONET_clean_newpred_PPSscore_imp.xlsx")
# )
# 
# # Subset to CHR only
# PPS_PRONET_clean_newpred_PPSscore_imp_CHR <- 
#   subset(PPS_PRONET_clean_newpred_PPSscore_imp, chr == "CHR")
# 
# # Quick inspection
# ggplot(PPS_PRONET_clean_newpred_PPSscore_imp, aes(x = psychs_pos_tot_final)) +
#   geom_histogram(binwidth = 20, fill = "orange") +
#   ggtitle("psychs total score")
# 
# ggplot(PPS_PRONET_clean_newpred_PPSscore_imp, aes(x = chrsofas_currscore_final)) +
#   geom_histogram(binwidth = 20, fill = "orange") +
#   ggtitle("sofas score")
# 
# shapiro.test(PPS_PRONET_clean_newpred_PPSscore_imp$psychs_pos_tot_final)
# qqnorm(PPS_PRONET_clean_newpred_PPSscore_imp$psychs_pos_tot_final); qqline(PPS_PRONET_clean_newpred_PPSscore_imp$psychs_pos_tot_final, col = "red")
# 
# shapiro.test(PPS_PRONET_clean_newpred_PPSscore_imp$chrsofas_currscore_final)
# qqnorm(PPS_PRONET_clean_newpred_PPSscore_imp$chrsofas_currscore_final); qqline(PPS_PRONET_clean_newpred_PPSscore_imp$chrsofas_currscore_final, col = "red")
# 
# # --- Spearman correlations (complete cases) ---
# spearman_sympt <- cor.test(
#   ~ PPS_score_newpred + psychs_pos_tot_final,
#   data   = PPS_PRONET_clean_newpred_PPSscore_imp_CHR,
#   method = "spearman",
#   use    = "complete.obs"
# )
# 
# spearman_func <- cor.test(
#   ~ PPS_score_newpred + chrsofas_currscore_final,
#   data   = PPS_PRONET_clean_newpred_PPSscore_imp_CHR,
#   method = "spearman",
#   use    = "complete.obs"
# )
# 
# rho_sympt <- round(as.numeric(spearman_sympt$estimate), 3)
# rho_func  <- round(as.numeric(spearman_func$estimate), 3)
# 
# # --- Plot 1: Association with symptom severity ---
# p_sympt <- ggplot(PPS_PRONET_clean_newpred_PPSscore_imp_CHR,
#                   aes(x = PPS_score_newpred, y = psychs_pos_tot_final)) +
#   geom_point(alpha = 0.6, size = 0.5, colour = "gray") +
#   geom_smooth(method = "lm", color = "steelblue") +
#   annotate("text", x = Inf, y = Inf, label = paste0("ρ = ", rho_sympt),
#            hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold") +
#   labs(
#     x = "PPS new score",
#     y = "Positive symptom severity",
#   ) +
#   theme_light() +
#   theme(
#     axis.title.x = element_text(face = "bold"),
#     axis.title.y = element_text(face = "bold")
#   )
# 
# # Save current working directory)
# ggsave("association_with_symptoms_PPSnew.png", p_sympt, width = 6.5, height = 5, dpi = 300, bg = "white")
# 
# # --- Plot 2: Association with functioning ---
# p_func <- ggplot(PPS_PRONET_clean_newpred_PPSscore_imp_CHR,
#                  aes(x = PPS_score_newpred, y = chrsofas_currscore_final)) +
#   geom_point(alpha = 0.6, size = 0.5, colour = "gray") +
#   geom_smooth(method = "lm", color = "seagreen4") +
#   annotate("text", x = Inf, y = Inf, label = paste0("ρ = ", rho_func),
#            hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold") +
#   labs(
#     x = "PPS new score",
#     y = "Socio-occupational functioning",
#   ) +
#   theme_light() +
#   theme(
#     axis.title.x = element_text(face = "bold"),
#     axis.title.y = element_text(face = "bold")
#   )
# 
# ggsave("association_with_functioning_PPSnew.png", p_func, width = 6.5, height = 5, dpi = 300, bg = "white")
# 
# # --- Pearson correlations + summary table (complete cases) ---
# pearson_sympt <- cor.test(
#   ~ PPS_score_newpred + psychs_pos_tot_final,
#   data   = PPS_PRONET_clean_newpred_PPSscore_imp,
#   method = "pearson",
#   use    = "complete.obs"
# )
# 
# pearson_func <- cor.test(
#   ~ PPS_score_newpred + chrsofas_currscore_final,
#   data   = PPS_PRONET_clean_newpred_PPSscore_imp,
#   method = "pearson",
#   use    = "complete.obs"
# )
# 
# cor_summary <- data.frame(
#   Comparison  = c("PPS(newpred) vs Symptom Severity", "PPS(newpred) vs Functioning",
#                   "PPS(newpred) vs Symptom Severity", "PPS(newpred) vs Functioning"),
#   Method      = c("Spearman", "Spearman", "Pearson", "Pearson"),
#   Correlation = c(as.numeric(spearman_sympt$estimate),
#                   as.numeric(spearman_func$estimate),
#                   as.numeric(pearson_sympt$estimate),
#                   as.numeric(pearson_func$estimate)),
#   P_value     = c(spearman_sympt$p.value,
#                   spearman_func$p.value,
#                   pearson_sympt$p.value,
#                   pearson_func$p.value),
#   N           = c(as.numeric(spearman_sympt$parameter) + 2,
#                   as.numeric(spearman_func$parameter) + 2,
#                   as.numeric(pearson_sympt$parameter) + 2,
#                   as.numeric(pearson_func$parameter) + 2)
# )
# 
# write.csv(cor_summary,
#           file.path(getwd(), "PPS_newpred_correlation_summary.csv"),
#           row.names = FALSE)
# 
# message('Saved: "association with symptoms.png", "association with functioning.png", "PPS_newpred_correlation_summary.csv"')
