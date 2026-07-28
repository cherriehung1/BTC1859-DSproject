# BTC1859H Team Project
# Missing PSQI Sensitivity Analysis
#
# Purpose:
#   1. Quantify missing PSQI data.
#   2. Compare participants with and without an observed PSQI score.
#   3. Examine whether observed participant characteristics predict PSQI
#      missingness.
#
# This script does not impute PSQI. Multiple imputation should only be added
# after the team agrees on an imputation model.

rm(list = ls())

# -------------------------------------------------------------------------
# 1. LOAD AND CHECK DATA
# -------------------------------------------------------------------------

data_path <- "project_data_cleaned.csv"
output_dir <- "psqi_sensitivity_results"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path, na.strings = c("NA", ""))

stopifnot(nrow(df) == 268)
stopifnot(!anyDuplicated(df$Subject))

df$PSQI_missing <- factor(
  ifelse(is.na(df$PSQI), "Missing", "Observed"),
  levels = c("Observed", "Missing")
)

# Confirm that the binary PSQI outcome preserves missing PSQI values and
# agrees with the clinical threshold among participants with observed PSQI.
stopifnot(all(is.na(df$PSQI) == is.na(df$PSQI_binary)))
stopifnot(all(df$PSQI_binary[!is.na(df$PSQI)] ==
                as.numeric(df$PSQI[!is.na(df$PSQI)] > 4)))

# -------------------------------------------------------------------------
# 2. MISSINGNESS SUMMARY
# -------------------------------------------------------------------------

n_total <- nrow(df)
n_missing <- sum(is.na(df$PSQI))
n_observed <- sum(!is.na(df$PSQI))

missingness_summary <- data.frame(
  Total = n_total,
  PSQI_observed = n_observed,
  PSQI_missing = n_missing,
  PSQI_missing_percent = round(100 * n_missing / n_total, 1)
)

# -------------------------------------------------------------------------
# 3. OBSERVED VERSUS MISSING PSQI COMPARISONS
# -------------------------------------------------------------------------

continuous_variables <- c(
  "Age", "BMI", "TimeSinceTransplant", "ESS", "AIS", "PCS", "MCS"
)

categorical_variables <- c(
  "Gender", "LiverDiagnosis", "Recurrence", "Rejection", "AnyFibrosis",
  "RenalFailure", "Depression", "Corticosteroid", "Berlin"
)

continuous_rows <- lapply(continuous_variables, function(variable) {
  observed <- df[df$PSQI_missing == "Observed", variable]
  missing <- df[df$PSQI_missing == "Missing", variable]

  test <- t.test(observed, missing)

  data.frame(
    Variable = variable,
    Level = "",
    PSQI_observed = sprintf(
      "%.2f (%.2f); n=%d",
      mean(observed, na.rm = TRUE),
      sd(observed, na.rm = TRUE),
      sum(!is.na(observed))
    ),
    PSQI_missing = sprintf(
      "%.2f (%.2f); n=%d",
      mean(missing, na.rm = TRUE),
      sd(missing, na.rm = TRUE),
      sum(!is.na(missing))
    ),
    P_value = test$p.value,
    Test = "Welch two-sample t-test"
  )
})

categorical_rows <- lapply(categorical_variables, function(variable) {
  complete <- !is.na(df[[variable]])
  tab <- table(df[[variable]][complete], df$PSQI_missing[complete])

  expected <- suppressWarnings(chisq.test(tab)$expected)
  use_fisher <- any(expected < 5)
  test <- if (use_fisher) fisher.test(tab) else chisq.test(tab, correct = FALSE)

  levels_present <- rownames(tab)

  do.call(rbind, lapply(seq_along(levels_present), function(index) {
    level <- levels_present[index]
    observed_n <- tab[index, "Observed"]
    missing_n <- tab[index, "Missing"]

    data.frame(
      Variable = variable,
      Level = level,
      PSQI_observed = sprintf(
        "%d (%.1f%%)",
        observed_n,
        100 * observed_n / sum(tab[, "Observed"])
      ),
      PSQI_missing = sprintf(
        "%d (%.1f%%)",
        missing_n,
        100 * missing_n / sum(tab[, "Missing"])
      ),
      P_value = if (index == 1) test$p.value else NA_real_,
      Test = if (index == 1) {
        if (use_fisher) "Fisher's exact test" else "Pearson chi-square test"
      } else {
        ""
      }
    )
  }))
})

comparison_table <- rbind(
  do.call(rbind, continuous_rows),
  do.call(rbind, categorical_rows)
)

comparison_table$P_value <- ifelse(
  is.na(comparison_table$P_value),
  NA,
  round(comparison_table$P_value, 4)
)

# -------------------------------------------------------------------------
# 4. MULTIVARIABLE MODEL OF PSQI MISSINGNESS
# -------------------------------------------------------------------------

df$PSQI_missing_binary <- as.integer(df$PSQI_missing == "Missing")

df$Gender_f <- factor(
  df$Gender,
  levels = c(1, 2),
  labels = c("Male", "Female")
)
df$LiverDiagnosis_f <- factor(
  df$LiverDiagnosis,
  levels = c(1, 2, 3, 4, 5),
  labels = c("HepC", "HepB", "PSC_PBC_AHA", "Alcohol", "Other")
)
df$Recurrence_f <- factor(df$Recurrence, levels = c(0, 1), labels = c("No", "Yes"))
df$Rejection_f <- factor(df$Rejection, levels = c(0, 1), labels = c("No", "Yes"))
df$AnyFibrosis_f <- factor(df$AnyFibrosis, levels = c(0, 1), labels = c("No", "Yes"))
df$Depression_f <- factor(df$Depression, levels = c(0, 1), labels = c("No", "Yes"))
df$Corticosteroid_f <- factor(
  df$Corticosteroid,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

missingness_model <- glm(
  PSQI_missing_binary ~ Age + Gender_f + BMI + TimeSinceTransplant +
    LiverDiagnosis_f + Recurrence_f + Rejection_f + AnyFibrosis_f +
    Depression_f + Corticosteroid_f,
  data = df,
  family = binomial
)

model_coefficients <- summary(missingness_model)$coefficients
model_ci <- suppressMessages(confint(missingness_model))

missingness_model_results <- data.frame(
  Term = rownames(model_coefficients),
  Odds_ratio = exp(model_coefficients[, "Estimate"]),
  CI_low = exp(model_ci[, 1]),
  CI_high = exp(model_ci[, 2]),
  P_value = model_coefficients[, "Pr(>|z|)"],
  row.names = NULL
)

numeric_columns <- c("Odds_ratio", "CI_low", "CI_high", "P_value")
missingness_model_results[numeric_columns] <- lapply(
  missingness_model_results[numeric_columns],
  function(x) round(x, 4)
)

model_information <- data.frame(
  Metric = c(
    "Participants in full dataset",
    "Participants in missingness model",
    "Participants excluded because of missing covariates",
    "PSQI missing events in model"
  ),
  Value = c(
    nrow(df),
    nobs(missingness_model),
    nrow(df) - nobs(missingness_model),
    sum(model.response(model.frame(missingness_model)))
  )
)

# -------------------------------------------------------------------------
# 5. INVERSE-PROBABILITY-WEIGHTED PSQI SENSITIVITY ANALYSIS
# -------------------------------------------------------------------------
#
# The final adjusted PSQI predictor model includes Gender, Recurrence,
# AnyFibrosis and Depression. Complete-case analysis is valid only if the
# participants with observed PSQI are sufficiently representative after
# conditioning on observed characteristics.
#
# We therefore model the probability that PSQI is OBSERVED and give greater
# weight to participants who had a lower estimated probability of having PSQI
# recorded. Liver diagnosis and corticosteroid use are included as auxiliary
# variables because the broader missingness model above suggested that they
# may help explain PSQI availability. All variables in this weighting model
# are complete, so probabilities can be estimated for all 268 participants.

df$PSQI_observed_binary <- as.integer(!is.na(df$PSQI))

observation_model <- glm(
  PSQI_observed_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f +
    Depression_f + LiverDiagnosis_f + Corticosteroid_f,
  data = df,
  family = binomial
)

df$PSQI_observation_probability <- predict(
  observation_model,
  type = "response"
)

# Stabilized inverse-probability-of-observation weights have an average close
# to 1 and usually behave better than unstabilized weights. Multiplying every
# weight by the same stabilizing constant does not change the fitted
# coefficients.
probability_observed <- mean(df$PSQI_observed_binary)
df$PSQI_weight <- ifelse(
  df$PSQI_observed_binary == 1,
  probability_observed / df$PSQI_observation_probability,
  NA_real_
)

psqi_analysis_data <- df[df$PSQI_observed_binary == 1, ]

stopifnot(all(is.finite(psqi_analysis_data$PSQI_weight)))
stopifnot(all(psqi_analysis_data$PSQI_weight > 0))

final_psqi_formula <- PSQI_binary ~
  Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f

# Original complete-case model from the predictor analysis.
psqi_complete_case_model <- glm(
  final_psqi_formula,
  data = psqi_analysis_data,
  family = binomial
)

# Weighted version of the same model. A quasibinomial family avoids treating
# the non-integer survey-style weights as literal binomial trial counts.
psqi_weighted_model <- glm(
  final_psqi_formula,
  data = psqi_analysis_data,
  weights = PSQI_weight,
  family = quasibinomial
)

# Summarize whether any weights are unusually large and calculate the
# effective sample size after weighting.
weight_diagnostics <- data.frame(
  Metric = c(
    "Minimum stabilized weight",
    "Mean stabilized weight",
    "Maximum stabilized weight",
    "Effective sample size"
  ),
  Value = c(
    min(psqi_analysis_data$PSQI_weight),
    mean(psqi_analysis_data$PSQI_weight),
    max(psqi_analysis_data$PSQI_weight),
    sum(psqi_analysis_data$PSQI_weight)^2 /
      sum(psqi_analysis_data$PSQI_weight^2)
  )
)
weight_diagnostics$Value <- round(weight_diagnostics$Value, 3)

# Non-parametric bootstrap confidence intervals account for uncertainty in
# both the observation model and the weighted outcome model without requiring
# an additional R package.
set.seed(1859)
n_bootstrap <- 1000
coefficient_names <- names(coef(psqi_weighted_model))

bootstrap_coefficients <- matrix(
  NA_real_,
  nrow = n_bootstrap,
  ncol = length(coefficient_names),
  dimnames = list(NULL, coefficient_names)
)

for (bootstrap_index in seq_len(n_bootstrap)) {
  sampled_rows <- sample.int(nrow(df), replace = TRUE)
  bootstrap_data <- df[sampled_rows, ]

  bootstrap_observation_model <- try(
    glm(
      PSQI_observed_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f +
        Depression_f + LiverDiagnosis_f + Corticosteroid_f,
      data = bootstrap_data,
      family = binomial
    ),
    silent = TRUE
  )

  if (inherits(bootstrap_observation_model, "try-error")) {
    next
  }

  bootstrap_data$PSQI_observation_probability <- suppressWarnings(
    predict(
      bootstrap_observation_model,
      newdata = bootstrap_data,
      type = "response"
    )
  )

  bootstrap_probability_observed <- mean(
    bootstrap_data$PSQI_observed_binary
  )
  bootstrap_data$PSQI_weight <- ifelse(
    bootstrap_data$PSQI_observed_binary == 1,
    bootstrap_probability_observed /
      bootstrap_data$PSQI_observation_probability,
    NA_real_
  )

  bootstrap_psqi_data <- bootstrap_data[
    bootstrap_data$PSQI_observed_binary == 1,
  ]

  bootstrap_weighted_model <- try(
    suppressWarnings(
      glm(
        final_psqi_formula,
        data = bootstrap_psqi_data,
        weights = PSQI_weight,
        family = quasibinomial
      )
    ),
    silent = TRUE
  )

  if (inherits(bootstrap_weighted_model, "try-error")) {
    next
  }

  bootstrap_coefficients[bootstrap_index, ] <- coef(
    bootstrap_weighted_model
  )[coefficient_names]
}

valid_bootstrap_samples <- sum(complete.cases(bootstrap_coefficients))
stopifnot(valid_bootstrap_samples >= 0.95 * n_bootstrap)

weighted_confidence_intervals <- t(
  apply(
    bootstrap_coefficients,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

complete_case_coefficients <- summary(
  psqi_complete_case_model
)$coefficients
complete_case_confidence_intervals <- suppressMessages(
  confint(psqi_complete_case_model)
)

term_labels <- c(
  "(Intercept)" = "Intercept",
  "Gender_fFemale" = "Female vs Male",
  "Recurrence_fYes" = "Recurrence: Yes vs No",
  "AnyFibrosis_fYes" = "Any fibrosis: Yes vs No",
  "Depression_fYes" = "Depression: Yes vs No"
)

weighting_comparison <- data.frame(
  Predictor = unname(term_labels[coefficient_names]),
  Complete_case_OR = exp(coef(psqi_complete_case_model)),
  Complete_case_CI_low = exp(
    complete_case_confidence_intervals[coefficient_names, 1]
  ),
  Complete_case_CI_high = exp(
    complete_case_confidence_intervals[coefficient_names, 2]
  ),
  Complete_case_P_value = complete_case_coefficients[
    coefficient_names,
    "Pr(>|z|)"
  ],
  Weighted_OR = exp(coef(psqi_weighted_model)),
  Weighted_bootstrap_CI_low = exp(
    weighted_confidence_intervals[coefficient_names, 1]
  ),
  Weighted_bootstrap_CI_high = exp(
    weighted_confidence_intervals[coefficient_names, 2]
  ),
  Percent_change_in_OR = 100 * (
    exp(coef(psqi_weighted_model)) -
      exp(coef(psqi_complete_case_model))
  ) / exp(coef(psqi_complete_case_model)),
  row.names = NULL
)

weighting_comparison[, -1] <- lapply(
  weighting_comparison[, -1],
  function(x) round(x, 4)
)

# -------------------------------------------------------------------------
# 6. SIMPLE MISSINGNESS FIGURE
# -------------------------------------------------------------------------

png(
  file.path(output_dir, "psqi_missingness_barplot.png"),
  width = 1800,
  height = 1200,
  res = 200
)

barplot(
  c(Observed = n_observed, Missing = n_missing),
  col = c("#4C78A8", "#E45756"),
  ylab = "Number of participants",
  main = "Availability of PSQI scores",
  ylim = c(0, 210)
)

text(
  x = c(0.7, 1.9),
  y = c(n_observed, n_missing) + 8,
  labels = c(
    sprintf("%d (%.1f%%)", n_observed, 100 * n_observed / n_total),
    sprintf("%d (%.1f%%)", n_missing, 100 * n_missing / n_total)
  )
)

dev.off()

# -------------------------------------------------------------------------
# 7. CONSOLE SUMMARY
# -------------------------------------------------------------------------

cat("\nPSQI MISSINGNESS SUMMARY\n")
print(missingness_summary)

cat("\nPSQI OBSERVED VERSUS MISSING COMPARISONS\n")
print(comparison_table, row.names = FALSE)

cat("\nMULTIVARIABLE MODEL OF PSQI MISSINGNESS\n")
print(missingness_model_results, row.names = FALSE)

cat("\nMODEL INFORMATION\n")
print(model_information, row.names = FALSE)

cat("\nIPW WEIGHT DIAGNOSTICS\n")
print(weight_diagnostics, row.names = FALSE)

cat(
  "\nCOMPLETE-CASE VERSUS WEIGHTED FINAL PSQI PREDICTOR MODEL\n",
  "Bootstrap samples used: ", valid_bootstrap_samples, "\n",
  sep = ""
)
print(weighting_comparison, row.names = FALSE)
