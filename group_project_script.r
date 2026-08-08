# ==============================================================================
# BTC1859H TEAM PROJECT — ANALYSIS SCRIPT
# ==============================================================================

# ==============================================================================
# SECTION 1 — DATA CLEANING
# ==============================================================================

# ============================================================
# CLEANED PROJECT DATASET
# ============================================================

# ------------------------------------------------------------
# 1. IMPORT THE ORIGINAL DATASET
# ------------------------------------------------------------

project_data <- read.csv(
  "project_data.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "Unknown")
)


# ------------------------------------------------------------
# 2. KEEP ONLY THE VARIABLES NEEDED FOR THE PROJECT
# ------------------------------------------------------------

clean_data <- project_data[, c(
  "Subject",
  "Gender",
  "Age",
  "BMI",
  "Time.from.transplant",
  "Liver.Diagnosis",
  "Recurrence.of.disease",
  "Rejection.graft.dysfunction",
  "Any.fibrosis",
  "Renal.Failure",
  "Depression",
  "Corticoid",
  "Epworth.Sleepiness.Scale",
  "Pittsburgh.Sleep.Quality.Index.Score",
  "Athens.Insomnia.Scale",
  "Berlin.Sleepiness.Scale",
  "SF36.PCS",
  "SF36.MCS"
)]


# ------------------------------------------------------------
# 3. RENAME VARIABLES
# ------------------------------------------------------------

names(clean_data)[names(clean_data) == "Time.from.transplant"] <-
  "TimeSinceTransplant"

names(clean_data)[names(clean_data) == "Liver.Diagnosis"] <-
  "LiverDiagnosis"

names(clean_data)[names(clean_data) == "Recurrence.of.disease"] <-
  "Recurrence"

names(clean_data)[names(clean_data) == "Rejection.graft.dysfunction"] <-
  "Rejection"

names(clean_data)[names(clean_data) == "Any.fibrosis"] <-
  "AnyFibrosis"

names(clean_data)[names(clean_data) == "Renal.Failure"] <-
  "RenalFailure"

names(clean_data)[names(clean_data) == "Corticoid"] <-
  "Corticosteroid"

names(clean_data)[names(clean_data) == "Epworth.Sleepiness.Scale"] <-
  "ESS"

names(clean_data)[names(clean_data) ==
                    "Pittsburgh.Sleep.Quality.Index.Score"] <-
  "PSQI"

names(clean_data)[names(clean_data) == "Athens.Insomnia.Scale"] <-
  "AIS"

names(clean_data)[names(clean_data) == "Berlin.Sleepiness.Scale"] <-
  "Berlin"

names(clean_data)[names(clean_data) == "SF36.PCS"] <-
  "PCS"

names(clean_data)[names(clean_data) == "SF36.MCS"] <-
  "MCS"


# ------------------------------------------------------------
# 4. CONVERT BLANKS AND TEXT MISSING VALUES TO NA
# ------------------------------------------------------------

clean_data[clean_data == ""] <- NA
clean_data[clean_data == "NA"] <- NA
clean_data[clean_data == "Unknown"] <- NA


# ------------------------------------------------------------
# 5. CONVERT VARIABLES TO NUMERIC
# ------------------------------------------------------------

numeric_variables <- c(
  "Subject",
  "Gender",
  "Age",
  "BMI",
  "TimeSinceTransplant",
  "LiverDiagnosis",
  "Recurrence",
  "Rejection",
  "AnyFibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid",
  "ESS",
  "PSQI",
  "AIS",
  "Berlin",
  "PCS",
  "MCS"
)

clean_data[numeric_variables] <- lapply(
  clean_data[numeric_variables],
  function(x) as.numeric(as.character(x))
)


# ------------------------------------------------------------
# 6. CHECK AND REPLACE INVALID VALUES
# ------------------------------------------------------------

# ESS valid range: 0 to 24
invalid_ESS <- !is.na(clean_data$ESS) &
  (clean_data$ESS < 0 | clean_data$ESS > 24)

clean_data$ESS[invalid_ESS] <- NA


# PSQI valid range: 0 to 21
invalid_PSQI <- !is.na(clean_data$PSQI) &
  (clean_data$PSQI < 0 | clean_data$PSQI > 21)

clean_data$PSQI[invalid_PSQI] <- NA


# AIS valid range: 0 to 24
invalid_AIS <- !is.na(clean_data$AIS) &
  (clean_data$AIS < 0 | clean_data$AIS > 24)

clean_data$AIS[invalid_AIS] <- NA


# Berlin should only contain 0 or 1
invalid_Berlin <- !is.na(clean_data$Berlin) &
  !(clean_data$Berlin %in% c(0, 1))

clean_data$Berlin[invalid_Berlin] <- NA


# ------------------------------------------------------------
# 7. CREATE BINARY SLEEP VARIABLES
# ------------------------------------------------------------

# ESS greater than 10 indicates excessive daytime sleepiness
clean_data$ESS_binary <- ifelse(
  is.na(clean_data$ESS),
  NA,
  ifelse(clean_data$ESS > 10, 1, 0)
)


# PSQI greater than 4 indicates poor sleep quality
clean_data$PSQI_binary <- ifelse(
  is.na(clean_data$PSQI),
  NA,
  ifelse(clean_data$PSQI > 4, 1, 0)
)


# AIS greater than 5 indicates insomnia symptoms
clean_data$AIS_binary <- ifelse(
  is.na(clean_data$AIS),
  NA,
  ifelse(clean_data$AIS > 5, 1, 0)
)


# Berlin was already binary
clean_data$Berlin_binary <- clean_data$Berlin


# ------------------------------------------------------------
# 8. CHECK THE CLEANED DATASET
# ------------------------------------------------------------

dim(clean_data)
names(clean_data)
summary(clean_data)

# Check missing values
colSums(is.na(clean_data))

# Check ESS after removing invalid values
range(clean_data$ESS, na.rm = TRUE)

# Confirm binary variables only contain 0, 1, or NA
table(clean_data$ESS_binary, useNA = "ifany")
table(clean_data$PSQI_binary, useNA = "ifany")
table(clean_data$AIS_binary, useNA = "ifany")
table(clean_data$Berlin_binary, useNA = "ifany")


# ------------------------------------------------------------
# 9. OPTIONAL: SAVE A COPY OF THE CLEANED DATASET
# ------------------------------------------------------------

write.csv(
  clean_data,
  "project_data_cleaned.csv",
  row.names = FALSE,
  na = "NA"
)

# ==============================================================================
# SECTION 2 — DESCRIPTIVE STATISTICS
# ==============================================================================

# ============================================================
# DESCRIPTIVE STATISTICS 
# File: project_data_cleaned.csv
# ============================================================


# ============================================================
# DESCRIPTIVE STATISTICS FOR THE CLEANED PROJECT DATASET
# ============================================================
# Descriptive statistics were calculated to summarize the characteristics of 
# the study population before conducting inferential analyses. For continuous 
# variables (Age, BMI, Time Since Transplant, ESS, PSQI, AIS, PCS, and MCS), 
# the number of observations (N), number of missing values, mean, standard 
# deviation (SD), median, first quartile (Q1), third quartile (Q3), and minimum 
# and maximum values were reported to describe the central tendency and 
# variability of the data. For categorical variables (Gender, Liver Diagnosis, 
# Recurrence, Rejection, Any Fibrosis, Renal Failure, Depression, Corticosteroid 
# Use, and Berlin Sleep Apnea Risk), frequencies and percentages were calculated 
# to summarize the distribution of participants across each category. In 
# addition, the prevalence of each sleep disturbance was calculated using the 
# established clinical cut-off values, and a missing data summary was produced 
# to describe the completeness of the dataset for each variable. These 
# descriptive statistics provide an overview of the study sample and the key 
# variables prior to subsequent regression analyses.

# ------------------------------------------------------------
# 1. USE THE CLEANED DATAFRAME CREATED ABOVE
# ------------------------------------------------------------

# clean_data is carried forward directly from the data-cleaning section.
# No CSV re-import is needed, so all cleaned values and variable classes remain
# exactly as produced by the cleaning code.

# Check the dataset
dim(clean_data)
names(clean_data)
summary(clean_data)


# ============================================================
# TABLE 1A: CONTINUOUS DESCRIPTIVE STATISTICS
# ============================================================


# Select continuous variables
continuous_variables <- c(
  "Age",
  "BMI",
  "TimeSinceTransplant",
  "ESS",
  "PSQI",
  "AIS",
  "PCS",
  "MCS"
)


# Function to calculate descriptive statistics
continuous_summary <- function(x) {
  
  data.frame(
    N = sum(!is.na(x)),
    Missing = sum(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    Q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
}


# Calculate statistics for each variable
continuous_table <- do.call(
  rbind,
  lapply(clean_data[continuous_variables], continuous_summary)
)


# Add variable names
continuous_table$Variable <- rownames(continuous_table)
rownames(continuous_table) <- NULL


# Reorder columns
continuous_table <- continuous_table[, c(
  "Variable",
  "N",
  "Missing",
  "Mean",
  "SD",
  "Median",
  "Q1",
  "Q3",
  "Minimum",
  "Maximum"
)]


# Round statistics
continuous_table[, c(
  "Mean",
  "SD",
  "Median",
  "Q1",
  "Q3",
  "Minimum",
  "Maximum"
)] <- round(
  continuous_table[, c(
    "Mean",
    "SD",
    "Median",
    "Q1",
    "Q3",
    "Minimum",
    "Maximum"
  )],
  2
)


# View continuous descriptive statistics
print(continuous_table)


# Save continuous table
write.csv(
  continuous_table,
  "Table1A_Continuous_Descriptive_Statistics.csv",
  row.names = FALSE
)


# ============================================================
# TABLE 1B: CATEGORICAL DESCRIPTIVE STATISTICS
# ============================================================


# Select categorical variables
categorical_variables <- c(
  "Gender",
  "LiverDiagnosis",
  "Recurrence",
  "Rejection",
  "AnyFibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid",
  "Berlin"
)


# Function to calculate counts and percentages
categorical_summary <- function(data, variable) {
  
  observed_values <- data[[variable]][!is.na(data[[variable]])]
  
  counts <- table(observed_values)
  
  percentages <- prop.table(counts) * 100
  
  data.frame(
    Variable = variable,
    Category = names(counts),
    N = as.numeric(counts),
    Percent = round(as.numeric(percentages), 1),
    Missing = sum(is.na(data[[variable]]))
  )
}


# Calculate categorical statistics
categorical_table <- do.call(
  rbind,
  lapply(
    categorical_variables,
    function(variable) {
      categorical_summary(clean_data, variable)
    }
  )
)


# View categorical descriptive statistics
print(categorical_table)


# Save categorical table
write.csv(
  categorical_table,
  "Table1B_Categorical_Descriptive_Statistics.csv",
  row.names = FALSE
)


# ============================================================
# TABLE 2: PREVALENCE OF SLEEP DISTURBANCES
# ============================================================


# Select binary sleep variables
sleep_binary_variables <- c(
  "ESS_binary",
  "PSQI_binary",
  "AIS_binary",
  "Berlin_binary"
)


# Function to calculate prevalence and 95% confidence intervals
prevalence_summary <- function(data, variable) {
  
  x <- data[[variable]]
  
  observed <- x[!is.na(x)]
  
  total_n <- length(observed)
  
  cases <- sum(observed == 1)
  
  prevalence <- cases / total_n
  
  confidence_interval <- prop.test(
    cases,
    total_n,
    correct = FALSE
  )$conf.int
  
  data.frame(
    SleepMeasure = variable,
    Cases = cases,
    TotalObserved = total_n,
    Missing = sum(is.na(x)),
    PrevalencePercent = round(prevalence * 100, 1),
    Lower95CI = round(confidence_interval[1] * 100, 1),
    Upper95CI = round(confidence_interval[2] * 100, 1)
  )
}


# Calculate prevalence
prevalence_table <- do.call(
  rbind,
  lapply(
    sleep_binary_variables,
    function(variable) {
      prevalence_summary(clean_data, variable)
    }
  )
)


# Replace variable names with clearer labels
prevalence_table$SleepMeasure <- c(
  "Excessive daytime sleepiness (ESS > 10)",
  "Poor sleep quality (PSQI > 4)",
  "Insomnia symptoms (AIS > 5)",
  "High risk of sleep apnea (Berlin)"
)


# View prevalence table
print(prevalence_table)


# Save prevalence table
write.csv(
  prevalence_table,
  "Table2_Sleep_Disturbance_Prevalence.csv",
  row.names = FALSE
)


# ============================================================
# TABLE 3: MISSING DATA SUMMARY
# ============================================================


missing_table <- data.frame(
  Variable = names(clean_data),
  MissingN = colSums(is.na(clean_data)),
  MissingPercent = round(
    colSums(is.na(clean_data)) / nrow(clean_data) * 100,
    1
  )
)


# Keep only variables with at least one missing value
missing_table <- missing_table[
  missing_table$MissingN > 0,
]


# View missing data table
print(missing_table)


# Save missing data table
write.csv(
  missing_table,
  "Table3_Missing_Data_Summary.csv",
  row.names = FALSE
)


# ============================================================
# SINGLE TABLE FOR REPORT
# ============================================================


# Create formatted continuous results as mean ± SD
continuous_report <- data.frame(
  Variable = continuous_table$Variable,
  Category = "",
  Result = paste0(
    continuous_table$Mean,
    " ± ",
    continuous_table$SD
  ),
  Missing = continuous_table$Missing
)


# Create formatted categorical results as n (%)
categorical_report <- data.frame(
  Variable = categorical_table$Variable,
  Category = categorical_table$Category,
  Result = paste0(
    categorical_table$N,
    " (",
    categorical_table$Percent,
    "%)"
  ),
  Missing = categorical_table$Missing
)


# Combine continuous and categorical results
table1_report <- rbind(
  continuous_report,
  categorical_report
)


# View combined Table 1
print(table1_report)


# Save combined Table 1
write.csv(
  table1_report,
  "Table1_Combined_Participant_Characteristics.csv",
  row.names = FALSE
)


# ============================================================
# FINAL CHECKS
# ============================================================


# Total number of participants
nrow(clean_data)


# ESS valid and missing observations
sum(!is.na(clean_data$ESS))
sum(is.na(clean_data$ESS))


# Check current ESS range
range(clean_data$ESS, na.rm = TRUE)


# Check binary sleep-variable distributions
table(clean_data$ESS_binary, useNA = "ifany")
table(clean_data$PSQI_binary, useNA = "ifany")
table(clean_data$AIS_binary, useNA = "ifany")
table(clean_data$Berlin_binary, useNA = "ifany")

# ==============================================================================
# SECTION 3 — REGRESSION AND QUALITY-OF-LIFE ANALYSES
# ==============================================================================

## BTC1859 - Team 1
# Predictors of Sleep Disturbance (logistic regression) + SF36 QoL (linear regression)
# Merged script - namespaced to avoid collisions between the two analyses.
# Naming convention: everything in the sleep-disturbance analysis uses a
# "_sleep" or "sleep_" prefix/dataframe (df_sleep); everything in the QoL
# analysis uses a "_qol"/"qol_" prefix/dataframe (df_qol). The two halves
# begin from separate copies of the same cleaned dataframe, so neither
# section can silently overwrite the other's analysis data.

# ---------------------------------------------------------------
# SETUP: packages (all imports up front, used by BOTH halves of this script)
# ---------------------------------------------------------------

required_packages <- c("ggplot2", "car", "dplyr", "stringr", "broom", "knitr", "MASS")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
# patchwork is optional (only used for one combined 3x2 scatter figure) and is
# loaded conditionally further down via requireNamespace(), so it is not
# required here.

# =================================================================
# ANALYSIS 1: PREDICTORS OF SLEEP DISTURBANCE (logistic regression)
# All objects in this analysis use df_sleep and are prefixed sleep_/_sleep
# where a name might otherwise be ambiguous.
# =================================================================

# ---------------------------------------------------------------
# 0. CREATE AN ANALYSIS COPY OF THE CLEANED DATAFRAME
# ---------------------------------------------------------------

df_sleep <- clean_data

# Recode factors with meaningful labels (per data dictionary)
df_sleep$Gender_f          <- factor(df_sleep$Gender, levels=c(1,2),
                                     labels=c("Male","Female"))
df_sleep$LiverDiagnosis_f  <- factor(df_sleep$LiverDiagnosis, levels=c(1,2,3,4,5),
                                     labels=c("HepC","HepB","PSC_PBC_AHA","Alcohol","Other"))
df_sleep$LiverDiagnosis_f  <- relevel(df_sleep$LiverDiagnosis_f, ref="PSC_PBC_AHA")  # largest group as reference

df_sleep$Recurrence_f      <- factor(df_sleep$Recurrence,     levels=c(0,1),
                                     labels=c("No","Yes"))
df_sleep$Rejection_f       <- factor(df_sleep$Rejection,      levels=c(0,1),
                                     labels=c("No","Yes"))
df_sleep$AnyFibrosis_f     <- factor(df_sleep$AnyFibrosis,    levels=c(0,1),
                                     labels=c("No","Yes"))
df_sleep$RenalFailure_f    <- factor(df_sleep$RenalFailure,   levels=c(0,1),
                                     labels=c("No","Yes"))
df_sleep$Depression_f      <- factor(df_sleep$Depression,     levels=c(0,1),
                                     labels=c("No","Yes"))
df_sleep$Corticosteroid_f  <- factor(df_sleep$Corticosteroid, levels=c(0,1),
                                     labels=c("No","Yes"))

# The four binary sleep disturbance outcomes (done during cleaning)
# ESS_binary   : ESS > 10   (excessive daytime sleepiness)
# PSQI_binary  : PSQI > 4   (poor sleep quality)
# AIS_binary   : AIS > 5    (insomnia)
# Berlin_binary: Berlin scale

# ---------------------------------------------------------------
# 1. PREVALENCE OF SLEEP DISTURBANCE
# ---------------------------------------------------------------

prevalence <- function(x) {
  n_valid <- sum(!is.na(x))
  n_pos   <- sum(x, na.rm=TRUE)
  ci <- prop.test(n_pos, n_valid)$conf.int   # Wilson score interval (with continuity correction)
  c(n_valid=n_valid, n_positive=n_pos,
    prevalence_pct=round(100*n_pos/n_valid,1),
    CI_low_pct=round(100*ci[1],1), CI_high_pct=round(100*ci[2],1))
}

sapply(df_sleep[,c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")], prevalence)

# NOTE: PSQI has substantially more missing data (~32%) than the other three
# instruments (~2-6%). This should be flagged as a limitation - any PSQI-based
# model runs on a smaller and possibly non-random subsample.

# ---------------------------------------------------------------
# 2. THE RENAL FAILURE ISSUE (only 4/268 patients)
# ---------------------------------------------------------------

for (out in c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")) {
  cat("\n---", out, "vs RenalFailure ---\n")
  tab <- table(df_sleep$RenalFailure_f, df_sleep[[out]])
  print(tab)
  print(fisher.test(tab))
}

# All four Fisher's exact tests are non-significant (p = 0.16-0.63).
# Including RenalFailure as a predictor in the logistic models causes
# (quasi-)complete separation for ESS, PSQI, and Berlin (all 4 renal-failure
# patients fall in a single outcome category for those three measures) -
# glm() will throw "fitted probabilities numerically 0 or 1 occurred" and
# produce a degenerate OR/CI. RenalFailure is therefore EXCLUDED from the
# multivariable models below and reported descriptively only. Suggested
# write-up language:
# "Renal failure showed no significant association with any sleep
# disturbance measure (Fisher's exact p = 0.16-0.63); however, with only
# 4 renal-failure patients in the sample, these estimates are too
# imprecise to draw firm conclusions."

# ---------------------------------------------------------------
# 3. UNIVARIABLE ANALYSES
# ---------------------------------------------------------------
# Screen each predictor individually against each outcome.
# Candidates: Age, Gender, BMI, TimeSinceTransplant, LiverDiagnosis,
# Recurrence, Rejection, AnyFibrosis, Depression, Corticosteroid
# (RenalFailure handled separately above via Fisher's exact test)

sleep_outcomes   <- c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")
sleep_predictors <- c("Age","Gender_f","BMI","TimeSinceTransplant","LiverDiagnosis_f",
                      "Recurrence_f","Rejection_f","AnyFibrosis_f","Depression_f","Corticosteroid_f")

# Collect every predictor row (across all models) into one tidy data frame,
# used both for the printed table and the heatmap below
univariable_results <- data.frame()
for (out in sleep_outcomes) {
  for (pred in sleep_predictors) {
    f <- as.formula(paste(out, "~", pred))
    m <- glm(f, data=df_sleep, family=binomial)
    co <- summary(m)$coefficients
    ci <- confint(m)
    rows <- data.frame(
      outcome = out,
      term    = rownames(co)[-1],
      OR      = exp(co[-1, 1]),
      CI_low  = exp(ci[-1, 1]),
      CI_high = exp(ci[-1, 2]),
      p_value = co[-1, 4],
      row.names = NULL
    )
    cat("\n===", out, "~", pred, "===\n")
    print(round(cbind(OR=rows$OR, CI_low=rows$CI_low, CI_high=rows$CI_high, p=rows$p_value), 3))
    univariable_results <- rbind(univariable_results, rows)
  }
}

# write.csv(univariable_results, "univariable_OR_CI_p_table.csv", row.names = FALSE)

# See project notes / report Table 1 for the compiled OR/CI/p summary table
# across all predictors and all four outcomes.

# ---------------------------------------------------------------
# 3b. HEATMAP OF UNIVARIABLE ORs (ggplot2)
# ---------------------------------------------------------------
# Compact alternative view of the same univariable_results table: predictors
# as rows, outcomes as columns, cell color = OR (log scale, diverging around
# OR=1), cell label = OR value with significance stars.

heat_data <- univariable_results
heat_data$sig_label <- with(heat_data,
                            paste0(round(OR, 2),
                                   ifelse(p_value < 0.01, "**",
                                          ifelse(p_value < 0.05, "*",
                                                 ifelse(p_value < 0.10, "^", "")))))

# Add RenalFailure back in for visual completeness. It was excluded from the
# univariable loop above because 3 of 4 outcomes hit complete separation
# (see Section 2). Rather than hardcoding the OR/p/n values by hand , fit each of the
# four RenalFailure univariable models here and read the numbers off the
# fitted objects directly. Separation is detected via the actual glm()
# warning rather than an eyeballed OR threshold.
fit_renal_univariable <- function(outcome) {
  tab <- table(df_sleep$RenalFailure_f, df_sleep[[outcome]])
  separated <- any(tab == 0)   # any empty cell -> (quasi-)complete separation
  
  n <- sum(!is.na(df_sleep$RenalFailure_f) & !is.na(df_sleep[[outcome]]))
  
  if (separated) {
    return(data.frame(outcome = outcome, term = "RenalFailure_f[Yes vs No]",
                      OR = NA, CI_low = NA, CI_high = NA, p_value = NA,
                      sig_label = "n/e", n = n))
  }
  
  m  <- glm(as.formula(paste(outcome, "~ RenalFailure_f")), data = df_sleep, family = binomial)
  co <- summary(m)$coefficients
  term_name <- grep("RenalFailure", rownames(co), value = TRUE)
  
  beta <- co[term_name, "Estimate"]
  p    <- co[term_name, "Pr(>|z|)"]
  ci   <- tryCatch(confint(m)[term_name, ], error = function(e) c(NA, NA))
  OR   <- exp(beta)
  star <- ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.10, "^", "")))
  data.frame(outcome = outcome, term = "RenalFailure_f[Yes vs No]",
             OR = OR, CI_low = exp(ci[1]), CI_high = exp(ci[2]), p_value = p,
             sig_label = paste0(round(OR, 2), star), n = n)
}

renal_rows <- bind_rows(lapply(c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary"),
                               fit_renal_univariable))
heat_data <- rbind(heat_data, renal_rows[, names(heat_data)])

heat_data$outcome <- factor(heat_data$outcome,
                            levels = c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary"),
                            labels = c("ESS","PSQI","AIS","Berlin"))
# keep predictors in a sensible reading order (reverse for top-to-bottom display)
term_labels <- c(
  "Age"                       = "Age",
  "Gender_fFemale"            = "Gender (Female)",
  "BMI"                       = "BMI",
  "TimeSinceTransplant"       = "Time Since Transplant",
  "LiverDiagnosis_fHepC"      = "Liver Dx: Hep C",
  "LiverDiagnosis_fHepB"      = "Liver Dx: Hep B",
  "LiverDiagnosis_fAlcohol"   = "Liver Dx: Alcohol",
  "LiverDiagnosis_fOther"     = "Liver Dx: Other",
  "Recurrence_fYes"           = "Recurrence",
  "Rejection_fYes"            = "Rejection",
  "AnyFibrosis_fYes"          = "Any Fibrosis",
  "Depression_fYes"           = "Depression",
  "Corticosteroid_fYes"       = "Corticosteroid Use",
  "RenalFailure_f[Yes vs No]" = "Renal Failure"
)
heat_data$term <- term_labels[heat_data$term]

heat_data$term <- factor(heat_data$term, levels = rev(term_labels))

ggplot(heat_data, aes(x = outcome, y = term, fill = log(OR))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sig_label), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       na.value = "#D9D9D9",
                       breaks = log(c(0.5, 1, 2, 4)),
                       labels = c("0.5", "1", "2", "4"),
                       name = "Odds Ratio") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL,
       title = "Univariable predictors of sleep disturbance, by instrument",
       caption = paste0("** p<0.01, * p<0.05, ^ p<0.10. Renal failure (grey/'n/e' cells) could not be\n",
                        "estimated for some outcomes due to complete separation (n=",
                        sum(df_sleep$RenalFailure == 1, na.rm = TRUE), " cases) - see Section 2.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# ggsave("univariable_heatmap.png", width = 8, height = 9.5, dpi = 300)

# ---------------------------------------------------------------
# 3c. NOTE ON MULTIPLE TESTING AND VARIABLE SELECTION
# ---------------------------------------------------------------
# Section 3 ran ~10 predictor blocks x 4 outcomes (56 individual coefficients
# once LiverDiagnosis's 4 dummy levels are counted separately). At a=0.05,
# pure chance alone would be expected to produce roughly 2-3 "significant"
# results across that many tests even if no predictor were actually related
# to sleep disturbance at all. Separately, and more consequentially: Section 4
# uses these univariable p<0.20 results to decide which predictors even get
# considered for the adjusted models. That screening-then-refitting sequence
# is a well-documented source of optimistic bias - a predictor that crossed
# p<0.20 partly by chance is more likely to be carried into the adjusted
# model, where its significance may or may not hold up. This is not
# hypothetical here: Any Fibrosis for ESS moved from p=0.054 to p=0.048
# between two versions of this dataset (a one-patient difference in ESS
# missingness), and dropped out of significance entirely once adjusted for
# other predictors - exactly the kind of selection-driven instability this
# caveat is about.
#
# No formal correction (e.g., Bonferroni) is applied here. Bonferroni assumes
# independent tests, and the four outcomes are explicitly NOT independent -
# ESS, PSQI, and AIS are correlated measures of overlapping constructs (see
# the univariable heatmap), so a predictor associated with one is more likely
# than chance to also show up for another. A correction built for independent
# tests would over-correct this correlated-outcome structure and discard real
# signal along with noise (a strict Bonferroni threshold here, ~0.05/56,
# would leave only BMI->Berlin and barely Depression->PSQI standing).
#
# Findings that replicate across multiple outcomes and multiple analytic 
# approaches (e.g., Depression's association with both PSQI and AIS, 
# in both the binary and continuous-score models) should be treated as more 
# robust than single-outcome, borderline findings 
# (e.g., Any Fibrosis for ESS, p~0.05), which may reflect sampling
# variability in a moderate sample (n=183-262) rather than a true effect.

# ---------------------------------------------------------------
# 4. MULTIVARIABLE MODELS
# ---------------------------------------------------------------

# Predictor sets were chosen by: (a) univariable screening at p<0.20, then
# (b) trimming to respect the rule-of-thumb sample size restriction
# (p < m/15, where m = size of the smaller outcome class)
# RenalFailure excluded from all four models (see Section 2).

## --- Model 1: ESS ---
# m/15 budget ~4 predictors (m=66, smaller class among n=250 - updated after
# the data refresh that shifted ESS's missingness from 17 to 18
# LiverDiagnosis collapsed to Hep C vs Other (1 df instead of 4) to fit budget,
# since only the Hep C level was significant univariably.
df_sleep$LiverDx_HepC <- factor(ifelse(df_sleep$LiverDiagnosis==1,"HepC","Other"))

ess_model <- glm(ESS_binary ~ Gender_f + LiverDx_HepC + Recurrence_f + Rejection_f,
                 data=df_sleep, family=binomial)
summary(ess_model)
round(cbind(OR=exp(coef(ess_model)), exp(confint(ess_model))), 3)
vif(ess_model)

# Compare against the larger "fully screened" model (all p<0.20 predictors,
# before budget trimming) to justify the simpler model:
ess_full <- glm(ESS_binary ~ Gender_f + LiverDiagnosis_f + Recurrence_f + Rejection_f +
                  AnyFibrosis_f + Depression_f + Corticosteroid_f,
                data=df_sleep, family=binomial)
anova(ess_model, ess_full, test="Chisq")   # non-significant -> simpler model preferred

# ESS: full p<0.20-screened candidate set (using the full 4-level LiverDiagnosis_f,
# not the ess_model's collapsed HepC-vs-Other version, so AIC can decide for itself
# whether to keep it whole, drop it, or - unlike the primary model - it can only
# keep/drop the whole factor, not collapse individual levels)
ess_stepwise_subset <- df_sleep[complete.cases(df_sleep[, c("ESS_binary","Gender_f","LiverDiagnosis_f",
                                                            "Recurrence_f","Rejection_f","AnyFibrosis_f",
                                                            "Depression_f","Corticosteroid_f")]), ]
ess_full_forstep <- glm(ESS_binary ~ Gender_f + LiverDiagnosis_f + Recurrence_f + Rejection_f +
                          AnyFibrosis_f + Depression_f + Corticosteroid_f,
                        data = ess_stepwise_subset, family = binomial)
ess_step <- stepAIC(ess_full_forstep, direction = "both", trace = TRUE)
summary(ess_step)

## --- Model 2: PSQI ---
# m/15 budget ~4 predictors (m=66, smaller class among n=183)
psqi_model <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                  data=df_sleep, family=binomial)
summary(psqi_model)
round(cbind(OR=exp(coef(psqi_model)), exp(confint(psqi_model))), 3)
vif(psqi_model)

psqi_full <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                 data=df_sleep, family=binomial)

# NOTE: psqi_model above is fit on n=183 (only limited by PSQI_binary's own
# missingness, since Gender/Recurrence/AnyFibrosis/Depression have no NAs).
# psqi_full adds BMI, which has 23 missing values, dropping n to 165.
# anova() requires both models fit on the IDENTICAL set of rows,
# so the reduced model must be refit on psqi_full's subset before comparing -
# comparing psqi_model (n=183) directly against psqi_full (n=165) would error.
psqi_subset <- df_sleep[complete.cases(df_sleep[, c("PSQI_binary","Gender_f","Recurrence_f",
                                                    "AnyFibrosis_f","Depression_f","BMI")]), ]
psqi_model_samesub <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                          data=psqi_subset, family=binomial)
psqi_full_samesub  <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                          data=psqi_subset, family=binomial)
anova(psqi_model_samesub, psqi_full_samesub, test="Chisq")  # BMI does not improve fit significantly, n=165 for both

# PSQI example - pre-subset to the shared complete-case set FIRST,
# same fix as the psqi_model/psqi_full anova() comparison, to avoid
# stepAIC() comparing models fit on different samples as BMI drops in/out
psqi_stepwise_subset <- df_sleep[complete.cases(df_sleep[, c("PSQI_binary","Gender_f","Recurrence_f",
                                                             "AnyFibrosis_f","Depression_f","BMI")]), ]
psqi_full_forstep <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                         data = psqi_stepwise_subset, family = binomial)
psqi_step <- stepAIC(psqi_full_forstep, direction = "both", trace = TRUE)
summary(psqi_step)

## --- Model 3: AIS ---
# m/15 budget ~7-8 predictors (m=117, smaller class among n=262)
ais_model <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                 data=df_sleep, family=binomial)
summary(ais_model)
round(cbind(OR=exp(coef(ais_model)), exp(confint(ais_model))), 3)
vif(ais_model)

ais_full <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f +
                  Corticosteroid_f, data=df_sleep, family=binomial)
anova(ais_model, ais_full, test="Chisq")   # Corticosteroid does not improve fit significantly

# Age (p=0.154) and TimeSinceTransplant (p=0.124) both passed the p<0.20
# univariable screen for AIS but were never added to ais_full or tested via
# anova() - unlike BMI (PSQI, Section 4 Model 2) and Corticosteroid (AIS,
# just above), which both got an explicit "add + anova()" test. Testing
# here for consistency. ais_model is already at 4 of 7-8 available df
# (m/15 rule, m=117), so this is a sensitivity check

# Same anova() requirement as the PSQI/BMI comparison above: refit ais_model
# on the SAME complete-case subset as each expanded model before comparing,
# since Age/TimeSinceTransplant may carry their own missingness.
ais_subset_age <- df_sleep[complete.cases(df_sleep[, c("AIS_binary","LiverDiagnosis_f","Recurrence_f",
                                                       "AnyFibrosis_f","Depression_f","Age")]), ]
ais_model_samesub_age <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                             data=ais_subset_age, family=binomial)
ais_full_age <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f + Age,
                    data=ais_subset_age, family=binomial)
anova(ais_model_samesub_age, ais_full_age, test="Chisq")

ais_subset_tst <- df_sleep[complete.cases(
  df_sleep[, c("AIS_binary","LiverDiagnosis_f","Recurrence_f",
                 "AnyFibrosis_f","Depression_f","TimeSinceTransplant")]), ]
ais_model_samesub_tst <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                             data=ais_subset_tst, family=binomial)
ais_full_tst <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f + TimeSinceTransplant,
                    data=ais_subset_tst, family=binomial)
anova(ais_model_samesub_tst, ais_full_tst, test="Chisq")

ais_subset_both <- df_sleep[complete.cases(
  df_sleep[, c("AIS_binary","LiverDiagnosis_f","Recurrence_f",
                "AnyFibrosis_f","Depression_f","Age","TimeSinceTransplant")]), ]
ais_model_samesub_both <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                              data=ais_subset_both, family=binomial)
ais_full_both <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f +
                       Age + TimeSinceTransplant, data=ais_subset_both, family=binomial)
anova(ais_model_samesub_both, ais_full_both, test="Chisq")

cat("n (Age subset):", nobs(ais_model_samesub_age),
    " | n (TimeSinceTransplant subset):", nobs(ais_model_samesub_tst),
    " | n (both subset):", nobs(ais_model_samesub_both), "\n")

# AIS check - widen the scope to include everything that passed p<0.20,
# including Age/TimeSinceTransplant/Corticosteroid, pre-subset the same way
ais_stepwise_subset <- df_sleep[complete.cases(df_sleep[, c("AIS_binary","LiverDiagnosis_f","Recurrence_f",
                                                            "AnyFibrosis_f","Depression_f","Corticosteroid_f",
                                                            "Age","TimeSinceTransplant")]), ]
ais_full_forstep <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f +
                          Corticosteroid_f + Age + TimeSinceTransplant,
                        data = ais_stepwise_subset, family = binomial)
ais_step <- stepAIC(ais_full_forstep, direction = "both", trace = TRUE)
summary(ais_step)

## --- Model 4: Berlin ---
# m/15 budget: m should be computed on the sample the model actually runs on,
# not just Berlin_binary's own availability. Berlin_binary alone has n=262
# (102 positive/160 negative, m=102), but Age and BMI missingness further
# restrict the complete-case sample to n=237, where the split is 89
# positive/148 negative -> m=89, m/15=5.9 (~5-6 predictors). Still comfortably
# fits the 3 predictors used below.
berlin_model <- glm(Berlin_binary ~ Age + BMI + TimeSinceTransplant,
                    data=df_sleep, family=binomial)
summary(berlin_model)
round(cbind(OR=exp(coef(berlin_model)), exp(confint(berlin_model))), 3)
vif(berlin_model)

# Berlin: note the full p<0.20-screened set (Age, BMI, TimeSinceTransplant) is
# already identical to berlin_model's current 3 predictors - no other candidate
# passed the p<0.20 screen for Berlin, so this mainly checks whether AIC would
# trim any of the three, rather than testing a genuinely larger candidate pool
berlin_stepwise_subset <- df_sleep[complete.cases(df_sleep[, c("Berlin_binary","Age","BMI",
                                                               "TimeSinceTransplant")]), ]
berlin_full_forstep <- glm(Berlin_binary ~ Age + BMI + TimeSinceTransplant,
                           data = berlin_stepwise_subset, family = binomial)
berlin_step <- stepAIC(berlin_full_forstep, direction = "both", trace = TRUE)
summary(berlin_step)

# ---------------------------------------------------------------
# 5. SENSITIVITY ANALYSIS: CONTINUOUS SLEEP SCORES (linear regression)
# ---------------------------------------------------------------
# Uses the raw ESS/PSQI/AIS scores (not dichotomized) with the same predictor
# sets as the corresponding logistic model, to check whether dichotomization
# at the clinical cutoff is discarding useful information. Berlin has no
# continuous version (binary at source), so it is omitted here.

ess_lin  <- lm(ESS  ~ Gender_f + LiverDx_HepC + Recurrence_f + Rejection_f, data=df_sleep)
psqi_lin <- lm(PSQI ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f, data=df_sleep)
ais_lin  <- lm(AIS  ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f, data=df_sleep)

summary(ess_lin);  shapiro.test(resid(ess_lin))
summary(psqi_lin); shapiro.test(resid(psqi_lin))
summary(ais_lin);  shapiro.test(resid(ais_lin))

# Shapiro-Wilk alone only tests normality of residuals, and says nothing
# about linearity, homoscedasticity, or influential/leverage points 
# Residuals vs Fitted (linearity/homoscedasticity), 
# Normal Q-Q (normality - the visual counterpart to the
# Shapiro-Wilk test above), Scale-Location (homoscedasticity, another view),
# and Residuals vs Leverage (Cook's distance / influential cases).
continuous_sleep_models <- list(ess_lin = ess_lin, psqi_lin = psqi_lin, ais_lin = ais_lin)
for (model_name in names(continuous_sleep_models)) {
  m <- continuous_sleep_models[[model_name]]
  png(paste0("diagnostics_", model_name, ".png"), width = 800, height = 800)
  par(mfrow = c(2, 2))
  plot(m)
  dev.off()
}

# Residuals show mild non-normality (Shapiro-Wilk p<0.001 for all three) -
# common for right-skewed psychometric scores. With n=180-260, OLS estimates
# remain reasonably robust; note this as a limitation rather than a fatal flaw.
# Check the saved diagnostics_*.png files for: any curvature in Residuals vs
# Fitted (would indicate a linearity violation), funnel shapes in
# Scale-Location (heteroscedasticity), and any point with Cook's distance
# > 1 in Residuals vs Leverage (an influential case worth investigating
# individually, e.g. is it one of the 4 renal-failure patients?).
# NOTE: using the continuous ESS score, "Rejection" is significant (p=0.006)
# even though it was only borderline (p=0.068) in the binary ESS model -
# a concrete example of information lost by dichotomizing at the cutoff.

# ---------------------------------------------------------------
# 6. FOREST PLOT OF ADJUSTED ODDS RATIOS 
# ---------------------------------------------------------------

# Tidy a glm model's output (OR, 95% CI, p) into a plotting data frame,
# excluding the intercept row. The facet label's sample size is read directly
# off the fitted model (nobs()) rather than typed by hand, so it can't go
# stale if the underlying data changes later (as happened when ESS's n moved
# from 251 to 250 after a data refresh).
tidy_glm <- function(model, outcome_name) {
  co <- summary(model)$coefficients
  ci <- confint(model)
  outcome_label <- paste0(outcome_name, " (n=", nobs(model), ")")
  data.frame(
    outcome  = outcome_label,
    term     = rownames(co)[-1],
    OR       = exp(co[-1, 1]),
    conf.low = exp(ci[-1, 1]),
    conf.high= exp(ci[-1, 2]),
    p.value  = co[-1, 4],
    row.names = NULL
  )
}

forest_data <- rbind(
  tidy_glm(ess_model,    "ESS"),
  tidy_glm(psqi_model,   "PSQI"),
  tidy_glm(ais_model,    "AIS"),
  tidy_glm(berlin_model, "Berlin")
)

# Significance flag - drives the black (p<0.05) vs grey (n.s.) point/CI color
# in the plot below via aes(color = sig)
forest_data$sig <- factor(ifelse(forest_data$p.value < 0.05, "p < 0.05", "n.s."),
                          levels = c("p < 0.05", "n.s."))

# Clean, readable predictor labels (mirrors the heatmap's term_labels in
# Section 3b, extended to cover every dummy-coded term that actually appears
# across the four adjusted models - including model-specific forms like
# LiverDx_HepCOther (ESS's collapsed factor) and the full 4-level
# LiverDiagnosis_f dummies (AIS's model), which are named differently from
# each other and from the heatmap's own LiverDiagnosis_f terms)
forest_term_labels <- c(
  "Gender_fFemale"          = "Gender (Female)",
  "LiverDx_HepCOther"       = "Liver Dx: Other vs. Hep C",
  "Recurrence_fYes"         = "Recurrence",
  "Rejection_fYes"          = "Rejection",
  "AnyFibrosis_fYes"        = "Any Fibrosis",
  "Depression_fYes"         = "Depression",
  "LiverDiagnosis_fHepC"    = "Liver Dx: Hep C",
  "LiverDiagnosis_fHepB"    = "Liver Dx: Hep B",
  "LiverDiagnosis_fAlcohol" = "Liver Dx: Alcohol",
  "LiverDiagnosis_fOther"   = "Liver Dx: Other",
  "Age"                     = "Age",
  "BMI"                     = "BMI",
  "TimeSinceTransplant"     = "Time Since Transplant"
)
forest_data$term <- forest_term_labels[as.character(forest_data$term)]
# preserve model-fit order (reversed so first predictor plots at the top)
forest_data$term <- factor(forest_data$term, levels = rev(unique(forest_data$term)))

# facet order, built from the same dynamic labels used above (not hardcoded)
outcome_facet_order <- c(
  paste0("ESS (n=", nobs(ess_model), ")"),
  paste0("PSQI (n=", nobs(psqi_model), ")"),
  paste0("AIS (n=", nobs(ais_model), ")"),
  paste0("Berlin (n=", nobs(berlin_model), ")")
)
forest_data$outcome <- factor(forest_data$outcome, levels = outcome_facet_order)

# creating the forest plot
forest_plot <- ggplot(forest_data, aes(x = OR, y = term, color = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) +
  geom_point(size = 2.5) +
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10)) +
  scale_color_manual(values = c("p < 0.05" = "black", "n.s." = "grey60")) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(x = "Adjusted Odds Ratio (95% CI, log scale)", y = NULL, color = NULL,
       title = str_wrap("Adjusted odds ratios for predictors of sleep disturbance, by instrument", width = 55)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

forest_plot
# NOTE: this ggsave call was previously fully commented out (including a
# stray "#" partway through the second line), so the PNG was never actually
# written to disk. Fixed - both lines are now live code.
ggsave("forest_plot_sleep_disturbance.png",
       plot = forest_plot, width = 11, height = 8, dpi = 300)


# =================================================================
# ANALYSIS 2: SF36 QUALITY OF LIFE (linear regression)
# All objects in this section use df_qol and are prefixed qol_/_qol
# where a name might otherwise be ambiguous. This is a separate copy of
# clean_data, so it does not overwrite df_sleep or any object created in
# Analysis 1 above.
# =================================================================

df_qol <- clean_data

## ---- 7. Recode / label variables we need -----------------------------------

df_qol <- df_qol %>%
  mutate(
    # sleep instruments (continuous)
    ESS  = ESS,
    PSQI = PSQI,
    AIS  = AIS,
    BSS  = Berlin,          # already binary (0/1)
    
    # QoL outcomes
    PCS = PCS,
    MCS = MCS,
    
    # binary sleep-disturbance flags using the clinically accepted cut-offs
    # given in the assignment (NOT the pre-existing "Poor.sleep.quality" /
    # "Insomnia" columns, so that the thresholds are explicit and reproducible)
    ESS_dist  = factor(ifelse(ESS  > 10, "Disturbed", "No disturbance"),
                       levels = c("No disturbance", "Disturbed")),
    PSQI_dist = factor(ifelse(PSQI > 4,  "Disturbed", "No disturbance"),
                       levels = c("No disturbance", "Disturbed")),
    AIS_dist  = factor(ifelse(AIS  > 5,  "Disturbed", "No disturbance"),
                       levels = c("No disturbance", "Disturbed")),
    BSS_dist  = factor(ifelse(BSS  == 1, "Disturbed", "No disturbance"),
                       levels = c("No disturbance", "Disturbed")),
    
    # covariates for the adjusted models, as factors where appropriate.
    # NOTE: renamed from dotted-style names (e.g. "Recurrence.of.disease") to
    # underscore_case for readability and to match the _f/underscore
    # convention used in the Analysis 3 half of this script. "Corticoid" was
    # also corrected to "Corticosteroid_f" (typo in the original - dropped
    # "steroid" - and clarified as the factor-recoded version).
    Gender                       = factor(Gender, levels = c(1, 2),
                                          labels = c("Male", "Female")),
    Liver_Diagnosis               = factor(LiverDiagnosis, levels = c(1,2,3,4,5),
                                           labels = c("HepC","HepB","PSC_PBC_AHA","Alcohol","Other")),
    Liver_Diagnosis <- relevel(LiverDiagnosis, ref="PSC_PBC_AHA"),  # largest group as reference (same as before)
    
    Recurrence_of_Disease         = factor(Recurrence, levels = c(0, 1),
                                           labels = c("No", "Yes")),
    Rejection_Graft_Dysfunction   = factor(Rejection, levels = c(0, 1),
                                           labels = c("No", "Yes")),
    Any_Fibrosis                  = factor(AnyFibrosis, levels = c(0, 1),
                                           labels = c("No", "Yes")),
    Renal_Failure                  = factor(RenalFailure, levels = c(0, 1),
                                            labels = c("No", "Yes")),
    Depression                   = factor(Depression, levels = c(0, 1),
                                          labels = c("No", "Yes")),
    Corticosteroid_f              = factor(Corticosteroid, levels = c(0, 1),
                                           labels = c("No", "Yes")),
    Age                          = Age,
    BMI                          = BMI,
    Time_From_Transplant           = TimeSinceTransplant
  )


## =============================================================================
## 8. SIMPLE RELATIONSHIPS: scatterplots + correlation (ESS, PSQI, AIS)
## =============================================================================
## Berlin is binary, so a scatterplot/correlation does not apply here.
## Handled in the group-comparison step (section 10).

## ---- 8a. Scatterplots -------------------------------------------------------

plot_scatter <- function(data, xvar, xlab) {
  p_pcs <- ggplot(data, aes(x = .data[[xvar]], y = PCS)) +
    geom_point(alpha = 0.6, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, colour = "black", na.rm = TRUE) +
    labs(x = xlab, y = "SF-36 Physical Component Score (PCS)",
         title = paste(xlab, "vs. Physical QoL")) +
    theme_bw()
  
  p_mcs <- ggplot(data, aes(x = .data[[xvar]], y = MCS)) +
    geom_point(alpha = 0.6, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, colour = "black", na.rm = TRUE) +
    labs(x = xlab, y = "SF-36 Mental Component Score (MCS)",
         title = paste(xlab, "vs. Mental QoL")) +
    theme_bw()
  
  list(pcs = p_pcs, mcs = p_mcs)
}

ess_plots_qol  <- plot_scatter(df_qol, "ESS",  "Epworth Sleepiness Scale (ESS)")
psqi_plots_qol <- plot_scatter(df_qol, "PSQI", "Pittsburgh Sleep Quality Index (PSQI)")
ais_plots_qol  <- plot_scatter(df_qol, "AIS",  "Athens Insomnia Scale (AIS)")

# Save all six scatterplots to file (2 outcomes x 3 instruments)
ggsave("scatter_ESS_PCS.png",  ess_plots_qol$pcs,  width = 5, height = 4)
ggsave("scatter_ESS_MCS.png",  ess_plots_qol$mcs,  width = 5, height = 4)
ggsave("scatter_PSQI_PCS.png", psqi_plots_qol$pcs, width = 5, height = 4)
ggsave("scatter_PSQI_MCS.png", psqi_plots_qol$mcs, width = 5, height = 4)
ggsave("scatter_AIS_PCS.png",  ais_plots_qol$pcs,  width = 5, height = 4)
ggsave("scatter_AIS_MCS.png",  ais_plots_qol$mcs,  width = 5, height = 4)

# Combined 3x2 grid using patchwork
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_scatter_qol <- (ess_plots_qol$pcs | ess_plots_qol$mcs) /
    (psqi_plots_qol$pcs | psqi_plots_qol$mcs) /
    (ais_plots_qol$pcs | ais_plots_qol$mcs)
  ggsave("scatter_all_sleep_vs_qol.png", combined_scatter_qol, width = 10, height = 12)
}

                   
## ---- 8b. Correlations --------------------------------------------------------
## Decision rule: use Pearson by default; switch to Spearman if a scatterplot
## shows a clearly non-linear (but still monotonic) pattern, or if a variable
## is markedly skewed / has influential outliers (PSQI and AIS scores from
## symptom-questionnaires are typically right-skewed with a floor effect at 0,
## which is a common reason to prefer Spearman for these instruments).



check_skew <- function(x) {
  x <- x[!is.na(x)]
  c(n = length(x),
    mean = mean(x), sd = sd(x),
    median = median(x),
    skewness = mean((x - mean(x))^3) / sd(x)^3)
}

skew_tbl_qol <- rbind(
  ESS  = check_skew(df_qol$ESS),
  PSQI = check_skew(df_qol$PSQI),
  AIS  = check_skew(df_qol$AIS)
)
print(round(skew_tbl_qol, 2))

# All six scatterplots roughly follow a negative linear trend which supports the use of Pearson. # Skewness values were 0.62, 0.6, and 0.82, for ESS, PSQI, and AIS, respectively. 
# Since these values indicate moderate skewness (between 0.5 and 1), Pearson's test could be justifiably used for all three.
# However, the outliers present on these scatterplots could be influencing the Pearson coefficient values.
# Therefore, both Pearson and Spearman's tests will be conducted to observe differences. If differences are minimal, 
# then Pearson will be selected since it is more directly interpretable given that the assumptions have been met.
# If Pearson and Spearman values diverge meaningfully (by more than 0.1), Spearman will be reported instead, 
# as this would suggest the outliers or residual skew are distorting the Pearson estimate. 
# This comparison and decision will be made independently for each of the six correlation pairs.
                   
cor_test_both <- function(x, y, xname, yname) {
  ok <- complete.cases(x, y)
  pear <- cor.test(x[ok], y[ok], method = "pearson")
  spear <- cor.test(x[ok], y[ok], method = "spearman", exact = FALSE)
  data.frame(
    sleep_measure = xname, qol_measure = yname, n = sum(ok),
    pearson_r  = round(unname(pear$estimate), 3),
    pearson_p  = signif(pear$p.value, 3),
    spearman_rho = round(unname(spear$estimate), 3),
    spearman_p   = signif(spear$p.value, 3)
  )
}

cor_results_qol <- bind_rows(
  cor_test_both(df_qol$ESS,  df_qol$PCS, "ESS",  "PCS"),
  cor_test_both(df_qol$ESS,  df_qol$MCS, "ESS",  "MCS"),
  cor_test_both(df_qol$PSQI, df_qol$PCS, "PSQI", "PCS"),
  cor_test_both(df_qol$PSQI, df_qol$MCS, "PSQI", "MCS"),
  cor_test_both(df_qol$AIS,  df_qol$PCS, "AIS",  "PCS"),
  cor_test_both(df_qol$AIS,  df_qol$MCS, "AIS",  "MCS")
)

print(cor_results_qol)

write.csv(cor_results_qol, "correlation_results_sleep_vs_qol.csv", row.names = FALSE)

# Since the Pearson and Spearman values for all 6 plots are within 0.1, Pearson will be used for all six plots.
# Using a significance value of alpha = 0.05, the null hypothesis, that correlation is 0, is rejected 
# for all six sleep measure - QoL pairs (all p < .001).
# This provides evidence that greater sleep disturbance (higher ESS, PSQI, and AIS scores) is 
# associated with lower quality of life, across both physical (PCS) and mental (MCS) domains. 
# The strength of these associations varies by sleep measure and QoL domain: AIS and PSQI show 
# moderate-to-strong negative correlations with QoL (r = -.35 to -.55), while ESS shows comparatively 
# weaker correlations (r = -.28 to -.30). Across all three sleep measures, the association with MCS 
# is stronger than with PCS, suggesting sleep disturbance may relate more closely to mental than 
# physical quality of life in this sample.

## =============================================================================
## 9. QoL COMPARISON BETWEEN DISTURBED vs. NON-DISTURBED GROUPS
## =============================================================================
## For each instrument (ESS, PSQI, AIS, Berlin): the mean PCS and mean MCS
## between the "disturbed" and "not disturbed" groups were compared.
## Primary test: Welch two-sample t-test (does NOT assume equal variances)
## Secondary/robustness check: Wilcoxon rank-sum test, reported if normality
## or equal-variance assumptions look seriously violated (e.g. via
## Shapiro-Wilk test and visual inspection of boxplots/QQ-plots).

# However, the results of the Shapiro-Wilk test found that all 8 groups violated
# the normality assumption so the primary test relied on was the Wilcoxon rank-sum test.

compare_groups <- function(data, group_var, outcome_var, sleep_label) {
  d <- data %>% select(grp = all_of(group_var), y = all_of(outcome_var)) %>%
    filter(!is.na(grp), !is.na(y))
  
  # Welch t-test (unequal variances assumed by default)
  tt <- t.test(y ~ grp, data = d)  # Welch by default in R (var.equal = FALSE)
  
  # Wilcoxon rank-sum test as a non-parametric alternative
  wt <- wilcox.test(y ~ grp, data = d)
  
  # Shapiro-Wilk normality check within each group (flag violations)
  shapiro_p <- d %>% group_by(grp) %>%
    summarise(p = tryCatch(shapiro.test(y)$p.value, error = function(e) NA_real_)) %>%
    pull(p)
  
  means <- d %>% group_by(grp) %>%
    summarise(mean = mean(y), sd = sd(y), n = n(), .groups = "drop")
  
  data.frame(
    sleep_measure = sleep_label,
    outcome = outcome_var,
    n_no_disturbance = means$n[means$grp == "No disturbance"],
    mean_no_disturbance = round(means$mean[means$grp == "No disturbance"], 1),
    sd_no_disturbance   = round(means$sd[means$grp == "No disturbance"], 1),
    n_disturbance = means$n[means$grp == "Disturbed"],
    mean_disturbance = round(means$mean[means$grp == "Disturbed"], 1),
    sd_disturbance   = round(means$sd[means$grp == "Disturbed"], 1),
    welch_t_p  = signif(tt$p.value, 3),
    wilcoxon_p = signif(wt$p.value, 3),
    shapiro_p_min = signif(min(shapiro_p, na.rm = TRUE), 3)
  )
}

qol_instruments <- list(
  ESS  = "ESS_dist",
  PSQI = "PSQI_dist",
  AIS  = "AIS_dist",
  Berlin = "BSS_dist"
)

pcs_table_qol <- bind_rows(lapply(names(qol_instruments), function(nm)
  compare_groups(df_qol, qol_instruments[[nm]], "PCS", nm)))

mcs_table_qol <- bind_rows(lapply(names(qol_instruments), function(nm)
  compare_groups(df_qol, qol_instruments[[nm]], "MCS", nm)))

cat("\n--- Physical QoL (PCS) by disturbance group ---\n")
print(pcs_table_qol)
cat("\n--- Mental QoL (MCS) by disturbance group ---\n")
print(mcs_table_qol)

# The Shapiro-Wilk tests indicated non-normality in at least one group for all eight 
# comparisons (all p < .05), which was consistent with the moderate skewness seen in the sleep measures. Given that 
# the normality assumption did not hold, the Wilcoxon rank-sum test was used as the primary test.
#
# Using a significance level of alpha = 0.05, the null hypothesis that the distributions of PCS/MCS scores 
# did not differ between non-disturbed vs. disturbed groups was rejected for all eight comparisons, 
# since all Wilcoxon p-values were below 0.05. This provides evidence that sleep disturbance was associated with 
# lower physical and mental quality of life across all four sleep measures.
#
# Results from Welch's t-test and Wilcoxon were consistent in direction and significance across all 
# comparisons, suggesting the non-normality did not materially affect the conclusions.
                                  
write.csv(pcs_table_qol, "group_comparison_PCS.csv", row.names = FALSE)
write.csv(mcs_table_qol, "group_comparison_MCS.csv", row.names = FALSE)

## Boxplots to accompany the tables

make_boxplot <- function(data, group_var, outcome_var, ylab, title) {
  d <- data %>% filter(!is.na(.data[[group_var]]), !is.na(.data[[outcome_var]]))
  ggplot(d, aes(x = .data[[group_var]], y = .data[[outcome_var]])) +
    geom_boxplot(fill = "grey85", outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.4) +
    labs(x = NULL, y = ylab, title = title) +
    theme_bw()
}

plot_list <- list()
for (nm in names(instruments)) {
  gv <- instruments[[nm]]
  plot_list[[paste0(nm, "_PCS")]] <- make_boxplot(df, gv, "PCS", "SF-36 PCS", paste(nm, "- Physical QoL"))
  plot_list[[paste0(nm, "_MCS")]] <- make_boxplot(df, gv, "MCS", "SF-36 MCS", paste(nm, "- Mental QoL"))
}

combined_panel <- wrap_plots(plot_list, ncol = 2) +
  plot_annotation(title = "Quality of Life by Sleep Disturbance Group")

ggsave("combined_boxplots.png", combined_panel,
       width = 9, height = 4 * length(instruments), dpi = 300)
## =============================================================================
## 10. ADJUSTED LINEAR REGRESSION (one sleep instrument at a time)
## =============================================================================
## Rationale for NOT combining ESS + PSQI + AIS + BSS in a single model:
##  - They are correlated measures of overlapping/related constructs. Fit
##    the "kitchen sink" model below and check its VIFs: these come out
##    around 3.7-4.0 for PSQI/AIS, which is moderate collinearity by the
##    VIF<5 threshold from Lecture 9 - not severe, and not on its own reason
##    to distrust the combined model's coefficients. The stronger reasons to
##    keep the four instruments in separate models are (a) each is a
##    different construct (daytime sleepiness, overall sleep quality,
##    insomnia, OSA risk - see the univariable heatmap), so a single shared
##    coefficient obscures which facet of "sleep disturbance" is actually
##    driving the QoL association, and (b) interpretability: "a one-point
##    increase in PSQI is associated with an X-point change in PCS/MCS,
##    holding covariates constant" is a clean, reportable statement, whereas
##    a coefficient from a model with 4 correlated sleep terms is much
##    harder to interpret on its own.
##  - PSQI has ~32% missingness in this dataset; forcing it into every
##    model would needlessly drop ~1/3 of subjects from all analyses.
##  - Interpretation is much cleaner one instrument at a time: "a one-point
##    increase in PSQI is associated with an X-point change in PCS/MCS,
##    holding covariates constant."
##
## Adjustment set (per the assignment's demographic/clinical variable list):
##   Age, Gender, BMI, Time_From_Transplant, Liver_Diagnosis,
##   Recurrence_of_Disease, Rejection_Graft_Dysfunction, Any_Fibrosis,
##   Renal_Failure, Depression, Corticosteroid_f
##
## CAUTION - Depression as a covariate here may be a mediator, not just a
## confounder. In Analysis 3, Depression is the single most consistent
## predictor of sleep disturbance (significant for PSQI and AIS, both binary
## and continuous versions, after adjustment). If depression is on the causal
## pathway from sleep disturbance to quality of life - e.g., poor sleep
## contributes to depressed mood, which in turn lowers SF-36 MCS - rather
## than being an independent common cause of both, then adjusting for it here
## would partly remove the very effect these models are trying to estimate
## (over-adjustment for a mediator). The reverse causal story is equally
## plausible in this population: depression predating transplant, or
## independent of it, could directly drive both poor sleep and poor QoL, in
## which case adjusting for it is exactly correct (it's a genuine
## confounder). The dataset alone cannot distinguish these two structures -
## that would need longitudinal data on onset of depression relative to sleep
## symptoms and QoL decline, which is not available here. This is a modeling
## assumption to state explicitly in Methods/Limitations, not a bug to fix:
## whichever framing you choose, name it, and consider reporting the
## sleep-instrument coefficient both with and without Depression in the
## covariate set as a sensitivity check, since a large shift in that
## coefficient when Depression is dropped would itself be informative about
## which structure is more likely.

qol_covariates <- c("Age", "Gender", "BMI", "Time_From_Transplant",
                    "Liver_Diagnosis", "Recurrence_of_Disease",
                    "Rejection_Graft_Dysfunction", "Any_Fibrosis",
                    "Renal_Failure", "Depression", "Corticosteroid_f")

fit_adjusted <- function(data, sleep_var, outcome_var, covars) {
  form <- as.formula(
    paste(outcome_var, "~", sleep_var, "+", paste(covars, collapse = " + "))
  )
  model_data <- data %>% select(all_of(outcome_var), all_of(sleep_var), all_of(covars)) %>%
    na.omit()
  fit <- lm(form, data = model_data)
  list(fit = fit, n = nrow(model_data), formula = form)
}

qol_sleep_vars <- c("ESS", "PSQI", "AIS", "BSS")
qol_outcomes   <- c("PCS", "MCS")

adjusted_models <- list()
for (s in qol_sleep_vars) {
  for (o in qol_outcomes) {
    key <- paste(s, o, sep = "_")
    adjusted_models[[key]] <- fit_adjusted(df_qol, s, o, qol_covariates)
  }
}

## ---- 10a. Print tidy summaries for each model -------------------------------

for (key in names(adjusted_models)) {
  m <- adjusted_models[[key]]
  cat("\n=====================================================\n")
  cat("Model:", key, " (n =", m$n, ")\n")
  cat("Formula:", deparse(m$formula), "\n")
  print(kable(tidy(m$fit, conf.int = TRUE) %>%
                mutate(across(where(is.numeric), ~round(., 3)))))
  cat("Adjusted R-squared:", round(summary(m$fit)$adj.r.squared, 3), "\n")
}

## ---- 10. Diagnostics for each fitted model ---------------------------------
## Check: linearity/homoscedasticity (residuals vs fitted), normality of
## residuals (QQ-plot), and multicollinearity (VIF) for every model.

for (key in names(adjusted_models)) {
  m <- adjusted_models[[key]]$fit
  png(paste0("diagnostics_", key, ".png"), width = 800, height = 800)
  par(mfrow = c(2, 2))
  plot(m)
  dev.off()
  
  cat("\nVIFs for", key, ":\n")
  print(round(vif(m), 2))
}

## ---- 10c. Illustration: why separate models are preferred over combining --
## Fit one combined ("kitchen sink") model on PCS with complete cases across
## all four instruments simultaneously, to illustrate the moderate collinearity
## (VIF ~3.7-4.0 for PSQI/AIS - see printed output) and sample-size loss that
## come from forcing all four sleep instruments into one model. Not used as a
## primary model - the four separate models above remain the primary results.

combined_data_qol <- df_qol %>%
  select(PCS, ESS, PSQI, AIS, BSS, all_of(qol_covariates)) %>%
  na.omit()

cat("\nSample size available for the 'kitchen sink' combined model: n =",
    nrow(combined_data_qol), "vs. n =", adjusted_models[["PSQI_PCS"]]$n,
    "for the PSQI-only adjusted model.\n")

combined_fit_qol <- lm(PCS ~ ESS + PSQI + AIS + BSS +
                         Age + Gender + BMI + Time_From_Transplant +
                         Liver_Diagnosis + Recurrence_of_Disease +
                         Rejection_Graft_Dysfunction + Any_Fibrosis +
                         Renal_Failure + Depression + Corticosteroid_f,
                       data = combined_data_qol)

cat("\nVIFs in the combined ('kitchen sink') model (illustrative only):\n")
print(round(vif(combined_fit_qol), 2))

## ---- 11d. Extract a clean summary table of the sleep-instrument coefficient
##      (i.e., the adjusted effect of each sleep measure on each outcome) -----

extract_sleep_coef <- function(key) {
  m <- adjusted_models[[key]]$fit
  s <- adjusted_models[[key]]
  sleep_var <- str_split(key, "_")[[1]][1]
  outcome   <- str_split(key, "_")[[1]][2]
  td <- tidy(m, conf.int = TRUE) %>% filter(term == sleep_var)
  data.frame(
    sleep_measure = sleep_var,
    outcome = outcome,
    n = s$n,
    beta = round(td$estimate, 3),
    ci_low = round(td$conf.low, 3),
    ci_high = round(td$conf.high, 3),
    p_value = signif(td$p.value, 3)
  )
}

sleep_effect_summary_qol <- bind_rows(lapply(names(adjusted_models), extract_sleep_coef))
cat("\n--- Adjusted effect of each sleep instrument on QoL outcomes ---\n")
print(sleep_effect_summary_qol)
write.csv(sleep_effect_summary_qol, "adjusted_sleep_effects_summary.csv", row.names = FALSE)

## =============================================================================
## 10e. Full covariate tables for all 8 adjusted models (appendix table)
## =============================================================================

extract_full_model <- function(key) {
  m <- adjusted_models[[key]]$fit
  n <- adjusted_models[[key]]$n
  parts <- str_split(key, "_")[[1]]
  sleep_var <- parts[1]
  outcome   <- parts[2]
  
  tidy(m, conf.int = TRUE) %>%
    mutate(
      sleep_measure = sleep_var,
      outcome = outcome,
      n = n,
      estimate  = round(estimate, 3),
      conf.low  = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value   = signif(p.value, 3)
    ) %>%
    select(sleep_measure, outcome, n, term, estimate,
           conf.low, conf.high, p.value)
}

full_model_results_qol <- bind_rows(lapply(names(adjusted_models), extract_full_model))

# Optional: flag which terms are significant at the 0.05 level, for quick scanning
full_model_results_qol <- full_model_results_qol %>%
  mutate(significant = ifelse(p.value < 0.05, "*", ""))

print(full_model_results_qol)

write.csv(full_model_results_qol, "full_model_coefficients.csv", row.names = FALSE)

## =============================================================================
## End of Analysis script.
## Suggested report text for each significant sleep-instrument coefficient:
##   "A one-point increase in <instrument> was associated with an estimated
##    <beta>-point <increase/decrease> in <PCS/MCS> (95% CI: <low>, <high>,
##    p = <p>), after adjustment for age, gender, BMI, time since transplant,
##    liver diagnosis, recurrence of disease, rejection/graft dysfunction,
##    fibrosis, renal failure, depression, and corticosteroid use."
##
## CAUTION: Renal_Failure is forced into all 8 models above despite having
## only 4 cases in the whole dataset. All 4 happen to survive complete-case
## listwise deletion in every one of the 8 models (verified), so this will
## not crash vif()/lm() with an aliasing error - but a coefficient estimated
## from 4 observations is inherently unstable. Consider checking leverage/
## Cook's distance for these 4 rows in each model, and apply the same
## "imprecise, not a reliable effect estimate" framing used for renal
## failure in the Analysis 3 write-up.
## =============================================================================

# ==============================================================================
# SECTION 4 — MISSING PSQI SENSITIVITY ANALYSIS
# ==============================================================================

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

# -------------------------------------------------------------------------
# 1. LOAD AND CHECK DATA
# -------------------------------------------------------------------------

output_dir <- "psqi_sensitivity_results"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Use a separate copy of the cleaned dataframe created at the start of this
# master script. This preserves clean_data for any later checks or analyses.
df <- clean_data

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
