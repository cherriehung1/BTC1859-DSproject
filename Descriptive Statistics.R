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
# 1. IMPORT THE CLEANED DATASET
# ------------------------------------------------------------

clean_data <- read.csv(
  "project_data_cleaned.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


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
# OPTIONAL: CREATE A SINGLE TABLE 1 FOR THE REPORT
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