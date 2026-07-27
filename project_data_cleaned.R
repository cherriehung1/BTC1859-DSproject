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
# 9. SAVE THE CLEANED DATASET
# ------------------------------------------------------------

write.csv(
  clean_data,
  "project_data_cleaned_test.csv",
  row.names = FALSE,
  na = "NA"
)
