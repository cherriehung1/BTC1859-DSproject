## BTC1859 - Team 1
# Predictors of Sleep Disturbance (logistic regression)

# Own:
#   univariable predictor analyses;
# logistic regression;
# adjusted models;
# odds ratios and 95% CI;
# model diagnostics;
# predictor table/forest plot;
# corresponding Methods + Results.
# Also investigate odd issues such as renal failure having only four cases.
# Presentation: Which patients are most likely to have sleep disturbance?
#   


# Installing packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("car")) install.packages("car")
rm(list=ls())
library(car)
# ---------------------------------------------------------------
# 0. LOAD DATA
# ---------------------------------------------------------------

df <- read.csv("project_data_cleaned.csv")

# Recode factors with meaningful labels (per data dictionary)
df$Gender_f          <- factor(df$Gender, levels=c(1,2), 
                               labels=c("Male","Female"))
df$LiverDiagnosis_f  <- factor(df$LiverDiagnosis, levels=c(1,2,3,4,5),
                               labels=c("HepC","HepB","PSC_PBC_AHA","Alcohol","Other"))
df$LiverDiagnosis_f  <- relevel(df$LiverDiagnosis_f, ref="PSC_PBC_AHA")  # largest group as reference

df$Recurrence_f      <- factor(df$Recurrence,     levels=c(0,1), 
                               labels=c("No","Yes"))
df$Rejection_f       <- factor(df$Rejection,      levels=c(0,1), 
                               labels=c("No","Yes"))
df$AnyFibrosis_f     <- factor(df$AnyFibrosis,    levels=c(0,1), 
                               labels=c("No","Yes"))
df$RenalFailure_f    <- factor(df$RenalFailure,   levels=c(0,1), 
                               labels=c("No","Yes"))
df$Depression_f      <- factor(df$Depression,     levels=c(0,1), 
                               labels=c("No","Yes"))
df$Corticosteroid_f  <- factor(df$Corticosteroid, levels=c(0,1), 
                               labels=c("No","Yes"))

# The four binary sleep disturbance outcomes 
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
  c(n_valid=n_valid, n_positive=n_pos, 
    prevalence_pct=round(100*n_pos/n_valid,1))
}

sapply(df[,c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")], 
       prevalence)

# NOTE: PSQI has substantially more missing data (~32%) than the other three
# instruments (~2-6%). This should be flagged as a limitation - any PSQI-based
# model runs on a smaller and possibly non-random subsample.
