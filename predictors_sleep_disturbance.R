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
  c(n_valid=n_valid, n_positive=n_pos, 
    prevalence_pct=round(100*n_pos/n_valid,1))
}

sapply(df[,c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")], 
       prevalence)

# NOTE: PSQI has substantially more missing data (~32%) than the other three
# instruments (~2-6%). This should be flagged as a limitation - any PSQI-based
# model runs on a smaller and possibly non-random subsample.

# ---------------------------------------------------------------
# 2. THE RENAL FAILURE ISSUE (only 4/268 patients)
# ---------------------------------------------------------------

for (out in c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")) {
  cat("\n---", out, "vs RenalFailure ---\n")
  tab <- table(df$RenalFailure_f, df[[out]])
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

outcomes <- c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")
predictors <- c("Age","Gender_f","BMI","TimeSinceTransplant",
                "LiverDiagnosis_f","Recurrence_f","Rejection_f",
                "AnyFibrosis_f","Depression_f","Corticosteroid_f")

univariable_results <- list()
for (out in outcomes) {
  for (pred in predictors) {
    f <- as.formula(paste(out, "~", pred))
    m <- glm(f, data=df, family=binomial)
    cat("\n===", out, "~", pred, "===\n")
    print(round(cbind(OR=exp(coef(m)), exp(confint(m)), 
                      p=summary(m)$coefficients[,4]), 3)
  }
}

# See project notes / report Table 1 for the compiled OR/CI/p summary table
# across all predictors and all four outcomes.

# ---------------------------------------------------------------
# 4. ADJUSTED (MULTIVARIABLE) MODELS
# ---------------------------------------------------------------
# Predictor sets were chosen by: (a) univariable screening at p<0.20, then
# (b) trimming to respect the rule-of-thumb sample size restriction
# (p < m/15, where m = size of the smaller outcome class.
# RenalFailure excluded from all four models (see Section 2).

## --- Model 1: ESS ---
# m/15 budget ~4 predictors (m=67, smaller class among n=251)
# LiverDiagnosis collapsed to Hep C vs Other (1 df instead of 4) to fit budget,
# since only the Hep C level was significant univariably.
df$LiverDx_HepC <- factor(ifelse(df$LiverDiagnosis==1,"HepC","Other"))

ess_model <- glm(ESS_binary ~ Gender_f + LiverDx_HepC + 
                   Recurrence_f + Rejection_f,
                 data=df, family=binomial)
summary(ess_model)
round(cbind(OR=exp(coef(ess_model)), exp(confint(ess_model))), 3)
vif(ess_model)

# Compare against the larger "fully screened" model (all p<0.20 predictors,
# before budget trimming) to justify the simpler model:
ess_full <- glm(ESS_binary ~ Gender_f + LiverDiagnosis_f + Recurrence_f + Rejection_f +
                  AnyFibrosis_f + Depression_f + Corticosteroid_f,
                data=df, family=binomial)
anova(ess_model, ess_full, test="Chisq")   # non-significant -> simpler model preferred

## --- Model 2: PSQI ---
# m/15 budget ~4 predictors (m=66, smaller class among n=183)
psqi_model <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                  data=df, family=binomial)
summary(psqi_model)
round(cbind(OR=exp(coef(psqi_model)), exp(confint(psqi_model))), 3)
vif(psqi_model)

psqi_full <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                 data=df, family=binomial)
anova(psqi_model, psqi_full, test="Chisq")  # BMI does not improve fit significantly

## --- Model 3: AIS ---
# m/15 budget ~7-8 predictors (m=117, smaller class among n=262)
ais_model <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                 data=df, family=binomial)
summary(ais_model)
round(cbind(OR=exp(coef(ais_model)), exp(confint(ais_model))), 3)
vif(ais_model)

ais_full <- glm(AIS_binary ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f +
                  Corticosteroid_f, data=df, family=binomial)
anova(ais_model, ais_full, test="Chisq")   # Corticosteroid does not improve fit significantly

## --- Model 4: Berlin ---
# m/15 budget ~6-7 predictors (m=102, smaller class among n=262); well under budget
berlin_model <- glm(Berlin_binary ~ Age + BMI + TimeSinceTransplant,
                    data=df, family=binomial)
summary(berlin_model)
round(cbind(OR=exp(coef(berlin_model)), exp(confint(berlin_model))), 3)
vif(berlin_model)

# ---------------------------------------------------------------
# 5. SENSITIVITY ANALYSIS: CONTINUOUS SLEEP SCORES (linear regression)
# ---------------------------------------------------------------
# Uses the raw ESS/PSQI/AIS scores (not dichotomized) with the same predictor
# sets as the corresponding logistic model, to check whether dichotomization
# at the clinical cutoff is discarding useful information. Berlin has no
# continuous version (binary at source), so it is omitted here.

ess_lin  <- lm(ESS  ~ Gender_f + LiverDx_HepC + Recurrence_f + Rejection_f, data=df)
psqi_lin <- lm(PSQI ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f, data=df)
ais_lin  <- lm(AIS  ~ LiverDiagnosis_f + Recurrence_f + AnyFibrosis_f + Depression_f, data=df)

summary(ess_lin);  shapiro.test(resid(ess_lin))
summary(psqi_lin); shapiro.test(resid(psqi_lin))
summary(ais_lin);  shapiro.test(resid(ais_lin))

# Residuals show mild non-normality (Shapiro-Wilk p<0.001 for all three) -
# common for right-skewed psychometric scores. With n=180-260, OLS estimates
# remain reasonably robust; note this as a limitation rather than a fatal flaw.
# NOTE: using the continuous ESS score, "Rejection" is significant (p=0.006)
# even though it was only borderline (p=0.068) in the binary ESS model -
# a concrete example of information lost by dichotomizing at the cutoff.

# ---------------------------------------------------------------
# 6. FOREST PLOT OF ADJUSTED ODDS RATIOS (ggplot2, faceted)
# ---------------------------------------------------------------

# Tidy a glm model's output (OR, 95% CI, p) into a plotting data frame,
# excluding the intercept row
tidy_glm <- function(model, outcome_label) {
  co <- summary(model)$coefficients
  ci <- confint(model)
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
  tidy_glm(ess_model,    "ESS (n=251)"),
  tidy_glm(psqi_model,   "PSQI (n=183)"),
  tidy_glm(ais_model,    "AIS (n=262)"),
  tidy_glm(berlin_model, "Berlin (n=237)")
)

forest_data$sig  <- factor(ifelse(forest_data$p.value < 0.05, "p < 0.05", "n.s."),
                           levels = c("p < 0.05","n.s."))
# preserve model-fit order (reversed so first predictor plots at the top)
forest_data$term <- factor(forest_data$term, 
                           levels = rev(unique(forest_data$term)))
forest_data$outcome <- factor(forest_data$outcome,
                              levels = c("ESS (n=251)","PSQI (n=183)",
                                         "AIS (n=262)","Berlin (n=237)"))

# creating the forest plot
forest_plot <- ggplot(forest_data, aes(x = OR, y = term, color = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) +
  geom_point(size = 2.5) +
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10)) +
  scale_color_manual(values = c("p < 0.05" = "black", "n.s." = "grey60")) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(x = "Adjusted Odds Ratio (95% CI, log scale)", y = NULL, color = NULL,
       title = "Adjusted odds ratios for predictors of sleep disturbance, by instrument") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

forest_plot
#ggsave("forest_plot_sleep_disturbance.png", 
     #  plot = forest_plot, width = 11, height = 8, dpi = 300)