## BTC1859 - Team 1
# Predictors of Sleep Disturbance (logistic regression) + SF36 QoL (linear regression)
# Merged script - namespaced to avoid collisions between the two analyses.
# Naming convention: everything in the sleep-disturbance analysis uses a
# "_sleep" or "sleep_" prefix/dataframe (df_sleep); everything in the QoL
# analysis uses a "_qol"/"qol_" prefix/dataframe (df_qol). The two halves
# read the raw CSV independently and never share a variable name, so neither
# section can silently overwrite the other's objects.

# Own (sleep disturbance, Analysis 3):
#   univariable predictor analyses; logistic regression; adjusted models;
#   odds ratios and 95% CI; model diagnostics; predictor table/forest plot;
#   corresponding Methods + Results.
# Also investigate odd issues such as renal failure having only four cases.
# Presentation: Which patients are most likely to have sleep disturbance?

# ---------------------------------------------------------------
# SETUP: packages (all imports up front, used by BOTH halves of this script)
# ---------------------------------------------------------------

rm(list = ls())

required_packages <- c("ggplot2", "car", "dplyr", "stringr", "broom", "knitr")
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
# 0. LOAD DATA
# ---------------------------------------------------------------

df_sleep <- read.csv("project_data_cleaned.csv")

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

write.csv(univariable_results, "univariable_OR_CI_p_table.csv", row.names = FALSE)

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
# (see Section 2). Rather than hardcoding the OR/p/n values by hand (which
# would silently go stale if the data changes - exactly what happened when
# ESS's n shifted from 251 to 250 after a data refresh), fit each of the
# four RenalFailure univariable models here and read the numbers off the
# fitted objects directly. Separation is detected via the actual glm()
# warning rather than an eyeballed OR threshold.
fit_renal_univariable <- function(outcome) {
  sep_warning <- FALSE
  m <- withCallingHandlers(
    glm(as.formula(paste(outcome, "~ RenalFailure_f")), data = df_sleep, family = binomial),
    warning = function(w) {
      if (grepl("fitted probabilities numerically 0 or 1 occurred", conditionMessage(w))) {
        sep_warning <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  n    <- nobs(m)
  co   <- summary(m)$coefficients
  term_name <- grep("RenalFailure", rownames(co), value = TRUE)
  
  if (sep_warning || length(term_name) == 0) {
    return(data.frame(outcome = outcome, term = "RenalFailure_f[Yes vs No]",
                      OR = NA, CI_low = NA, CI_high = NA, p_value = NA,
                      sig_label = "n/e", n = n))
  }
  
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
heat_data$term <- factor(heat_data$term, levels = rev(unique(heat_data$term)))

ggplot(heat_data, aes(x = outcome, y = term, fill = log(OR))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sig_label), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       na.value = "#D9D9D9",
                       name = "Odds Ratio", labels = function(x) round(exp(x), 2)) +
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

ggsave("univariable_heatmap.png", width = 8, height = 9.5, dpi = 300)

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
# Practical takeaway for interpretation: findings that replicate across
# multiple outcomes and multiple analytic approaches (e.g., Depression's
# association with both PSQI and AIS, in both the binary and continuous-score
# models) should be treated as more robust than single-outcome, borderline
# findings (e.g., Any Fibrosis for ESS, p~0.05), which may reflect sampling
# variability in a moderate sample (n=183-262) rather than a true effect.

# ---------------------------------------------------------------
# 4. MULTIVARIABLE MODELS
# ---------------------------------------------------------------

# Predictor sets were chosen by: (a) univariable screening at p<0.20, then
# (b) trimming to respect the rule-of-thumb sample size restriction
# (p < m/15, where m = size of the smaller outcome class - see Lecture 9).
# RenalFailure excluded from all four models (see Section 2).

## --- Model 1: ESS ---
# m/15 budget ~4 predictors (m=66, smaller class among n=250 - updated after
# the data refresh that shifted ESS's missingness from 17 to 18; the budget
# conclusion is unchanged, but the exact n/m are current as of that update).
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
# anova() requires both models fit on the IDENTICAL set of rows (Tutorial 9),
# so the reduced model must be refit on psqi_full's subset before comparing -
# comparing psqi_model (n=183) directly against psqi_full (n=165) would error.
psqi_subset <- df_sleep[complete.cases(df_sleep[, c("PSQI_binary","Gender_f","Recurrence_f",
                                                    "AnyFibrosis_f","Depression_f","BMI")]), ]
psqi_model_samesub <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                          data=psqi_subset, family=binomial)
psqi_full_samesub  <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                          data=psqi_subset, family=binomial)
anova(psqi_model_samesub, psqi_full_samesub, test="Chisq")  # BMI does not improve fit significantly, n=165 for both

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
# about linearity, homoscedasticity, or influential/leverage points - the
# same 4-panel diagnostic already used for the 8 QoL models in Section 11b
# is added here for consistency: Residuals vs Fitted (linearity/
# homoscedasticity), Normal Q-Q (normality - the visual counterpart to the
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
# 6. FOREST PLOT OF ADJUSTED ODDS RATIOS (ggplot2, faceted)
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

forest_data$sig  <- factor(ifelse(forest_data$p.value < 0.05, "p < 0.05", "n.s."),
                           levels = c("p < 0.05","n.s."))
# preserve model-fit order (reversed so first predictor plots at the top)
forest_data$term <- factor(forest_data$term,
                           levels = rev(unique(forest_data$term)))
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
       title = "Adjusted odds ratios for predictors of sleep disturbance, by instrument") +
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
# All objects in this analysis use df_qol and are prefixed qol_/_qol
# where a name might otherwise be ambiguous. This is an INDEPENDENT read of
# the same source CSV (own na.strings handling) - it never touches or
# overwrites df_sleep or any object created in Analysis 3 above.
# =================================================================

df_qol <- read.csv("project_data_cleaned.csv", na.strings = c("NA", ""))

## ---- 8. Recode / label variables we need -----------------------------------

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

# Match the reference level used in the sleep-disturbance models (Section 0):
# without this, factor() defaults to the lowest numeric code (Hep C) as the
# reference, while df_sleep uses PSC/PBC/AHA - meaning every Liver_Diagnosis
# coefficient in this report would otherwise be interpreted against a
# different baseline depending on which analysis you're reading.
df_qol$Liver_Diagnosis <- relevel(df_qol$Liver_Diagnosis, ref = "PSC_PBC_AHA")

## =============================================================================
## 9. SIMPLE RELATIONSHIPS: scatterplots + correlation (ESS, PSQI, AIS)
## =============================================================================
## Berlin is binary, so a scatterplot/correlation does not apply to it here;
## it is handled instead in the group-comparison step (section 10).

## ---- 9a. Scatterplots -------------------------------------------------------

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

# Optional: combined 3x2 grid if patchwork is available
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_scatter_qol <- (ess_plots_qol$pcs | ess_plots_qol$mcs) /
    (psqi_plots_qol$pcs | psqi_plots_qol$mcs) /
    (ais_plots_qol$pcs | ais_plots_qol$mcs)
  ggsave("scatter_all_sleep_vs_qol.png", combined_scatter_qol, width = 10, height = 12)
}

## ---- 9b. Correlations --------------------------------------------------------
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
# In the report: report Pearson's r when skewness is modest and the
# scatterplot/loess trend looks linear; report Spearman's rho (and explain why)
# for any instrument-outcome pair where skewness is high (e.g., |skew| > 1)
# or the scatterplot suggests a monotonic-but-curved relationship.

write.csv(cor_results_qol, "correlation_results_sleep_vs_qol.csv", row.names = FALSE)

## =============================================================================
## 10. QoL COMPARISON BETWEEN DISTURBED vs. NON-DISTURBED GROUPS
## =============================================================================
## For each instrument (ESS, PSQI, AIS, Berlin): compare mean PCS and mean MCS
## between the "disturbed" and "not disturbed" groups.
## Primary test: Welch two-sample t-test (does NOT assume equal variances -
## more defensible by default than Student's t-test).
## Secondary/robustness check: Wilcoxon rank-sum test, reported if normality
## or equal-variance assumptions look seriously violated (e.g. via
## Shapiro-Wilk test and visual inspection of boxplots/QQ-plots).

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

for (nm in names(qol_instruments)) {
  gv <- qol_instruments[[nm]]
  ggsave(paste0("box_", nm, "_PCS.png"),
         make_boxplot(df_qol, gv, "PCS", "SF-36 PCS", paste(nm, "- Physical QoL")),
         width = 4.5, height = 4)
  ggsave(paste0("box_", nm, "_MCS.png"),
         make_boxplot(df_qol, gv, "MCS", "SF-36 MCS", paste(nm, "- Mental QoL")),
         width = 4.5, height = 4)
}

## =============================================================================
## 11. ADJUSTED LINEAR REGRESSION (one sleep instrument at a time)
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

## ---- 11a. Print tidy summaries for each model -------------------------------

for (key in names(adjusted_models)) {
  m <- adjusted_models[[key]]
  cat("\n=====================================================\n")
  cat("Model:", key, " (n =", m$n, ")\n")
  cat("Formula:", deparse(m$formula), "\n")
  print(kable(tidy(m$fit, conf.int = TRUE) %>%
                mutate(across(where(is.numeric), ~round(., 3)))))
  cat("Adjusted R-squared:", round(summary(m$fit)$adj.r.squared, 3), "\n")
}

## ---- 11b. Diagnostics for each fitted model ---------------------------------
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

## ---- 11c. Illustration: why separate models are preferred over combining --
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
## 11e. Full covariate tables for all 8 adjusted models (appendix table)
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
## End of Analysis 4 script.
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