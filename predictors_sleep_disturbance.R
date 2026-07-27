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
  ci <- prop.test(n_pos, n_valid)$conf.int   # Wilson score interval (with continuity correction)
  c(n_valid=n_valid, n_positive=n_pos,
    prevalence_pct=round(100*n_pos/n_valid,1),
    CI_low_pct=round(100*ci[1],1), CI_high_pct=round(100*ci[2],1))
}

sapply(df[,c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary")], prevalence)

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
predictors <- c("Age","Gender_f","BMI","TimeSinceTransplant","LiverDiagnosis_f",
                "Recurrence_f","Rejection_f","AnyFibrosis_f","Depression_f","Corticosteroid_f")

# Collect every predictor row (across all models) into one tidy data frame,
# used both for the printed table and the heatmap below
univariable_results <- data.frame()
for (out in outcomes) {
  for (pred in predictors) {
    f <- as.formula(paste(out, "~", pred))
    m <- glm(f, data=df, family=binomial)
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
# (see Section 2); only AIS produced an estimable OR. The "not estimable"
# cells are shown as blank/grey tiles with an "n/e" label instead of a number.
renal_rows <- data.frame(
  outcome = c("ESS_binary","PSQI_binary","AIS_binary","Berlin_binary"),
  term    = "RenalFailure_f[Yes vs No]",
  OR      = c(NA, NA, 2.45, NA),
  CI_low  = NA, CI_high = NA,
  p_value = c(NA, NA, 0.440, NA),
  sig_label = c("n/e", "n/e", "2.45", "n/e")
)
heat_data <- rbind(heat_data, renal_rows)

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
       caption = "** p<0.01, * p<0.05, ^ p<0.10. Renal failure (grey/'n/e' cells) could not be\nestimated for ESS, PSQI, or Berlin due to complete separation (n=4 cases) - see Section 2.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("univariable_heatmap.png", width = 8, height = 9.5, dpi = 300)

# ---------------------------------------------------------------
# 4. MULTIVARIABLE MODELS
# ---------------------------------------------------------------

# Predictor sets were chosen by: (a) univariable screening at p<0.20, then
# (b) trimming to respect the rule-of-thumb sample size restriction
# (p < m/15, where m = size of the smaller outcome class - see Lecture 9).
# RenalFailure excluded from all four models (see Section 2).
 
## --- Model 1: ESS ---
# m/15 budget ~4 predictors (m=67, smaller class among n=251)
# LiverDiagnosis collapsed to Hep C vs Other (1 df instead of 4) to fit budget,
# since only the Hep C level was significant univariably.
df$LiverDx_HepC <- factor(ifelse(df$LiverDiagnosis==1,"HepC","Other"))
 
ess_model <- glm(ESS_binary ~ Gender_f + LiverDx_HepC + Recurrence_f + Rejection_f,
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
 
# NOTE: psqi_model above is fit on n=183 (only limited by PSQI_binary's own
# missingness, since Gender/Recurrence/AnyFibrosis/Depression have no NAs).
# psqi_full adds BMI, which has 23 missing values, dropping n to 165.
# anova() requires both models fit on the IDENTICAL set of rows (Tutorial 9),
# so the reduced model must be refit on psqi_full's subset before comparing -
# comparing psqi_model (n=183) directly against psqi_full (n=165) would error.
psqi_subset <- df[complete.cases(df[, c("PSQI_binary","Gender_f","Recurrence_f",
                                         "AnyFibrosis_f","Depression_f","BMI")]), ]
psqi_model_samesub <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f,
                           data=psqi_subset, family=binomial)
psqi_full_samesub  <- glm(PSQI_binary ~ Gender_f + Recurrence_f + AnyFibrosis_f + Depression_f + BMI,
                           data=psqi_subset, family=binomial)
anova(psqi_model_samesub, psqi_full_samesub, test="Chisq")  # BMI does not improve fit significantly, n=165 for both
 
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
# m/15 budget ~6-7 predictors (m=89, n = 237, smaller class among n=262)
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

df <- read.csv("project_data_cleaned.csv", na.strings = c("NA", ""))

## ---- 1. Recode / label variables we need -----------------------------------


                       
df <- df %>%
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
    
    # covariates for the adjusted models, as factors where appropriate
    Gender                      = factor(Gender, levels = c(1, 2),
                                         labels = c("Male", "Female")),
    Liver.Diagnosis             = factor(LiverDiagnosis),
    Recurrence.of.disease       = factor(Recurrence, levels = c(0, 1),
                                         labels = c("No", "Yes")),
    Rejection.graft.dysfunction = factor(Rejection, levels = c(0, 1),
                                         labels = c("No", "Yes")),
    Any.fibrosis                = factor(AnyFibrosis, levels = c(0, 1),
                                         labels = c("No", "Yes")),
    Renal.Failure                = factor(RenalFailure, levels = c(0, 1),
                                          labels = c("No", "Yes")),
    Depression                   = factor(Depression, levels = c(0, 1),
                                          labels = c("No", "Yes")),
    Corticoid                    = factor(Corticosteroid, levels = c(0, 1),
                                          labels = c("No", "Yes")),
    Age                          = Age,
    BMI                          = BMI,
    Time.from.transplant         = TimeSinceTransplant
  )

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

ess_plots  <- plot_scatter(df, "ESS",  "Epworth Sleepiness Scale (ESS)")
psqi_plots <- plot_scatter(df, "PSQI", "Pittsburgh Sleep Quality Index (PSQI)")
ais_plots  <- plot_scatter(df, "AIS",  "Athens Insomnia Scale (AIS)")

# Save all six scatterplots to file (2 outcomes x 3 instruments)
ggsave("scatter_ESS_PCS.png",  ess_plots$pcs,  width = 5, height = 4)
ggsave("scatter_ESS_MCS.png",  ess_plots$mcs,  width = 5, height = 4)
ggsave("scatter_PSQI_PCS.png", psqi_plots$pcs, width = 5, height = 4)
ggsave("scatter_PSQI_MCS.png", psqi_plots$mcs, width = 5, height = 4)
ggsave("scatter_AIS_PCS.png",  ais_plots$pcs,  width = 5, height = 4)
ggsave("scatter_AIS_MCS.png",  ais_plots$mcs,  width = 5, height = 4)

# Optional: combined 3x2 grid if gridExtra/patchwork is available
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- (ess_plots$pcs | ess_plots$mcs) /
    (psqi_plots$pcs | psqi_plots$mcs) /
    (ais_plots$pcs | ais_plots$mcs)
  ggsave("scatter_all_sleep_vs_qol.png", combined, width = 10, height = 12)
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

skew_tbl <- rbind(
  ESS  = check_skew(df$ESS),
  PSQI = check_skew(df$PSQI),
  AIS  = check_skew(df$AIS)
)
print(round(skew_tbl, 2))

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

cor_results <- bind_rows(
  cor_test_both(df$ESS,  df$PCS, "ESS",  "PCS"),
  cor_test_both(df$ESS,  df$MCS, "ESS",  "MCS"),
  cor_test_both(df$PSQI, df$PCS, "PSQI", "PCS"),
  cor_test_both(df$PSQI, df$MCS, "PSQI", "MCS"),
  cor_test_both(df$AIS,  df$PCS, "AIS",  "PCS"),
  cor_test_both(df$AIS,  df$MCS, "AIS",  "MCS")
)

print(cor_results)
# In the report: report Pearson's r when skewness is modest and the
# scatterplot/loess trend looks linear; report Spearman's rho (and explain why)
# for any instrument-outcome pair where skewness is high (e.g., |skew| > 1)
# or the scatterplot suggests a monotonic-but-curved relationship.

write.csv(cor_results, "correlation_results_sleep_vs_qol.csv", row.names = FALSE)

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

instruments <- list(
  ESS  = "ESS_dist",
  PSQI = "PSQI_dist",
  AIS  = "AIS_dist",
  Berlin = "BSS_dist"
)

pcs_table <- bind_rows(lapply(names(instruments), function(nm)
  compare_groups(df, instruments[[nm]], "PCS", nm)))

mcs_table <- bind_rows(lapply(names(instruments), function(nm)
  compare_groups(df, instruments[[nm]], "MCS", nm)))

cat("\n--- Physical QoL (PCS) by disturbance group ---\n")
print(pcs_table)
cat("\n--- Mental QoL (MCS) by disturbance group ---\n")
print(mcs_table)

write.csv(pcs_table, "group_comparison_PCS.csv", row.names = FALSE)
write.csv(mcs_table, "group_comparison_MCS.csv", row.names = FALSE)

## Boxplots to accompany the tables
make_boxplot <- function(data, group_var, outcome_var, ylab, title) {
  d <- data %>% filter(!is.na(.data[[group_var]]), !is.na(.data[[outcome_var]]))
  ggplot(d, aes(x = .data[[group_var]], y = .data[[outcome_var]])) +
    geom_boxplot(fill = "grey85", outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.4) +
    labs(x = NULL, y = ylab, title = title) +
    theme_bw()
}

for (nm in names(instruments)) {
  gv <- instruments[[nm]]
  ggsave(paste0("box_", nm, "_PCS.png"),
         make_boxplot(df, gv, "PCS", "SF-36 PCS", paste(nm, "- Physical QoL")),
         width = 4.5, height = 4)
  ggsave(paste0("box_", nm, "_MCS.png"),
         make_boxplot(df, gv, "MCS", "SF-36 MCS", paste(nm, "- Mental QoL")),
         width = 4.5, height = 4)
}

## =============================================================================
## 11. ADJUSTED LINEAR REGRESSION (one sleep instrument at a time)
## =============================================================================
## Rationale for NOT combining ESS + PSQI + AIS + BSS in a single model:
##  - They are correlated measures of overlapping/related constructs
##    (multicollinearity would inflate SEs and make coefficients unstable
##    and hard to interpret - check with VIF below on a combined model to
##    illustrate this).
##  - PSQI has ~32% missingness in this dataset; forcing it into every
##    model would needlessly drop ~1/3 of subjects from all analyses.
##  - Interpretation is much cleaner one instrument at a time: "a one-point
##    increase in PSQI is associated with an X-point change in PCS/MCS,
##    holding covariates constant."
##
## Adjustment set (per the assignment's demographic/clinical variable list):
##   Age, Gender, BMI, Time.from.transplant, Liver.Diagnosis,
##   Recurrence.of.disease, Rejection.graft.dysfunction, Any.fibrosis,
##   Renal.Failure, Depression, Corticoid

covariates <- c("Age", "Gender", "BMI", "Time.from.transplant",
                "Liver.Diagnosis", "Recurrence.of.disease",
                "Rejection.graft.dysfunction", "Any.fibrosis",
                "Renal.Failure", "Depression", "Corticoid")

fit_adjusted <- function(data, sleep_var, outcome_var, covars) {
  form <- as.formula(
    paste(outcome_var, "~", sleep_var, "+", paste(covars, collapse = " + "))
  )
  model_data <- data %>% select(all_of(outcome_var), all_of(sleep_var), all_of(covars)) %>%
    na.omit()
  fit <- lm(form, data = model_data)
  list(fit = fit, n = nrow(model_data), formula = form)
}

sleep_vars <- c("ESS", "PSQI", "AIS", "BSS")
outcomes   <- c("PCS", "MCS")

adjusted_models <- list()
for (s in sleep_vars) {
  for (o in outcomes) {
    key <- paste(s, o, sep = "_")
    adjusted_models[[key]] <- fit_adjusted(df, s, o, covariates)
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

## ---- 11c. Illustration: why NOT to combine all 4 instruments ---------------
## Fit one combined ("kitchen sink") model on PCS with complete cases across
## all four instruments simultaneously, purely to demonstrate collinearity /
## sample-size loss - NOT recommended as a primary model.

combined_data <- df %>%
  select(PCS, ESS, PSQI, AIS, BSS, all_of(covariates)) %>%
  na.omit()

cat("\nSample size available for the 'kitchen sink' combined model: n =",
    nrow(combined_data), "vs. n =", adjusted_models[["PSQI_PCS"]]$n,
    "for the PSQI-only adjusted model.\n")

combined_fit <- lm(PCS ~ ESS + PSQI + AIS + BSS +
                     Age + Gender + BMI + Time.from.transplant +
                     Liver.Diagnosis + Recurrence.of.disease +
                     Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid,
                   data = combined_data)

cat("\nVIFs in the combined ('kitchen sink') model (illustrative only):\n")
print(round(vif(combined_fit), 2))

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

sleep_effect_summary <- bind_rows(lapply(names(adjusted_models), extract_sleep_coef))
cat("\n--- Adjusted effect of each sleep instrument on QoL outcomes ---\n")
print(sleep_effect_summary)
write.csv(sleep_effect_summary, "adjusted_sleep_effects_summary.csv", row.names = FALSE)

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

full_model_results <- bind_rows(lapply(names(adjusted_models), extract_full_model))

# Optional: flag which terms are significant at the 0.05 level, for quick scanning
full_model_results <- full_model_results %>%
  mutate(significant = ifelse(p.value < 0.05, "*", ""))

print(full_model_results)

write.csv(full_model_results, "full_model_coefficients.csv", row.names = FALSE)

## =============================================================================
## End of Analysis 4 script.
## Suggested report text for each significant sleep-instrument coefficient:
##   "A one-point increase in <instrument> was associated with an estimated
##    <beta>-point <increase/decrease> in <PCS/MCS> (95% CI: <low>, <high>,
##    p = <p>), after adjustment for age, gender, BMI, time since transplant,
##    liver diagnosis, recurrence of disease, rejection/graft dysfunction,
##    fibrosis, renal failure, depression, and corticosteroid use."
## =============================================================================
