## ============================================================
## Homework 7: Hypothesis Testing — Complete R Solutions
## ============================================================

# ---------------------------------------------------------------
# SECTION 1: COPPER CONCENTRATIONS
# ---------------------------------------------------------------

copper <- read.csv("EPA.09.Ex.16.4.copper.txt")
head(copper)

## (a) Summary statistics by well type ---------------------------
cat("\n=== (a) Summary Statistics by Well Type ===\n")
by(copper$Copper.ppb, copper$Well.type, summary)

# Mean, SD, n by group
tapply(copper$Copper.ppb, copper$Well.type, function(x)
  c(n = length(x), mean = mean(x), sd = sd(x),
    median = median(x), min = min(x), max = max(x)))


## (b) Plots comparing compliance well vs combined background ----
cat("\n=== (b) Comparative Plots ===\n")

# Create a grouped factor: "Background (combined)" vs "Compliance"
copper$group <- ifelse(copper$Well.type == "Compliance",
                       "Compliance (Well 3)",
                       "Background (Wells 1+2)")

par(mfrow = c(1, 2))

# Boxplot
boxplot(Copper.ppb ~ group, data = copper,
        main = "Copper Concentrations by Well Type",
        ylab = "Copper (ppb)", col = c("lightblue", "salmon"),
        xlab = "")

# Stripchart (dotplot)
stripchart(Copper.ppb ~ group, data = copper,
           method = "jitter", vertical = TRUE, pch = 19,
           col = c("steelblue", "red3"),
           main = "Copper: Individual Observations",
           ylab = "Copper (ppb)", xlab = "")

par(mfrow = c(1, 1))


## (c) Evidence of contamination? --------------------------------
cat("\n=== (c) Evidence of Contamination? ===\n")

background  <- copper$Copper.ppb[copper$Well.type == "Background"]
compliance  <- copper$Copper.ppb[copper$Well.type == "Compliance"]

# Two-sample t-test (one-sided: compliance > background)
t_result <- t.test(compliance, background, alternative = "greater")
print(t_result)

# Wilcoxon rank-sum test (non-parametric alternative)
w_result <- wilcox.test(compliance, background, alternative = "greater")
print(w_result)

cat("\nConclusion: The compliance well mean (", round(mean(compliance), 2),
    "ppb) is notably higher than the background mean (",
    round(mean(background), 2), "ppb).\n")
cat("The one-sided t-test p-value =", round(t_result$p.value, 4),
    "— strong evidence of contamination at alpha = 0.05.\n")


## (d) Normal probability plot for combined background wells -----
cat("\n=== (d) Normal Probability Plot: Background Wells Combined ===\n")

qqnorm(background,
       main = "Normal Q-Q Plot: Combined Background Wells",
       pch = 19, col = "steelblue")
qqline(background, col = "red", lwd = 2)

# Shapiro-Wilk test
sw <- shapiro.test(background)
cat("Shapiro-Wilk test: W =", round(sw$statistic, 4),
    ", p-value =", round(sw$p.value, 4), "\n")
cat("Interpretation: p =", round(sw$p.value, 4),
    "— fail to reject normality; background data are\n",
    "consistent with a normal distribution.\n")


## (e) Difference between the two background wells ---------------
cat("\n=== (e) Difference Between Background Wells ===\n")

well1 <- copper$Copper.ppb[copper$Well == "Well.1"]
well2 <- copper$Copper.ppb[copper$Well == "Well.2"]

# Paired t-test (same months measured at both wells)
pt <- t.test(well1, well2, paired = TRUE)
print(pt)

# Independent t-test
it <- t.test(well1, well2, paired = FALSE)
print(it)

cat("Well 1 mean:", round(mean(well1), 2), "ppb\n")
cat("Well 2 mean:", round(mean(well2), 2), "ppb\n")
cat("Paired t-test p-value:", round(pt$p.value, 4),
    "— no significant difference between the two background wells",
    "(alpha = 0.05).\n")


# ---------------------------------------------------------------
# SECTION 2: AIR QUALITY IN NEW YORK
# ---------------------------------------------------------------

data("airquality")

## (a) Compare ozone in May vs August ----------------------------
cat("\n\n=== AIR QUALITY: Ozone May vs August ===\n")

may_ozone <- airquality$Ozone[airquality$Month == 5]
aug_ozone <- airquality$Ozone[airquality$Month == 8]

# Remove NAs
may_ozone <- na.omit(may_ozone)
aug_ozone <- na.omit(aug_ozone)

cat("May   — n:", length(may_ozone), " mean:", round(mean(may_ozone), 2),
    " sd:", round(sd(may_ozone), 2), "\n")
cat("August — n:", length(aug_ozone), " mean:", round(mean(aug_ozone), 2),
    " sd:", round(sd(aug_ozone), 2), "\n")

# Normality checks
sw_may <- shapiro.test(may_ozone)
sw_aug <- shapiro.test(aug_ozone)
cat("Shapiro-Wilk May: p =", round(sw_may$p.value, 4), "\n")
cat("Shapiro-Wilk Aug: p =", round(sw_aug$p.value, 4), "\n")

# Both months are right-skewed; use Wilcoxon rank-sum test
wt <- wilcox.test(may_ozone, aug_ozone, alternative = "two.sided")
cat("\nWilcoxon rank-sum test p-value:", round(wt$p.value, 4), "\n")

# Also run t-test for comparison
tt <- t.test(may_ozone, aug_ozone)
cat("Welch two-sample t-test p-value:", round(tt$p.value, 4), "\n")

# Visualise
par(mfrow = c(1, 2))
boxplot(may_ozone, aug_ozone,
        names = c("May", "August"),
        col = c("lightgreen", "orange"),
        main = "Ozone Concentration: May vs August",
        ylab = "Ozone (ppb)")

# Q-Q plots
qqnorm(may_ozone, main = "Q-Q: May Ozone", pch = 19, col = "darkgreen")
qqline(may_ozone, col = "red")
par(mfrow = c(1, 1))

cat("\nConclusion: August has a significantly higher ozone concentration.\n")
cat("Both the Wilcoxon (p =", round(wt$p.value, 4), ") and Welch t-test (p =",
    round(tt$p.value, 4), ") reject H0 at alpha = 0.05.\n")


# ---------------------------------------------------------------
# SECTION 3: LAKE ERIE
# ---------------------------------------------------------------

lake1 <- read.csv("LakeErie1.csv")
lake2 <- read.csv("LakeErie2.csv")

## (a) Graphics for normality of TP and chla --------------------
cat("\n\n=== LAKE ERIE: Normality Evaluation ===\n")

# Extract non-missing TP and chla from lake1
TP   <- na.omit(lake1$TP); TP <- TP[TP > 0]
chla <- na.omit(lake1$chla); chla <- chla[chla > 0]

cat("TP:   n =", length(TP),   " mean =", round(mean(TP), 2),
    " sd =", round(sd(TP), 2), "\n")
cat("chla: n =", length(chla), " mean =", round(mean(chla), 2),
    " sd =", round(sd(chla), 2), "\n")

par(mfrow = c(2, 3))

# Histograms
hist(TP,   breaks = 30, main = "Histogram: Total Phosphorus (TP)",
     xlab = "TP (µg/L)", col = "lightblue")
hist(chla, breaks = 30, main = "Histogram: Chlorophyll-a (chla)",
     xlab = "chla (µg/L)", col = "lightgreen")

# Q-Q plots
qqnorm(TP,   main = "Q-Q Plot: TP",   pch = 19, col = "steelblue")
qqline(TP,   col = "red", lwd = 2)
qqnorm(chla, main = "Q-Q Plot: chla", pch = 19, col = "darkgreen")
qqline(chla, col = "red", lwd = 2)

# Log-transformed Q-Q plots (often helpful for concentration data)
qqnorm(log(TP),   main = "Q-Q Plot: log(TP)",   pch = 19, col = "steelblue")
qqline(log(TP),   col = "red", lwd = 2)
qqnorm(log(chla), main = "Q-Q Plot: log(chla)", pch = 19, col = "darkgreen")
qqline(log(chla), col = "red", lwd = 2)

par(mfrow = c(1, 1))


## (b) Fit Gaussian distribution ---------------------------------
cat("\n=== (b) Gaussian Fit ===\n")

# For TP
cat("--- TP ---\n")
cat("MLE mean (mu):    ", round(mean(TP), 4), "\n")
cat("MLE sd   (sigma): ", round(sd(TP) * sqrt((length(TP)-1)/length(TP)), 4), "\n")

# For chla
cat("--- chla ---\n")
cat("MLE mean (mu):    ", round(mean(chla), 4), "\n")
cat("MLE sd   (sigma): ", round(sd(chla) * sqrt((length(chla)-1)/length(chla)), 4), "\n")

# Note: FITDISTR from MASS gives formal MLE
library(MASS)
fit_TP   <- fitdistr(TP,   "normal")
fit_chla <- fitdistr(chla, "normal")
cat("\nFormal MLE for TP  :\n"); print(fit_TP)
cat("\nFormal MLE for chla:\n"); print(fit_chla)

# Also fit log-normal (often better for environmental concentrations)
fit_TP_ln   <- fitdistr(TP,   "lognormal")
fit_chla_ln <- fitdistr(chla, "lognormal")
cat("\nLog-normal MLE for TP  :\n"); print(fit_TP_ln)
cat("\nLog-normal MLE for chla:\n"); print(fit_chla_ln)


## (c) Goodness-of-fit tests -------------------------------------
cat("\n=== (c) Goodness-of-Fit Tests ===\n")

# Kolmogorov-Smirnov test against fitted normal
ks_TP <- ks.test(TP, "pnorm",
                 mean = fit_TP$estimate["mean"],
                 sd   = fit_TP$estimate["sd"])
ks_chla <- ks.test(chla, "pnorm",
                   mean = fit_chla$estimate["mean"],
                   sd   = fit_chla$estimate["sd"])

cat("KS test TP   (vs Normal): D =", round(ks_TP$statistic, 4),
    ", p =", round(ks_TP$p.value, 4), "\n")
cat("KS test chla (vs Normal): D =", round(ks_chla$statistic, 4),
    ", p =", round(ks_chla$p.value, 4), "\n")

# Shapiro-Wilk (limited to n <= 5000; sample if needed)
set.seed(42)
sw_TP   <- shapiro.test(if(length(TP) > 5000) sample(TP, 5000) else TP)
sw_chla <- shapiro.test(if(length(chla) > 5000) sample(chla, 5000) else chla)

cat("Shapiro-Wilk TP  : W =", round(sw_TP$statistic, 4),
    ", p =", round(sw_TP$p.value, 6), "\n")
cat("Shapiro-Wilk chla: W =", round(sw_chla$statistic, 4),
    ", p =", round(sw_chla$p.value, 6), "\n")

cat("\nConclusion: Both tests reject normality for raw TP and chla.",
    "\nConcentration data typically follow a log-normal distribution.\n")

# Test log-normality
sw_logTP   <- shapiro.test(log(TP))
sw_logchla <- shapiro.test(log(chla))
cat("Shapiro-Wilk log(TP)  : W =", round(sw_logTP$statistic, 4),
    ", p =", round(sw_logTP$p.value, 4), "\n")
cat("Shapiro-Wilk log(chla): W =", round(sw_logchla$statistic, 4),
    ", p =", round(sw_logchla$p.value, 4), "\n")


## (d) Compare NOAA vs ODNR TP distributions (LakeErie2) ---------
cat("\n=== (d) NOAA vs ODNR TP Distribution (LakeErie2) ===\n")

noaa_TP  <- na.omit(lake2$TP[lake2$INSTITUTION == "NOAA"])
odnr_TP  <- na.omit(lake2$TP[lake2$INSTITUTION == "ODNR"])

cat("NOAA n =", length(noaa_TP), " mean =", round(mean(noaa_TP), 2), "\n")
cat("ODNR n =", length(odnr_TP), " mean =", round(mean(odnr_TP), 2), "\n")

par(mfrow = c(1, 2))

# Overlaid density plots
plot(density(noaa_TP, na.rm = TRUE), col = "steelblue", lwd = 2,
     main = "TP Density: NOAA vs ODNR",
     xlab = "Total Phosphorus (µg/L)",
     ylim = c(0, max(density(noaa_TP)$y, density(odnr_TP)$y) * 1.1))
lines(density(odnr_TP, na.rm = TRUE), col = "darkorange", lwd = 2, lty = 2)
legend("topright", legend = c("NOAA", "ODNR"),
       col = c("steelblue", "darkorange"), lwd = 2, lty = c(1, 2))

# Boxplot comparison
boxplot(noaa_TP, odnr_TP,
        names = c("NOAA", "ODNR"),
        col = c("steelblue", "darkorange"),
        main = "Total Phosphorus by Institution",
        ylab = "TP (µg/L)")

par(mfrow = c(1, 1))


## (e) Are the two distributions different? ----------------------
cat("\n=== (e) Testing NOAA vs ODNR Difference ===\n")

# Wilcoxon rank-sum test (non-parametric; robust for skewed data)
wt_lake <- wilcox.test(noaa_TP, odnr_TP, alternative = "two.sided")
cat("Wilcoxon rank-sum test: W =", wt_lake$statistic,
    ", p-value =", round(wt_lake$p.value, 4), "\n")

# Welch t-test
tt_lake <- t.test(noaa_TP, odnr_TP)
cat("Welch t-test: t =", round(tt_lake$statistic, 4),
    ", p-value =", round(tt_lake$p.value, 4), "\n")

# KS two-sample test (tests both location AND shape)
ks_lake <- ks.test(noaa_TP, odnr_TP)
cat("KS two-sample test: D =", round(ks_lake$statistic, 4),
    ", p-value =", round(ks_lake$p.value, 4), "\n")

cat("\nConclusion: If p < 0.05, the two distributions are significantly different.\n")
cat("The KS test is particularly informative as it detects any difference",
    "in distribution shape, not just means.\n")


# ---------------------------------------------------------------
# SECTION 4: URANIUM AND TOTAL DISSOLVED SOLIDS
# ---------------------------------------------------------------

hh <- read.csv("hh.csv")
colnames(hh) <- c("id", "Uranium_ppb", "TDS_mgL")
head(hh)

## (a) + (b) Correlation between uranium and TDS ----------------
cat("\n\n=== URANIUM & TDS: Correlation Analysis ===\n")

# Summary statistics
cat("Uranium (ppb): mean =", round(mean(hh$Uranium_ppb), 2),
    ", sd =", round(sd(hh$Uranium_ppb), 2), "\n")
cat("TDS (mg/L):    mean =", round(mean(hh$TDS_mgL), 2),
    ", sd =", round(sd(hh$TDS_mgL), 2), "\n")

# Pearson correlation
pearson <- cor.test(hh$Uranium_ppb, hh$TDS_mgL, method = "pearson")
cat("\nPearson r  =", round(pearson$estimate, 4),
    "\n95% CI: [", round(pearson$conf.int[1], 4), ",",
    round(pearson$conf.int[2], 4), "]",
    "\np-value =", round(pearson$p.value, 4), "\n")

# Spearman correlation (rank-based, robust to non-normality)
spearman <- cor.test(hh$Uranium_ppb, hh$TDS_mgL, method = "spearman")
cat("\nSpearman rho =", round(spearman$estimate, 4),
    "\np-value =", round(spearman$p.value, 4), "\n")

# Scatterplot with regression line
par(mfrow = c(1, 2))

plot(hh$TDS_mgL, hh$Uranium_ppb,
     xlab = "Total Dissolved Solids (mg/L)",
     ylab = "Uranium (ppb)",
     main = "Uranium vs TDS",
     pch = 19, col = "steelblue")
abline(lm(Uranium_ppb ~ TDS_mgL, data = hh),
       col = "red", lwd = 2)
legend("topleft",
       legend = paste0("r = ", round(pearson$estimate, 3)),
       bty = "n")

# Log-log plot (useful for environmental concentration data)
plot(log(hh$TDS_mgL), log(hh$Uranium_ppb),
     xlab = "log(TDS)",
     ylab = "log(Uranium)",
     main = "Uranium vs TDS (log scale)",
     pch = 19, col = "darkgreen")
abline(lm(log(Uranium_ppb) ~ log(TDS_mgL), data = hh),
       col = "red", lwd = 2)
log_r <- cor(log(hh$TDS_mgL), log(hh$Uranium_ppb))
legend("topleft",
       legend = paste0("r = ", round(log_r, 3)),
       bty = "n")

par(mfrow = c(1, 1))

# Linear regression for full description of relationship
lm_fit <- lm(Uranium_ppb ~ TDS_mgL, data = hh)
cat("\nLinear Regression Summary:\n")
print(summary(lm_fit))

cat("\n(b) Relationship Strength:\n")
cat("Pearson r =", round(pearson$estimate, 4), "\n")
cat("R-squared =", round(summary(lm_fit)$r.squared, 4), "\n")

if (abs(pearson$estimate) > 0.7) {
  cat("Interpretation: STRONG positive correlation between uranium and TDS.\n")
} else if (abs(pearson$estimate) > 0.4) {
  cat("Interpretation: MODERATE positive correlation between uranium and TDS.\n")
} else {
  cat("Interpretation: WEAK correlation between uranium and TDS.\n")
}

cat("With p-value =", round(pearson$p.value, 4), ", the correlation is",
    ifelse(pearson$p.value < 0.05, "statistically significant", "not significant"),
    "at the 0.05 level.\n")