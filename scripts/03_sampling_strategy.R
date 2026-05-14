# =============================================================================
# StatEval — Script 03: Sampling Strategy & Power Analysis (Week 2)
# =============================================================================
# Core question Dr. Cho raised: "how do we know how many samples, how do we
# know how we should sample to make sure we're actually doing the right
# evaluation?"
#
# This script answers that rigorously:
#   1. Simple random sampling vs stratified sampling comparison
#   2. Power analysis: minimum n to detect a meaningful rubric difference
#   3. Optimal allocation across strata (Neyman allocation)
#   4. Bootstrap variance estimates to validate sampling design
# =============================================================================

# Clear environment
rm(list = ls())

library(tidyverse)
library(pwr)         # power calculations
library(boot)        # bootstrap

set.seed(42)

eval_data <- read_csv("data/eval_data.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. DEFINE STRATA
# Strata = complexity × domain (15 cells)
# Goal: ensure rare cells (complex × medication) are adequately represented
# -----------------------------------------------------------------------------

strata_summary <- eval_data %>%
  group_by(complexity, domain) %>%
  summarise(
    N_h       = n(),
    mean_ai   = round(mean(ai_composite), 3),
    sd_ai     = round(sd(ai_composite),   3),
    .groups   = "drop"
  ) %>%
  mutate(
    stratum = paste0(complexity, "_", domain),
    weight  = N_h / nrow(eval_data)
  )

cat("=== Stratum Summary ===\n")
print(strata_summary, n = 20)

# -----------------------------------------------------------------------------
# 2. POWER ANALYSIS
# Minimum detectable effect: 0.05 difference in composite rubric score
# (meaningful in clinical context — 5 percentage points)
# Significance level: 0.05, Power: 0.80
# -----------------------------------------------------------------------------

# Pooled SD from data
pooled_sd <- sd(eval_data$ai_composite)
mde       <- 0.05   # minimum detectable effect in score units
effect_d  <- mde / pooled_sd  # Cohen's d

pwr_result <- pwr.t.test(
  d    = effect_d,
  sig.level = 0.05,
  power     = 0.80,
  type      = "two.sample"
)

cat("\n=== Power Analysis ===\n")
cat(sprintf("Pooled SD             : %.4f\n", pooled_sd))
cat(sprintf("Minimum detectable Δ  : %.3f score points\n", mde))
cat(sprintf("Cohen's d             : %.3f\n", effect_d))
cat(sprintf("Required n per group  : %d\n", ceiling(pwr_result$n)))
cat(sprintf("Total n (two groups)  : %d\n", 2 * ceiling(pwr_result$n)))

# Power curve across sample sizes
power_curve <- tibble(
  n     = seq(20, 400, by = 10),
  power = map_dbl(n, ~ pwr.t.test(n = .x, d = effect_d, sig.level = 0.05,
                                    type = "two.sample")$power)
)

# Also compute required n across different MDEs
mde_sensitivity <- tibble(
  mde_points = c(0.02, 0.03, 0.05, 0.08, 0.10),
  cohen_d    = mde_points / pooled_sd,
  n_per_group = map_int(cohen_d, ~ ceiling(
    pwr.t.test(d = .x, sig.level = 0.05, power = 0.80, type = "two.sample")$n
  ))
)

cat("\n=== MDE Sensitivity Table ===\n")
print(mde_sensitivity)

# -----------------------------------------------------------------------------
# 3. NEYMAN OPTIMAL ALLOCATION
# Allocate total sample budget (n=100) across strata proportional to N_h * SD_h
# This minimizes variance of the stratified estimator
# -----------------------------------------------------------------------------

TOTAL_BUDGET <- 100  # human calibration budget

neyman <- strata_summary %>%
  mutate(
    nh_sigma  = N_h * sd_ai,
    alloc_raw = nh_sigma / sum(nh_sigma) * TOTAL_BUDGET,
    n_alloc   = pmax(ceiling(alloc_raw), 2),  # minimum 2 per stratum
  ) %>%
  mutate(
    n_alloc = round(n_alloc * TOTAL_BUDGET / sum(n_alloc))  # rescale to budget
  )

cat("\n=== Neyman Allocation (budget =", TOTAL_BUDGET, ") ===\n")
print(neyman %>% select(stratum, N_h, mean_ai, sd_ai, n_alloc), n = 20)
cat("Total allocated:", sum(neyman$n_alloc), "\n")

# -----------------------------------------------------------------------------
# 4. BOOTSTRAP VARIANCE: SRS vs STRATIFIED
# Compare precision of composite score estimator under two designs
# -----------------------------------------------------------------------------

# Simple Random Sampling bootstrap
srs_boot <- function(data, indices) {
  mean(data[indices, "ai_composite"])
}
srs_boot_result <- boot(as.data.frame(eval_data), srs_boot, R = 1000)

# Stratified bootstrap: sample proportional to Neyman allocation
stratified_mean <- function(data, neyman_alloc) {
  replicate(1000, {
    strata_names <- neyman_alloc$stratum

    stratum_means <- map_dbl(strata_names, function(s) {
      parts     <- str_split(s, "_", n = 2)[[1]]
      comp_lev  <- parts[1]
      dom_lev   <- paste(parts[-1], collapse = "_")

      stratum_data <- data %>%
        filter(complexity == comp_lev, domain == dom_lev)

      n_draw <- neyman_alloc$n_alloc_adj[neyman_alloc$stratum == s]
      n_draw <- min(n_draw, nrow(stratum_data))

      mean(sample(stratum_data$ai_composite, n_draw, replace = TRUE))
    })

    weights <- neyman_alloc$weight[match(strata_names, neyman_alloc$stratum)]
    weighted.mean(stratum_means, weights)
  })
}

strat_estimates <- stratified_mean(eval_data, neyman)

cat("\n=== Variance Comparison: SRS vs Stratified ===\n")
cat(sprintf("SRS     SE : %.5f\n", sd(srs_boot_result$t)))
cat(sprintf("Stratified SE : %.5f\n", sd(strat_estimates)))
cat(sprintf("Variance reduction : %.1f%%\n",
            (1 - var(strat_estimates) / var(srs_boot_result$t)) * 100))

# -----------------------------------------------------------------------------
# 5. SAVE OUTPUTS
# -----------------------------------------------------------------------------
write_csv(power_curve,       "outputs/power_curve.csv")
write_csv(mde_sensitivity,   "outputs/mde_sensitivity.csv")
write_csv(neyman,            "outputs/neyman_allocation.csv")

cat("\n✓ Sampling outputs saved to output/\n")
