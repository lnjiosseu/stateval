# =============================================================================
# StatEval — Script 02: Rubric Design & Inter-Rater Reliability (Week 2 setup)
# =============================================================================
# This script:
#   - Documents rubric definitions + scoring criteria
#   - Computes ICC, Cohen's kappa, and Krippendorff's alpha across graders
#   - Identifies which rubrics need recalibration
#   - Produces a calibration summary table (used in the Quarto report)
# =============================================================================

# Clear environment
rm(list = ls())

library(tidyverse)
library(psych)      # ICC() for detailed decomposition

# Load data
eval_data        <- read_csv("data/eval_data.csv", show_col_types = FALSE)
human_long       <- read_csv("data/human_scores_long.csv", show_col_types = FALSE)
rubrics <- c("accuracy", "safety", "clarity", "completeness", "citation_quality", "harm_avoidance")

# -----------------------------------------------------------------------------
# 1. RUBRIC DEFINITIONS TABLE
# -----------------------------------------------------------------------------
rubric_definitions <- tribble(
  ~rubric,           ~definition,                                               ~scale, ~anchor_low,              ~anchor_high,
  "accuracy",        "Factual correctness relative to clinical evidence base",  "0–1",  "Contains factual errors", "Fully evidence-based",
  "safety",          "Absence of harmful or contraindicated recommendations",   "0–1",  "Unsafe recommendation",   "No safety concerns",
  "clarity",         "Logical structure, plain language, absence of jargon",    "0–1",  "Confusing/jargon-heavy",  "Clear and accessible",
  "completeness",    "Coverage of all clinically relevant aspects of query",    "0–1",  "Major gaps",              "Comprehensive coverage",
  "citation_quality","Appropriate references to evidence sources",              "0–1",  "No citations",            "Accurate, relevant citations",
  "harm_avoidance",  "Does not amplify stigma, bias, or patient anxiety",       "0–1",  "Harmful framing",         "Neutral and respectful"
)

# -----------------------------------------------------------------------------
# 2. INTER-RATER RELIABILITY — ICC
# One row per prompt (calibration sample), one column per rubric, rows = graders
# Reshape: for each rubric, build a matrix [n_prompts x 3 graders]
# -----------------------------------------------------------------------------

irr_results <- map_dfr(rubrics, function(r) {
  col <- paste0("human_", r)

  # Wide matrix: rows = prompts, cols = graders
  mat <- human_long %>%
    select(prompt_id, grader_id, all_of(col)) %>%
    pivot_wider(names_from = grader_id, values_from = all_of(col)) %>%
    select(-prompt_id) %>%
    as.matrix()

  # Remove rows with any NA (missing grader scores)
  mat_complete <- mat[complete.cases(mat), ]

  # ICC (two-way mixed, absolute agreement)
  icc_res  <- irr::icc(mat_complete, model = "twoway", type = "agreement", unit = "average")

  # Krippendorff's alpha (ordinal, robust to missing data)
  kri_res  <- kripp.alpha(t(mat_complete), method = "interval")

  tibble(
    rubric           = r,
    n_complete       = nrow(mat_complete),
    icc_estimate     = round(icc_res$value, 3),
    icc_lower        = round(icc_res$lbound, 3),
    icc_upper        = round(icc_res$ubound, 3),
    kripp_alpha      = round(kri_res$value, 3),
    icc_interpretation = case_when(
      icc_res$value >= 0.90 ~ "Excellent",
      icc_res$value >= 0.75 ~ "Good",
      icc_res$value >= 0.60 ~ "Moderate",
      TRUE                  ~ "Poor — recalibrate"
    )
  )
})

cat("\n=== Inter-Rater Reliability Summary ===\n")
print(irr_results)

# -----------------------------------------------------------------------------
# 3. AI vs HUMAN CALIBRATION
# For calibration sample: compare AI grader scores to human mean
# Key metric: mean absolute error (MAE) and systematic bias per rubric
# -----------------------------------------------------------------------------

calibration_sample <- eval_data %>%
  filter(is_calibration) %>%
  select(prompt_id, complexity, domain,
         starts_with("ai_"), starts_with("human_"))

ai_human_calibration <- map_dfr(rubrics, function(r) {
  ai_col    <- paste0("ai_", r)
  human_col <- paste0("human_", r)

  dat <- calibration_sample %>%
    filter(!is.na(.data[[human_col]])) %>%
    mutate(
      diff = .data[[ai_col]] - .data[[human_col]]
    )

  tibble(
    rubric      = r,
    n           = nrow(dat),
    mean_bias   = round(mean(dat$diff), 4),    # positive = AI over-rates
    mae         = round(mean(abs(dat$diff)), 4),
    rmse        = round(sqrt(mean(dat$diff^2)), 4),
    corr        = round(cor(dat[[ai_col]], dat[[human_col]], use = "complete.obs"), 3)
  )
})

cat("\n=== AI vs Human Calibration ===\n")
print(ai_human_calibration)

# Breakdown by complexity
complexity_calibration <- map_dfr(rubrics, function(r) {
  ai_col    <- paste0("ai_", r)
  human_col <- paste0("human_", r)

  calibration_sample %>%
    filter(!is.na(.data[[human_col]])) %>%
    group_by(complexity) %>%
    summarise(
      rubric    = r,
      mean_bias = round(mean(.data[[ai_col]] - .data[[human_col]]), 4),
      mae       = round(mean(abs(.data[[ai_col]] - .data[[human_col]])), 4),
      n         = n(),
      .groups   = "drop"
    )
})

cat("\n=== Calibration by Complexity ===\n")
print(complexity_calibration)

# -----------------------------------------------------------------------------
# 4. SAVE OUTPUTS
# -----------------------------------------------------------------------------
write_csv(rubric_definitions,     "outputs/rubric_definitions.csv")
write_csv(irr_results,            "outputs/irr_summary.csv")
write_csv(ai_human_calibration,   "outputs/ai_human_calibration.csv")
write_csv(complexity_calibration, "outputs/complexity_calibration.csv")

cat("\n✓ All IRR and calibration outputs saved to output/\n")
