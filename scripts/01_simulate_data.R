# =============================================================================
# StatEval: LLM Evaluation Framework with Causal Analysis
# Script 01 — Data Simulation (Week 1)
# =============================================================================
# Generates two datasets:
#   1. eval_data.csv  — LLM responses with rubric scores (human + AI grader)
#   2. user_data.csv  — User interaction logs for causal analysis
# =============================================================================

# Clear environment
rm(list = ls())

library(tidyverse)
library(truncnorm)

set.seed(42)

N_PROMPTS    <- 300   # number of prompt-response pairs
N_GRADERS    <- 3     # number of human graders per response (subset)
CALIBRATION_SAMPLE_FRAC <- 0.10  # 10% of responses get full human calibration

# -----------------------------------------------------------------------------
# 1. PROMPT METADATA
# -----------------------------------------------------------------------------
# Domain: clinical/health Q&A — on-brand given your background and Dr. Cho's team

prompt_domains <- c("diagnosis", "medication", "nutrition", "mental_health", "preventive_care")
prompt_complexity <- c("simple", "moderate", "complex")  # drives rubric score distributions

prompts <- tibble(
  prompt_id       = sprintf("P%04d", 1:N_PROMPTS),
  domain          = sample(prompt_domains, N_PROMPTS, replace = TRUE,
                           prob = c(0.25, 0.20, 0.15, 0.20, 0.20)),
  complexity      = sample(prompt_complexity, N_PROMPTS, replace = TRUE,
                           prob = c(0.35, 0.40, 0.25)),
  prompt_length   = case_when(
    complexity == "simple"   ~ as.integer(round(rtruncnorm(N_PROMPTS, a=10, b=80,  mean=30, sd=10))),
    complexity == "moderate" ~ as.integer(round(rtruncnorm(N_PROMPTS, a=40, b=200, mean=90, sd=30))),
    complexity == "complex"  ~ as.integer(round(rtruncnorm(N_PROMPTS, a=100,b=400, mean=200,sd=60)))
  ),
  has_context     = rbinom(N_PROMPTS, 1, prob = ifelse(complexity == "simple", 0.2, 0.6))
)

# -----------------------------------------------------------------------------
# 2. RUBRIC DEFINITIONS
# Six rubrics following clinical NLP evaluation literature
# -----------------------------------------------------------------------------
rubrics <- c("accuracy", "safety", "clarity", "completeness", "citation_quality", "harm_avoidance")

# True underlying quality (latent, unobserved) — function of complexity + domain
latent_quality <- with(prompts, {
  base <- case_when(
    complexity == "simple"   ~ rtruncnorm(N_PROMPTS, a=0, b=1, mean=0.78, sd=0.12),
    complexity == "moderate" ~ rtruncnorm(N_PROMPTS, a=0, b=1, mean=0.65, sd=0.15),
    complexity == "complex"  ~ rtruncnorm(N_PROMPTS, a=0, b=1, mean=0.52, sd=0.18)
  )
  # Domain modifier: medication and diagnosis have lower base quality (harder)
  domain_mod <- case_when(
    prompts$domain == "medication"  ~ -0.08,
    prompts$domain == "diagnosis"   ~ -0.05,
    prompts$domain == "preventive_care" ~ 0.06,
    TRUE ~ 0
  )
  pmin(pmax(base + domain_mod, 0), 1)
})

# -----------------------------------------------------------------------------
# 3. AI GRADER SCORES (available for all N_PROMPTS)
# AI grader is well-calibrated but has rubric-specific biases
# -----------------------------------------------------------------------------
rubric_ai_bias <- c(
  accuracy         =  0.03,   # slight over-estimation
  safety           = -0.02,   # slight under-estimation (conservative)
  clarity          =  0.05,   # over-rates clarity
  completeness     = -0.04,   # under-rates completeness on complex prompts
  citation_quality =  0.00,
  harm_avoidance   =  0.02
)

ai_scores <- map_dfc(rubrics, function(r) {
  noise <- rnorm(N_PROMPTS, mean = rubric_ai_bias[r], sd = 0.08)
  score <- pmin(pmax(latent_quality + noise, 0), 1)
  round(score, 3)
}) %>% set_names(paste0("ai_", rubrics))

# -----------------------------------------------------------------------------
# 4. HUMAN GRADER SCORES (calibration sample only — 10% of prompts)
# Multiple graders with individual random effects + systematic fatigue
# -----------------------------------------------------------------------------
calibration_ids <- sample(prompts$prompt_id, size = round(N_PROMPTS * CALIBRATION_SAMPLE_FRAC))

# Three graders with different biases and noise levels
grader_profiles <- tibble(
  grader_id = c("G01", "G02", "G03"),
  bias      = c( 0.04, -0.03,  0.00),   # positive/negative/neutral
  noise_sd  = c( 0.07,  0.10,  0.06),   # G02 is least reliable
  fatigue   = c( 0.00,  0.02,  0.01)    # score drift per 10 prompts reviewed
)

human_scores_long <- map_dfr(calibration_ids, function(pid) {
  idx <- which(prompts$prompt_id == pid)
  lq  <- latent_quality[idx]
  map_dfr(seq_len(nrow(grader_profiles)), function(g) {
    gp <- grader_profiles[g, ]
    scores <- setNames(
      lapply(rubrics, function(rb) round(pmin(pmax(lq + rnorm(1, gp$bias, gp$noise_sd), 0), 1), 3)),
      rubrics
    )
    bind_cols(tibble(prompt_id = pid, grader_id = gp$grader_id), as_tibble(scores))
  })
})

# Wide format: one row per prompt-grader pair
human_scores_wide <- human_scores_long %>%
  select(prompt_id, grader_id, all_of(rubrics)) %>%
  rename_with(~ paste0("human_", .), all_of(rubrics))

# -----------------------------------------------------------------------------
# 5. FINAL EVAL DATASET
# One row per prompt; AI scores for all, human scores for calibration subset
# -----------------------------------------------------------------------------
eval_data <- prompts %>%
  bind_cols(ai_scores) %>%
  mutate(
    is_calibration  = prompt_id %in% calibration_ids,
    latent_quality  = round(latent_quality, 3),
    # Composite AI score (equal-weighted mean across rubrics)
    ai_composite    = round(rowMeans(across(starts_with("ai_"))), 3)
  )

# Attach human scores (mean across graders for the calibration sample)
human_means <- human_scores_wide %>%
  group_by(prompt_id) %>%
  summarise(across(starts_with("human_"), mean, .names = "{.col}"), .groups = "drop") %>%
  mutate(across(starts_with("human_"), ~ round(., 3)))

eval_data <- eval_data %>%
  left_join(human_means, by = "prompt_id") %>%
  mutate(
    human_composite = round(rowMeans(across(starts_with("human_")), na.rm = TRUE), 3)
  )

# -----------------------------------------------------------------------------
# 6. USER INTERACTION DATA (for causal analysis in Module 2)
# Simulates session-level logs: user characteristics + prompt usage + outcomes
# -----------------------------------------------------------------------------
N_USERS <- 500

user_data <- tibble(
  user_id        = sprintf("U%04d", 1:N_USERS),
  user_expertise = sample(c("novice", "intermediate", "expert"), N_USERS, replace = TRUE,
                          prob = c(0.45, 0.35, 0.20)),
  age_group      = sample(c("18-34", "35-54", "55+"), N_USERS, replace = TRUE,
                          prob = c(0.35, 0.40, 0.25)),
  # Treatment: does the user include clinical context in their prompts?
  # This is the "treatment" in our causal analysis
  uses_context   = rbinom(N_USERS, 1, prob = case_when(
    user_expertise == "expert"       ~ 0.72,
    user_expertise == "intermediate" ~ 0.45,
    TRUE                              ~ 0.22
  )),
  n_sessions     = rpois(N_USERS, lambda = 8) + 1,
  platform       = sample(c("web", "mobile", "api"), N_USERS, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15))
) %>%
  mutate(
    # Propensity to use context depends on expertise — a confounder
    # Outcome: mean satisfaction score (1-5) — affected by context use + expertise
    satisfaction = round(
      pmin(pmax(
        2.5 +
          0.4 * uses_context +
          case_when(
            user_expertise == "expert"       ~  0.6,
            user_expertise == "intermediate" ~  0.2,
            TRUE                              ~ -0.1
          ) +
          rnorm(N_USERS, 0, 0.4),
        1), 5), 2
    ),
    # Outcome 2: did the user find the response medically actionable?
    actionable = rbinom(N_USERS, 1, prob = plogis(
      -0.5 +
        0.8 * uses_context +
        case_when(
          user_expertise == "expert"       ~  1.2,
          user_expertise == "intermediate" ~  0.4,
          TRUE                              ~ -0.2
        )
    ))
  )

# -----------------------------------------------------------------------------
# 7. SAVE DATA
# -----------------------------------------------------------------------------
write_csv(eval_data,      "data/eval_data.csv")
write_csv(user_data,      "data/user_data.csv")
write_csv(human_scores_wide, "data/human_scores_long.csv")

cat("\n✓ eval_data.csv      :", nrow(eval_data), "rows,", ncol(eval_data), "cols\n")
cat("✓ user_data.csv      :", nrow(user_data), "rows,", ncol(user_data), "cols\n")
cat("✓ human_scores_long.csv:", nrow(human_scores_wide), "rows\n")
cat("\nCalibration sample   :", sum(eval_data$is_calibration), "prompts with human scores\n")
cat("Rubrics              :", paste(rubrics, collapse=", "), "\n")
