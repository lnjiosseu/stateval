# =============================================================================
# StatEval — Script 04: Causal Inference
# Uses: MASS, base R (IPW/AIPW), tidyverse
# =============================================================================

# Clear environment
rm(list = ls())

library(tidyverse)

set.seed(42)
user_data <- read_csv("data/user_data.csv", show_col_types = FALSE)

user_model <- user_data %>%
  mutate(
    expertise_num = as.numeric(factor(user_expertise, levels = c("novice","intermediate","expert"))),
    age_num       = as.numeric(factor(age_group, levels = c("18-34","35-54","55+"))),
    platform_web  = as.integer(platform == "web"),
    platform_api  = as.integer(platform == "api")
  )

# --- Propensity score ---
ps_fit <- glm(uses_context ~ expertise_num + age_num + platform_web + platform_api + n_sessions,
              data = user_model, family = binomial())
user_model <- user_model %>%
  mutate(ps = fitted(ps_fit),
         ipw_ate = ifelse(uses_context == 1, 1/ps, 1/(1-ps)))

cat("=== PS Summary ===\n")
cat(sprintf("Treated PS: [%.3f, %.3f]  Control PS: [%.3f, %.3f]\n",
            min(user_model$ps[user_model$uses_context==1]),
            max(user_model$ps[user_model$uses_context==1]),
            min(user_model$ps[user_model$uses_context==0]),
            max(user_model$ps[user_model$uses_context==0])))

# --- Balance ---
wtd_var <- function(x,w){ w<-w/sum(w); xbar<-sum(w*x); sum(w*(x-xbar)^2) }
smd_fn  <- function(x,tr,w=NULL){
  if(is.null(w)) w<-rep(1,length(x))
  m1<-weighted.mean(x[tr==1],w[tr==1]); m0<-weighted.mean(x[tr==0],w[tr==0])
  s1<-sqrt(wtd_var(x[tr==1],w[tr==1]));  s0<-sqrt(wtd_var(x[tr==0],w[tr==0]))
  (m1-m0)/sqrt((s1^2+s0^2)/2)
}
covs <- c("expertise_num","age_num","platform_web","platform_api","n_sessions")
balance_table <- map_dfr(covs, ~tibble(
  covariate = .x,
  smd_unwtd = round(smd_fn(user_model[[.x]], user_model$uses_context), 3),
  smd_wtd   = round(smd_fn(user_model[[.x]], user_model$uses_context, user_model$ipw_ate), 3)
))
cat("\n=== Balance (SMD) ===\n"); print(balance_table)
cat(sprintf("Max |SMD| weighted: %.3f %s\n", max(abs(balance_table$smd_wtd)),
            ifelse(max(abs(balance_table$smd_wtd)) < 0.10, "✓","✗")))

# --- IPW outcome models ---
ipw_sat <- lm(satisfaction ~ uses_context + expertise_num + age_num,
              data = user_model, weights = ipw_ate)
ipw_act <- glm(actionable ~ uses_context + expertise_num + age_num,
               data = user_model, weights = ipw_ate, family = quasibinomial())

ate_sat <- coef(ipw_sat)["uses_context"]
se_sat  <- sqrt(diag(vcov(ipw_sat)))["uses_context"]
ci_sat  <- confint(ipw_sat)["uses_context",]
ate_act <- coef(ipw_act)["uses_context"]

cat(sprintf("\n=== IPW ===\nATE satisfaction: %.4f (SE=%.4f, 95%% CI [%.4f, %.4f])\n",
            ate_sat, se_sat, ci_sat[1], ci_sat[2]))
cat(sprintf("ATE actionable:   %.4f log-odds (OR=%.3f)\n", ate_act, exp(ate_act)))

# --- AIPW (doubly robust) ---
out_fit <- lm(satisfaction ~ uses_context * expertise_num + age_num + n_sessions, data = user_model)
user_model <- user_model %>% mutate(
  mu1 = predict(out_fit, newdata = mutate(user_model, uses_context=1)),
  mu0 = predict(out_fit, newdata = mutate(user_model, uses_context=0)),
  aipw = mu1 - mu0 +
    uses_context/(ps)         * (satisfaction - mu1) -
    (1-uses_context)/(1-ps)   * (satisfaction - mu0)
)
ate_aipw <- mean(user_model$aipw)
se_aipw  <- sd(user_model$aipw)/sqrt(nrow(user_model))
ci_aipw  <- ate_aipw + c(-1,1)*1.96*se_aipw
cat(sprintf("\n=== AIPW ===\nATE: %.4f  SE: %.4f  95%% CI [%.4f, %.4f]\n",
            ate_aipw, se_aipw, ci_aipw[1], ci_aipw[2]))

# --- E-value ---
std_ate   <- ate_aipw / sd(user_model$satisfaction)
approx_rr <- exp(0.91 * std_ate)
e_value   <- approx_rr + sqrt(approx_rr * (approx_rr - 1))
cat(sprintf("\n=== E-Value ===\nE-value: %.3f (unmeasured confounder needs RR >= %.2f)\n",
            e_value, e_value))

# --- HTE by expertise ---
hte_results <- user_model %>% group_by(user_expertise) %>%
  group_modify(~{
    fit <- lm(satisfaction ~ uses_context, data=.x, weights=.x$ipw_ate)
    tibble(cate=coef(fit)["uses_context"],
           se=sqrt(diag(vcov(fit)))["uses_context"], n=nrow(.x),
           ci_lower=coef(fit)["uses_context"]-1.96*sqrt(diag(vcov(fit)))["uses_context"],
           ci_upper=coef(fit)["uses_context"]+1.96*sqrt(diag(vcov(fit)))["uses_context"])
  }) %>% ungroup()
cat("\n=== CATE by Expertise ===\n"); print(hte_results)

# --- Save ---
write_csv(tibble(
  estimand = c("IPW-ATE (satisfaction)","IPW-ATE (actionable log-OR)","AIPW-ATE (satisfaction)"),
  estimate = round(c(ate_sat, ate_act, ate_aipw), 4),
  se       = round(c(se_sat, NA, se_aipw), 4),
  method   = c("Weighted OLS","Weighted logistic","Augmented IPW")
), "outputs/causal_results.csv")
write_csv(hte_results,   "outputs/hte_by_expertise.csv")
write_csv(balance_table, "outputs/balance_table.csv")
cat("\n✓ Causal outputs saved\n")