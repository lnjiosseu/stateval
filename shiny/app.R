# =============================================================================
# StatEval — Shiny Dashboard (Week 4)
# Interactive exploration of LLM evaluation results + causal analysis
# =============================================================================
# Run from project root: shiny::runApp("shiny/")
# Reads pre-computed outputs from ../outputs/
# =============================================================================

library(shiny)
library(tidyverse)
library(bslib)

# --- Data loading (relative to shiny/ folder) --------------------------------
DATA_PATH <- "../outputs"

irr       <- read_csv(file.path(DATA_PATH, "irr_summary.csv"),          show_col_types = FALSE)
calib     <- read_csv(file.path(DATA_PATH, "ai_human_calibration.csv"), show_col_types = FALSE)
comp_cal  <- read_csv(file.path(DATA_PATH, "complexity_calibration.csv"),show_col_types = FALSE)
power_crv <- read_csv(file.path(DATA_PATH, "power_curve.csv"),           show_col_types = FALSE)
mde_tab   <- read_csv(file.path(DATA_PATH, "mde_sensitivity.csv"),       show_col_types = FALSE)
neyman    <- read_csv(file.path(DATA_PATH, "neyman_allocation.csv"),     show_col_types = FALSE)
causal    <- read_csv(file.path(DATA_PATH, "causal_results.csv"),        show_col_types = FALSE)
hte       <- read_csv(file.path(DATA_PATH, "hte_by_expertise.csv"),      show_col_types = FALSE)
balance   <- read_csv(file.path(DATA_PATH, "balance_table.csv"),         show_col_types = FALSE)
rubric_def<- read_csv(file.path(DATA_PATH, "rubric_definitions.csv"),    show_col_types = FALSE)

# Pre-compute ATE for display
ate_val <- causal$estimate[causal$estimand == "AIPW-ATE (satisfaction)"]
e_val   <- {
  std_ate <- ate_val / 0.4  # approx outcome SD
  rr      <- exp(0.91 * std_ate)
  round(rr + sqrt(rr * (rr - 1)), 2)
}

# --- Colour palette ----------------------------------------------------------
COL_MAIN <- "#1a6b9a"
COL_WARN <- "#e67e22"
COL_GOOD <- "#27ae60"
COL_BAD  <- "#c0392b"

icc_colors <- c("Excellent" = COL_MAIN, "Good" = COL_GOOD,
                 "Moderate"  = COL_WARN, "Poor - recalibrate" = COL_BAD)

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "StatEval",
  theme = bs_theme(bootswatch = "flatly", primary = COL_MAIN),
  bg    = "#1a6b9a",
  inverse = TRUE,

  # ── Tab 1: Overview ────────────────────────────────────────────────────────
  nav_panel("Overview",
    div(class = "container-fluid mt-3",
      h3("StatEval: Rigorous LLM Evaluation in Clinical Health Q&A"),
      p("An end-to-end framework combining statistical evaluation design with causal inference.",
        "Built in R. Domain: clinical health Q&A — the highest-stakes context for LLM response quality."),
      hr(),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Rubrics evaluated",  value = "6",   theme = "primary"),
        value_box(title = "Human calibration n",value = "30 prompts",             theme = "success"),
        value_box(title = "AIPW ATE (satisfaction)", value = sprintf("+%.2f pts", ate_val),    theme = "info"),
        value_box(title = "E-value",            value = as.character(e_val),      theme = "secondary")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Framework Overview"),
          p(strong("Module 1 — Evaluation Pipeline")),
          tags$ul(
            tags$li("6-rubric framework: accuracy, safety, clarity, completeness, citation quality, harm avoidance"),
            tags$li("AI grader deployed at scale; human calibration on 10% stratified sample"),
            tags$li("Inter-rater reliability via ICC (two-way mixed) and Krippendorff's alpha"),
            tags$li("Neyman optimal allocation minimises calibration cost at fixed budget")
          ),
          p(strong("Module 2 — Causal Analysis")),
          tags$ul(
            tags$li("Treatment: does the user include clinical context in their prompt?"),
            tags$li("Confounder: user expertise (drives both context use and satisfaction)"),
            tags$li("Estimators: IPW and doubly-robust AIPW; E-value sensitivity analysis"),
            tags$li("Heterogeneous treatment effects estimated by expertise stratum")
          )
        ),
        card(
          card_header("Rubric Definitions"),
          tableOutput("rubric_table")
        )
      )
    )
  ),

  # ── Tab 2: Inter-Rater Reliability ─────────────────────────────────────────
  nav_panel("Reliability",
    div(class = "container-fluid mt-3",
      h4("Inter-Rater Reliability & AI-Human Calibration"),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("ICC Estimates with 95% CI"),
          plotOutput("icc_plot", height = "340px"),
          p(class = "text-muted small mt-1",
            "Dashed lines at 0.75 (Good) and 0.90 (Excellent). ICC3k: two-way mixed, absolute agreement, average unit.")
        ),
        card(
          card_header("IRR Summary Table"),
          tableOutput("irr_table")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("AI vs Human Calibration — select metric"),
          layout_columns(
            col_widths = c(3, 9),
            selectInput("calib_metric", NULL,
                        choices = c("Mean Bias (AI - Human)" = "mean_bias",
                                    "Mean Absolute Error"    = "mae",
                                    "RMSE"                   = "rmse",
                                    "Pearson Correlation"    = "corr"),
                        selected = "mean_bias"),
            plotOutput("calib_plot", height = "280px")
          )
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Calibration Bias by Complexity — select rubric"),
          layout_columns(
            col_widths = c(3, 9),
            selectInput("rubric_sel", NULL,
                        choices = c("accuracy","safety","clarity","completeness",
                                    "citation_quality","harm_avoidance"),
                        selected = "completeness"),
            plotOutput("complexity_cal_plot", height = "260px")
          )
        )
      )
    )
  ),

  # ── Tab 3: Sampling Design ──────────────────────────────────────────────────
  nav_panel("Sampling Design",
    div(class = "container-fluid mt-3",
      h4("Statistical Sampling Strategy"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Power Curve — adjust MDE"),
          sliderInput("mde_slider", "Minimum detectable effect (score points):",
                      min = 0.02, max = 0.15, value = 0.05, step = 0.01),
          plotOutput("power_plot", height = "300px")
        ),
        card(
          card_header("Sample Size by MDE"),
          tableOutput("mde_table"),
          p(class = "text-muted small", "Two-sample t-test, alpha=0.05, power=0.80. Pooled SD from simulated data.")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Neyman Optimal Allocation — budget n=100 across complexity x domain strata"),
          plotOutput("neyman_plot", height = "380px"),
          p(class = "text-muted small mt-1",
            "Bar width = allocated calibration samples. Colour = stratum SD (higher SD → more samples needed).")
        )
      )
    )
  ),

  # ── Tab 4: Causal Analysis ──────────────────────────────────────────────────
  nav_panel("Causal Analysis",
    div(class = "container-fluid mt-3",
      h4("Causal Effect of Clinical Context on User Satisfaction"),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Causal Structure (DAG)"),
          div(style = "font-family: monospace; font-size: 13px; line-height: 1.8; padding: 8px;",
            HTML(
              "user_expertise &rarr; <strong>uses_context</strong><br>
               user_expertise &rarr; <em>satisfaction</em><br>
               <strong>uses_context</strong> &rarr; <em>satisfaction</em> &nbsp;
               <span style='color:#1a6b9a'>[effect of interest]</span><br>
               age_group &rarr; <strong>uses_context</strong><br>
               platform &rarr; <strong>uses_context</strong><br>
               n_sessions &rarr; <em>satisfaction</em>"
            ),
            hr(),
            p(strong("Adjustment set:"), " {user_expertise, age_group, platform}"),
            p(strong("Estimator:"), " IPW (ATE) + AIPW (doubly-robust)"),
            p(strong("Sensitivity:"), " E-value = ", strong(as.character(e_val))),
            p(class = "text-muted small",
              "An unmeasured confounder would need RR \u2265 ", e_val,
              " with both treatment and outcome to explain away the effect.")
          )
        ),
        card(
          card_header("Covariate Balance Before/After IPW Weighting"),
          plotOutput("balance_plot", height = "280px"),
          p(class = "text-muted small", "Target: |SMD| < 0.10 after weighting.")
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Causal Estimates"),
          tableOutput("causal_table"),
          hr(),
          p(class = "text-muted small",
            "IPW = inverse probability weighting. AIPW = augmented IPW (doubly-robust): consistent if either the propensity score or outcome model is correctly specified.")
        ),
        card(
          card_header("Conditional ATE by User Expertise"),
          plotOutput("hte_plot", height = "280px"),
          p(class = "text-muted small",
            "Effect of including clinical context on satisfaction (1-5 scale). Points = CATE, bars = 95% CI.")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # --- Overview tab ---
  output$rubric_table <- renderTable({
    rubric_def %>% select(rubric, definition, scale)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- Reliability tab ---
  output$icc_plot <- renderPlot({
    irr %>%
      mutate(rubric = fct_reorder(rubric, icc_estimate)) %>%
      ggplot(aes(x = icc_estimate, xmin = icc_lower, xmax = icc_upper,
                 y = rubric, color = icc_interpretation)) +
      geom_pointrange(size = 0.9, linewidth = 0.8) +
      geom_vline(xintercept = c(0.75, 0.90), linetype = "dashed", color = "gray70") +
      annotate("text", x = 0.75, y = 0.55, label = "Good",      size = 3.2, color = "gray50", hjust = 1.1) +
      annotate("text", x = 0.90, y = 0.55, label = "Excellent", size = 3.2, color = "gray50", hjust = 1.1) +
      scale_color_manual(values = icc_colors) +
      scale_x_continuous(limits = c(0.70, 1.0)) +
      labs(x = "ICC (two-way mixed, absolute agreement)", y = NULL, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$irr_table <- renderTable({
    irr %>%
      select(rubric, icc_estimate, icc_lower, icc_upper, kripp_alpha, icc_interpretation) %>%
      rename(Rubric = rubric, ICC = icc_estimate,
             `CI Low` = icc_lower, `CI High` = icc_upper,
             `Kripp. alpha` = kripp_alpha, Interpretation = icc_interpretation)
  }, striped = TRUE, digits = 3)

  output$calib_plot <- renderPlot({
    metric_label <- names(which(c(
      "Mean Bias (AI - Human)" = "mean_bias",
      "Mean Absolute Error"    = "mae",
      "RMSE"                   = "rmse",
      "Pearson Correlation"    = "corr") == input$calib_metric))

    calib %>%
      ggplot(aes(x = rubric, y = .data[[input$calib_metric]], fill = rubric)) +
      geom_col(show.legend = FALSE, width = 0.6) +
      geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = metric_label, x = NULL, y = metric_label) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$complexity_cal_plot <- renderPlot({
    comp_cal %>%
      filter(rubric == input$rubric_sel) %>%
      mutate(complexity = factor(complexity, levels = c("simple","moderate","complex"))) %>%
      ggplot(aes(x = complexity, y = mean_bias, fill = complexity)) +
      geom_col(show.legend = FALSE, width = 0.5) +
      geom_errorbar(aes(ymin = mean_bias - mae, ymax = mean_bias + mae), width = 0.15) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      scale_fill_manual(values = c("simple" = "#2ecc71","moderate" = "#f39c12","complex" = "#c0392b")) +
      labs(title = paste("Mean bias (AI - Human) for:", input$rubric_sel),
           x = "Prompt complexity", y = "Mean bias ± MAE") +
      theme_minimal(base_size = 13)
  })

  # --- Sampling Design tab ---
  output$power_plot <- renderPlot({
    pooled_sd <- 0.179
    mde_sel   <- input$mde_slider
    d_sel     <- mde_sel / pooled_sd
    n_req     <- ceiling(pwr::pwr.t.test(d=d_sel, sig.level=0.05, power=0.80, type="two.sample")$n)

    power_crv %>%
      mutate(power_dynamic = map_dbl(n, ~pwr::pwr.t.test(n=.x, d=d_sel, sig.level=0.05, type="two.sample")$power)) %>%
      ggplot(aes(x = n, y = power_dynamic)) +
      geom_line(color = COL_MAIN, linewidth = 1.2) +
      geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = n_req, linetype = "dotted", color = COL_BAD) +
      annotate("text", x = n_req + 15, y = 0.45,
               label = paste0("n=", n_req, "\n(80% power)"),
               size = 3.5, color = COL_BAD, hjust = 0) +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      labs(title = sprintf("Power Curve (MDE = %.2f score points, Cohen's d = %.3f)", mde_sel, d_sel),
           x = "Sample size per group", y = "Statistical power") +
      theme_minimal(base_size = 13)
  })

  output$mde_table <- renderTable({
    mde_tab %>%
      rename(`Delta (pts)` = mde_points, `Cohen's d` = cohen_d, `n/group` = n_per_group)
  }, striped = TRUE, digits = 3)

  output$neyman_plot <- renderPlot({
    neyman %>%
      mutate(stratum = fct_reorder(stratum, n_alloc)) %>%
      ggplot(aes(x = n_alloc, y = stratum, fill = sd_ai)) +
      geom_col() +
      scale_fill_gradient(low = "#c8e6f5", high = COL_MAIN, name = "Stratum SD") +
      labs(x = "Allocated calibration samples (n)", y = NULL) +
      theme_minimal(base_size = 12)
  })

  # --- Causal Analysis tab ---
  output$balance_plot <- renderPlot({
    balance %>%
      pivot_longer(c(smd_unwtd, smd_wtd), names_to = "weighting", values_to = "smd") %>%
      mutate(
        weighting = recode(weighting, smd_unwtd = "Unweighted", smd_wtd = "IPW weighted"),
        weighting = factor(weighting, levels = c("Unweighted","IPW weighted"))
      ) %>%
      ggplot(aes(x = abs(smd), y = covariate, color = weighting, shape = weighting)) +
      geom_point(size = 3.5) +
      geom_vline(xintercept = 0.10, linetype = "dashed", color = "gray50") +
      annotate("text", x = 0.10, y = 0.5, label = "threshold\n(0.10)", size = 3, color = "gray40", hjust = -0.1) +
      scale_color_manual(values = c("Unweighted" = COL_WARN, "IPW weighted" = COL_MAIN)) +
      labs(x = "|Standardized Mean Difference|", y = NULL, color = NULL, shape = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$causal_table <- renderTable({
    causal %>%
      rename(Estimand = estimand, Estimate = estimate, SE = se, Method = method)
  }, striped = TRUE, digits = 4, na = "—")

  output$hte_plot <- renderPlot({
    hte %>%
      mutate(user_expertise = factor(user_expertise, levels = c("novice","intermediate","expert"))) %>%
      ggplot(aes(x = user_expertise, y = cate, ymin = ci_lower, ymax = ci_upper, color = user_expertise)) +
      geom_pointrange(size = 1.0, linewidth = 0.9, show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      scale_color_manual(values = c("novice" = "#e74c3c","intermediate" = "#f39c12","expert" = COL_MAIN)) +
      labs(x = "User expertise", y = "Causal effect on satisfaction (1-5 scale)") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
