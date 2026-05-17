# =============================================================================
# StatEval: shiny/app.R — AI Evaluation & Experimental Design Dashboard
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(ggrepel)

# --- Data loading ------------------------------------------------------------
load_data <- function() {
  list(
    irr        = read_csv("../outputs/irr_summary.csv",            show_col_types = FALSE),
    calib      = read_csv("../outputs/ai_human_calibration.csv",  show_col_types = FALSE),
    comp_cal   = read_csv("../outputs/complexity_calibration.csv",show_col_types = FALSE),
    power_crv  = read_csv("../outputs/power_curve.csv",           show_col_types = FALSE),
    mde_tab    = read_csv("../outputs/mde_sensitivity.csv",       show_col_types = FALSE),
    neyman     = read_csv("../outputs/neyman_allocation.csv",     show_col_types = FALSE),
    causal     = read_csv("../outputs/causal_results.csv",        show_col_types = FALSE),
    hte        = read_csv("../outputs/hte_by_expertise.csv",      show_col_types = FALSE),
    balance    = read_csv("../outputs/balance_table.csv",         show_col_types = FALSE),
    rubric_def = read_csv("../outputs/rubric_definitions.csv",    show_col_types = FALSE)
  )
}

d <- load_data()

# --- KPI objects -------------------------------------------------------------
ate_val <- d$causal$estimate[
  d$causal$estimand == "AIPW-ATE (satisfaction)"
]

irr_val <- round(mean(d$irr$kripp_alpha, na.rm = TRUE), 3)

power80 <- d$power_crv %>%
  filter(power >= 0.80) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

# --- Color palette -----------------------------------------------------------
PAL <- list(
  blue = "#2980b9",
  green = "#27ae60",
  red = "#e74c3c",
  gold = "#f39c12",
  purple = "#8e44ad",
  dark = "#2c3e50"
)

ui <- page_navbar(
  title = "StatEval — AI Evaluation Analytics",
  theme = bs_theme(bootswatch = "flatly", primary = PAL$dark),
  
  # ── Tab 1: Overview ────────────────────────────────────────────────────────
  nav_panel("Overview",
            layout_columns(
              col_widths = c(3,3,3,3),
              
              value_box("Inter-Rater Reliability", round(irr_val, 3),
                        showcase = bsicons::bs_icon("clipboard-check"), theme = "primary"),
              
              value_box("Estimated ATE", round(ate_val, 3),
                        showcase = bsicons::bs_icon("diagram-3"), theme = "success"),
              
              value_box("80% Power Threshold", scales::comma(power80),
                        showcase = bsicons::bs_icon("graph-up"), theme = "warning"),
              
              value_box("Evaluation Rubrics", nrow(d$rubric_def),
                        showcase = bsicons::bs_icon("journal-text"), theme = "danger")
            ),
            
            layout_columns(
              col_widths = c(6,6),
              
              card(
                card_header("AI vs Human Calibration"),
                plotOutput("plot_calibration", height = "320px")
              ),
              
              card(
                card_header("Inter-Rater Reliability Metrics"),
                tableOutput("tbl_irr")
              )
            )
  ),
  
  # ── Tab 2: Calibration Analysis ────────────────────────────────────────────
  nav_panel("Calibration Analysis",
            layout_columns(
              col_widths = c(6,6),
              
              card(
                card_header("Complexity vs Calibration"),
                plotOutput("plot_complexity", height = "320px")
              ),
              
              card(
                card_header("Calibration Metrics"),
                tableOutput("tbl_calibration")
              )
            )
  ),
  
  # ── Tab 3: Power & Experimental Design ────────────────────────────────────
  nav_panel("Power & Experimental Design",
            layout_columns(
              col_widths = c(6,6),
              
              card(
                card_header("Power Curve"),
                plotOutput("plot_power", height = "320px")
              ),
              
              card(
                card_header("Minimum Detectable Effect"),
                plotOutput("plot_mde", height = "320px")
              )
            ),
            
            card(
              card_header("Neyman Allocation"),
              tableOutput("tbl_neyman")
            )
  ),
  
  # ── Tab 4: Causal Inference ────────────────────────────────────────────────
  nav_panel("Causal Inference",
            layout_columns(
              col_widths = c(6,6),
              
              card(
                card_header("Covariate Balance"),
                plotOutput("plot_balance", height = "320px")
              ),
              
              card(
                card_header("Heterogeneous Treatment Effects"),
                plotOutput("plot_hte", height = "320px")
              )
            ),
            
            card(
              card_header("Causal Estimates"),
              tableOutput("tbl_causal")
            )
  ),
  
  # ── Tab 5: Rubric Definitions ──────────────────────────────────────────────
  nav_panel("Rubric Definitions",
            card(
              card_header("Evaluation Rubrics"),
              DT::dataTableOutput("tbl_rubrics")
            )
  )
)

server <- function(input, output, session) {
  
  # Overview
  output$plot_calibration <- renderPlot({
    d$calib %>%
      ggplot(aes(x = mean_bias, y = mae, color = rubric)) +
      geom_point(size = 4, alpha = 0.85) +
      geom_text_repel(aes(label = rubric), size = 4) +
      labs(x = "Mean Bias (AI − Human)",
           y = "Mean Absolute Error",
           color = "Rubric") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_irr <- renderTable({
    d$irr %>%
      mutate(across(where(is.numeric), round, 3))
  }, striped = TRUE, hover = TRUE)
  
  # Calibration Analysis
  output$plot_complexity <- renderPlot({
    d$comp_cal %>%
      ggplot(aes(x = complexity,
                 y = mae,
                 color = rubric,
                 group = rubric)) +
      geom_point(size = 3) +
      geom_line() +
      labs(x = "Prompt Complexity",
           y = "Mean Absolute Error",
           color = "Rubric") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_calibration <- renderTable({
    d$calib %>%
      mutate(across(where(is.numeric), round, 3))
  }, striped = TRUE, hover = TRUE)
  
  # Power & Experimental Design
  output$plot_power <- renderPlot({
    d$power_crv %>%
      ggplot(aes(x = n, y = power)) +
      geom_line(color = PAL$blue, size = 1.2) +
      geom_hline(yintercept = 0.80,
                 linetype = "dashed",
                 color = PAL$red) +
      labs(x = "Sample Size",
           y = "Statistical Power") +
      theme_minimal(base_size = 13)
  })
  
  output$plot_mde <- renderPlot({
    d$mde_tab %>%
      ggplot(aes(x = mde_points, y = n_per_group)) +
      geom_line(color = PAL$green, size = 1.2) +
      geom_point(color = PAL$green, size = 3) +
      labs(x = "Minimum Detectable Effect",
           y = "Required n per Group") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_neyman <- renderTable({
    d$neyman %>%
      mutate(across(where(is.numeric), round, 2))
  }, striped = TRUE, hover = TRUE)
  
  # Causal Inference
  output$plot_balance <- renderPlot({
    d$balance %>%
      pivot_longer(cols = c(smd_unwtd, smd_wtd),
                   names_to = "type",
                   values_to = "smd") %>%
      ggplot(aes(x = covariate,
                 y = smd,
                 color = type,
                 group = type)) +
      geom_point(size = 3) +
      geom_line() +
      geom_hline(yintercept = 0.10,
                 linetype = "dashed",
                 color = PAL$red) +
      coord_flip() +
      labs(x = NULL,
           y = "Standardized Mean Difference",
           color = NULL) +
      theme_minimal(base_size = 13)
  })
  
  output$plot_hte <- renderPlot({
    d$hte %>%
      ggplot(aes(x = user_expertise,
                 y = cate,
                 ymin = ci_lower,
                 ymax = ci_upper,
                 color = user_expertise)) +
      geom_pointrange(size = 1.1) +
      geom_hline(yintercept = 0,
                 linetype = "dashed") +
      coord_flip() +
      labs(x = NULL,
           y = "Conditional Treatment Effect") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_causal <- renderTable({
    d$causal %>%
      mutate(across(where(is.numeric), round, 3))
  }, striped = TRUE, hover = TRUE)
  
  # Rubric Definitions
  output$tbl_rubrics <- DT::renderDataTable({
    DT::datatable(
      d$rubric_def,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)