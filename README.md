# StatEval: Rigorous LLM Evaluation in Clinical Health Q&A

A portfolio project applying **statistical evaluation design** and **causal inference** to the problem of LLM quality assessment - built in R, motivated by a real gap in how production evaluation pipelines are built today.

---

## Why This Project Exists

Most LLM evaluation is engineer-driven: fixed test sets, aggregate accuracy metrics, no principled grader calibration. When I spoke with a data scientist at a major AI company, the bottleneck they described wasn't the modeling - it was the evaluation infrastructure. Specifically:

> *"How do we know how many samples we need? How do we know our sampling design is actually doing the right evaluation?"*

This project answers that question rigorously, using tools from clinical trial design applied to the problem of evaluating language models.

---

## What It Does

**Module 1 - Evaluation Pipeline**

- Designs a 6-rubric framework (accuracy, safety, clarity, completeness, citation quality, harm avoidance) for clinical health Q&A
- Deploys an AI grader at scale; human calibration on a 10% stratified sample
- Computes inter-rater reliability: ICC (two-way mixed, absolute agreement) and Krippendorff's α
- Quantifies AI-human calibration bias per rubric and per complexity stratum

**Module 2 - Causal Analysis**

- Estimates the causal effect of including clinical context in prompts on user satisfaction
- Adjusts for confounding (user expertise) via IPW and doubly-robust AIPW
- Reports heterogeneous treatment effects by expertise stratum
- Quantifies robustness with E-value sensitivity analysis

**Deliverables**

- Interactive Shiny dashboard (4 tabs: Overview, Reliability, Sampling Design, Causal Analysis)
- Quarto HTML report with reproducible code, tables, and figures

---

## Key Results

| Finding | Value |
|---------|-------|
| ICC range across rubrics | 0.886 – 0.953 |
| Weakest rubric (ICC) | Safety (0.886) - warrants ongoing recalibration |
| AI grader: highest bias rubric | Completeness (MAE = 0.107, under-rates) |
| Neyman allocation budget | 100 calibration samples across 15 strata |
| AIPW ATE on satisfaction | +0.38 points (95% CI: [0.30, 0.46]) |
| E-value | 3.14 - robust to moderate unmeasured confounding |

---

## Methods

| Component | Method | Package |
|-----------|--------|---------|
| Rubric reliability | ICC (two-way mixed, absolute), Krippendorff's α | `psych`, base R |
| Sampling design | Power analysis, Neyman optimal allocation | `pwr`, `boot` |
| Causal structure | DAG (manual specification) | base R |
| Causal estimation | IPW, augmented IPW (AIPW) | base R |
| Balance diagnostics | Standardized mean differences | base R |
| Sensitivity | E-value (VanderWeele & Ding, 2017) | base R |

---

## Project Structure

```
stateval/
├── scripts/
│   ├── 01_simulate_data.R       # Data generation: clinical Q&A prompts + user logs
│   ├── 02_irr_analysis.R        # Rubric design, ICC, Krippendorff's alpha
│   ├── 03_sampling_strategy.R   # Power analysis, Neyman allocation, bootstrap variance
│   └── 04_causal_analysis.R     # DAG, IPW, AIPW, E-value, HTE
├── data/                        # Generated datasets (git-ignored)
├── outputs/                     # Analysis outputs: CSVs (git-ignored)
├── shiny/
│   └── app.R                    # Interactive dashboard
├── stateval_report.qmd          # Quarto report
└── README.md
```

---

## Reproducing the Project

```r
# Install dependencies
install.packages(c("tidyverse", "truncnorm", "psych", "pwr", "boot", "bslib", "shiny"))

# Run in order from project root
source("scripts/01_simulate_data.R")
source("scripts/02_irr_analysis.R")
source("scripts/03_sampling_strategy.R")
source("scripts/04_causal_analysis.R")

# Launch dashboard
shiny::runApp("shiny/")

# Render report
quarto::quarto_render("stateval_report.qmd")
```

---

## Why This Matters for LLM Teams

The statistical problems this project addresses - optimal sampling design, grader IRR, AI-human calibration drift, causal attribution of quality drivers - are the exact gaps separating robust evaluation infrastructure from ad-hoc benchmarking. The combination of causal inference rigor with evaluation system design is rare in practice, and increasingly in demand as LLMs move into high-stakes domains.

---

## Author

**Ludovic** | MS Biostatistics, NYU | Data Scientist & Statistician  
5+ years across healthcare, pharma, and public health  
Open to roles in product analytics, AI/ML evaluation, and data science
