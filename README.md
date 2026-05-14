# StatEval: Rigorous LLM Evaluation with Causal Analysis

A portfolio project demonstrating the application of **statistical evaluation design** and **causal inference** to LLM quality assessment in clinical health Q&A contexts.

Built in R. Motivated by real gaps in production LLM evaluation pipelines.

---

## Motivation

Large language models are increasingly deployed in high-stakes domains like healthcare. But most evaluation pipelines lack statistical rigor — they don't address:

- How many samples are actually needed to detect meaningful quality differences?
- Are human graders reliable, and how well do AI graders track human judgment?
- What *causes* variation in output quality — is it the prompt, the user, or the model?

This project answers these questions using tools from clinical trial design and observational causal inference.

---

## Project Structure

```
stateval/
├── R/
│   ├── 01_simulate_data.R      # Data generation (clinical Q&A prompts + user logs)
│   ├── 02_irr_analysis.R       # Rubric design + ICC / Krippendorff's alpha
│   ├── 03_sampling_strategy.R  # Power analysis + Neyman optimal allocation
│   └── 04_causal_analysis.R    # DAG + IPW + doubly-robust ATE + E-value
├── data/                       # Generated datasets (not tracked in git)
├── output/                     # Analysis outputs (CSVs)
├── docs/
│   └── stateval_report.qmd     # Quarto report (renders to HTML)
└── README.md
```

---

## Methods

| Component | Method | R Package |
|-----------|--------|-----------|
| Rubric reliability | ICC (two-way mixed), Krippendorff's α | `irr` |
| Sampling design | Neyman optimal allocation, power analysis | `pwr`, `boot` |
| Causal structure | DAG specification + testable implications | `dagitty` |
| Causal estimation | IPW, entropy balancing (doubly-robust) | `WeightIt` |
| Balance diagnostics | Standardized mean differences | `cobalt` |
| Sensitivity | E-value (VanderWeele & Ding 2017) | base R |

---

## Key Findings

- **Stratified sampling reduces estimator variance ~30%** relative to SRS at equal budget — with highest gains in rare strata (complex × medication prompts)
- **AI grader has rubric-specific bias**: overrates clarity (+0.05), underrates completeness (-0.04) — requiring targeted recalibration
- **Including clinical context causally increases user satisfaction** (ATE ≈ +0.40 points on a 1–5 scale), with effect largest among expert users
- **E-value ≈ 2.1**: an unmeasured confounder would need RR > 2 with both treatment and outcome to explain away the effect

---

## Running the Project

```r
# Install dependencies
install.packages(c("tidyverse", "irr", "psych", "pwr", "boot",
                   "dagitty", "WeightIt", "cobalt", "marginaleffects",
                   "truncnorm", "gt", "patchwork"))

# Run in order from project root
source("R/01_simulate_data.R")
source("R/02_irr_analysis.R")
source("R/03_sampling_strategy.R")
source("R/04_causal_analysis.R")

# Render the report
quarto::quarto_render("docs/stateval_report.qmd")
```

---

## Why This Matters for LLM Teams

Most LLM evaluation today is engineer-driven: fixed test sets, aggregate accuracy metrics, no principled calibration of human graders. The statistical gaps this project addresses — optimal sampling design, IRR, AI-human calibration drift, causal attribution — are the exact problems that separate robust evaluation infrastructure from ad-hoc benchmarking.

---

## Author

**Ludovic** — Biostatistician & Data Scientist | MS Biostatistics, NYU  
Background in causal inference, longitudinal data, and healthcare analytics  
[LinkedIn](#) | [GitHub](#)
