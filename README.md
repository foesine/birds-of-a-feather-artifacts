# Supplementary Materials

This repository contains the code, figures, and numerical results accompanying the submitted manuscript.

## Repository Structure

```text
.
├── code/
│   ├── analysis/
│   │   ├── analyze_predictions.Rmd
│   │   ├── analyze_real_data.qmd
│   │   └── analyze_reinforcement.Rmd
│   ├── data_generation/
│   │   ├── data_sim.Rmd
│   │   ├── synthesize_data.Rmd
│   │   └── synthesize_real_data.qmd
│   ├── predictions/
│   │   ├── pred_on_sim.Rmd
│   │   ├── pred_on_parmsyn.Rmd
│   │   ├── pred_on_cartsyn.Rmd
│   │   ├── pred_on_svmsyn.Rmd
│   │   ├── pred_on_bnsyn.Rmd
│   │   └── pred_real_data.qmd
│   ├── prediction.R
│   ├── simulation.R
│   ├── synthesis.R
│   └── synthesis_real_data.R
├── results/
│   ├── additional_materials/
│   └── figures/
├── .gitignore
├── LICENSE
└── README.md
```

The `data/` directory is not included because it contains source datasets and large intermediate objects.

## Reproducing the Simulation Analysis

Run the following files from the repository root in this order:

1. `code/data_generation/data_sim.Rmd`
2. `code/data_generation/synthesize_data.Rmd`
3. Prediction files:
   - `code/predictions/pred_on_sim.Rmd`
   - `code/predictions/pred_on_parmsyn.Rmd`
   - `code/predictions/pred_on_cartsyn.Rmd`
   - `code/predictions/pred_on_svmsyn.Rmd`
   - `code/predictions/pred_on_bnsyn.Rmd`
4. `code/analysis/analyze_predictions.Rmd`
5. `code/analysis/analyze_reinforcement.Rmd`

The prediction files in Step 3 can be run in any order after the original and synthetic datasets have been generated. Some synthesis and prediction steps are computationally intensive and use parallel processing. The number of workers can be changed in the corresponding R Markdown files.

Generated intermediate datasets and prediction objects are stored in `data/`. Figures are written to `results/figures/`, and machine-readable numerical results are written to `results/additional_materials/`.

## Reproducing the Real-Data Analysis

The real-data workflow is:

1. `code/data_generation/synthesize_real_data.qmd`
2. `code/predictions/pred_real_data.qmd`
3. `code/analysis/analyze_real_data.qmd`

The source datasets are not redistributed. They are publicly available from the UCI Machine Learning Repository.

## Numerical Results

The main machine-readable outputs include:

- `prediction_results.csv`: simulation prediction metrics;
- `reinforcement_matrix_summary.csv`: synthesis-by-prediction contrasts;
- `diagonal_advantage_by_metric.csv`: metric-specific diagonal advantages;
- `diagonal_inference_summary.csv`: non-ceiling cluster-bootstrap results;
- `diagonal_inference_method_comparison.csv`: comparison with the random-label procedure;
- `ranking_overall.csv`: overall ranking-reversal results;
- `ranking_summary.csv`: ranking results by simulation setting;
- `real_data_prediction_values.csv`: real-data prediction metrics;
- `real_data_ranking_overall.csv`: overall real-data ranking results;
- `real_data_ranking_summary.csv`: real-data ranking results by dataset and synthesis family.

These outputs allow the numerical claims in the manuscript to be inspected without rerunning the complete computational workflow.

## Software

The analyses were conducted in R. The principal packages include:

```text
MASS
caret
rpart
glmnet
nnet
synthpop
e1071
bnlearn
kernlab
FNN
future
furrr
parallelly
data.table
Matrix
ggplot2
dplyr
tidyr
purrr
stringr
tibble
readr
gridExtra
here
```

## License

The code is released under the MIT License. Third-party datasets remain subject to their original licenses and terms of use.
