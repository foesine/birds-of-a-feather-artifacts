# Supplementary Materials

This repository contains the code and processed results accompanying the submitted manuscript.

## Repository Structure

```text
.
├── README.md
├── LICENSE
├── .gitignore
│
├── code/
│   ├── simulation.R
│   ├── synthesis.R
│   ├── prediction.R
│   │
│   ├── 01_generate_simulations.Rmd
│   ├── 02_generate_synthetic_data.Rmd
│   │
│   ├── predictions/
│   │   ├── pred_on_original.Rmd
│   │   ├── pred_on_cartsyn.Rmd
│   │   ├── pred_on_parmsyn.Rmd
│   │   ├── pred_on_svmsyn.Rmd
│   │   └── pred_on_bnsyn.Rmd
│   │
│   └── analysis/
│       ├── reinforcement_analysis.Rmd
│       ├── ranking_analysis.Rmd
│       └── real_data_analysis.Rmd
│
└── results/
    └── figures/
```

## Where to Find What

- **Data generation:** `code/simulation.R`
- **Synthetic data generation:** `code/synthesis.R`
- **Prediction methods and evaluation metrics:** `code/prediction.R`
- **Generate simulated datasets:** `code/01_generate_simulations.Rmd`
- **Generate synthetic datasets:** `code/02_generate_synthetic_data.Rmd`
- **Predictions on original data:** `code/predictions/pred_on_original.Rmd`
- **Predictions on synthetic data:** `code/predictions/`
- **Reinforcement analysis:** `code/analysis/reinforcement_analysis.Rmd`
- **Model-ranking analysis:** `code/analysis/ranking_analysis.Rmd`
- **Real-data applications:** `code/analysis/real_data_analysis.Rmd`
- **Processed results and figures:** `results/`

## Reproducing the Analysis

The full workflow is:

1. Run `01_generate_simulations.Rmd`
2. Run `02_generate_synthetic_data.Rmd`
3. Run the prediction scripts in `code/predictions/`
4. Run the analysis scripts in `code/analysis/`

The full pipeline is computationally intensive. Processed outputs are provided in `results/` so that the reported results can be inspected without rerunning all synthesis and prediction steps.

## Real-World Data

The empirical applications use publicly available datasets from the UCI Machine Learning Repository:

- Adult
- Bank Marketing
- Bike Sharing
- German Credit

The datasets are not redistributed in this repository and should be obtained from their original sources.

## Software

The analyses are implemented in R. The main packages used include:

```text
MASS
caret
rpart
glmnet
synthpop
e1071
bnlearn
kernlab
FNN
future
furrr
ggplot2
dplyr
tidyr
purrr
```

Some steps are parallelized. The number of workers can be adjusted in the corresponding R Markdown files.

## License

Code in this repository is released under the MIT License.

Third-party datasets remain subject to their original licenses and terms of use.
