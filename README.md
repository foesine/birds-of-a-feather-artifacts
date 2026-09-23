# Supplementary Materials

This repository contains the code and processed results accompanying the submitted manuscript.

## Repository Structure



## Real-World Data

The empirical applications use publicly available datasets from the UCI Machine Learning Repository:

- Adult
- Bank Marketing
- Bike Sharing
- German Credit

The datasets are not redistributed in this repository and can be obtained from their original sources.

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
