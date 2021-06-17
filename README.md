# Global modeling of subnational habitual nutrient intake distributions

This GitHub repository contains the data and code for the following paper:

Passarelli S, Free CM, Shepon A, Lee C, Moursi M, Cao L, Li Y, Crispim S, Schmidhuber J, Bromage S, Beal T, Golden CD (in prep) Global modeling of subnational habitual nutrient intake distributions. _Near submission_.

## Repository structure

The repository is organized using the following structure:

- data............. folder containing intake distributions and other key data
- code............. folder containing code to fit, describe, and compare intake distributions
- figures.......... folder containing figures for the manuscript
- tables........... folder containing tables for the manuscript

Please email Simone Passarelli (spassarelli@g.harvard.edu) if you have any questions about the paper and Chris Free (cfree14@gmail.com) if you have any questions about the data, code, and repository.


## The "nutriR" R package

This paper and repository leverages functions for analyzing and using habitual intake distributions provided in the "nutriR" R package available here: https://github.com/cfree14/nutriR

The package includes the following functions:

- Extract subnational intake distributions of interest: `?get_dists`
- Plot subnational intake distributions of interest: `?plot_dists`
- Generate subnational intake distributions for independent plotting: `?generate_dists`
- Shift subnational intake distributions of interest: `?shift_dists`
- Calculate summary exposure values (SEVs; a.k.a., nutrient deficiencies): `?sev`
- Calculate the mean of intake distributions: `?mean_dist`
- Calculate the coefficient of variation (CV) and variance of intake distributions: `?cv`, `?variance`
- Calculate the skewness and kurtosis of intake distributions: `?skewness`, `?kurtosis`
- Calculate percent overlap (Bhattacharyya coefficient) between intake distributions: `?overlap`

It also includes the habitual intake distributions documented in this paper as well as other important human nutrition datasets.

Please cite the R package functions as:

Free CM, Passarelli S, Shepon A, Lee C, Moursi M, Cao L, Li Y, Crispim S, Schmidhuber J, Bromage S, Beal T, Golden CD (2021) nutriR: Nutritional intake functions for R. Available at: https://github.com/cfree14/nutriR

Please cite the data served in the R package as:

Passarelli S, Free CM, Shepon A, Lee C, Moursi M, Cao L, Li Y, Crispim S, Schmidhuber J, Bromage S, Beal T, Golden CD (in prep) Global modeling of subnational habitual nutrient intake distributions. _Near submission_.
