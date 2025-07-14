
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saeHB.TF.beta

<!-- badges: start -->
<!-- badges: end -->

`saeHB.TF.beta` provides several functions for area and subarea level of
small area estimation under Twofold Subarea Level Model using
hierarchical Bayesian (HB) method with Beta distribution for variables
of interest. Some dataset simulated by a data generation are also
provided. The ‘rstan’ package is employed to obtain parameter estimates
using STAN.

## Function

## Installation

You can install the development version of saeHB.TF.beta from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install.github("Nasyazahira/saeHB.TF.beta")
```

## Example

Here is a basic example of using the **betaTF** function to make
estimates based on sample data in this package

``` r
library(saeHB.TF.beta)

#Load Dataset
data(dataBeta) #for dataset with nonsampled subarea use dataBetaNS

#Fitting model
fit <- betaTF(y~X1+X2, area="codearea", weight="w", data=dataBeta)
```

Extract subarea mean estimation

``` r
fit$Est_sub
```

Extract area mean estimation

``` r
fit$Est_area
```

Extract coefficient estimation $\beta$

``` r
fit$coefficient
```

Extract estimation of subarea and area random effect $u$ and $v$

``` r
fit$area_randeff
fit$sub_randeff
```

Extract estimation of subarea and area random effect variance
$\sigma^2_u$ and $\sigma^2_v$

``` r
fit$refVar
```

Calculate Relative Standard Error (RSE)

``` r
RSE <- (fit$Est_sub$SD)/(fit$Est_sub$Mean)*100
summary(RSE)
```
