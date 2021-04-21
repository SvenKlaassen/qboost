
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qboost

<!-- badges: start -->
<!-- badges: end -->

High-dimensional quantile regression using boosting. This repository is
the official implementation of …

## Description

Quantile Regression is a popular tool to model the conditional quantiles
of a response variable. In a high-dimensional setting, the theoretical
results are largely limited to a penalized quantile regression
model([Belloni and Chernozhukov,
2011](https://projecteuclid.org/journals/annals-of-statistics/volume-39/issue-1/%e2%84%931-penalized-quantile-regression-in-high-dimensional-sparse-models/10.1214/10-AOS827.full)).<br />
Instead, we employ a greedy procedure to gradually improve the
regression function. To overcome the non-differentiability of the check
loss funciton, we employ the convolution-type smoothed quantile
regression ([Fernandes, Guerre and Horta,
2019](https://www.tandfonline.com/doi/abs/10.1080/07350015.2019.1660177?journalCode=ubes20)
and [He, Pan, Tan, Zhou](https://arxiv.org/abs/2012.05187)).

## Installation qboost

To install development version from [GitHub](https://github.com/), run
the following commands:

``` r
install.packages("devtools")
library(devtools)
install_github("SvenKlaassen/GGMtest")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(MASS)
library(qboost)
set.seed(42)
n <- 100; p <- 500; s <- 4
beta <- rep(c(1,0),c(s+1,p-s))

X = mvrnorm(n, rep(0, p), diag(p))
err = rt(n, 2)
Y = cbind(1, X) %*% beta + err
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<a href="https://github.com/r-lib/actions/tree/master/examples" class="uri">https://github.com/r-lib/actions/tree/master/examples</a>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## References

-   He, Xuming, et al. “Smoothed quantile regression with large-scale
    inference.” arXiv preprint arXiv:2012.05187 (2020).
    [Paper](https://arxiv.org/abs/2012.05187)

-   Belloni, Alexandre, and Victor Chernozhukov. “ℓ1-penalized quantile
    regression in high-dimensional sparse models.” The Annals of
    Statistics 39.1 (2011): 82-130.
    [Paper](https://projecteuclid.org/journals/annals-of-statistics/volume-39/issue-1/%e2%84%931-penalized-quantile-regression-in-high-dimensional-sparse-models/10.1214/10-AOS827.full)

-   Fernandes, Marcelo, Emmanuel Guerre, and Eduardo Horta. “Smoothing
    quantile regressions.” Journal of Business & Economic Statistics
    39.1 (2021): 338-357.
    [Paper](https://www.tandfonline.com/doi/full/10.1080/07350015.2019.1660177?casa_token=bkJ73Q8oXYIAAAAA%3A0g8P9Bb5elGlCBU8_bsuN_oLauFgMfQcojZmI4ERJO1WVD1M5UOw1RLRix7mMxDMMWukfZR-sbE)
