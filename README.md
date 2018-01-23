# Stata-like Regression Functionality

This is a work-in-progress to explore how to design Stata-like regression modelling tools for R, namely those that allow plug-and-play variance-covariance estimation procedures and also to provide arguments to modelling functions is data-formula order (rather than the traditional formula-data order).

Contributions and feedback are welcome on [GitHub](https://github.com/leeper/reggie/issues).

## Code Examples



In addition to plug-and-play variance-covariance procedures, the `reg()` function also provides pretty print methods.


```r
library("reggie")

# reg
reg(ChickWeight, weight ~ Time + Diet)
```

```
## Generalized Linear Model
- Model:  weight ~ Time + Diet
- Family: gaussian (link: identity)
- Data (n=578): ChickWeight

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)    10.92       3.36     3.3    0.001
Time            8.75       0.22    39.5   <2e-16
Diet2          16.17       4.09     4.0    8e-05
Diet3          36.50       4.09     8.9   <2e-16
Diet4          30.23       4.11     7.4    2e-13
```

```r
# reg
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "const")
```

```
## Generalized Linear Model
- Model:  weight ~ Time + Diet
- Family: gaussian (link: identity)
- Data (n=578): ChickWeight

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)    10.92       3.36     3.3    0.001
Time            8.75       0.22    39.5   <2e-16
Diet2          16.17       4.09     4.0    8e-05
Diet3          36.50       4.09     8.9   <2e-16
Diet4          30.23       4.11     7.4    2e-13
```

```r
# reg, vce(robust)
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "HC0")
```

```
## Generalized Linear Model
- Model:  weight ~ Time + Diet
- Family: gaussian (link: identity)
- Data (n=578): ChickWeight

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)    10.92       2.82     3.9    1e-04
Time            8.75       0.26    33.6   <2e-16
Diet2          16.17       4.41     3.7    2e-04
Diet3          36.50       4.49     8.1    4e-16
Diet4          30.23       3.13     9.7   <2e-16
```

```r
# reg, vce(boot)
reg(ChickWeight, weight ~ Time + Diet, vcov_type = "boot")
```

```
## Generalized Linear Model
- Model:  weight ~ Time + Diet
- Family: gaussian (link: identity)
- Data (n=578): ChickWeight

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)    10.92       2.98     3.7    3e-04
Time            8.75       0.28    31.6   <2e-16
Diet2          16.17       4.52     3.6    3e-04
Diet3          36.50       4.61     7.9    2e-15
Diet4          30.23       3.13     9.7   <2e-16
```

```r
# reg, vce(cluster Chick)
reg(ChickWeight, weight ~ Time + Diet, vcov_cluster = ~Chick)
```

```
## Generalized Linear Model
- Model:  weight ~ Time + Diet
- Family: gaussian (link: identity)
- Data (n=578): ChickWeight

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)    10.92       5.39     2.0     0.04
Time            8.75       0.53    16.7   <2e-16
Diet2          16.17      10.91     1.5     0.14
Diet3          36.50       9.86     3.7    2e-04
Diet4          30.23       6.67     4.5    6e-06
```

```r
# bootstrap, cluster(Chick) reps(5000): reg reg(ChickWeight, weight ~ Time + Diet, vcov_cluster = ~ Chick, vcov_type =
# 'boot') DOESN'T CURRENTLY WORK, BUT WHY?

# svy: reg
library("survey")
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
reg(dstrat, api00 ~ ell + meals + mobility)
```

```
## Generalized Linear Model
- Model:  api00 ~ ell + meals + mobility
- Family: gaussian (link: identity)
- Data (n=200):
Stratified Independent Sampling design
svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, 
    fpc = ~fpc)

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
(Intercept)   820.89      10.08    81.5   <2e-16
ell            -0.48       0.39    -1.2      0.2
meals          -3.14       0.28   -11.1   <2e-16
mobility        0.23       0.39     0.6      0.6
```

The "model" object class contains the underlying model object as its `model` argument, and methods for various commonly used generic functions (`coef()`, `vcov()`, `plot()`, `terms()`, `predict()`) are provided that behave like those operations on a standard modelling object.

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/reggie)](https://cran.r-project.org/package=reggie)
![Downloads](https://cranlogs.r-pkg.org/badges/reggie)
[![Travis Build Status](https://travis-ci.org/leeper/reggie.png?branch=master)](https://travis-ci.org/leeper/reggie)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/PROJECTNUMBER?svg=true)](https://ci.appveyor.com/project/leeper/reggie)
[![codecov.io](https://codecov.io/github/leeper/reggie/coverage.svg?branch=master)](https://codecov.io/github/leeper/reggie?branch=master)

This package is not yet on CRAN. To install the latest development version you can pull a potentially unstable version directly from GitHub:

```R
if (!require("ghit")) {
    install.packages("ghit")
}
ghit::install_github("leeper/reggie")
```
