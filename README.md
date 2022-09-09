
<!-- README.md is generated from README.Rmd. Please edit that file -->

# U.T.lity

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of U.T.-lity is to provide a collection of tools that allow you
to analyse data from user experience and usability tests, assess and
track quality, and steer your efforts to get your desired cost-benefit
out of your research.

## Installation

You can install the development version of Repo from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SigurdJanson/U.T.-lity")
```

## Example

This is the famous plot showing the ratio between the likelihood of
finding usability defects and the sample size. Note: `p.occ` being 31%
(i.e. 0.31) is the number which Nielsen & Landauer (1993) introduced in
their well-known publication and which led to the even more well-known
post in [Nielsens AlertBox (Nielsen,
2000)](https://www.nngroup.com/articles/why-you-only-need-to-test-with-5-users/).

``` r
library(U.T.lity)
nDefectsPlot(p.occ=c(0.1, 0.2, 0.31, 0.5))
```

<img src="man/figures/README-SampleSizePlot-1.png" width="100%" />

## Further Readings

Nielsen, J., & Landauer, T. K. (1993). *A mathematical model of the
finding of usability problems*. Proceedings of the INTERACT ’93 and CHI
’93 Conference on Human Factors in Computing Systems, 206–213.
<https://doi.org/10.1145/169059.169166>

Nielsen, J. (2000). Why You Only Need to Test with 5 Users.
<https://www.nngroup.com/articles/why-you-only-need-to-test-with-5-users/>;
accessed 2022-09-09
