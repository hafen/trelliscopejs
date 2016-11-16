## trelliscopejs R Package

[![Build Status](https://travis-ci.org/hafen/trelliscopejs.svg?branch=master)](https://travis-ci.org/hafen/trelliscopejs)
[![codecov.io](https://codecov.io/github/hafen/trelliscopejs/coverage.svg?branch=master)](https://codecov.io/github/hafen/trelliscopejs?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/trelliscopejs)](https://cran.r-project.org/package=trelliscopejs)

Trelliscope is a scalable, flexible, interactive approach to visualizing data. The trelliscopejs R package provides methods that make it easy to create a Trelliscope display specification for the Trelliscope JavaScript library [trelliscopejs-lib](https://github.com/hafen/trelliscopejs-lib). High-level functions are provided for creating displays from within dplyr (via `summarise()`) or ggplot2 (via `facet_trelliscope()`) workflows. Low-level functions are also provided for creating new interfaces.

### Install

```r
devtools::install_github("hafen/trelliscopejs")
```

## Usage

The examples below are minimal. Please see the package vignettes for more examples of what can be done.

Also, see [here](http://hafen.github.io/trelliscopejs-demo/) for an example hosted live.

#### Simple usage with ggplot

Example:

```r
library(trelliscopejs)
library(ggplot2)

qplot(cty, hwy, data = mpg) +
  facet_trelliscope(~ class + manufacturer)
```

#### Simple with dplyr (and rbokeh)

Example:

```r
library(trelliscopejs)
library(dplyr)
library(rbokeh)

ggplot2::mpg %>%
  group_by(class, manufacturer) %>%
  summarise(
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg") %>%
        ly_points(cty, hwy))) %>%
  trelliscope(name = "city_vs_highway_mpg_bk")
```
