## trelliscope2

[![Build Status](https://travis-ci.org/hafen/trelliscope2.svg?branch=master)](https://travis-ci.org/hafen/trelliscope2)
[![codecov.io](https://codecov.io/github/hafen/trelliscope2/coverage.svg?branch=master)](https://codecov.io/github/hafen/trelliscope2?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/trelliscope2)](https://cran.r-project.org/package=trelliscope2)

Trelliscope is a scalable, flexible, interactive approach to visualizing data. trelliscope2 provides methods that make it easy to create a Trelliscope display specification for [TrelliscopeJS](https://github.com/hafen/trelliscopejs). High-level functions are provided for creating displays from within dplyr (via `summarise()`) or ggplot2 (via `facet_trelliscope()`) workflows. Low-level functions are also provided for creating new interfaces.

### Install

```r
devtools::install_github("hafen/trelliscope2")
```

## Usage

The examples below are minimal. Please see the package vignettes for more examples of what can be done.

Also, see [here](http://hafen.github.io/trelliscopejs-demo/) for an example hosted live.

#### Simple usage with ggplot

Example:

```r
library(trelliscope2)
library(ggplot2)

qplot(cty, hwy, data = mpg) +
  facet_trelliscope(~ class + manufacturer)
```

#### Simple with dplyr (and rbokeh)

Example:

```r
library(trelliscope2)
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
