## trelliscopejs R Package

[![Build Status](https://travis-ci.org/hafen/trelliscopejs.svg?branch=master)](https://travis-ci.org/hafen/trelliscopejs)
[![codecov.io](https://codecov.io/github/hafen/trelliscopejs/coverage.svg?branch=master)](https://codecov.io/github/hafen/trelliscopejs?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/trelliscopejs)](https://cran.r-project.org/package=trelliscopejs)

Trelliscope is a scalable, flexible, interactive approach to visualizing data. The trelliscopejs R package provides methods that make it easy to create a Trelliscope display specification for the Trelliscope JavaScript library [trelliscopejs-lib](https://github.com/hafen/trelliscopejs-lib). High-level functions are provided for creating displays from within dplyr (via `summarise()`) or ggplot2 (via `facet_trelliscope()`) workflows. Low-level functions are also provided for creating new interfaces.

Note that this package, **trelliscopejs** is the successor of the [**trelliscope**] package which is available on CRAN and is part of the DeltaRho project. Eventually the **trelliscopejs** package will replace **trelliscope** and plug in to the DeltaRho ecosystem as well.

### Install

```r
devtools::install_github("hafen/trelliscopejs")
```

## Demos

- [Gapminder](http://hafen.github.io/trelliscopejs-demo/gapminder)
- [Gapminder with Plotly panels](http://hafen.github.io/trelliscopejs-demo/gapminder_plotly)
- [Monthly U.S. home prices by county](http://hafen.github.io/trelliscopejs-demo/housing)
- [Pokemon](http://hafen.github.io/trelliscopejs-demo/pokemon)

## Examples

The examples below are minimal. Please see the [package vignettes](https://hafen.github.io/trelliscopejs) for more.

#### ggplot

```r
library(trelliscopejs)
library(ggplot2)
library(gapminder)

qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)
```

#### tidyverse

```r
library(trelliscopejs)
library(tidyverse)
library(rbokeh)
library(gapminder)

# nest gapminder data by country
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

# add in a plot column with map_plot
by_country <- by_country %>% mutate(
  panel = map_plot(data,
    ~ figure(xlim = c(1948, 2011), ylim = c(10, 95), width = 300, tools = NULL) %>%
        ly_points(year, lifeExp, data = .x, hover = .x)
  ))

# plot it
by_country %>%
  trelliscope("gapminder", nrow = 2, ncol = 7)
```
