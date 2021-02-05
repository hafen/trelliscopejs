<!-- badges: start -->
[![Build Status](https://travis-ci.org/hafen/trelliscopejs.svg?branch=master)](https://travis-ci.org/hafen/trelliscopejs)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/hafen/trelliscopejs?branch=master&svg=true)](https://ci.appveyor.com/project/hafen/trelliscopejs)
[![codecov.io](https://codecov.io/github/hafen/trelliscopejs/coverage.svg?branch=master)](https://codecov.io/github/hafen/trelliscopejs?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/trelliscopejs)](https://cran.r-project.org/package=trelliscopejs)
<!-- badges: end -->

# trelliscopejs <img src="man/figures/logo.png" align="right" width="120px" />

Trelliscope is a scalable, flexible, interactive approach to visualizing data. The trelliscopejs R package provides methods that make it easy to create a Trelliscope display specification for the Trelliscope JavaScript library [trelliscopejs-lib](https://github.com/hafen/trelliscopejs-lib). High-level functions are provided for creating displays from within dplyr (via `summarise()`) or ggplot2 (via `facet_trelliscope()`) workflows. Low-level functions are also provided for creating new interfaces.

### Install

```r
install.packages("trelliscopejs")
```

To install the latest development version:

```r
# install.packages("remotes") # if "remotes" is not already installed
devtools::install_github("hafen/trelliscopejs")
```

## Demos

- [Gapminder](https://hafen.github.io/trelliscopejs-demo/gapminder/)
- [Gapminder with Plotly panels](https://hafen.github.io/trelliscopejs-demo/gapminder_plotly/)
- [Monthly U.S. home prices by county](https://hafen.github.io/trelliscopejs-demo/housing/)
- [Pokemon](https://hafen.github.io/trelliscopejs-demo/pokemon/)

## Examples

Here is a simple example using the ggplot2 interface. Using trelliscopejs in this way is as easy as swapping `facet_wrap()` with `facet_trelliscope()` and specifying some additional options.

Please see the [package vignettes](https://hafen.github.io/trelliscopejs/) for more.

```r
library(trelliscopejs)
library(ggplot2)
library(gapminder)

qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)
```

<div style="margin-top:20px; margin-bottom: 20px">
<div id="ebbfa969" class="trelliscope-not-spa" style="width:850px; height:600px;"></div>
</div>
