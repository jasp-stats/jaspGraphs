<div align="right">

[![R build status](https://github.com/jasp-stats/jaspGraphs/workflows/R-CMD-check/badge.svg)](https://github.com/jasp-stats/jaspGraphs/actions)
[![Unit Tests](https://github.com/jasp-stats/jaspGraphs/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspGraphs/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspGraphs/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspGraphs)
<br>
<b>Maintainer:</b> <a href="https://github.com/vandenman/">Don van den Bergh</a>

</div>

# jaspGraphs

## Overview

jaspGraphs contains selective functions that extend ggplot2 for creating plots for JASP.


## Typical workflow

For almost all plots, the idea is that you only use two functions of jaspGraphs: `geom_rangeframe()`, and `themeJaspRaw()`.

Assuming you created some ggplot2 object called `plot`, you can do:
```r
plot +
  jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
  jaspGraphs::themeJaspRaw()      # add the JASP theme
```

## Installation
jaspGraphs is only available through GitHub, so you need to use some package that supports that (e.g., `remotes`, `renv`, `pak`, etc.)
```r
remotes::install_github("jasp-stats/jaspGraphs")
```
