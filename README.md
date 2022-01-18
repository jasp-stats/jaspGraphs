# JASPgraphs

[![R build status](https://github.com/jasp-stats/jaspGraphs/workflows/R-CMD-check/badge.svg)](https://github.com/jasp-stats/jaspGraphs/actions)
[![Documentation](https://img.shields.io/badge/doc-latest-blue.svg)](https://vandenman.github.io/jaspGraphs)


## Overview

jaspGraphs contains selective functions that extend ggplot2 for creating plots for JASP.

EDIT 2022-01-17: to make seamless with jonathon-love/jsq, changing all instances of jaspGraphs to JASPgraphs (reversing name change from 2020 in jasp-stats repos)

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
