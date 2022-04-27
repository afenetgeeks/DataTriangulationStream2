
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/nphcda-logo.svg" align="center" />

# Data Triangulation Stream 2 Dashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The goal of Data Triangulation Stream 2 Dashboard is to is to
Triangulate Nigeria Routine Immunization And Vaccine Preventable
Diseases Surveillance Data

## Description

The Nigeria Routine Immunization (RI) & Vaccine Preventable Diseases
Surveillance (VPDs) Data Triangulation Dashboard visualizes selected
indicators across RI & VPDs programme.

The dashboard developed on R Shiny technology will be integrated with
the country’s DHIS2 national instance, Surveillance Outbreak Response
Management and Analysis System (SORMAS), and the Multi-Source Data
Analytics and Triangulation (MSDAT) Platform.

The dashboard provides access to timely information required for
decision making at the national and sub-national levels.

## Installation

You can install the development version of DataTriangulationStream2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("afenetgeeks/DataTriangulationStream2")
```

## Process of the Dashboard development

First, the dashboard was built using R shiny and hosted on the
shinyapps.io servers the Rstudio hosting services, then the app url was
used to integrate into the 3 platforms using an iframe.

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## The Maps

The boundaries and names shown and the designations used on this map do
not imply the expression of any opinion whatsoever on the part of the
AFENET GEEKs concerning the legal status of any country, territory, city
or area or of its authorities, or concerning the delimitation of its
frontiers or boundaries. Dotted and dashed lines on maps represent
approximate border lines for which there may not yet be full agreement.
