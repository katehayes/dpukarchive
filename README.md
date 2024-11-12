
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpukarchive

<!-- badges: start -->
<!-- badges: end -->

[Data.police.uk](https://data.police.uk) makes lots of police data
available for public use. It offers an API - but, on exploration, it
seems like the API only gives access to data back as far as 2021. The
site also has an [‘archive’
page](https://data.police.uk/data/archive/) - the archive hosts data
from Dec 2010 onwards. <br>

However, the archive is difficult to use. It hosts a number of zipped
folders, available for download. Each zipped folder contains a number of
subfolders, which contain one month’s worth of data. For every month,
there should be one csv for each police force, for each data series
(crime, outcome, stop & search data), though some are missing. In sum,
there are lots of files inside folders inside folders, and to extract
the particular data files you want to use presently involves downloading
and manually searching through many files you don’t want. <br>

Plan: to try develop a package with functions that make interacting with
the UK police data archive easier.
<!-- I'm trying to take stock of the archive - figure out what data files are absent/present/present in multiple versions, how to interact with the archive in the most efficient way, etc. -->

## Installation

You can install the development version of dpukarchive from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("katehayes/dpukarchive")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## library(dpukarchive)
## basic example code
```
