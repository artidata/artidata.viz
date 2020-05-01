
<!-- README.md is generated from README.Rmd. Please edit that file -->

# artidata.viz

<!-- badges: start -->

<!-- badges: end -->

Artidata’s Personal Data Visualization
Package

## Installation

<!-- You can install the released version of artidata.viz from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("artidata.viz") -->

<!-- ``` -->

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("artidata/artidata.viz")
```

## Example

Setting up a random dataset:

``` r
library(artidata.viz)
library(data.table)
library(ggplot2)
library(scales)

set.seed(240193)

N=10000

dtA=data.table(x=runif(N,-3,3),y=runif(N,-3,3),z="A")

dtB=data.table(x=rnorm(N),y=rnorm(N),z="B")

dtC=data.table(x=seq(-3,3,length.out = N))
dtC[,":="(y=0.5*x+rnorm(N,sd=0.5),z="C"),]

dtD=data.table(x=c(rnorm(N/2,-1,.75),rnorm(N/2,1,.75)),
               y=c(rnorm(N/2,1,.75),rnorm(N/2,-1,.75)),
               z="D")

dt1=rbindlist(list(dtA,dtB,dtC,dtD))
```

The default scatter plot:

``` r
ggplot(dt1,aes(x,y))+
  geom_point(size=0.1)+
  facet_wrap(vars(z))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

The default ggplot2 2D-histogram:

``` r
ggplot(dt1,aes(x,y))+
  geom_bin2d()+
  facet_wrap(vars(z))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

2D-Histogram output:

``` r
hist2d(dt1,facet=1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

You can also add Linear Regression line:

``` r
hist2d(dt1,facet=1,hasLine=T)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
