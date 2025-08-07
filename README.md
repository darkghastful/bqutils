
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bqutils

<!-- badges: start -->
<!-- badges: end -->

bqutils is a collection of utility functions I use to minimize redundant
code.

## Installation

The development version of bqutils is available for installation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("darkghastful/bqutils")
#> Downloading GitHub repo darkghastful/bqutils@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\Bailey Quinn\AppData\Local\Temp\Rtmp6rh1c8\remotes6eec3fe02dba\darkghastful-bqutils-5077f63/DESCRIPTION' ...  ✔  checking for file 'C:\Users\Bailey Quinn\AppData\Local\Temp\Rtmp6rh1c8\remotes6eec3fe02dba\darkghastful-bqutils-5077f63/DESCRIPTION' (897ms)
#>       ─  preparing 'bqutils': (538ms)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (516ms)
#>   ─  checking for empty or unneeded directories
#>       ─  building 'bqutils_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/Bailey Quinn/AppData/Local/Temp/Rtmpy8FG4G/temp_libpath4e1c1461266'
#> (as 'lib' is unspecified)
```

## Usage

### Loading

``` r
library(bqutils)
```

### Numeric and Statistical

The following group of functions are for reporting and use of numeric
and significance values. round.to rounds to the nearest provided value
and can be changed to round either up or down depending on need

### sigfill

``` r
# Used to prep and properly format p-values
sigfill(c(0.043, 0.0034), 2, TRUE)
#> [1] "p=0.04" "p<0.01"
```

``` r
round.to(0.00987, 0.001)
#> [1] 0.01
round.to(0.00987, 0.001, "top")
#> [1] 0.01
round.to(0.00987, 0.001, "bottom")
#> [1] 0.009
```

``` r
count.decimals(0.00987)
#> [1] 5
```

``` r
remove.na(list(NA, 1, NA, 2, 3))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
replace.na(list(NA, 1, NA, 2, 3), 0)
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1] 1
#> 
#> [[3]]
#> [1] 0
#> 
#> [[4]]
#> [1] 2
#> 
#> [[5]]
#> [1] 3
replace.nan(c(NaN, 1, NaN, 2, 3), 0)
#> [1] 0 1 0 2 3
replace.specific(c(1, 2, 3), 1, 3)
#> [1] 3 2 3
```

``` r
frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
count.n(frame, c("a", "b"))
#> [1] 3
locate.element(frame, 1, "b")
#> [1] 1 2 3
duplicated.values(frame[,"b"])
#> [1] 1 1 2 2 3 3
uuln(frame[,"b"])
#> [1] 1 2 3
uln(frame[,"b"])
#> [1] 1 1 1 2 2 2 3 3 3
subset.object(frame, 1, "b")
#>   a b c
#> 1 1 1 1
#> 2 1 1 2
#> 3 1 1 3
row.names.to.column(frame)
#>   a b c row.name
#> 1 1 1 1        1
#> 2 1 1 2        2
#> 3 1 1 3        3
#> 4 1 2 4        4
#> 5 1 2 5        5
#> 6 1 2 6        6
#> 7 1 3 7        7
#> 8 1 3 8        8
#> 9 1 3 9        9
```

``` r
frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
frame_2 <- data.frame(frame, "d"=rowSums(frame))
merge.by.overlap(list(frame, frame_2))
#>   a b c  d
#> 1 1 1 1  3
#> 2 1 1 2  4
#> 3 1 1 3  5
#> 4 1 2 4  7
#> 5 1 2 5  8
#> 6 1 2 6  9
#> 7 1 3 7 11
#> 8 1 3 8 12
#> 9 1 3 9 13
```

``` r
frame <- data.frame("a"=c(3, 0.12, 0, 0, 0.001), "b"=c(0, 1, 0, 6, 9))
two.by.two.paired(frame, "a", "b")
#>       a.yes a.no
#> b.yes     2    1
#> b.no      1    1
```

