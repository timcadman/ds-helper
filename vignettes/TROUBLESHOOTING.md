Troubleshooting
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installing the package

### Dependencies

If the `nloptr` dependency does not install and your working on a
linux-based system please install `nlopt`.

For CentOS

``` bash
yum install nlopt nlopt-devel
```

For Ubuntu

``` bash
apt-get install libnlopt-dev
```

For Mac

``` bash
brew install nlopt
```

### Tidyr/CPP 11 install error

If you get an error when trying to install tidyr, run the following
lines of code:

``` r
remove.packages("tidyr")
remove.packages("cpp11")
library(remotes)
install_github('r-lib/cpp11', ref = 'v0.1.0')
install_github('tidyverse/tidyr', ref = 'v1.1.0')
```
