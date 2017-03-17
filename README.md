
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/kippchicago/teachboostr.svg?branch=master)](https://travis-ci.org/kippchicago/teachboostr) [![Coverage Status](https://img.shields.io/codecov/c/github/kippchicago/teachboostr/master.svg)](https://codecov.io/github/kippchicago/teachboostr?branch=master) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/kippchicago/teachboostr?branch=master&svg=true)](https://ci.appveyor.com/project/kippchicago/teachboostr)

`teachboostr` is a simple R package that provides bindings to the TeachBoost API.

### Getting Started

The package is only available on github:

``` r
devtools::install_github('kippchicago/teachboostr')
```

`get_tb()` is a wrapper around the workhourse `teachboost_api()` function. If you save your API key in an `.Renviron` file with the variable name `KEY_TB` the `get_tb()` function will look it up; otherwise you should pass your key to the `key` parameter.

``` r
library(teachboostr)

users <- get_tb("users")

users
```

You'll notice that `users` is a list of `teachboost_api` objects (which themselves are lists of the respons, content, and call). This is result of the 1,000 record limit that TeachBoost imposes. The `get_tb()` function automates repining the api to get every record.

You can use the `unpack_tb()` function to extract the content from the `get_tb` call:

``` r
users_df <- unpack_tb(users)
```

Notice that nested JSON returned by the API will be stored as nested list-columns in the data frame.
