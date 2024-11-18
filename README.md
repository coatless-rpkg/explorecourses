
<!-- README.md is generated from README.Rmd. Please edit that file -->

# explorecourses <img src="man/figures/logo-explorecourses.png" align="right" alt="Logo: a person looking at all the courses to choose from" width="150"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/explorecourses/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/explorecourses/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> \[!IMPORTANT\]
>
> This package is part of a homework exercise for STATS 290 regarding
> data mining and web APIs.

The goal of `explorecourses` is to automatically retrieve course
information from Stanford University’s
[ExploreCourses](https://explorecourses.stanford.edu/) API.

## Installation

You can install the development version of explorecourses from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::remotes("coatless-rpkg/explorecourses")
```

## Usage

First, load the package:

``` r
library(explorecourses)
```

The package contains three main functions:

1.  `fetch_all_courses()`: Fetches all courses from the ExploreCourses
    API for a set of departments (Default: all).
2.  `fetch_department_courses()`: Fetches the courses for a specific
    department.
3.  `fetch_departments()`: Fetches the list of departments from the
    ExploreCourses API.

By default, we’ll retrieve all courses across all departments for the
current academic year using:

``` r
all_courses <- fetch_all_courses()
```

We can also request specific courses for a set of departments in a given
academic year. For example, to retrieve all courses for the departments
of “STATS” and “MATH” for the academic year 2023-2024, we can use:

``` r
stats_and_math_courses <- fetch_all_courses(c("STATS", "MATH"), year = "20232024")
```

This function is excellent for retrieving course information across
multiple departments for a given academic year as it allows for parallel
processing of the data.

If we know the department shortcode for a specific department, we can
use the `fetch_department_courses()` function to retrieve the courses
for that department. This function does not support parallel processing
and is designed for fetching courses for a single department. For
example, to retrieve all courses for the “STATS” department, we can use:

``` r
department_courses <- fetch_department_courses("STATS")
```

To determine possible department shortcodes, we can use:

``` r
departments <- fetch_departments()
```

This will return a data frame with the department short name, long name,
and school the department is associated with.

### Cache

To cache the data, we can use the `cache_dir` parameter in the
`fetch_all_courses()`, `fetch_department_courses()`, and
`fetch_departments()` functions. This will cause the XML data downloaded
from the API to be stored in the specified directory and reused on
subsequent calls.

We can list the current cache contents using the `list_cache()`
function:

``` r
list_cache() # List current cache
```

``` r
# Cache contents:
# 
# Found 256 cached files
# Directory: explorecourses_cache
# 
# AA ACCT AFRICAAM ALP AMELANG
# AMHRLANG AMSTUD ANES ANTHRO APPPHYS
# ARABLANG ARCHLGY ARMELANG ARTHIST ARTSINST
# ...
```

### Parallel Processing

We can speed up the process of fetching courses by using parallel
processing. For the `fetch_all_courses()` function, we’ve set up
parallel processing using the `furrr` package, which provides `purrr`’s
functional interface to the `future` parallel processing library. As a
result, we will be able to download and process all courses for every
department in parallel. Moreover, we’ve set up progress reporting using
the `progressr` package to track the progress of the parallel
processing.

``` r
library(explorecourses)
library(future)
library(progressr)

# Set up parallel processing
plan(multisession)

# Set up progress reporting
handlers(handler_progress())

# Show progress bar for fetching all courses
with_progress({
  # Fetch all courses for the departments in parallel
  all_courses <- fetch_all_courses()
})

# Reset to sequential processing
plan(sequential)
```

Please note, we need to ensure we deactivate the `multisession` plan by
resetting it to `sequential` after we’ve finished using it.

## License

AGPL (\>= 3)
