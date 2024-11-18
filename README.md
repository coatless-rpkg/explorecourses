
<!-- README.md is generated from README.Rmd. Please edit that file -->

# explorecourses

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/explorecourses/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/explorecourses/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> \[!IMPORTANT\]
>
> This package is part of a homework exercise for STATS 290 regarding
> data mining and web APIs.

The goal of explorecourses is to automatically retrieve course
information from Stanford University’s ExploreCourses API.

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
    API.
2.  `fetch_departments()`: Fetches the list of departments from the
    ExploreCourses API.
3.  `fetch_department_courses()`: Fetches the courses for a specific
    department.

By default, we’ll retrieve all courses across all departments for the
current academic year using:

``` r
all_courses <- fetch_all_courses()
```

This information is stored in the `schedule_ay24_25` data frame.

For just a single department, we can use it’s code to retrieve a list of
all classes:

``` r
department_courses <- fetch_department_courses("STATS")
```

To determine possible department shortcodes, we can use:

``` r
departments <- fetch_departments()
```

## License

AGPL (\>= 3)
