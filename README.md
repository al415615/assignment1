
# Cycling Infrastructure Analysis (assignment1)

<!-- badges: start -->

<!-- badges: end -->

The goal of this package is to download, classify, and visualize cycling
infrastructure data from OpenStreetMap. It builds spatial cycling
networks, classifies infrastructure by safety type, computes summary
statistics, and produces maps and plots for analysis.

## Installation

You can install the development version of assignment1 from
[GitHub](https://github.com/) with:

``` r

devtools::install_github("al415615/assignment1")
#> Warning: `install_github()` was deprecated in devtools 2.5.0.
#> ℹ Please use pak::pak("user/repo") instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo al415615/assignment1@HEAD
#> Rcpp    (1.1.1 -> 1.1.1-1.1) [CRAN]
#> cpp11   (0.5.4 -> 0.5.5    ) [CRAN]
#> openssl (2.4.0 -> 2.4.1    ) [CRAN]
#> sf      (1.1-0 -> 1.1-1    ) [CRAN]
#> Installing 4 packages: Rcpp, cpp11, openssl, sf
#> Installing packages into '/private/var/folders/6y/xpxbmdq93qvcq359c9jsf1y00000gn/T/RtmppoPN4M/temp_libpath520a61f8aadc'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/6y/xpxbmdq93qvcq359c9jsf1y00000gn/T//RtmpFiVc4z/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/6y/xpxbmdq93qvcq359c9jsf1y00000gn/T/RtmpFiVc4z/remotes6d34276eac98/al415615-assignment1-e3bd3b5/DESCRIPTION’ ... OK
#> * preparing ‘assignment1’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#>   NB: this package now depends on R (>= 3.5.0)
#>   WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>   serialize/load version 3 cannot be read in older versions of R.
#>   File(s) containing such objects:
#>     ‘assignment1/Amsterdam__Netherlands_5km_cycling.rds’
#>     ‘assignment1/Muenster__Germany_5km_cycling.rds’
#>     ‘assignment1/tests/testthat/Muenster__Germany_1km_cycling.rds’
#> * building ‘assignment1_0.0.0.9000.tar.gz’
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   ‘assignment1/assignment1_files/libs/quarto-html/quarto-syntax-highlighting-7f8f88aac4f3542376d5c11b86a4c14d.css’
#> Installing package into '/private/var/folders/6y/xpxbmdq93qvcq359c9jsf1y00000gn/T/RtmppoPN4M/temp_libpath520a61f8aadc'
#> (as 'lib' is unspecified)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(assignment1)

# Download cycling network
net <- get_cycling_network("Muenster, Germany")
#> Loading cached data for:  Muenster, Germany

# Print and plot network
net
#> cycling_network object
#>   City         : Muenster, Germany 
#>   Download date: 2026-05-25 
#>   Network lines: 4715 segments
#>   CRS          : EPSG:4326
plot(net)
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

``` r

# Classify infrastructure
classif <- classify_bike_infrastructure(net)

# Print and plot classification
classif
#> cycling_classification object
#>   City         : Muenster, Germany 
#>   Download date: 2026-05-25 
#>   Segments     : 4715 
#> 
#> Infrastructure summary:
#> 
#> 
#> |infra_type      | total_length_km|
#> |:---------------|---------------:|
#> |footway track   |          357.57|
#> |shared road     |          135.98|
#> |dedicated track |           30.34|
#> |painted lane    |            1.82|
plot_cycling_safety_map(classif)
#> 
#> Infrastructure summary for Muenster, Germany :
#> 
#> 
#> |infra_type      | total_length_km|
#> |:---------------|---------------:|
#> |footway track   |          357.57|
#> |shared road     |          135.98|
#> |dedicated track |           30.34|
#> |painted lane    |            1.82|
#> Zoom: 13
```

<img src="man/figures/README-example-2.png" alt="" width="100%" />

<!-- badges: start -->

<!-- badges: end -->

This package was developed as part of a university assignment on spatial
data analysis and cycling infrastructure accessibility.
