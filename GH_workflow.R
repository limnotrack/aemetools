
usethis::use_gpl_license(version = 2)


library(usethis)
use_readme_rmd()
devtools::dev_sitrep()
use_lifecycle_badge("experimental")
use_testthat()

# Build README.md
devtools::build_readme()



# use_pkgdown()
use_coverage()
use_github_action()
use_github_action("test-coverage")
# use_data()
usethis::use_data_raw()

use_logo("inst/figures/aeme.png")



# CRAN packages
use_package("airGR")
use_package("dplyr")
use_package("ggplot2")
use_package("sf")


use_package("job")
use_package("lubridate")
use_package("psychrolib")
use_package("sf")
use_package("units")
use_package("stars")
use_package("stringr")
use_package("parallel")
use_package("methods")
use_package("yaml")
use_package("ncdf4")
use_package("RColorBrewer")
use_package("tidyr")
use_package("stats")
use_package("tools")
use_package("utils")
use_package("zoo")

# GitHub packages


job::job({
  devtools::check(error_on = "error")
}, title = "devtools - check")

# Code coverage
job::job({
  # covr::codecov()
  covr::package_coverage()
}, title = "Code coverage")

# Build pkgdown
job::job({
  pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)
}, title = "Build pkgdown")


# R CMD CHECK
rcmdcheck::rcmdcheck()


attach(loadNamespace("aemetools"), name = "aemetools_all")
# attach(loadNamespace("glmtools"), name = "glmtools_all")

