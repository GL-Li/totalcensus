
# liscence

# imports
usethis::use_package("data.table", type = "Imports")
usethis::use_package("stringr", type = "Import")
usethis::use_package("magrittr", type = "Import")
usethis::use_package("purrr", type = "Import")
usethis::use_package("utils", type = "Import")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("ggplot2", type = "Suggests")
usethis::use_package("ggplot2", type = "Suggests")

# suggests

# create test directories
usethis::use_testthat()
# add test files
usethis::use_test("convert_fips_to_names")
usethis::use_test("")
usethis::use_test("")
# run tests
devtools::test()

# create data set
# usethis::use_data(selfSrvData_allowed, overwrite = TRUE)

# notes
# import functions from a package
#' @importFrom dplyr filter mutate select
#'
