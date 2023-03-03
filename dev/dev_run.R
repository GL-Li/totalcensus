
# create documents, delete NAMESPACE at the first run
# roxygen2::roxygenise()
devtools::document()

# run test code
devtools::test()

# do a full check for errors, warnings, and notes
devtools::check()

# install package for test run
devtools::install()

# build source code
devtools::build()

# view test coverage, package must not being used.
# detach(package:payEquity)
covr::report()                # report coverage in DT table
covr::package_coverage()      # print coverage in R console

# load functions, use them, and examine output
devtools::load_all()

# lint
lintr::lint_package()
