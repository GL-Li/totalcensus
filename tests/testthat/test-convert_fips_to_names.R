test_that("convert_fips_to_names", {
  expect_equal(convert_fips_to_names(c("11", "44")), c("DC", "RI"))
  expect_equal(convert_fips_to_names(c("001", "013"), states = c("RI", "MA"), geo_header = "COUNTY"),
               c("Bristol County", "Hampden County"))
})
