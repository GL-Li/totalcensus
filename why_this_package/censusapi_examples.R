# examples: https://hrecht.github.io/censusapi/articles/example-masterlist.html

library(censusapi)

system.time({
    data2010 <- getCensus(name = "dec/sf1",
                          vintage = 2010,
                          vars = c("NAME", "P001001", "H010001"),
                          region = "metropolitan statistical area/micropolitan statistical area:*")

})

system.time({
    acs_income <- getCensus(name = "acs/acs5",
                            vintage = 2017,
                            vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"),
                            region = "tract:*",
                            regionin = "state:02")

})
