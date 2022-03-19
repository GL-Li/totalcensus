# test the newly added survey data using RI. Make sure no error in reading.

library(totalcensus)
library(data.table)
library(magrittr)

# acs 5-year ==================================================================
# randomly select 100 varaibles from ACS5 year 2019
refs <- search_tablecontents("acs5", view = FALSE) %>%
    .[!is.na(acs5_2019), reference] %>%
    sample(100)

for (ref in refs){
    aaa = read_acs5year(2020, 'RI', table_contents = ref)
    cat("\n=== output ===\n")
    print(dim(aaa))
}
