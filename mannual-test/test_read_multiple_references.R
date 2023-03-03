# read multiple references from same segment to make sure values are assigned
# correctly.
# This is to address a bug in v0.6.5 where the read_decennial incorrectly
# assigned values when reading multple references. The value are assigned by the
# string order of the references, not by their actual order in the reference
# vector. Seems the vector is treated as a factor.
#
# Where is the bug?
# It is in read_decennial.R in this line
# loc <- which(all_contents %in% table_contents)
# It orders the table_contents by the order they appear in all_contents instead
# the actual oder. !!!!!!!!!!!


library(totalcensus)

# ACS =========================================================================
race_ref <- c("B03002_012",
              "B03002_003",
              "B02001_003",
              "B02001_005")
race <- read_acs5year(
    2019, "RI",
    table_contents = race_ref
)

for (rc in race_ref){
    race_1 <- read_acs5year(2019, "RI", table_contents = rc)
    stopifnot(all(race_1[[rc]] == race[[rc]], na.rm = TRUE))
    cat("\nALL passed")
}



edu_ref <- c("B06009_002",
             "B06009_005",
             "B06009_003",
             "B06009_004",
             "B06009_006")
education <- read_acs5year(
    2019, "RI",
    table_contents = edu_ref
)

for (edu in edu_ref){
    edu_1 <- read_acs5year(2019, "RI", table_contents = edu)
    stopifnot(all(edu_1[[edu]] == education[[edu]], na.rm = TRUE))
    cat("\nALL passed")
}


# decennial ===================================================================

# .. dec 2000 ----
race_ref <- c("P0030004",
              "P0040002",
              "P0030006",
              "P0040005"
)

race <- read_decennial(
    2000, "RI",
    table_contents = race_ref
)

for (rc in race_ref){
    race_1 <- read_decennial(2000, "RI", table_contents = rc)
    stopifnot(all(race_1[[rc]] == race[[rc]], na.rm = TRUE))
    cat("\nALL passed")
}

# .. dec 2010 ----
race_ref <- c("P0050010",
              "P0050003",
              "P0030003",
              "P0030005")

race <- read_decennial(
    2010, "RI",
    table_contents = race_ref
)

for (rc in race_ref){
    print(rc)
    race_1 <- read_decennial(2010, "RI", table_contents = rc)
    stopifnot(all(race_1[[rc]] == race[[rc]], na.rm = TRUE))
    cat("passed\n\n")
}
