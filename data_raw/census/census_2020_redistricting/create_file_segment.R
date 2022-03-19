library(data.table)
library(magrittr)
library(stringr)

f01 <- fread("file01.csv", header = FALSE, sep = "\n") %>%
    setnames("table_content") %>%
    .[1, reference := "FILEID"] %>%
    .[2, reference := "STUSAB"] %>%
    .[3, reference := "CHARITER"] %>%
    .[4, reference := "CIFSN"] %>%
    .[5, reference := "LOGRECNO"] %>%
    .[6:76, ":="(reference = paste0("P00100", str_pad(1:71, width = 2, side = "left", pad = "0")),
                 table_name = "Race",
                 universe = "Total population")] %>%
    .[77:149, ":="(reference = paste0("P00200", str_pad(1:73, width = 2, side = "left", pad = "0")),
                   table_name = "Hispanic or Latino, and Not Hispanic or Latino by Race",
                   universe = "Total population")] %>%
    .[, file_seg := "01"]


f02 <- fread("file02.csv", header = FALSE, sep = "\n") %>%
    setnames("table_content") %>%
    .[1, reference := "FILEID"] %>%
    .[2, reference := "STUSAB"] %>%
    .[3, reference := "CHARITER"] %>%
    .[4, reference := "CIFSN"] %>%
    .[5, reference := "LOGRECNO"] %>%
    .[6:76, ":="(reference = paste0("P00300", str_pad(1:71, width = 2, side = "left", pad = "0")),
              table_name = "Race for the Population 18 Years and Over",
              universe = "Total population 18 years and over")] %>%
    .[77:149, ":="(reference = paste0("P00400", str_pad(1:73, width = 2, side = "left", pad = "0")),
                   table_name = "Hispanic or Latino, and Not Hispanic or Latino by Race for the Population 18 Years and Over",
                   universe = "Total population 18 years and over")] %>%
    .[150:152, ":="(reference = paste0("H00100", str_pad(1:3, width = 2, side = "left", pad = "0")),
                    table_name = "Occupancy Status",
                    universe = "Housing units")] %>%
    .[, file_seg := "02"]


f03 <- fread("file03.csv", header = FALSE, sep = "\n") %>%
    setnames("table_content") %>%
    .[1, reference := "FILEID"] %>%
    .[2, reference := "STUSAB"] %>%
    .[3, reference := "CHARITER"] %>%
    .[4, reference := "CIFSN"] %>%
    .[5, reference := "LOGRECNO"] %>%
    .[6:15, ":="(reference = paste0("P00500", str_pad(1:10, width = 2, side = "left", pad = "0")),
                 table_name = "Group Quarters Population by Major Group Quarters Type",
                 universe = "Population in group quarters")] %>%
    .[, file_seg := "03"]


file_all <- rbindlist(list(f01, f02, f03)) %>%
    .[, .(reference, table_content, table_name, universe, file_seg)]
