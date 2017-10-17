# the geographic header is CBSA, the code for
# Metropolitan Statistical Area/Micropolitan Statistical Area

# source data is the csv data from
# http://www.nber.org/data/cbsa-fips-county-crosswalk.html

library(data.table)
library(magrittr)
dict_cbsa <- fread("data_raw/cbsa_to_fips.csv", encoding = "Latin-1") %>%
    .[-1] %>%
    # rename and reorder columns
    .[, .(CBSA = cbsacode,
          CBSA_title = cbsatitle,
          state_full = statename,
          county = countycountyequivalent,
          CSA = csacode,
          CSA_title = csatitle,
          METDIV = metrodivisioncode,
          METDIV_title = metropolitandivisiontitle,
          metro_micro = metropolitanmicropolitanstatis,
          STATE = fipsstatecode,
          COUNTY = fipscountycode,
          central_outlying = centraloutlyingcounty)]


save(dict_cbsa, file = "data/dict_cbsa.RData", compress = "xz", compression_level = 9)
