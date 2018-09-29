#' Quiet "no visible global function definition for" for variable read from csv
#'
#'
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# Also include function View from package utils. If imported to package, it
# prevents openning data frame in RStudio but instead in a popup window.

utils::globalVariables(unique(c(
    ".", "lon", "lat", "GEOID", "PLACE", "SUMLEV", "PLACE_tmp", "COUSUB",
    "COUSUB_tmp", "LOGRECNO", "V1", "dict_fips", "state_abbr", "STATE", "fips",
    "state", "NAME", "COUNTY", "county", "GEOCOMP", "state_full", "abbr",
    "states_DC", "lookup_acs1year_2014", "reference", "table_content",
    "table_name", "universe", "restriction", "lookup_acs1year_2015",
    "lookup_acs1year_2016", "lookup_acs5year_2015", "lookup_acs1year_2016",
    "content_acs1_2014", "content_acs1_2015", "content_acs1_2016",
    "name_acs1_2014", "name_acs1_2015", "name_acs1_2016", "universe_acs1_2014",
    "universe_acs1_2015", "universe_acs1_2016", "lookup_acs5year_2015",
    "lookup_acs5year_2016", "content_acs5_2015", "content_acs5_2016",
    "name_acs5_2015", "name_acs5_2016", "universe_acs5_2015", "universe_acs5_2016",
    "INTPTLON", "INTPTLAT", "STATE", "PLACE", "COUNTY", "COUSUB", "TRACT", "BLKGRP",
    "BLOCK", "CD", "SLDU", "SLDL", "CBSA", "METDIV", "CSA", "CONCIT",
    "ANRC", "AIANHH", "AIHHTLI", "AITSCE", "CNECTA", "NECTA",
    "NECTADIV", "UA", "PUMA", "SDELM", "SDSEC", "SDUNI", "TTRACT",
    "TBLKGRP", "ZCTA5", "NAME", "lookup_decennial_2010", "Census2010", "dict_cbsa",
    "CBSA_title", "file_segment", "content", "id", "code", "name", "geoheader",
    "area", "dict_acs_geoheader", "file_seg", "dict_decennial_geoheader", "start",
    "end", "comb", "geo_component", "summary_level", "field", "View",
    "dict_acs_geoheader_2005_1year", "dict_acs_geoheader_2006_2008_1year",
    "dict_acs_geoheader_2009_1year", "dict_acs_geoheader_2009_5year",
    "dict_acs_geoheader_2010", "lookup_acs1year_2008",
    "lookup_acs1year_2010", "lookup_acs1year_2017", "lookup_acs5year_2010",
    "total_files", "content_acs1_2017", "name_acs1_2017", "universe_acs1_2017",
    "content_acs5_2013", "content_acs5_2014", "name_acs5_2013", "name_acs5_2014",
    "universe_acs5_2013", "universe_acs5_2014", "content_acs1_2013",
    "name_acs1_2013", "universe_acs1_2013"
)))
