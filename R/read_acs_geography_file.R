#' Read ACS 1-year survey geography file of a state
#'
#' @description  Read ACS 1-year survey geography file of a state and return
#' logical record number, summary level, geographic component
#' and selected geographic headers. To find geographic headers, browse
#' \code{\link{dict_acs_geoheaders}} or search with \code{\link{search_acs_geoheader}}.
#'
#'
#' @param path_to_census path to the directory holding downloaded ACS files.
#' @param state abbreviation of a state, for example "IN" for "Indiana".
#' @param year year of the survey
#' @param references vector of references of selected geographic headers to be included in the return
#' @param show_progress show progress of reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return data.table whose columns are logical record number, summary level,
#' geographic compoenent, and selected references. LOGRECNO serves as
#' the key.
#'
#' @examples
#' \dontrun{
#' # read selected geographic headers of Rhode Island
#' ri  <- read_acs1year_geo_("your_local_path_to_census_data", "RI", 2015,
#'                       c("NAME", "STATE"))
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_sub str_trim
#'

read_acs1year_geo_ <- function(path_to_census, state, year,
                               references = c("NAME", "GEOID"),
                               show_progress = TRUE) {
    # if (show_progress) {
    #     cat(paste("Reading", state, "geography file\n"))
    # }

    # allow lowercase input for state and references
    state <- tolower(state)
    references <- toupper(references)

    file <- paste0(path_to_census, "/", "acs1year/", year, "/g", year, "1",
                   tolower(state), ".csv")

    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, encoding = "Latin-1" ,
                 showProgress = show_progress, colClasses = "character") %>%
        setnames(names(.), dict_acs_geoheader$reference) %>%
        .[, c(c("LOGRECNO", "SUMLEV", "GEOCOMP"), references), with = FALSE] %>%
        .[, LOGRECNO := as.numeric(LOGRECNO)]

    setkey(geo, LOGRECNO)

    return(geo)
}



#' Read ACS 5-year survey geography file of a state
#'
#' @description  Read ACS 5-year survey geography file of a state and return
#' logical record number, summary level, geographic component
#' and selected geographic headers. To find geographic headers, browse
#' \code{\link{dict_acs_geoheaders}} or search with \code{\link{search_acs_geoheader}}.
#'
#'
#' @param path_to_census path to the directory holding downloaded ACS files.
#' @param state abbreviation of a state, for example "IN" for "Indiana".
#' @param year end year of the 5-year survey
#' @param references vector of references of selected geographic headers to be included in the return
#' @param show_progress show progress of reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return data.table whose columns are logical record number, summary level,
#' geographic compoenent, and selected references. LOGRECNO serves as
#' the key.
#'
#' @examples
#' \dontrun{
#' # read selected geographic headers of Rhode Island
#' ri  <- read_acs5year_geo_("your_local_path_to_census_data", "RI", 2015,
#'                       c("NAME", "STATE"))
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_sub str_trim
#'

read_acs5year_geo_ <- function(path_to_census, state, year,
                               references = c("NAME", "GEOID"),
                               show_progress = TRUE) {
    # if (show_progress) {
    #     cat(paste("Reading", state, "geography file\n"))
    # }

    # allow lowercase input for state and references
    state <- tolower(state)
    references <- toupper(references)

    file <- paste0(path_to_census, "/", "acs5year/", year, "/g", year, "5",
                   tolower(state), ".csv")

    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, encoding = "Latin-1" ,
                 showProgress = show_progress, colClasses = "character") %>%
        setnames(names(.), dict_acs_geoheader$reference) %>%
        .[, c(c("LOGRECNO", "SUMLEV", "GEOCOMP"), references), with = FALSE] %>%
        .[, LOGRECNO := as.numeric(LOGRECNO)]

    setkey(geo, LOGRECNO)

    return(geo)
}

