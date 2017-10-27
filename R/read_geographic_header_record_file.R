#' Read geographic header record file of a state
#'
#' @description  Read geographic header record file of a state and return logical record number
#' and selected geographic headers. To find geographic headers, browse
#' \code{\link{dict_geoheader}} or search with \code{\link{search_geoheader}}.
#'
#'
#' @param state abbreviation of a state, for example "IN" for "Indiana".
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
#' ri  <- read_2010geoheader("your_local_path_to_census_data", "RI",
#'                       c("NAME", "SUMLEV", "INTPTLAT", "INTPTLON"))
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_sub str_trim
#'

read_2010geoheader <- function(state,
                               geo_headers = NULL,
                               show_progress = TRUE) {

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (show_progress) {
        cat(paste("Reading", state, "geographic header record file\n"))
    }

    # allow lowercase input for state and references
    state <- toupper(state)
    references <- toupper(geo_headers)

    file <- paste0(path_to_census, "/census2010/", state, "/", tolower(state), "geo2010.ur1")
    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, sep = "\n", encoding = "Latin-1" ,
                 showProgress = show_progress)

    # Below has been solved with encoding = "Latin-1". ===
    #
    # Replace unicodes with a single character "9" in geofile of these state.
    # A unicode such as "\xf1" is treated as one letter. do not break it apart.
    # They are in the geographic header file, not because fread reads them into
    # unicode
    # To save time do not treat each unicode individually.
    # if (state %in% c("US", "TX", "NM", "CA", "AZ", "CO")) {
    #    geo[, V1 := gsub("[\xf1\xe1\xe9\xed\xfc\xf3\xfa]", "9", V1)]
    # }

    # always keep the logical record number in the output data
    dt <- geo[, .(LOGRECNO = as.numeric(str_sub(V1, 19, 25)),
                  SUMLEV = str_sub(V1, 9, 11),
                  GEOCOMP = str_sub(V1, 12, 13))]

    # add all selected fields to output data
    if (!is.null(geo_headers)) {
        for (ref in geo_headers) {
            # identify numeric hearder
            if (ref %in% c("INTPTLAT", "INTPTLON", "AREALAND", "AREAWATR", "POP100",
                           "HU100")) {
                # place variable in () to add new columns
                dt[, (ref) := as.numeric(str_sub(geo[, V1],
                                                 dict_geoheader[reference == ref, start],
                                                 dict_geoheader[reference == ref, end]))]
            } else if (ref %in% c("LOGRECNO", "SUMLEV", "GEOCOMP")) {
                cat(paste(ref, "is included in return by default.\n"))
            } else {
                dt[, (ref) := str_trim(str_sub(geo[, V1],
                                               dict_geoheader[reference == ref, start],
                                               dict_geoheader[reference == ref, end]))]
            }
        }
    }


    setkey(dt, LOGRECNO)

    return(dt)
}



