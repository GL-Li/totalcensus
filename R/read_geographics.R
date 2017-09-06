#' Read geographic header record file of a state and return logical record number
#' and selected fields
#'
#' @param path_to_census path to the directory holding downloaded
#'     census 2010 summary file 1 with urban/rural update
#' @param state abbreviation of a state, for example "IN" for "Indiana"
#' @param field vector of references of selected geographci headers to be included in the return
#'
#' @return data.table whose columns are logical record number and selected fields
#'
#' @examples
#'
#' @export

read_geographics <- function(path_to_census, state, field, show_progress = TRUE) {
    if (show_progress) {
        print(paste("reading", state, "geographic data"))
    }
    file <- paste0(path_to_census, "/", state, "/", tolower(state), "geo2010.ur1")
    geo <- fread(file, header = FALSE, sep = "\n", showProgress = show_progress)

    # Replace unicodes with a single character "9" in geofile of these state.
    # A unicode such as "\xf1" is treated as one letter. do not break it apart.
    # They are in the geographic header file, not because fread reads them into
    # unicode
    # To save time do not treat each unicode individually.
    if (state %in% c("US", "TX", "NM", "CA", "AZ", "CO")) {
        geo[, V1 := gsub("[\xf1\xe1\xe9\xed\xfc\xf3\xfa]", "9", V1)]
    }

    # always keep the logical record number in the output data
    dt <- geo[, .(LOGRECNO = as.numeric(str_sub(V1, 19, 25)))]

    # add all selected fields to output data
    for (fld in toupper(field)) {
        # identify numeric hearder
        if (fld %in% c("INTPTLAT", "INTPTLON")) {
            # place variable in () to add new columns
            dt[, (fld) := as.numeric(str_sub(geo[, V1],
                                             geo_dict[reference == fld, start],
                                             geo_dict[reference == fld, end]))]
        } else {
            dt[, (fld) := str_sub(geo[, V1],
                                  geo_dict[reference == fld, start],
                                  geo_dict[reference == fld, end])]
        }

    }

    return(dt)
}



#' Search for field reference with a keyword or search for the description of a
#' a field from its reference.
#'
#' @param ref reference of a field such as "SUMLEV"
#' @param keyword keyword in description
#'
#' @return data.table match the search criteria
#'
#' @examples
#'
#' @export
search_geodict <- function(ref = NULL, keyword = NULL) {
    if (!any(c(is.null(ref), is.null(keyword)))) {
        stop("Please use only one of reference or keyword to search")
    } else if (!is.null(ref)) {
        # for geo_dict dataset to work properly as a data.table, have to place
        # data.table in Depends in DESCRPTION file!!!!
        dt <- geo_dict[tolower(reference) == tolower(ref)] %>%
            .[, .(description, reference)]
        return(dt)
    } else if (!is.null(keyword)) {
        dt <- geo_dict[grepl(keyword, tolower(description))] %>%
            .[, .(description, reference)]
        return(dt)
    }
}


