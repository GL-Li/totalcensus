#' Read geographic header record file
#'
#' Read geographic header record file of a state and return logical record number
#' and selected fields
#'
#'
#' @param path_to_census path to the directory holding downloaded
#'     census 2010 summary file 1 with urban/rural update
#' @param state abbreviation of a state, for example "IN" for "Indiana"
#' @param field vector of references of selected geographci headers to be included in the return
#' @param show_progress show progress of reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return data.table whose columns are logical record number and selected fields
#'
#' @examples
#' \dontrun{
#' path <- your_local_path_to_census_data
#' selected_geoheader <- read_geoheader(
#'     path_to_census = path,
#'     state = "RI",
#'     field = c("SUMLEV", "INTPTLAT", "INTPTLON")
#' )
#' }
#'
#'
#' @export

read_geoheader <- function(path_to_census, state, field, show_progress = TRUE) {
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
                                             dict_geoheader[reference == fld, start],
                                             dict_geoheader[reference == fld, end]))]
        } else {
            dt[, (fld) := str_sub(geo[, V1],
                                  dict_geoheader[reference == fld, start],
                                  dict_geoheader[reference == fld, end])]
        }

    }

    return(dt)
}



