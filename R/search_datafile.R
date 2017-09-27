#' Search data files
#'
#' Search in all data files in order to find the file containing
#' the keyword.
#'
#' @param keyword keyword to be searched
#' @param view display the search result with View if TRUE
#' @param table_only only search in and strictly match table_num
#' @param file_only only search in file numbers
#'
#' @return A data.table
#'
#' @seealso \code{\link{dict_datafile}}
#'
#'
#' @examples
#' # search exactly table "p3"
#' search_datafile("p3", table_only = TRUE)
#'
#' # search reference "p0030002"
#' search_datafile("p0030002")
#'
#' # search file 03
#' search_datafile("03", file_only = TRUE)
#'
#' # search keyword "asian total population". too many matches, usually do not do it
#' search_datafile("asian total population")
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_split

search_datafile <- function(keyword, table_only = FALSE, file_only = FALSE, view = TRUE) {
    if (table_only) {
        # search for exact match
        dt <- dict_datafile[tolower(keyword) == tolower(table_number)]
    } else if (file_only){
        dt <- dict_datafile[as.numeric(keyword) == file_segment]
    } else {
        dt <- dict_datafile
        keywords <- unlist(str_split(tolower(keyword), " "))
        for (kw in keywords){
            # combine all rows to form a new column for search
            dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
                .[grepl(kw, tolower(comb))] %>%
                .[, comb := NULL]
        }
    }

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
