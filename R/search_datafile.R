#' Search in data files
#'
#' @description Search in dataset \code{\link{dict_datafile}} to find the file
#' or table containing the keywords. This function helps users to find what data are
#' included in the summary file 1 (with urban/rural update).
#'
#' @param keyword keyword to be searched
#' @param table_only only search in and strictly match table number
#' @param file_only only search in file numbers
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#' @examples
#' \dontrun{
#'   # search exactly table "p3"
#'   search_datafile("p3", table_only = TRUE)
#'
#'   # search reference "p0030002"
#'   search_datafile("p0030002")
#'
#'   # search for file 03
#'   search_datafile("03", file_only = TRUE)
#'
#'   # search keyword "federal prision population".
#'   search_datafile("federal prison population")
#' }
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
