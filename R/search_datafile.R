#' Search data files
#'
#' Search in all data files in order to find the file containing
#' the keyword.
#'
#' @param keyword keyword to be searched
#' @param view display the search result with View if TRUE
#' @param table_only only search in and strictly match table_num
#'
#' @seealso \code{\link{dict_datafile}}
#'
#' @export
#'
#' @examples
#' # search exactly table "p3"
#' search_datafile("p3", table_only = TRUE)
#'
#' # search reference "p0030002"
#' search_datafile("p0030002")
#'
#' # search file 03
#' search_datafile("file_03")
#'
#' # search keyword "total population". too many matches, usually do not do it
#' search_datafile("total population")
#'
#'

search_datafile <- function(keyword, table_only = FALSE, view = TRUE) {
    if (table_only) {
        # search for exact match
        dt <- dict_datafile[tolower(keyword) == tolower(table_num)]
    } else {
        # search in multple columns
        dt1 <- dict_datafile[grepl(tolower(keyword), tolower(field))]
        dt2 <- dict_datafile[grepl(tolower(keyword), tolower(table_num))]
        dt3 <- dict_datafile[grepl(tolower(keyword), tolower(table))]
        dt4 <- dict_datafile[grepl(tolower(keyword), tolower(reference))]
        dt5 <- dict_datafile[grepl(tolower(keyword), tolower(file))]

        dt <- rbindlist(list(dt1, dt2, dt3, dt4, dt5)) %>%
            .[, .(file, field, reference, table_num)] %>%
            unique()
    }

    if (view) View(dt)

    return(dt)
}
