#' Read selected geoheader and table content
#'
#'
#' @param path_to_census path to the directory holding downloaded
#'     census 2010 summary file 1 with urban/rural update
#' @param state abbreviation of a state, for example "IN" for "Indiana"
#' @param geoheaders vector of references of selected geographci headers to be included in the return
#' @param table_contents selected references of contents in census tables
#' @param show_progress show progress of reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return A data.table of selected geoheaders and table contents, with LOGRECNO
#' as key.
#'
#' @examples
#' \dontrun{
#' aaa <- read_2010census("~/dropbox_datasets/US_2010_census/", "RI",
#'                        c("STATE", "NAME", "SUMLEV"),
#'                        c("PCT012F139", "PCT012F138", "P0150008", "H0070016"))
#' }
#'
#' @export
#'
#'

read_2010census <- function(path_to_census, state, geoheaders,
                            table_contents, show_progress = TRUE){
    geo <- read_2010geoheader(path_to_census, state, geoheaders,
                              show_progress = show_progress)
    data <- read_2010tablecontents(path_to_census, state, table_contents,
                                   show_progress = show_progress)
    geo[data]
}
