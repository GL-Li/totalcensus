#' Read selected geographic headers and table contents
#'
#' @description  Read geographic header record file and data files of states and return logical record number,
#' selected geoheaders, state abbreviation, and table contents. These table contents can be from different tables, for example,
#' c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
#' The rows of short contents that have no corresponding LOGRECNOs are filled with
#' NAs. To find the references of table contents of interest, search with function
#' \code{\link{search_datafile}}. To find geographic headers, browse
#' \code{\link{dict_geoheader}} or search with function \code{\link{search_geoheader}}.
#'
#'
#' @param path_to_census path to the directory holding downloaded
#'     census 2010 summary file 1 (with urban/rural update). Inside this directiory
#'     are sub-folders of each state.
#' @param state vector of abbreviation of states, for example "IN" or c("MA", "RI").
#' @param geoheaders vector of references of selected geographci headers to be included in the return.
#' @param table_contents selected references of contents in census tables.
#' @param show_progress show progress of file reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return A data.table whose columns are LOGRECNO, SUMLEV, GEOCOMP, the selected geoheaders and
#' the table contents, with LOGRECNO as the key. The data.table contains all rows
#' of geoheader and data files.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island and Massachusette summary data
#' ri <- read_2010census("your_local_path_to_2010_census", c("RI", "MA"),
#'                        c("GEOCOMP", "NAME", "SUMLEV"),
#'                        c("PCT012F139", "PCT012F138", "P0150008", "H0070016"))
#'
#' # read the national summary data
#' us <- read_2010census("your_local_path_to_2010_census", "US",
#'                        c("STATE", "COUNTY", "COUSUB", "GEOCOMP", "NAME", "SUMLEV"),
#'                        c("PCT012F139", "PCT012F138", "P0150008", "H0070016"))
#' }
#'
#' @export
#'
#'

read_2010census <- function(path_to_census, state, geoheaders ,
                            table_contents, show_progress = TRUE){
    lst <- list()
    for (st in toupper(state)){
        # add st to geoheaders as there are multiple state
        geo <- read_2010geoheader(path_to_census, st, c(geoheaders),
                                  show_progress = show_progress) %>%
            .[, state := st]
        data <- read_2010tablecontents(path_to_census, st, table_contents,
                                       show_progress = show_progress)
        census <- geo[data]

        lst[[st]] = census
    }
    rbindlist(lst)
}
