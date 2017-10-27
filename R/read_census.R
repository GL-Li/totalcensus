#' Read selected geographic headers and table contents of selected summary level
#' and geographic component.
#'
#' @description  Read geographic header record file and data files of states and return
#' selected geoheaders, state abbreviation, and table contents of selected summary
#' level and geographic component. These table contents can be from different tables, for example,
#' c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
#' The rows of short contents that have no corresponding LOGRECNOs are filled with
#' NAs. To find the references of table contents of interest, search with function
#' \code{\link{search_datafile}}. To find geographic headers, browse
#' \code{\link{dict_geoheader}} or search with function \code{\link{search_geoheader}}.
#'
#'
#' @param state vector of abbreviation of states, for example "IN" or c("MA", "RI").
#' @param geoheaders vector of references of selected geographci headers to be included in the return.
#' @param table_contents selected references of contents in census tables.
#' @param summary_level select which summary level to keep, "*" to keep all. It takes strings
#'        including "state", "county", "county_subdivision", "place", "tract", "block_group",
#'        and "block" for the most common levels. It also take code for level. Search all codes with
#'        \code{\link{search_sumlev}} for state files or \code{\link{search_sumlev_US}} for national files.
#' @param geo_comp select which geographic component to keep, "*" to keep all.
#'        Summary level of a state contains all geographic component. County
#'        subdivision and higher level have all area ("00"), urban ("01"), and rural ("43").
#'        census tract and lower level have no geographic component, all are "00". Browse
#'        \code{\link{dict_geocomp}} for other geographic components.
#' @param show_progress show progress of file reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return A data.table whose columns include the selected geoheaders and
#' table contents plus LOGRECNO, SUMLEV, GEOCOMP, and state. state indicates
#' the data from state files with state abbreviation or national files with "US".
#'
#' @examples
#' \dontrun{
#' # read Rhode Island and Massachusette summary data at block level
#' ri <- read_2010census("your_local_path_to_2010_census", c("RI", "MA"),
#'                        geoheaders = c("NAME", "INTPTLON", "INTPTLAT"),
#'                        table_contents = c("PCT012F139", "P0150008"),
#'                        summary_level = "block")
#'
#' # read the national summary data
#' us <- read_2010census("your_local_path_to_2010_census", "US",
#'                        geoheaders = c("STATE", "COUNTY", "COUSUB", "NAME"),
#'                        table_contents = c("PCT012F139", "P0150008"),
#'                        summary_level = "county_subdivision")
#' }
#'
#' @export
#'
#'

read_census2010 <- function(state,
                            geoheaders = NULL,
                            table_contents = NULL,
                            summary_level = "*",
                            geo_comp = "*",
                            show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # switch summary level to code
    if (summary_level %in% c("state", "county", "county_subdivision", "place",
                             "tract", "block_group", "block")){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county_subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block_group" = "150",
                                "block" = "100")
    }

    lst <- list()
    iter <- 0
    N <- length(state)
    for (st in toupper(state)){
        iter <- iter + 1
        cat(paste0("reading ", iter, "/", N, " states\n"))

        # add st to geoheaders as there are multiple state
        geo <- read_2010geoheader(st, geoheaders,
                                  show_progress = show_progress) %>%
            .[, state := st]
        if (!is.null(table_contents)) {
            data <- read_2010tablecontents(st, table_contents,
                                           show_progress = show_progress)
            census <- geo[data]
        } else {
            census <- geo
        }

        lst[[st]] <- census[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]
    }
    rbindlist(lst)
}
