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
#' @param year year of the census
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
#' aaa = read_decennial(year = 2010,
#'                      states = c("ut", "ri"),
#'                      table_contents = c("P0030002", "P0150008"),
#'                      geo_headers = c("place = ut62360",  # place in UT
#'                                      "place = 14140",    # place in UT or RI
#'                                      "cousub = ri41500"),
#'                      summary_level = "block")
#' }
#'
#' @export
#'
#'

read_decennial <- function(year,
                           states,
                           table_contents = NULL,
                           geo_headers = NULL,
                           summary_level = "*",
                           geo_comp = "*",
                           with_coord = TRUE,
                           with_population = TRUE,
                           show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    geo_headers_0 <- geo_headers  # keep a copy for latter use

    # switch summary level to code
    if (summary_level %in% c("state", "county", "county_subdivision", "place",
                             "tract", "block_group", "block")){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block group" = "150",
                                "block" = "100")
    }

    # data to covert STATE (fips) to state (abbreviation)
    state_fips <- dict_fips[SUMLEV == "040",
                            .(state = state_abbr, STATE)]

    # extract information from argument geo_headers
    geo <- str_replace_all(geo_headers, " ", "") %>%
        toupper()
    dt_geo <- data.table(
        geoheader = str_extract(geo, "^[^=]*"),
        code = str_split(str_extract(geo, "[^=]*$"), ",")
    ) %>%
        .[, state := str_extract(code, "^[A-Z]*")] %>%
        .[, code := str_extract(code, "[0-9]*$")]


    # this is used to extract geoheaders
    geo_headers <- unique(dt_geo[, geoheader])

    if (with_coord){
        geo_headers = c(c("INTPTLON", "INTPTLAT"), geo_headers)
    }

    lst <- list()
    iter <- 0
    N <- length(states)
    for (st in toupper(states)){
        iter <- iter + 1
        cat(paste0("reading ", iter, "/", N, " states\n"))

        # add st to geoheaders as there are multiple state
        geo <- read_decennial_geoheader_(year, st, geo_headers,
                                  show_progress = show_progress) %>%
            # convert STATE fips to state abbreviation
            state_fips[., on = .(STATE)] %>%
            .[, STATE := NULL] %>%
            setkey(LOGRECNO)

        if (with_population){
            table_contents <- c("P0010001", table_contents)
            data <- read_decennial_tablecontents_(year, st, table_contents,
                                                  show_progress = show_progress)
            census <- geo[data] %>%
                setnames("P0010001", "population")
        } else {
            if (!is.null(table_contents)) {
                data <- read_decennial_tablecontents_(year, st, table_contents,
                                                      show_progress = show_progress)
                census <- geo[data, on = .(LOGRECNO)]
            } else {
                census <- geo
            }
        }

        lst[[st]] <- census[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]
    }
    combined <- rbindlist(lst) %>%
        .[, LOGRECNO := NULL]

    if (with_coord) {
        setnames(combined, c("INTPTLON", "INTPTLAT"), c("lon", "lat"))
        setcolorder(combined, c(c("lon", "lat"), setdiff(names(combined), c("lon", "lat"))))
    }

    # select data for argument geo_headers
    if (is.null(geo_headers_0)) {
        selected <- combined
    } else {
        selected <- map(1:nrow(dt_geo),
                        function(x) combined[get(dt_geo[x, geoheader]) %like% dt_geo[x, code] &
                                                 state %like% dt_geo[x, state]]) %>%
            rbindlist()
    }

    return(selected)
}
