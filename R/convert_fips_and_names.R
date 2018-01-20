#' convert fips codes to names of a geographies
#'
#' @param FIPs string vector of fips code such as c("021", "002")
#' @param states string vector of state abbreviations having same length as FIPs
#' @param geo_header string, taking values of "STATE", "COUNTY", "PLACE", "COUSUB"
#'      or "CBSA".
#' @param in_states which states are these FIPs generated from. Use state abbrevations
#'     or "US" for national. Vector of unique states.
#'
#' @return vector of names corresponding to FIPs and states
#'
#' @examples
#' convert_fips_to_names(c("11", "44"))
#' # [1] "DC" "RI"
#'
#' convert_fips_to_names(
#'     FIPs = c("14140", "76030"),
#'     states = c("RI", "MA"),
#'     geo_header = "PLACE",
#'     in_states = c("RI", "MA")
#' )
#' # [1] "Central Falls city" "Westfield city"
#'
#' convert_fips_to_names(
#'     FIPs = c("39300", "46740"),
#'     states = c(NA, NA),
#'     geo_header = "CBSA",
#'     in_states = "US"
#' )
#' # [1] "Providence-New Bedford-Fall River, RI-MA Metro Area" "Valley, AL Micro Area"
#'
#' @export
#'

convert_fips_to_names <- function(FIPs,
                                  states = NULL,
                                  geo_header = "STATE",
                                  in_states = NULL) {

    states <- toupper(states)
    # make data.table for later to join
    if (geo_header %in% c("STATE")){
        FIPs <- data.table(fips = FIPs)
    } else if (geo_header %in% c("COUNTY", "PLACE", "COUSUB", "CBSA")) {
        FIPs <- data.table(fips = FIPs, state = states)
    }


    if (geo_header == "STATE"){
        fips_geo <- dict_fips[SUMLEV == "040", .(state = state_abbr, fips = STATE)]
        names <- fips_geo[FIPs, on = .(fips)] %>%
            .[, state]
    } else if (geo_header == "COUNTY"){
        fips_geo <- dict_fips[SUMLEV == "050",
                              .(state = state_abbr, county = NAME, fips = COUNTY)]
        names <- fips_geo[FIPs, on = .(fips, state)] %>%
            .[, county]
    } else if (geo_header %in% c("PLACE", "COUSUB", "CBSA")){
        names <- get_name_from_census2010(FIPs, geo_header, in_states)

    } else {
        message(paste('This version only provides names for geographic headers',
                      'STATE, COUNTY, PLACE, COUSUB, and CBSA'))
        names <- "To be added"
    }

    return(names)
}

