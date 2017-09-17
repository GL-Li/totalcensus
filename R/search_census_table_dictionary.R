#' Search census table
#'
#' search census tables by keyword in table numbers or table descriptions
#'
#' @param keyword keyword to search in code or description. To search for a table
#'     contains two words "abc" and "defg", the keyword is simply a single string
#'     of "abc defg".
#'
#' @export
#'
#' @seealso \code{\link{censustable_dict}} lists all geocomponents and codes
#'
#' @examples
#' # search census table contains "occupancy"
#' search_table("occupancy")
#'
#' # search census table with table number "H6"
#' search_table("H6")

search_table <- function(keyword){
    # search rows that contains ALL keywords, NOT any
    dt <- censustable_dict
    keywords <- unlist(str_split(keyword, " "))
    for (kw in keywords) {
        dt1 <- dt[grepl(tolower(kw), tolower(table_num))]
        dt2 <- dt[grepl(tolower(kw), tolower(table))]

        # this dt is used for next loop of kw
        dt <- rbindlist(list(dt1, dt2)) %>%
            .[, .(table_num, table, universe)] %>%
            unique()
    }

    return(dt)
}
