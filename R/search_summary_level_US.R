#' Search summary levels of national files
#'
#' @description Search code or description of summary levels used in  national
#' files. The summary levels are stored in dataset \code{\link{dict_summarylevel_US}}.
#' This dataset is different from that for state files,
#' \code{\link{dict_summarylevel}}. Function \code{\link{search_sumlev}}
#' searches summary levels in state files.
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of summary levels match the search
#'
#'
#' @examples
#' \dontrun{
#'   # search summary level of geocomponent contains "block"
#'   search_sumlev_US("block")
#'
#'   # search summary level with code 40
#'   search_sumlev_US("40")
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#'


search_sumlev_US <- function(keyword, view = TRUE){
    dt1 <- dict_summarylevel_US[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_summarylevel_US[grepl(tolower(keyword), tolower(summary_level))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(summary_level, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
