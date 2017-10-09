#' Search summary levels of state files
#'
#' @description Search code or description of summary levels used in state files.
#' The summary levels are stored in dataset \code{\link{dict_summarylevel}}.
#' This dataset is different from that for national files,
#' \code{\link{dict_summarylevel_US}}. Function \code{\link{search_sumlev_US}}
#' searches summary levels in national files.
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of searched results.
#'
#'
#' @examples
#' \dontrun{
#'   # search summary levels of geocomponent contains "block"
#'   search_sumlev("block")
#'
#'   # search summary levels of code 40
#'   search_sumlev("40")
#' }
#'
#' @export
#' @import data.table
#' @import magrittr


search_sumlev <- function(keyword, view = TRUE){
    dt1 <- dict_summarylevel[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_summarylevel[grepl(tolower(keyword), tolower(summary_level))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(summary_level, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
