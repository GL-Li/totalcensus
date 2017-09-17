#' Read selected table
#'
#' Read a table in a state
#'
#' @param path_to_census path to the directory holding downloaded census data
#' @param state abbrivation of a state, such as "IN" for Indiana
#' @param table_num table number such as "P3" and "PCT11A"
#'
#' @export
#'
#' @examples
#'
#'

read_table <- function(path_to_census, state, table_num){
    # determine file number for the table
    file_num <- search_datafile(table_num, table_only = TRUE) %>%
        .[, file] %>%
        unique() %>%
        str_sub(6, 7) %>%
        as.numeric()

    # determine table columns from the file
    table_col <- search_datafile(table_num, table_only = TRUE) %>%
        .[, reference]

    # read data file and select table columns plus logical record number
    table <- read_datafile(path_to_census, state, file_num) %>%
        .[, c("LOGRECNO", table_col), with = FALSE]

    return(table)
}
