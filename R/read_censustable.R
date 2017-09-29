#' Read selected census table of a state
#'
#' Read a census table in a state.
#'
#' @param path_to_census path to the directory holding downloaded census data
#' @param state abbrivation of a state, such as "IN" for Indiana
#' @param table_num table number such as "P3" and "PCT11A"
#'
#' @return A data.table with of the selected census table.
#'
#' @examples
#' \dontrun{
#' # read table P11 of Rhode Island
#' path <- "your_local_path_to_census_data"
#' tmp <- read_censustable(path, "RI", "P11")
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr

read_2010table <- function(path_to_census, state, table_number){
    # determine file number for the table
    file_num <- search_datafile(table_number, table_only = TRUE, view = FALSE) %>%
        .[, file_segment] %>%
        unique()

    # determine table columns from the file
    table_col <- search_datafile(table_number, table_only = TRUE, view = FALSE) %>%
        .[, reference]

    # determine location of these table columns among all columns
    all_col <- dict_datafile[file_segment == file_num, reference]
    table_loc <- which(all_col %in% table_col)

    # read data file and select table columns plus logical record number
    if (file_num < 10) file_num <- paste0("0", file_num)
    file <- paste0(path_to_census, state, "/", tolower(state), "000", file_num, "2010.ur1")
    tab <- fread(file, header = FALSE, select = c("V5", paste0("V", table_loc))) %>%
        set_colnames(c("LOGRECNO", table_col)) %>%
        setkey(LOGRECNO)

    return(tab)
}
