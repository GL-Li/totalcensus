#' Read a census table of a state
#'
#' @description  Read a census table of a state. To find table of interest, search with function
#' \code{\link{search_table}} or browse the dictionary \code{\link{dict_censustable}}.
#'
#' @param path_to_census path to the directory holding downloaded census data,
#'     under which are sub-folders of each state.
#' @param state abbrivation of a state, such as "IN" for Indiana
#' @param table_number table number such as "P3" and "PCT11A"
#'
#' @return A data.table with of the selected census table. LOGRECNO is the key.
#'
#' @examples
#' \dontrun{
#' # read table P11 of Rhode Island
#' ri <- read_2010table("your_local_path_to_census_folder", "RI", "P11")
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr

read_2010table <- function(state, table_number){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (!tolower(table_number) %in% tolower(dict_censustable$table_number)){
        stop("Please provide a correct table number.")
    }

    # also accept lowcase input
    state <- toupper(state)
    table_number <- toupper(table_number)

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
