#' Read data of selected table fields
#'
#' These fields can be from different tables, for example,
#' c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
#' The rows of short fields that have no corresponding LOGRECNOs are filled with
#' NAs.
#'
#'
#' @param path_to_census path to the directory holding downloaded census data
#' @param state abbrivation of a state, such as "IN" for Indiana
#' @param table_contents selected references of contents in census tables
#' @param show_progress show progress of reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return A data.table containing "LOGRECNO" and selected table contents
#'
#' @examples
#' \dontrun{
#' path <- your_local_path_to_census_data
#' contents <- c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138")
#' dt <- read_2010tablecontents(path, "RI", contents)
#'
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#'

read_2010tablecontents <- function(path_to_census, state, table_contents, show_progress = TRUE){
    # locate data files for the fields
    file_nums <- sapply(table_contents, function(x) search_datafile(x, view = FALSE)[, file_segment])
    file_content <- data.table(file_num = file_nums, content = table_contents)

    # read data
    lst = list()
    for (num in unique(file_nums)){
        cont <- file_content[file_num == num, content]
        # determine location of the flds in file_num
        all_contents <- dict_datafile[file_segment == num, reference]
        loc <- which(all_contents %in% cont)
        cols <- paste0("V", loc)

        if (num < 10) num <- paste0("0", num)
        file <- paste0(path_to_census, state, "/", tolower(state), "000", num, "2010.ur1")
        # fread assigns column names as "V1", "V2", ... when header = FALSE
        dt <- fread(file, header = FALSE, select = c("V5", cols), showProgress = show_progress) %>%
            set_colnames(c("LOGRECNO", cont)) %>%
            setkey(LOGRECNO)
        assign(paste0("dt_", num), dt)
        # list elements names must be character, not number
        lst[[as.character(num)]] <- get(paste0("dt_", num))
    }

    # merge into a large data.table as return using key LOGRECNO, fill with NA
    # for short files
    Reduce(function(x, y) merge(x, y, all = TRUE), lst)
}
