#' Read data from selected fields
#'
#' These fields can be from different tables but should have same number of rows,
#' for example, c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138")
#'
#' @export
#'
#' @param path_to_census path to the directory holding downloaded census data
#' @param state abbrivation of a state, such as "IN" for Indiana
#' @param fields selected field references in census tables
#'
#' @examples
#' \dontrun{
#' path <- your_local_path_to_census_data
#' fields <- c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138")
#' dt <- read_fields(path, "RI", fields)
#'
#' }

#'

read_fields <- function(path_to_census, state, fields){
    # locate data files for the fields
    file_nums <- sapply(fields, function(x) search_datafile(x, view = FALSE)[, file])
    file_field <- data.table(file_num = file_nums, field = fields)

    # read data
    lst = list()
    for (num in unique(file_nums)){
        flds <- file_field[file_num == num, field]
        # determine location of the flds in file_num
        all_fields <- dict_datafile[file == num, reference]
        loc <- which(all_fields %in% flds)
        cols <- paste0("V", loc)

        if (num < 10) num <- paste0("0", num)
        file <- paste0(path_to_census, state, "/", tolower(state), "000", num, "2010.ur1")
        # fread assigns column names as "V1", "V2", ... when header = FALSE
        dt <- fread(file, header = FALSE, select = c("V5", cols)) %>%
            set_colnames(c("LOGRECNO", flds)) %>%
            setkey(LOGRECNO)
        assign(paste0("dt_", num), dt)
        # list elements names must be character, not number
        lst[[as.character(num)]] <- get(paste0("dt_", num))
    }

    # merge into a large data.table as return using key LOGRECNO, fill with NA
    # for short files
    Reduce(function(x, y) merge(x, y, all = TRUE), lst)
}
