#' Read a data file of a state (internal)
#'
#' @description Read an entire data file in a state and assign correct reference
#' to each column. Search with \code{\link{search_datafile}} to find table contents
#' in a data file.
#'
#' @param path_to_census path to the directory of downloaded census 2010 data,
#' inside which are the subfolders of each state.
#' @param state abbrivation of a state, for example, "IN" for Indiana.
#' @param file_seg the number of the data file, for example, 5 for file 05.
#'
#' @return A data.table of all data in the data file. Its columns are the references
#' of LOGRECNO and tables contents in the file. LOGRECNO is its key.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island data file 02
#' ri_2 <- read_datafile("your_local_path_to_census_data", "RI", 2)
#' }
#'
#' @seealso \code{\link{dict_datafile}}
#'
#' @import data.table
#' @import magrittr
#'

read_2010fileseg_ <- function(path_to_census, state, file_seg){

    # allow lower case in state abbreviation
    state <- toupper(state)

    # convert to right file label and number
    if (file_seg < 10) {
        file <- paste0("0000", file_seg, "2010.ur1")
    } else {
        file <- paste0("000", file_seg, "2010.ur1")
    }
    file_dir <- paste0(path_to_census, "/", state, "/", tolower(state), file)
    dt <- fread(file_dir, header = FALSE)

    # assign columns names
    col_names <- dict_datafile[file_segment == file_seg, reference]
    setnames(dt, col_names)

    setkey(dt, LOGRECNO)

    return(dt)
}
