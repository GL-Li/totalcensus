#' Read data file
#'
#' Read an entire data file in a state and assign correct reference to each column
#'
#' @param path_to_census path to the directory of downloaded census 2010 data
#' @param state abbrivation of a state, for example, "IN" for Indiana
#' @param file_seg the number of the file, for example, 5 for file 05
#'
#' @return A data.table of all data in the data file
#'
#' @examples
#' \dontrun{
#' path <- your_local_path_to_census_data
#' aaa <- read_datafile(path, "RI", 2)
#' }
#'
#' @seealso \code{\link{dict_datafile}}
#'
#' @export
#' @import data.table
#' @import magrittr
#'

read_2010fileseg <- function(path_to_census, state, file_seg){
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
