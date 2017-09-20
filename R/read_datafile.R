#' Read data file
#'
#' Read an entire data file in a state and assign correct reference to each column
#'
#' @param path_to_census path to the directory of downloaded census 2010 data
#' @param state abbrivation of a state, for example, "IN" for Indiana
#' @param file_num the number of the file, for example, 5 for file 05
#'
#' @export
#'
#' @seealso \code{\link{dict_datafile}}
#'
#' @examples
#'
#'

read_datafile <- function(path_to_census, state, file_num){
    # convert to right file label and number
    if (file_num < 10) {
        file_lab <- paste0("file_0", file_num)
        file <- paste0("0000", file_num, "2010.ur1")
    } else {
        file_lab <- paste0("file_", file_num)
        file <- paste0("000", file_num, "2010.ur1")
    }
    file_dir <- paste0(path_to_census, "/", state, "/", tolower(state), file)
    dt <- fread(file_dir, header = FALSE)

    # assign columns names
    col_names <- dict_datafile[file == file_lab, reference]
    setnames(dt, col_names)

    return(dt)
}
