#' Read a ACS 1-year survey estimate of a state (internal)
#'
#' @description Read an entire ACS 1-year survey estimate file in a state and assign correct reference
#' to each column. Search with \code{\link{search_acs_datafile}} to find table contents
#' in a data file.
#'
#' @param path_to_census path to the directory of downloaded census 2010 data,
#' inside which are the subfolders of each state.
#' @param state abbrivation of a state, for example, "IN" for Indiana.
#' @param year year of the survey
#' @param file_seg the number of the data file, for example, "0012.
#' @param est_marg which data to read, "e" for estimate, "m" for margin
#'
#' @return A data.table of all data in the data file. LOGRECNO is its key.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island data file 0004, replace the path with your local folder
#' ri <- read_acs1year_estimate_margin_("~/census_data/", "ri", 2016, "0004", "e")
#' }
#'
#' @seealso \code{\link{dict_acs_datafile}}
#'
#' @export
#' @import data.table
#' @import magrittr
#'

read_acs1year_estimate_margin_ <- function(path_to_census, state, year, file_seg,
                                    est_marg = "e", show_progress = TRUE){
    # get column names from file segment, then add six ommitted ones
    lookup <- get(paste0("lookup_acs1year_", year))
    col_names <- lookup[file_segment == file_seg] %>%
        # get rid of references ending with ".5", which are not in the file
        .[str_extract(reference, "..$") != ".5", reference]
    ommitted <- c("FILEID", "FILETYPE", "STUSAB", "CHARITER", "SEQUENCE", "LOGRECNO")
    col_names <- c(ommitted, col_names)

    file <- paste0(path_to_census, "/", "acs1year/", year, "/", est_marg, year, "1",
                   tolower(state), file_seg, "000.txt")

    estimate <- fread(file, header = FALSE, showProgress = show_progress) %>%
        setnames(names(.), col_names) %>%
        setkey(LOGRECNO)

    return(estimate)
}



#' Read a ACS 5-year survey estimate of a state (internal)
#'
#' @description Read an entire ACS 5-year survey estimate file in a state and assign correct reference
#' to each column. Search with \code{\link{search_acs_datafile}} to find table contents
#' in a data file.
#'
#' @param path_to_census path to the directory of downloaded census 2010 data,
#' inside which are the subfolders of each state.
#' @param state abbrivation of a state, for example, "IN" for Indiana.
#' @param year end year of the 5-year survey
#' @param file_seg the number of the data file, for example, "0012.
#' @param est_marg which data to read, "e" for estimate, "m" for margin
#'
#' @return A data.table of all data in the data file. LOGRECNO is its key.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island data file 02
#' ri <- read_acs5year_estimate_margin_("~/census_data/", "ri", 2015, "group2",
#'                                      "0019", "e")
#' }
#'
#' @seealso \code{\link{dict_acs_datafile}}
#'
#' @export
#' @import data.table
#' @import magrittr
#'

read_acs5year_estimate_margin_ <- function(path_to_census, state, year,
                                           file_seg, est_marg = "e",
                                           show_progress = TRUE){
    # get column names from file segment, then add six ommitted ones
    lookup <- get(paste0("lookup_acs5year_", year))
    col_names <- lookup[file_segment == file_seg] %>%
        # get rid of references ending with ".5", which are not in the file
        .[str_extract(reference, "..$") != ".5", reference]
    ommitted <- c("FILEID", "FILETYPE", "STUSAB", "CHARITER", "SEQUENCE", "LOGRECNO")
    col_names <- c(ommitted, col_names)

    # row bind data in group1 and group2
    file1 <- paste0(path_to_census, "/", "acs5year/", year, "/", "group1/", est_marg, year, "5",
                   tolower(state), file_seg, "000.txt")
    file2 <- paste0(path_to_census, "/", "acs5year/", year, "/", "group2/", est_marg, year, "5",
                    tolower(state), file_seg, "000.txt")

    dt1 <- fread(file1, header = FALSE, showProgress = show_progress) %>%
        setnames(names(.), col_names)
    dt2 <- fread(file2, header = FALSE, showProgress = show_progress) %>%
        setnames(names(.), col_names)

    combined <- rbindlist(list(dt1, dt2)) %>%
        setkey(LOGRECNO)

    return(combined)
}
