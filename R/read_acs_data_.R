# Read a ACS 1-year survey estimate of a state =================================

read_acs1year_estimate_margin_ <- function(state, year, file_seg,
                                    est_marg = "e", show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

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



# Read a ACS 5-year survey estimate of a state =================================

read_acs5year_estimate_margin_ <- function(state, year,
                                           file_seg, est_marg = "e",
                                           show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

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
