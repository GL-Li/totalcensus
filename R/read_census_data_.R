# Read census 2010 datafile ====================================================


# read one whole datafile of a state
read_2010fileseg_ <- function(state, file_seg){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # allow lower case in state abbreviation
    state <- toupper(state)

    file <- paste0("000", file_seg, "2010.ur1")
    file_dir <- paste0(path_to_census, "/census2010/", state, "/", tolower(state), file)
    dt <- fread(file_dir, header = FALSE)

    # assign columns names
    col_names <- lookup_census_2010[file_segment == file_seg, reference]
    setnames(dt, col_names)

    setkey(dt, LOGRECNO)

    return(dt)
}

# read one whole table with all table contents
read_2010table_ <- function(state, table_number){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (!tolower(table_number) %in% tolower(dict_census_table$table_number)){
        stop("Please provide a correct table number.")
    }

    # also accept lowcase input
    state <- toupper(state)
    table_num <- toupper(table_number)

    # determine file number for the table
    file_seg <- lookup_census_2010[table_number == table_num] %>%
        .[, file_segment] %>%
        unique()

    # determine table columns from the file
    table_col <- lookup_census_2010[table_number == table_num] %>%
        .[, reference]

    # determine location of these table columns among all columns
    all_col <- lookup_census_2010[file_segment == file_seg, reference]
    table_loc <- which(all_col %in% table_col)

    # read data file and select table columns plus logical record number
    file <- paste0(path_to_census, "/census2010/", state, "/", tolower(state), "000", file_seg,
                   "2010.ur1")
    tab <- fread(file, header = FALSE, select = c("V5", paste0("V", table_loc))) %>%
        set_colnames(c("LOGRECNO", table_col)) %>%
        setkey(LOGRECNO)

    return(tab)
}


# read selected table content of a state
# These contents can be from different tables, for example,
# c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
# The rows of short contents that have no corresponding LOGRECNOs are filled with
# NAs.
read_decennial_tablecontents_ <- function(year,
                                          state,
                                   table_contents = NULL,
                                   show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup_census_2010$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    # also accept lowercase input
    state <- toupper(state)
    table_contents <- toupper(table_contents)

    # locate data files for the content
    if (year == 2010) {
        lookup_census <- lookup_census_2010
        file_content <- lookup_census[reference %in% table_contents,
                                           .(file_seg = file_segment,
                                             content = reference)]
    }


    # read data
    lst = list()
    for (num in unique(file_content$file_seg)){
        cont <- file_content[file_seg == num, content]
        # determine location of the flds in file_seg
        all_contents <- lookup_census[file_segment == num, reference]
        loc <- which(all_contents %in% cont)
        cols <- paste0("V", loc)

        file <- paste0(path_to_census, "/census", year, "/", state, "/", tolower(state),
                       "000", num, year, ".ur1")

        if (show_progress) {
            cat(paste("Reading", state, "file", num, "\n"))
        }

        # fread assigns column names as "V1", "V2", ... when header = FALSE
        dt <- fread(file, header = FALSE, select = c("V5", cols), showProgress = show_progress) %>%
            set_colnames(c("LOGRECNO", cont))
        assign(paste0("dt_", num), dt)
        # list elements names must be character, not number
        lst[[as.character(num)]] <- get(paste0("dt_", num))
    }

    # merge into a large data.table as return using key LOGRECNO, fill with NA
    # for short files
    Reduce(function(x, y) merge(x, y, all = TRUE), lst) %>%
        setkey(LOGRECNO)
}
