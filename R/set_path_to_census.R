#' Set file path to directory holding census data
#'
#' @description
#'
#' @param path path to directory holding all downloaded census data
#'
#' @export

# This function is modified from census_api_key() in package tidycensus
set_path_to_census <- function (path, overwrite = FALSE, install = FALSE){
    if (install == TRUE) {
        setwd(Sys.getenv("HOME"))  # set working directory to home directory

        if (!file.exists(".Renviron")) {
            file.create(".Renviron")
        } else {
            file.copy(".Renviron", ".Renviron_backup")
            if (isTRUE(overwrite)) {
                message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
                oldenv = read.table(".Renviron", stringsAsFactors = FALSE)
                newenv <- oldenv[-grep("PATH_TO_CENSUS", oldenv),]
                write.table(newenv, ".Renviron", quote = FALSE,
                            sep = "\n", col.names = FALSE, row.names = FALSE)
            } else {
                tv <- readLines(".Renviron")
                if (isTRUE(any(grepl("PATH_TO_CENSUS", tv)))) {
                    stop("A PATH_TO_CENSUS already exists. You can overwrite it with the argument overwrite=TRUE",
                         call. = FALSE)
                }
            }
        }
        keyconcat <- paste("PATH_TO_CENSUS=", "'", path, "'", sep = "")
        write(keyconcat, ".Renviron", sep = "\n", append = TRUE)
        message("Your census data path has been stored in your .Renviron and can be accessed by Sys.getenv(\"PATH_TO_CENSUS\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`")
        return(key)
    } else {
        message("To install your census data path for use in future sessions, run this function with `install = TRUE`.")
        Sys.setenv(PATH_TO_CENSUS = path)
    }
}
