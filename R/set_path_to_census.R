#' Set file path to directory holding census data
#'
#'
#' @param path path to directory holding all downloaded census data, such as
#' "E:/my_census_data" and "~/my_census_data/".
#'
#' @export

# This function is modified from census_api_key() in package tidycensus, MIT liscence
set_path_to_census <- function (path){

    # windows does not recognize directory ending with "/", so delete it if path
    # is end with "/"
    path_end <- str_trim(path) %>%
        str_extract(".$")
    if (path_end == "/") {
        path <- str_replace(path, "/$", "")
    }

    # get user permission
    message(paste(
        "Set path to the directory storing downloaded census data.",
        "You can choose to set a temporary path to the census data and",
        "use it for current R session only.",
        "Or you can choose to set a permanent path for all future R sessions",
        "by adding a vairable 'PATH_TO_CENSUS' to your .Renviron file.\n"
    ))

    cat("Your choice:")

    choice <- switch(
        menu(c("temporary path for this R session", "permanent path for this and all future R sessions")),
        "temporary",
        "permanent"
    )

    if (choice == "permanent") {
        # save initial working directory for later recover
        initial_wd <- getwd()

        setwd(Sys.getenv("HOME"))  # set working directory to home directory

        if (!file.exists(".Renviron")) {
            file.create(".Renviron")
        } else {
            file.copy(".Renviron", ".Renviron_backup")
            message("Your original .Renviron has been backed up and stored in your R HOME directory.")
            oldenv = read.table(".Renviron", stringsAsFactors = FALSE)
            newenv <- oldenv[-grep("PATH_TO_CENSUS", oldenv),]
            write.table(newenv, ".Renviron", quote = FALSE,
                        sep = "\n", col.names = FALSE, row.names = FALSE)
        }
        pathconcat <- paste("PATH_TO_CENSUS=", "'", path, "'", sep = "")
        write(pathconcat, ".Renviron", sep = "\n", append = TRUE)
        readRenviron("~/.Renviron")
        message(paste(
            "Your path to census data has been stored in your .Renviron and can be",
            "accessed by Sys.getenv(\"PATH_TO_CENSUS\")"
        ))

        # recover to initial working directory
        setwd(initial_wd)
    } else if (choice == "temporary"){
        Sys.setenv(PATH_TO_CENSUS = path)
    }
}
