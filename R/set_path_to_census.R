#' Set file path to directory storing downloaded census data
#'
#'
#' @param path path to directory holding all downloaded census data, such as
#'     "E:/my_census_data" and "~/my_census_data/".
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
        menu(c("temporary path for this R session",
               "permanent path for this and all future R sessions")),
        "temporary",
        "permanent"
    )

    if (choice == "permanent") {
        # save initial working directory for later recovery
        initial_wd <- getwd()

        # set working directory to home directory
        setwd(Sys.getenv("HOME"))

        if (!file.exists(".Renviron")) {
            file.create(".Renviron")
        } else {
            file.copy(".Renviron", ".Renviron_backup")
            message(paste(
                "Your original .Renviron has been backed up and stored as",
                ".Renviron_backup in your R HOME directory."
            ))
            oldenv = read.table(".Renviron", stringsAsFactors = FALSE)[[1]]
            newenv <- oldenv[!grepl("PATH_TO_CENSUS", oldenv)]
            write.table(newenv, ".Renviron", quote = FALSE,
                        sep = "\n", col.names = FALSE, row.names = FALSE)
        }
        path_variable <- paste0("PATH_TO_CENSUS=", "'", path, "'")
        write(path_variable, ".Renviron", sep = "\n", append = TRUE)
        readRenviron("~/.Renviron")
        message(paste(
            "Your path to census data has been stored in your .Renviron and can",
            "be accessed by Sys.getenv(\"PATH_TO_CENSUS\")"
        ))

        # recover to initial working directory
        setwd(initial_wd)

    } else if (choice == "temporary"){
        Sys.setenv(PATH_TO_CENSUS = path)
    }
}
