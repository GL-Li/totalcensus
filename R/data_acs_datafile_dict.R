#' Contents of ACS data files
#'
#' @description This dataset includes all data fields of data files in American
#' Community Survay 1-year and 5-year estimates. . Fucntion \code{\link{search_acs_datafile}}
#' searches content in this dataset.
#'
#'
#' @docType data
#'
#' @usage data("dict_acs_datafile")
#'
#' @format A data.table with 31835 rows and 6 variables:
#' \describe{
#'   \item{file_segment}{sequence number of segment data files, from 1 to 166}
#'   \item{table_content}{description of content in a census table}
#'   \item{reference}{reference of the field, such as "B01001_002"}
#'   \item{table_number}{table number such as "B01001"}
#'   \item{table_name}{description of table, which has many table_content}
#'   \item{universe}{the unit of the census data}
#' }
#'
#' @keywords datasets
#'
#' @source Sequence number / table number
#' \href{https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/user_tools/ACS_1yr_Seq_Table_Number_Lookup.txt}{lookup file}
#'
#'
#'

"dict_acs_datafile"
