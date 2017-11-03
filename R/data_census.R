# lookup_census_2010 ===========================================================
#' Lookup data files and table contents of Census 2010
#'
#' @description This dataset includes all data fields of data files in census 2010
#' summary file 1 (with urban/rural update). Fucntion \code{\link{search_tablecontents}}
#' searches content in this dataset.
#'
#'
#' @docType data
#'
#'
#' @format A data.table with 9199 rows and 6 variables:
#' \describe{
#'   \item{file_segment}{sequence number of segment data files, from 1 to 48}
#'   \item{table_content}{description of columns in a census table}
#'   \item{reference}{reference of table content, such as "PCT0240019"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of table, which has many table_content}
#'   \item{universe}{the universe of the census data}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 6.
#'
#'

"lookup_census_2010"


# dict_census_table ============================================================
#' Complete list of census tables
#'
#' @description This dataset contains all census tables in census 2010 summary
#' file 1 (with urban/rural update). Function \code{\link{search_table}} searches
#' this datasets for census tables.
#'
#'
#' @docType data
#'
#'
#' @format A data.table with 333 rows and 4 variables:
#' \describe{
#'   \item{table_ref}{reference code such as "H0010", "PCT022G"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of the table}
#'   \item{universe}{universe of the data}
#' }

#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 5.
#'
#'
"dict_census_table"


# dict_census_geocomponent =====================================================
#' List of geographic components and codes
#'
#' @description This dataset contains the geographic components and codes used in
#' Census 2010 summary file 1 (with urban/rural update). Search geographic components
#' with function \code{\link{search_geocomponents}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 96 rows and 4 variables:
#' \describe{
#'   \item{code}{code for the geocomponent, such as "01" and "M3"}
#'   \item{geo_component}{description of the geographic component}
#'   \item{in_state_file}{wheather the geocomponent available in state files}
#'   \item{in_US_file}{wheather the geocomponent available in national files}
#' }
#'
#' @keywords datasets
#'
#' @source  2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 6-15
#'
#'
"dict_census_geocomponent"


# dict_census_geoheader ========================================================
#' List of geographic headers
#'
#' @description  This dataset has the complete list of geographic header
#' references and their discription used in Census 2010 summary file 1 (with
#' urban/rural update). Search the dataset with function \code{\link{search_geoheaders}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 101 rows and 4 variables
#' \describe{
#'   \item{reference}{reference of the geoheader record}
#'   \item{field}{description of the geoheader record field}
#'   \item{start}{starting position of the geoheader in the record}
#'   \item{end}{ending position of the geoheader in the record}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 2-8
#'
#'
"dict_census_geoheader"


# dict_census_summarylevel =====================================================
#' Summary levels available in Census 2010
#'
#' @description  This data contains summary levels and codes used in census 2010
#'  summary file 1 (with urban/rural update). Search with function \code{\link{search_summarylevels}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 165 rows and 4 variables
#' \describe{
#'   \item{code}{code of summary level}
#'   \item{summary_level}{description of summary level}
#'   \item{in_state_file}{wheather the summary level available in state files}
#'   \item{in_US_file}{wheather the summary level available in national files}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 4-16 state summary file with urban/rural update
#'
#'
#'

"dict_census_summarylevel"

