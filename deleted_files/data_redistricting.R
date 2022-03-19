# lookup_decennial_2010 ===========================================================
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
#'   \item{table_content}{description of columns in a decennial table}
#'   \item{reference}{reference of table content, such as "PCT0240019"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of table, which has many table_content}
#'   \item{universe}{the universe of the decennial data}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' chapter 6.
#'
#'

"lookup_decennial_2010"


# lookup_decennial_2000 ===========================================================
#' Lookup data files and table contents of Census 2000
#'
#' @description This dataset includes all data fields of data files in census 2000
#' summary file 1. Fucntion \code{\link{search_tablecontents}}
#' searches content in this dataset.
#'
#'
#' @docType data
#'
#'
#' @format A data.table with 8321 rows and 6 variables:
#' \describe{
#'   \item{file_segment}{sequence number of segment data files, from 1 to 48}
#'   \item{table_content}{description of columns in a decennial table}
#'   \item{reference}{reference of table content, such as "PCT0240019"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of table, which has many table_content}
#'   \item{universe}{the universe of the decennial data}
#' }
#'
#' @keywords datasets
#'
#' @source 2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' chapter 7.
#'
#'

"lookup_decennial_2000"



# dict_decennial_table 2010 ============================================================
#' Complete list of 2010 census tables
#'
#' @description This dataset contains all census tables in census 2010 summary
#' file 1 (with urban/rural update).
#'
#'
#' @docType data
#'
#'
#' @format A data.table with 333 rows and 4 variables:
#' \describe{
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of the table}
#'   \item{universe}{universe of the data}
#'   \item{table_ref}{reference code such as "H0010", "PCT022G"}
#' }

#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' chapter 5.
#'
#'
"dict_decennial_table_2010"


# dict_decennial_table 2000 ============================================================
#' Complete list of 2000 census tables
#'
#' @description This dataset contains all census tables in census 2000 summary
#' file 1.
#'
#'
#' @docType data
#'
#'
#' @format A data.table with 286 rows and 4 variables:
#' \describe{
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of the table}
#'   \item{universe}{universe of the data}
#'   \item{table_ref}{reference code such as "H0010", "PCT022G"}
#' }

#'
#' @keywords datasets
#'
#' @source 2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' all across chapter 5.
#'
#'
"dict_decennial_table_2000"


# dict_decennial_geocomponent_2010 =====================================================
#' List of geographic components and codes in census 2010
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
#'   \item{state_file}{wheather the geocomponent available in state files}
#'   \item{US_file}{wheather the geocomponent available in national files}
#' }
#'
#' @keywords datasets
#'
#' @source  2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 6-15
#'
#'
"dict_decennial_geocomponent_2010"


# dict_decennial_geocomponent_2000 =====================================================
#' List of geographic components and codes in census 2000
#'
#' @description This dataset contains the geographic components and codes used in
#' Census 2000 summary file 1. Search geographic components
#' with function \code{\link{search_geocomponents}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 98 rows and 4 variables:
#' \describe{
#'   \item{code}{code for the geocomponent, such as "01" and "M3"}
#'   \item{geo_component}{description of the geographic component}
#'   \item{state_file}{wheather the geocomponent available in state files}
#'   \item{US_file}{wheather the geocomponent available in national files}
#' }
#'
#' @keywords datasets
#'
#' @source  2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' page 7-15
#'
#'
"dict_decennial_geocomponent_2000"



# dict_redistricting_geoheader_2010 ========================================================
#' List of geographic headers in census 2020 redistricting data
#'
#' @description  This dataset has the complete list of geographic header
#' references and their discription used in Census 2020 redistricting summary
#' file .
#'
#' @docType data
#'
#'
#' @format A data.table with 97 rows and 4 variables
#' \describe{
#'   \item{reference}{reference of the geoheader record}
#'   \item{field}{description of the geoheader record field}
#'   \item{start}{starting position of the geoheader in the record}
#'   \item{end}{ending position of the geoheader in the record}
#' }
#'
#' @keywords datasets
#'
#'
#'
"dict_redistricting_geoheader_2020"


# dict_decennial_geoheader 2000 ================================================
#' List of geographic headers in census 2000
#'
#' @description  This dataset has the complete list of geographic header
#' references and their discription used in Census 2000 summary file 1.
#' Search the dataset with function \code{\link{search_geoheaders}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 83 rows and 4 variables
#' \describe{
#'   \item{reference}{reference of the geoheader record}
#'   \item{field}{description of the geoheader record field}
#'   \item{start}{starting position of the geoheader in the record}
#'   \item{end}{ending position of the geoheader in the record}
#' }
#'
#' @keywords datasets
#'
#' @source 2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' page 2-7
#'
#'
"dict_decennial_geoheader_2000"




# dict_decennial_summarylevel_2010 =====================================================
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

"dict_decennial_summarylevel_2010"



# dict_decennial_summarylevel_2000 =====================================================
#' Summary levels available in Census 2000
#'
#' @description  This data contains summary levels and codes used in census 2000
#'  summary file 1. Search with function \code{\link{search_summarylevels}}.
#'
#' @docType data
#'
#'
#' @format A data.table with 114 rows and 4 variables
#' \describe{
#'   \item{code}{code of summary level}
#'   \item{summary_level}{description of summary level}
#'   \item{in_state_file}{wheather the summary level available in state files}
#'   \item{in_US_file}{wheather the summary level available in national files}
#' }
#'
#' @keywords datasets
#'
#' @source 2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' page 4-1.
#'
#'
#'

"dict_decennial_summarylevel_2000"



# dict_all_geocomponent_2010 =====================================================
#' List of all geographic components and codes 2010 version
#'
#' @description This dataset contains all available geographic components and codes.
#'
#' @docType data
#'
#'
#' @format A data.table with 114 rows and 2 variables:
#' \describe{
#'   \item{code}{code for the geocomponent, such as "01" and "M3"}
#'   \item{geo_component}{description of the geographic component}
#' }
#'
#' @keywords datasets
#'
#' @source  2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 6-15
#'
#'
"dict_all_geocomponent_2010"



# dict_all_geocomponent_2000 =====================================================
#' List of geographic components and codes in 2000 version
#'
#' @description This dataset contains all available geographic components and codes.
#'
#' @docType data
#'
#'
#' @format A data.table with 99 rows and 2 variables:
#' \describe{
#'   \item{code}{code for the geocomponent, such as "01" and "M3"}
#'   \item{geo_component}{description of the geographic component}
#' }
#'
#' @keywords datasets
#'
#' @source  2000 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2000/doc/sf1.pdf}{technical documentation}
#' page 7-15
#'
#'
"dict_all_geocomponent_2000"
