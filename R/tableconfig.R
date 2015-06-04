#' ABS Australian Census 2011: Some metainfo for BCP table
#'
#' ABS Australian Census 2011 Table Configuration
#'
#' Metadata concerning the description of the BCP data tables for the 2011 Australian Census Data.  Note that this is mostly for internal use in order to orchestrate the parsing of the raw tables into R datasets
#'
#' @source Based on Australian Bureau of Statistics data \url{http://www.abs.gov.au/}
#' @format List of metainformation indexed by table name e.g. 'B01'
#' \describe{
#'  \item{table}{Table Name e.g. 'B01'}
#'  \item{desc}{Short description of table contents}
#'  \item{patterns}{Regular expressions used for splitting column names into variables. Don't look at these unless you want to be shocked.}
#'  \item{stats}{The new variables to be created by splitting up the column names into components}
#' }
"tableconfig"