#' ABS Australian Census 2011: Basic splitting of column names into variables for each of the BCP tables
#'
#' ABS Australian Census 2011: Basic splitting of column names into variables for each of the BCP tables
#'
#' A basic splitting of the column names for each table into a more useful set of variables.  e.g. "Female_20-24_12" might become: gender='Female', age='20-24', cars_owned=12.
#'
#' See the vignettes for more examples on usage.
#'
#' @source Based on Australian Bureau of Statistics data \url{http://www.abs.gov.au/}
#' @format List of data.frames indexed by table name e.g. 'B01'
#' \describe{
#'  \item{colname}{The original (long) column name from the ABS CSV file}
#'  \item{...}{1 or more columns containing the parsed/split variables from this column name}
#' }
"split.variables"