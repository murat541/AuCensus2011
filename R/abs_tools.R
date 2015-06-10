
#' Convert a wide ABS table to a long one, adding in a base set of variables as appropriate
#'
#' Convert a wide ABS table to a long one, adding in a base set of variables as appropriate
#'
#' Convert a wide ABS table to a long one, adding in a base set of variables as appropriate
#'
#' @importFrom dplyr "%>%"
#' @export
#' @param abs_table An abs data.frame
#' @return Returns a data.frame in long format with augmented columns.
#' @examples
#' abs_table <- read_abs('BCP', 'B46', 'SA4')
#' abs_wide_to_long(abs_table)
#'
abs_wide_to_long <- function(abs_table) {
    # Convert from wide to long format.
    abs_table <- abs_table %>% tidyr::gather(colname, count, -1) %>%
        dplyr::mutate(colname = as.factor(as.character(colname)))

    # We don't have access to the table name here, so
    # do an exhaustive earch for possible columns to merge this with
    abs_table_cols <- unique(abs_table$colname)
    for (table in names(split.variables)) {
        this_table      <- split.variables[[table]]
        this_table_cols <- unique(this_table$colname)
        if (length(intersect(abs_table_cols, this_table_cols)) == length(this_table_cols)) {
            abs_table <- abs_table %>% dplyr::inner_join(this_table, by="colname")
            break
        }
    }

    # put the region_id and colname as the first 2 columns, and put the 'count' last.
    abs_table <- abs_table %>% dplyr::select(1, 2, setdiff(everything(), count), count)

    invisible(abs_table)
}