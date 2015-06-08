
#=============================================================================
# Code for general file pathnames, manipulation etc
#=============================================================================

#' Path to ABS DataPack CSV file
#'
#' \code{create_abs_filename} creates the path to a given ABS datapack file.
#'
#' MFC TODO: Describe exactly the file layout.
#' For now, root_dir = place where DataPack zip files were uncompressed to.
#'
#' @export
#' @param profile The short code for the profile. e.g. 'BCP' for Basic Community Profile.
#' @param table Table number including and suffix. e.g. 'B01', 'B08A'
#' @param level The level of the statistcal area: [AUS, CED, GCCSA, IARE, ILOC, IREG, LGA, POA, RA, SA1, SA2, SA3, SA4, SED, SLA, SOS, SOSR, SSC, STE, SUA, UCL]
#' @return Returns the full path to the CSV file containing the nominated data.
#' @examples
#' create_abs_filename('BCP', 'B46', 'AUS')
#'
create_abs_filename <- function(profile, table, level) {
    root_dir <- file.path(Sys.getenv('HOME'), "projectsdata/ABS2011/DataPacks/")
    if (level == 'AUS') {
        abs_filename <- paste0(root_dir,
                               "2011_", profile, "_ALL_for_AUST_long-header/",
                               "2011 Census ", profile, " All Geographies for AUST/",
                               "/AUST/2011Census_", table, "_AUST_long.csv")
    } else {
        abs_filename <- paste0(root_dir,
                               "2011_", profile, "_ALL_for_AUST_long-header/",
                               "2011 Census ", profile, " All Geographies for AUST/",
                               level, "/AUST/2011Census_", table, "_AUST_", level, "_long.csv")
    }
    abs_filename
}


#' Load an ABS DataPack file
#'
#' Given a profile, table & level, read the relevant ABS data into a data.frame
#'
#' Because of the size of the data, many tables are split over multiple files and numbered
#' BxxA, BxxB, BxxC etc.  Once a DataPack has over 200 columns (excluding the region_id column),
#' further columns are spilled into the next file.
#' \code{read_abs} will automatically read these multiple files and
#' merge the data into one really really wide data.frame
#'
#' @importFrom dplyr "%>%"
#' @export
#' @param profile The short code for the profile. e.g. 'BCP' for Basic Community Profile.
#' @param table Table number excluding suffix. e.g. 'B01', 'B08'
#' @param level The level of the statistcal area: [AUS, CED, GCCSA, IARE, ILOC, IREG, LGA, POA, RA, SA1, SA2, SA3, SA4, SED, SLA, SOS, SOSR, SSC, STE, SUA, UCL]
#' @param long If long==TRUE, then convert to long format, otherwise leave in wide format (default)
#' @return Returns a data.frame of the data.
#' @examples
#' read_abs('BCP', 'B46', 'AUS')
#'
read_abs <- function(profile, table, level, long=TRUE) {
    # Find all the files that match the table name + wildcard
    filename.glob <- create_abs_filename(profile, paste0(table, "*"), level)
    filenames <- Sys.glob(filename.glob)
    message(paste("Reading", length(filenames), "file(s) which make up Table", table))
    # Read all those files
    dfs <- lapply(filenames, readr::read_csv)
    # inner_join them all by region_id
    suppressMessages({
        df <- Reduce(dplyr::inner_join, dfs)
    })

    if (long) {
        # Convert from wide to long format.
        df <- df %>% tidyr::gather(colname, count, -region_id) %>%
            dplyr::mutate(colname = as.factor(as.character(colname)))
    }

    # region_id needs to be 'character' to be able to merge with 'region.descriptions'
    df <- df %>% dplyr::mutate(region_id = as.factor(as.character(region_id)))

    if (level %in% asgs.info$level) {
        # Then we'll recode the region_id column to match the level name in the
        # ASGS Shapefiles so that we can do datamerges without losing information
        code_name <- asgs.info$code_name[asgs.info$level == level]
        colnames(df)[1] <- code_name
    }

    df
}