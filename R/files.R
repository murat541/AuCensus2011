
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
#' create_abs_filename('BCP', 'B46', 'SSA')
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