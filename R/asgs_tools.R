if (FALSE) {
    library(sp)
    library(foreach)
}

asgs.volume.info <- list(
    'GCCSA' = list(volume="1270055001"),
    'SA1'   = list(volume="1270055001"),
    'SA2'   = list(volume="1270055001"),
    'SA3'   = list(volume="1270055001"),
    'SA4'   = list(volume="1270055001"),
    'STE'   = list(volume="1270055001"),
    'IARE'  = list(volume="1270055002"),
    'ILOC'  = list(volume="1270055002"),
    'IREG'  = list(volume="1270055002"),
    'ADD'   = list(volume="1270055003"),
    'CED'   = list(volume="1270055003"),
    'LGA'   = list(volume="1270055003"),
    'NRMR'  = list(volume="1270055003"),
    'POA'   = list(volume="1270055003"),
    'SED'   = list(volume="1270055003"),
    'SSC'   = list(volume="1270055003"),
    'TR'    = list(volume="1270055003"),
    'SOS'   = list(volume="1270055004"),
    'SOSR'  = list(volume="1270055004"),
    'SUA'   = list(volume="1270055004"),
    'UCL'   = list(volume="1270055004"),
    'RA'    = list(volume="1270055005")
)

#' Load an ASGS shapefile
#'
#' Load an ASGS shapefile supplied by the ABS
#'
#' Load an ASGS shapefile
#'
#' @export
#' @param level Structure level e.g. GCCSA, SSC, POA etc
#' @param shapefiles_dir location of shapefile directories
#' @return ASGS shapefile (SpatialPolygonsDataFrame)
#'
asgs_load_shapefile <- function(level, shapefiles_dir=paste0(Sys.getenv('HOME'), "/projectsdata/ABS2011/ASGS/")) {
    level    <- toupper(level)
    stopifnot(level %in% names(asgs.volume.info))
    level_lc <- tolower(level)
    info     <- asgs.volume.info[[level]]
    dsn   <- paste0(shapefiles_dir, info$volume, '_', level_lc, '_2011_aust_shape')
    layer <- paste0(level, "_2011_AUST")
    message(paste("Loading DSN: ", dsn, "/", layer, sep=""))

    boundaries <- NULL
    boundaries <- rgdal::readOGR(dsn, layer)
    boundaries
}


#' Extract a named region from an ASGS shapefile
#'
#' Extract a named region from an ASGS shapefile
#'
#' Extract a named region from an ASGS shapefile
#'
#' @export
#' @param region_name character string of the name to extract
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
asgs_extract_named_region <- function(region_name, boundaries) {
    # For ABS/ASGS files, the named column is always the second one.
    polygon_index <- which(boundaries@data[[2]] == region_name)
    # Extract the polygon geometry
    polygon       <- boundaries@polygons[[polygon_index]]
    # Give it the same projection string as the parent.
    polygon       <- sp::SpatialPolygons(list(polygon), proj4string = sp::CRS(sp::proj4string(boundaries)))
    polygon
}




#-----------------------------------------------------------------------------
# Calculating centroids within polygons
#-----------------------------------------------------------------------------
#' Return the centroid of a named region of a shapefile as a data.frame
#'
#' Return the centroid of a named region of a shapefile as a data.frame
#'
#' Return the centroid of a named region of a shapefile as a data.frame
#'
#' @export
#' @param name character string of region to extract
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @return data.frame with (longitude, latitude) of region centroid
asgs_get_centroid_by_name <- function(name, boundaries) {
    region   <- asgs_extract_named_region(name, boundaries)
    labpt    <- region@polygons[[1]]@Polygons[[1]]@labpt
    centroid <- data.frame(long=labpt[1], lat=labpt[2])
    centroid
}

#' Return the centroid of a named region of a shapefile as SpatialPoints
#'
#' Return the centroid of a named region of a shapefile as SpatialPoints
#'
#' Return the centroid of a named region of a shapefile as SpatialPoints
#'
#' @export
#' @param name character string of region to extract
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @return SpatialPoints structure containing (longitude, latitude) of region centroid
asgs_get_spcentroid_by_name <- function(name, boundaries) {
    centroid <- asgs_get_centroid_by_name(name, boundaries)
    sp::SpatialPoints(centroid, proj4string = sp::CRS(sp::proj4string(boundaries)))
}


#' Calculate a SpatialPointsDataFrame of the centroids of every region in an ASGS shapefile
#'
#' Calculate a SpatialPointsDataFrame of the centroids of every region in an ASGS shapefile
#'
#' Calculate a SpatialPointsDataFrame of the centroids of every region in an ASGS shapefile
#'
#' @export
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @return SpatialPointsDataFrame contains the centroids of all the regions in the given 'boundaries'
asgs_get_all_spcentroids <- function(boundaries) {
    all_centroids <- foreach(name=boundaries[[2]], .combine=rbind) %do% {
        asgs_get_centroid_by_name(name, boundaries)
    }
    spdf <- sp::SpatialPointsDataFrame(all_centroids, data=boundaries@data, proj4string = sp::CRS(sp::proj4string(boundaries)))
    spdf
}


if (FALSE) {

    states    <- asgs_load_shapefile(level='STE')
    suburbs   <- asgs_load_shapefile(level='SSC')
    postcodes <- asgs_load_shapefile(level='POA')
    gccsas    <- asgs_load_shapefile(level='GCCSA')
    sa3       <- asgs_load_shapefile(level='SA3')
    sa1       <- asgs_load_shapefile(level='SA1')


    parent_region <- gccsas
    child_region  <- suburbs

    system.time({
        # suburbs = 43s
        child_centroids <- asgs_get_all_spcentroids(child_region)
    })

    system.time({
        # suburbs in gccsas = 200s
        res <- rgeos::gContains(parent_region, child_region, byid=TRUE, returnDense=TRUE)
    })
    colnames(res) <- parent_region[[2]]
    rownames(res) <- child_region[[2]]
    head(res)

    # There should only be a single truth value per row
    # e.g. a suburb can only be in a single state.
    Nparents <- apply(res, 1, sum)

    if (all(Nparents != 1)) {
        bad <- which(Nparents != 1)
        Nparents[bad]
        res[bad,] %>% head

    }

    apply(res, 1, . %>% which %>% colnames(res)[[.]])


}

