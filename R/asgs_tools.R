library(magrittr)
library(dplyr)
library(AuCensus2011)
library(sp)
library(foreach)

#' @param level Structure level e.g. GCCSA, SSC, POA etc
#' @shapefiles_dir location of shapefile directories
#' @return SpatialPolygonsDataFrame
#'
asgs_load_shapefile <- function(level, shapefiles_dir=paste0(Sys.getenv('HOME'), "/projectsdata/ABS2011/ESRI-Shapefile/")) {
    # dsn name e.g. "2011_GCCSA_shape", layer name = "GCCSA_2011_AUST"
    dsn   <- paste0(shapefiles_dir, "2011_", level, "_shape/")
    layer <- paste0(level, "_2011_AUST")
    message(paste("Loading DSN:", dsn, "Layer:", layer))

    boundaries <- NULL
    boundaries <- rgdal::readOGR(dsn, layer)

    # change name to region_id so that we can merge with ABS data.
    colnames(boundaries@data)[1] <- 'region_id'

    boundaries
}

#' @param region_name character string of the name to extract
#' @param boundaries SpatialPolygonsDataFrame
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
#' @param name character string of region to extract
#' @param boundaries SpatialPolygonsDataFrame
#' @return data.frame with (longitude, latitude) of region centroid
asgs_get_centroid_by_name <- function(name, boundaries) {
    region   <- asgs_extract_named_region(name, boundaries)
    labpt    <- region@polygons[[1]]@Polygons[[1]]@labpt
    centroid <- data.frame(long=labpt[1], lat=labpt[2])
    centroid
}

#' @param name character string of region to extract
#' @param boundaries SpatialPolygonsDataFrame
#' @return SpatialPoints structure containing (longitude, latitude) of region centroid
asgs_get_spcentroid_by_name <- function(name, boundaries) {
    centroid <- asgs_get_centroid_by_name(name, boundaries)
    sp::SpatialPoints(centroid, proj4string = sp::CRS(sp::proj4string(boundaries)))
}


#' asgs_get_all_spcentroids
#' Calculate the centroids of all the objects in 'boundaries' and return them as a
#' a collection i.e. SpatialPointsDataFrame
#'
#' @param boundaries
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

    gccsa_centroids <- get_all_spcentroids(gccsas)
    system.time({
        res <- gContains(states, gccsa_centroids, byid=TRUE)
    })
    colnames(res) <- states[[2]]
    rownames(res) <- gccsa_centroids[[2]]
    print(res)
}

