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
read_asgs_shapefile <- function(level, shapefiles_dir=paste0(Sys.getenv('HOME'), "/projectsdata/ABS2011/ASGS/")) {
    level    <- toupper(level)
    stopifnot(level %in% names(asgs.volume.info))
    level_lc <- tolower(level)
    info     <- asgs.volume.info[[level]]
    dsn   <- paste0(shapefiles_dir, info$volume, '_', level_lc, '_2011_aust_shape')
    layer <- paste0(level, "_2011_AUST")
    message(paste("Loading DSN: ", dsn, "/", layer, sep=""))

    boundaries <- NULL
    boundaries <- rgdal::readOGR(dsn, layer)

    convert_to_numeric_if_appropriate <- function(x) {
        if (!is.factor(x)) {
            return(x)
        }
        if (all(!is.na(as.numeric(levels(x))))) {
            tmp <- as.numeric(levels(x))[x]
            maxint <- 2^31 - 1
            if (max(tmp) > maxint) {
                return(tmp)
            } else {
                return(as.integer(tmp))
            }
        }
        return(x)
    }

    for (col in colnames(boundaries@data)) {
        suppressWarnings({
            boundaries@data[[col]] <- convert_to_numeric_if_appropriate(boundaries@data[[col]])
        })
    }

    invisible(boundaries)
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


#' Simplify ASGS data
#'
#' Simplify ASGS data
#'
#' Simplify ASGS data
#'
#' @export
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @param simplification_tolerance higher numbers mean more simplification
#' @param preserve_topology preserve topology
#' @return SpatialPolygonsDataFrame of simplified region
asgs_simplify_boundaries <- function(boundaries, simplification_tolerance = 0.01, preserve_topology=TRUE) {
    # On STE data.
    # Original    => 35.33069 MB
    # tol = 0     => 36.46082 MB
    # tol = 0.001 =>  9.61164 MB
    # tol = 0.01  =>  8.44506 MB
    # tol = 0.1   =>  8.26605 MB
    # tol = 1.0   =>  8.24477 MB
    # tol = 10    =>  7.93640 MB
    # system.time(simplify_shape(boundaries, simplification_tolerance = 10))
    print("Simplifying...")
    # Must preserve topology while simplifying otherwise some polygons get removed
    # which means that re-merging into SpatialPolygonsDataFrame fails
    simpler.polygons   <- rgeos::gSimplify(boundaries, tol = simplification_tolerance, topologyPreserve=preserve_topology)
    simpler.boundaries <- sp::SpatialPolygonsDataFrame(simpler.polygons, boundaries@data)

    cat("      Full boundary:", as.numeric(pryr::object_size(        boundaries)) / 1e6, "MB\n")
    cat("Simplified boundary:", as.numeric(pryr::object_size(simpler.boundaries)) / 1e6, "MB\n")

    invisible(simpler.boundaries)
}

#' Select only the geometry within the given bounding box
#'
#' Select only the geometry within the given bounding box
#'
#' Select only the geometry within the given bounding box
#'
#' @export
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @param min_long min longitude
#' @param min_lat min latitude
#' @param max_long max longitude
#' @param max_lat max latitude
#' @return SpatialPolygonsDataFrame of bounded region
#'
asgs_select_bbox <- function(boundaries, min_long, min_lat, max_long, max_lat) {
    cat("asgs_select_bbox: Before:", as.numeric(pryr::object_size(boundaries)) / 1e6, "MB\n")

    # Create a sp SpatialPolygons object to represent the bounding area we are interested in.
    #     r1           <- cbind(c(limits$min_long, limits$max_long, limits$max_long, limits$min_long, limits$min_long),
    #                           c(limits$max_lat, limits$max_lat, limits$min_lat, limits$min_lat, limits$max_lat))
    r1 <- cbind(c(min_long, max_long, max_long, min_long, min_long),
                c(max_lat , max_lat , min_lat , min_lat , max_lat))
    sr1          <- sp::Polygons(list(sp::Polygon(r1, hole=FALSE)), "r1")
    bbox_polygon <- sp::SpatialPolygons(list(sr1), proj4string = sp::CRS(sp::proj4string(boundaries)))

    # Get the list of IDs of objects which lie within the bounding box
    isection_ids <- rgeos::gIntersects(bbox_polygon, boundaries, byid=TRUE)

    # Keep only the geometry which lies within the bounding box
    boundaries <- boundaries[which(isection_ids),]

    cat("asgs_select_bbox: After :", as.numeric(pryr::object_size(boundaries)) / 1e6, "MB\n")
    boundaries
}

#'
#' Merge the boundaries data and the absdata into a data.frame ready for ggplot
#'
# Merge the boundaries data and the absdata into a data.frame ready for ggplot
#'
#' Merge the boundaries data and the absdata into a data.frame ready for ggplot
#' By doing an inner_join here, we only return the plotting data that is the
#' region at the intersection of boundaries and absdata
#' @export
#' @param boundaries ASGS shapefile (SpatialPolygonsDataFrame)
#' @param absdata data.frame of ABS data in long format
#' @return data.frame ready for ggplot2 plotting
merge_asgs_abs <- function(boundaries, absdata) {
    # Split the boundaries@data into polygon coords and meta data.
    plot.data    <- boundaries@data
    plot.data$id <- as.numeric(row.names(plot.data))

    plot.coords    <- ggplot2::fortify(boundaries)
    plot.coords$id <- as.numeric(plot.coords$id)

    plot.data <- dplyr::inner_join(plot.data, absdata)

    # Create a suitable data.frame for plotting with ggplot with coords + metainfo
    plot.df <- dplyr::inner_join(plot.data, plot.coords)
}