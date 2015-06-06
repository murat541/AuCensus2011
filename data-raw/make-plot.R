
library(knitr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal) # port install gdal, R CMD INSTALL rgdal_0.9-1.tar.gz
library(pryr)
library(rgeos) # gSimplify  - port install geos; R CMD INSTALL rgeos_0.3-8.tar.gz

library(AuCensus2011)


#-----------------------------------------------------------------------------
# The extents of the captial cities was chosen arbitrarily
#-----------------------------------------------------------------------------
#                 TOP             LEFT          BOTTOM        RIGHT
australia <- list(latmax=-10    , lonmin=112  , latmin=-45  , lonmax=155)
brisbane  <- list(latmax=-27.4  , lonmin=152.9, latmin=-27.7, lonmax=153.2)
sydney    <- list(latmax=-33.75 , lonmin=151.1, latmin=-34.0, lonmax=151.3)
melbourne <- list(latmax=-37.7  , lonmin=144.7, latmin=-37.9, lonmax=145.1)
adelaide  <- list(latmax=-34.76 , lonmin=138.45,latmin=-35.0, lonmax=138.75)
perth     <- list(latmax=-31.7  , lonmin=115.7 ,latmin=-32.5, lonmax=116.13)
canberra  <- list(latmax=-35.19 , lonmin=149.0 ,latmin=-35.34,lonmax=149.2)
hobart    <- list(latmax=-42.82 , lonmin=147.26,latmin=-42.9 ,lonmax=147.38)
darwin    <- list(latmax=-12.32 , lonmin=130.80,latmin=-12.48,lonmax=130.91)

#-----------------------------------------------------------------------------
# Load data
#-----------------------------------------------------------------------------
abs_dir        <- paste0(Sys.getenv('HOME'), "/projectsdata/ABS2011/")
shapefiles_dir <- paste0(abs_dir, "ESRI-Shapefile/")

this_level <- 'SSC'

b46 <- AuCensus2011::read_abs("BCP", "B46", this_level, long=TRUE) %>% inner_join(AuCensus2011::split.variables[['B46']]) %>%
    filter(gender == 'Persons') %>%
    select(region_id, count, method_count, method)

#-----------------------------------------------------------------------------
# Summarise data and add metainfo
#-----------------------------------------------------------------------------
cycled <- b46 %>% filter(method=='Bicycle') %>% select(region_id, Bicycle=count)
total  <- b46 %>% filter(method_count %in% c('Total', 'Did_not_go_to_work', 'Worked_at_home')) %>%
    spread(method_count, count)

cycled %<>% inner_join(total) %>% mutate(cycled = 100 * Bicycle / (Total - Did_not_go_to_work - Worked_at_home))

cycled %<>% inner_join(geog.desc) %>% select(-area, -level)



#-----------------------------------------------------------------------------
# Load the shapefile
#-----------------------------------------------------------------------------
load_ABS_shapefile <- function(level, shapefiles_dir=paste0(Sys.getenv('HOME'), "/projectsdata/ABS2011/ESRI-Shapefile/")) {
    dsn   <- paste0(shapefiles_dir, "2011_", level, "_shape/")
    layer <- paste0(level, "_2011_AUST")
    message(paste("Loading DSN:", dsn, "Layer:", layer))

    boundaries <- NULL
    boundaries <- rgdal::readOGR(dsn, layer)

    # change name to region_id so that we can merge with ABS data.
    colnames(boundaries@data)[1] <- 'region_id'

    boundaries
}

boundaries <- load_ABS_shapefile(level=this_level)
orig.boundaries <- boundaries


#-----------------------------------------------------------------------------
# Simplify Polygons
#-----------------------------------------------------------------------------
simplify_polygons <- function(boundaries, simplification_tolerance = 0.01, preserve_topology=TRUE) {
    print("Simplifying...")
    # Must preserve topology while simplifying otherwise some polygons get removed
    # which means that re-merging into SpatialPolygonsDataFrame fails
    simpler.polygons   <- rgeos::gSimplify(boundaries, tol = simplification_tolerance, topologyPreserve=preserve_topology)
    simpler.boundaries <- sp::SpatialPolygonsDataFrame(simpler.polygons, boundaries@data)

    cat("      Full boundary:", as.numeric(pryr::object_size(        boundaries)) / 1e6, "MB\n")
    cat("Simplified boundary:", as.numeric(pryr::object_size(simpler.boundaries)) / 1e6, "MB\n")

    invisible(simpler.boundaries)
}


boundaries <- simplify_polygons(boundaries, simplification_tolerance = 0.01)


# Now trim the boundaries to the area of interest
# Create a sp SpatialPolygons object to represent the bounding area we are interested in.
r1           <- cbind(c(limits$lonmin, limits$lonmax, limits$lonmax, limits$lonmin, limits$lonmin),
                      c(limits$latmax, limits$latmax, limits$latmin, limits$latmin, limits$latmax))
sr1          <- Polygons(list(Polygon(r1, hole=FALSE)), "r1")
bbox_polygon <- SpatialPolygons(list(sr1), proj4string = CRS(proj4string(boundaries)))

# Get the list of IDs of objects which lie within the bounding box
isection_ids <- gIntersects(bbox_polygon, boundaries, byid=TRUE)

# Keep only the geometry which lies within the bounding box
boundaries <- boundaries[which(isection_ids),]




#-----------------------------------------------------------------------------
# experimental featuer to remove plot regions that are tiny
#-----------------------------------------------------------------------------
remove_small_polygons <- function(boundaries) {
    biguns <- plot.coords %>% group_by(id, group) %>% tally %>% filter(n > 100)
    biguns <- rbind(biguns, data.frame(id=8, group=8.1, n=NA))

    plot.coords %>% filter(group %in% biguns$group)
}



#-----------------------------------------------------------------------------
# Merge the boundaries data and the absdata into a data.frame ready for ggplot
#-----------------------------------------------------------------------------
create_plotready_data <- function(boundaries, absdata) {
    # Merging on region_id, so make sure it exists
    stopifnot("region_id" %in% colnames(absdata))

    # Split the boundaries@data into polygon coords and meta data.
    plot.data    <- boundaries@data
    plot.data$id <- as.numeric(row.names(plot.data))

    plot.coords    <- ggplot2::fortify(boundaries)
    plot.coords$id <- as.numeric(plot.coords$id)

    plot.data <- dplyr::inner_join(plot.data, absdata)

    # Create a suitable data.frame for plotting with ggplot with coords + metainfo
    plot.df <- dplyr::inner_join(plot.data, plot.coords)

}


#-----------------------------------------------------------------------------
# Basic base::plot
#-----------------------------------------------------------------------------
plot(boundaries)  # can I get a fill colour on this?

#-----------------------------------------------------------------------------
# Basic ggplot
#-----------------------------------------------------------------------------
plot.df <- create_plotready_data(boundaries, cycled)

m <- ggplot(plot.df) +
    geom_polygon(aes(x=long, y=lat, group=group, fill=cycled)) +
    coord_fixed()
print(m)


#-----------------------------------------------------------------------------
# ggmap plotting
#-----------------------------------------------------------------------------
library(ggmap)
region_bbox <- sp::bbox(boundaries)
amap <- ggmap::get_map(location = region_bbox, source = "stamen", maptype = "toner", crop = T)

ggmap(amap) + geom_polygon(data=plot.df, aes(x=long, y=lat, group=group, fill=cycled)) + coord_fixed()


#-----------------------------------------------------------------------------
# 'sp' plotting
#-----------------------------------------------------------------------------
# http://r-video-tutorial.blogspot.ch/2015/05/global-economic-maps.html
boundaries@data <- as.data.frame(cycled %<>% mutate(label = as.factor(label)))
sp::spplot(boundaries, "cycled")

library(RColorBrewer)
sp::spplot(boundaries, "label",
       col.regions = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(8),
       col = "white"  # colour of boundary lines
)



