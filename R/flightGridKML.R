#' flightGridKML
#'
#' This function takes a directory with raster files and returns a single kml
#' that contains the extent polygons for all the rasters. Useful to generate a
#' raster grid to see search region in Google Earth.
#'
#' @param rasterDir Directory with multiple raster files saved as *.tif.
#'
#' @return flightgrid.kml kml file with raster extents as polygons.
#'
#' @export
flightGridKML <- function(rasterDir){
  if(!dir.exists(rasterDir)) stop("Directory doesn't exist")
  rasterList <- list.files(rasterDir,full.names=TRUE,pattern="*.tif")
  if(length(rasterList==0)) stop("No rasters (*.tif) found")

  x <- raster::raster(rasterList[1])
  e <- raster::extent(x)
  p <- as(e,"SpatialPolygons")

  for(i in 2:length(rasterList)){
    xtemp <- raster(rasterList[i])
    etemp <- extent(xtemp)
    ptemp <- as(etemp, 'SpatialPolygons')
    p <<- bind(p,ptemp)
  }

  boxLabels <- data.frame(TileName =
                            tools::file_path_sans_ext(basename(rasterList)),
                          row.names = 1:length(rasterList))
  p.df <- SpatialPolygonsDataFrame(p,boxLabels)

  proj4string(p.df) <- CRS("+proj=longlat +datum=WGS84")
  writeOGR(p.df,"flightgrid.kml",layer="grid",driver="KML")
}
