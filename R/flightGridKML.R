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

  if(!dir.exists(rasterDir))
    stop("Directory doesn't exist")

  rasterList <- list.files(rasterDir,full.names=TRUE,pattern="*.tif")
  if(length(rasterList)==0)
    stop(paste0("No rasters (*.tif) found at ", rasterDir))

  x <- raster::raster(rasterList[1])
  e <- raster::extent(x)
  p <- as(e,"SpatialPolygons")
  crs <- raster::crs(x)

  print(sprintf("There are %s rasters in the folder.", length(rasterList)))

  pbapply::pbsapply(rasterList[2:length(rasterList)],
                    FUN = function(x){
                      xtemp <- raster::raster(x)
                      etemp <- raster::extent(xtemp)
                      ptemp <- as(etemp, 'SpatialPolygons')
                      p <<- raster::bind(p,ptemp)

  })

  boxLabels <- data.frame(TileName =
                            tools::file_path_sans_ext(basename(rasterList)),
                          row.names = seq_along(rasterList))
  p.df <- sp::SpatialPolygonsDataFrame(p,boxLabels)

  sp::proj4string(p.df) <- crs
  p.df.wgs <- sp::spTransform(p.df,CRSobj = sp::CRS("+init=epsg:4326"))
  exportDir <- file.path(dirname(rasterDir),
                         paste0(basename(rasterDir),
                               "_grid.kml"))
  rgdal::writeOGR(p.df.wgs,exportDir,layer="grid",driver="KML")
}
