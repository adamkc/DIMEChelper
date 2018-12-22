library(raster)
library(rgeos)
library(sp)
library(rgdal)


flightGridKML <- function(rasterDir){
  if(!dir.exists(rasterDir)) stop("Directory doesn't exist")
  rasterList <- list.files(rasterDir,full.names=TRUE,pattern="*.tif")
  x <- raster::raster(rasterList[1])
  e <- raster::extent(x)
  p <- as(e,"SpatialPolygons")

  for(i in 2:length(rasterList)){
    xtemp <- raster(rasterList[i])
    etemp <- extent(xtemp)
    ptemp <- as(etemp, 'SpatialPolygons')
    p <<- bind(p,ptemp)
  }

  boxLabels <- data.frame(TileName = substr(basename(rasterList),1,nchar(basename(rasterList))-4),row.names = 1:length(rasterList))
  p.df <- SpatialPolygonsDataFrame(p,boxLabels)

  proj4string(p.df) <- CRS("+proj=longlat +datum=WGS84")
  writeOGR(p.df,"flightgrid.kml",layer="grid",driver="KML")


  # r <- x > -Inf
  # pp <- rasterToPolygons(r, dissolve=TRUE)
  # plot(x)
  # plot(plwd=5,border="red",add=TRUE)

}
