library(rgdal)
library(plotKML)
#library(maptools)


#' KML exporter
#'
#' @param kmlExportData Requires columns named Latitude and Longitude in the epsg:4326 projection.  all other columns added to popup bubble of kml points.
#' @param filename
#' @param exportDir
#' @param layerName
#'
#' @return
#' @export kmlfile This function exports a kml into the designated directory
#'
#' @examples
kmlMaker <- function(kmlExportData=plotData,
                     filename="exportedkml",
                     exportDir=getwd(),
                     layerName="ModelOutput"){
  kmlExportData[, !names(kmlExportData) %in% c("Longitude","Latitude")] %<>% mutate_if(is.numeric,round,3)
  coordinates(kmlExportData) <- c("Longitude","Latitude")
  proj4string(kmlExportData)<- CRS("+init=epsg:4326")


  fileNumber <- 1
  while(file.exists(paste0(exportDir,"/",filename,".kml"))){
    filename <- paste0(filename,"_",fileNumber)

  }

  suppressWarnings(
    writeOGR(kmlExportData, dsn=paste0(exportDir,"/",filename,".kml"),
             layer= layerName,
             driver="KML"))


  ##Old Method:
  #proj4string(kmlExportData)<- CRS("+init=epsg:26910") ##Necessary to convert from UTM
  #p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  #kmlExportData<- spTransform(kmlExportData, CRS= p4s)
  # kml_open(filename)
  # kml_layer(obj=kmlExportData,
  #           subfolder.name = "Model Detections",
  #
  #           colour=TrespassTotal,
  #           colour_scale = brewer.pal(3,"PuRd"),
  #           shape="http://maps.google.com/mapfiles/kml/shapes/donut.png",
  #           labels = "",
  #           size=1,
  #           balloon = TRUE
  #           )
  # kml_close(filename)
  #
  #
  # plotKML::kml(kmlExportData,
  #              folder.name = "Model Detections",
  #              colour=TrespassTotal,
  #              colour_scale = brewer.pal(3,"PuRd"),
  #              shape="http://maps.google.com/mapfiles/kml/shapes/donut.png",
  #              html.table = paste("")
  #              #alpha=0.8,
  #              #labels="",
  #              #size=1.2,
  #              #balloon=TRUE
  #              )
  #


}
