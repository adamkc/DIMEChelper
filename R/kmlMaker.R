library(rgdal)
library(plotKML)
#library(maptools)


#' KML exporter
#'
#' This function exports a kml into the designated directory.  It converts a data.frame of model predictions and lat-long values into a geospatial file
#' that can be loaded into Google Earth.
#'
#' @param kmlExportData Requires columns named Latitude and Longitude in the epsg:4326 projection.  all other columns added to popup bubble of kml points.
#' @param filename Name of the new kml file that is generated
#' @param exportDir Directory to export the kml file
#' @param layerName layer name within the kml file
#'
#' @return
#' @export
#'
#' @examples


kmlMaker <- function(kmlExportData=plotData,
                     filename="exportedkml",
                     exportDir=getwd(),
                     layerName="ModelOutput"){
  kmlExportData[, !names(kmlExportData) %in% c("Longitude","Latitude")] %<>% dplyr::mutate_if(is.numeric,round,3)
  sp::coordinates(kmlExportData) <- c("Longitude","Latitude")
  sp::proj4string(kmlExportData)<- sp::CRS("+init=epsg:4326")


  fileNumber <- 1
  while(file.exists(paste0(exportDir,"/",filename,".kml"))){
    filename <- paste0(filename,"_",fileNumber)

  }

  suppressWarnings(
    rgdal::writeOGR(kmlExportData,
                    dsn=paste0(exportDir,"/",filename,".kml"),
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
