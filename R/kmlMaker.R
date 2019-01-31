#' KML exporter
#'
#' This function exports a kml into the designated directory.  It converts a
#' data.frame of model predictions and lat-long values into a geospatial file
#' that can be loaded into Google Earth.
#'
#' @param kmlExportData Requires columns named Latitude and Longitude in the
#' epsg:4326 projection.  all other columns added to popup bubble of kml points.
#' @param fileName Name of the new kml file that is generated
#' @param exportDir Directory to export the kml file
#' @param layerName layer name within the kml file
#'
#' @return  fileName.kml  This function exports a kml file.
#'
#' @export
kmlMaker <- function(kmlExportData=plotData,
                     fileName="exportedkml",
                     exportDir=getwd(),
                     layerName="ModelOutput"){
  kmlExportData[, !names(kmlExportData) %in% c("Longitude","Latitude")] <-
    kmlExportData[, !names(kmlExportData) %in% c("Longitude","Latitude")] %>%
    dplyr::mutate_if(is.numeric,round,3)
  sp::coordinates(kmlExportData) <- c("Longitude","Latitude")
  sp::proj4string(kmlExportData)<- sp::CRS("+init=epsg:4326")


  if(file.exists(file.path(exportDir,paste0(fileName,".kml")))){
    fileName <- paste0(fileName,"_",now())
  }

  suppressWarnings(
    rgdal::writeOGR(kmlExportData,
                    dsn=paste0(exportDir,"/",fileName,".kml"),
                    layer= layerName,
                    driver="KML"))


  ##Old Method:
  #proj4string(kmlExportData)<- CRS("+init=epsg:26910") ##Necessary to convert
  #from UTM
  #p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  #kmlExportData<- spTransform(kmlExportData, CRS= p4s)
  # kml_open(fileName)
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
  # kml_close(fileName)
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
