#' Top Image Export
#'
#' @param plotData data from which to extract the top images
#' @param class Target class
#' @param threshold threshold for subsetting the results
#' @param homeDir Base Directory.  Should have Chips folder
#' @param flightName Target flight
#' @param tileName Target Tile
#' @param exportDir Folder to export the top images to.
#' @param modelName Name of model file to be used.
#'
#' @return Directory.  Creates a directory and populates with images that meet
#' the prediction threshold.
#'
#' @export
topImageExporter <- function(plotData,
                             class = "TrespassPlants",
                             homeDir = getwd(),
                             flightName,
                             tileName,
                             modelName,
                             exportDir = "Model Output",
                             threshold = 0.90){
  #TODO:
  ## Subset based on threshold
  temp <- plotData[plotData[,class] > threshold,]
  topImages <- temp$Image

if(length(topImages > 0)){
  dir.create(file.path(homeDir,exportDir,flightName,tileName,modelName,class),
             recursive = TRUE,showWarnings = FALSE)
  file.copy(file.path(homeDir,"Chips",flightName,
                      tileName,"Unclassified",topImages),
            file.path(exportDir,flightName,tileName,modelName,class))
} else{
  return(paste0("No ", class, " predictions meet threshold of ",threshold, "."))
}

  #file.rename(from = paste0(exportdir,"/",class,"/",topImages),
  #            to = paste0(exportdir,class,"/",topnewname,".jpg") )

}

