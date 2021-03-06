#' Single Class Grabber
#'
#' This function searches across an entire flight and pulls classifications that
#' match a given class.  Use this to pull all greenhouses, for example.
#'
#' @param homeDir Base Directory. Should have Model Output and Model Output
#'  Summary
#' @param flightName flight name model output to merge for export
#' @param modelName Particular model version output to target
#' @param class Class name to compile
#' @param filterRate Predictions lower than this value are dropped. Default is
#' no threshold.
#' @param fileLabel Additional name modifier for exported kml file.
#'
#' @return saves a kml file in the "Model Output Summary" for the flight..
#' @export
#'
#'
#' @example
#' \dontrun{
#'
#' singleClassGrabber(flightName = "ca_mtshasta_20170709_rgb")
#' }


singleClassGrabber <- function(homeDir = getwd(),
                               flightName,
                               modelName=modelLabel,
                               class="PrivateGrow",
                               filterRate = 0.0,
                               fileLabel = "Region"){
  flightDir <- file.path(homeDir,"Model Output",flightName)
  outputDir <- file.path(homeDir,"Model Output Summary",flightName,modelName)
  csvList <- list.files(flightDir,recursive=TRUE,pattern="*.csv",full.name=TRUE)
  csvList <- csvList[grep(csvList,pattern = modelName)]
  classList <- pbapply::pblapply(X = csvList,function(x) {
    temp <- read.csv(x)
    temp <- subset(temp, Model_Prediction == class)
    filteredRows <- temp[,class]> filterRate
    temp <- temp[filteredRows,]
    temp
  })
  classList2 <-  do.call(what = rbind,args = classList)
  if(nrow(classList2) == 0)
    return("No points match criteria")
  fileName <- paste0(class,"_",
                    gsub(pattern = "\\.",
                         replacement = "_",filterRate))
  kmlMaker(kmlExportData = classList2,
           fileName = fileName,
           exportDir = outputDir,
           layerName = class)

}
