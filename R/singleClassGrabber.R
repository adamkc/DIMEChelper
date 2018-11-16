#' Title
#'
#' @param flightDir Directory of model output to merge for export
#' @param modelName Particular model version output to target
#' @param class Class name to compile
#' @param outputDir Target directory for export
#' @param filterRate Threshold value to filter the classifications by.
#' @param fileLabel Additional name modifier for exported kml file.
#'
#' @return
#' @export
#'
#' @examples



singleClassGrabber <- function(flightDir="F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Output",
                               modelName="M7",
                               class="PrivateGrow",
                               outputDir="F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Summary Output/M7",
                               filterRate = 0.8,
                               fileLabel = "Region"){
  csvList <- list.files(flightDir,recursive=TRUE,pattern="*.csv",full.name=TRUE)
  csvList <- csvList[grep(csvList,pattern = modelName)]
  classList <- pbapply::pblapply(X = csvList,function(x) {
    temp <- read.csv(x)
    temp <- temp %>% subset(Model_Prediction == class)
    filteredRows <- temp[,class]> filterRate
    temp <- temp[filteredRows,]
    temp
  })
  classList2 <- classList %>% do.call(what = rbind)
  if(nrow(classList2) == 0) return("No points match criteria")
  fileName = paste0(fileLabel,"_",modelName, "_",class,"_",gsub(pattern = "\\.",replacement = "_",filterRate))
  kmlmaker(kmlExportData = classList2,
           filename = fileName,
           exportDir = outputDir,
           layerName = class)

}

#e.g.:
# singleClassGrabber(class="PrivateGrow")
# singleClassGrabber(flightDir = "F:/Adam Cummings/GoogleImagery/ca_dosrios_20170813/Model Output",
#                    class="TrespassHoles",filterRate = 0.70,modelName = "M14",fileLabel = "DosRios",
#                    outputDir = "F:/Adam Cummings/GoogleImagery")
