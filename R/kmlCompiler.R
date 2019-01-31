#' KML Compiler
#'
#' @param homeDir Base directory. Should contain "Model Output" folder and
#'  "Model Summary Output" folder
#' @param flightName Name of flight to summarize
#' @param modelName Name of Model file output to summarize
#' @param copyKMLs Logical. Should function collect all kml outputs from flight
#' directory?
#' @param mergeKMLs Logical. Should function merge output kmls into one file for
#'  easier sharing?
#'
#' @return copyKMLs. Creates multiple directories at the given export dir and
#' populates them with all the exported kmls from the given flight directory.
#' This option is useful if the flight has many positives and you'd rather check
#' results one search tile at a time.
#'
#' @return mergeKMLs. Create a single kml for the flight by merging the exported
#' kml's at each tile.  This is useful if there are only a couple hundred
#' positives across the flight and you'd like to view them all at once.
#'
#' @examples
#' \dontrun{
#'
#' kmlCompiler(flightName = "ca_hayfork_20160529_rgb"
#'             modelName = "M17")
#' }
#' @export
kmlCompiler <- function(homeDir = getwd(),
                        flightName,
                        modelName,
                        copyKMLs = TRUE,
                        mergeKMLs = FALSE){
  ## Create Directory:
  exportDir <- file.path(homeDir,"Model Output Summary", flightName, modelName)
  dir.create(exportDir, showWarnings = FALSE)

  ##Copy all kml's exported from flight?:

  if(copyKMLs){
    exportDirs <- file.path(exportDir,
                            c("KMLs Full",
                              "KMLs Threshold",
                              "KMLs Predicted"))
    dir.create(exportDirs, showWarnings = FALSE)
    ## kml list:
    kmls <- list.files(file.path(homeDir,"Model Output",flightName),
                       recursive=TRUE,
                       pattern=paste0(modelName,".kml"),
                       full.names=TRUE)
    kmlsFull <- kmls[grep(pattern = "FULL",x = kmls)]
    kmlsTopThresh <- kmls[-c(grep(pattern = "FULL",x = kmls),
                             grep(pattern = "Predicted",x = kmls))]
    kmlsTopPred <- kmls[grep(pattern = "Predicted",x = kmls)]
    ## Move KMLs to new Dirs
    file.copy(kmlsFull,exportDirs[1])
    file.copy(kmlsTopThresh,exportDirs[2])
    file.copy(kmlsTopPred,exportDirs[3])
  }


  ## Merge files?:
  if(mergeKMLs){
    ## Read Files:
    rawDataFiles <- list.files(file.path(homeDir,"Model Output",flightName),
                          recursive=TRUE,
                          pattern="plotData.csv",
                          full.names=TRUE)
    rawDataFiles <- rawDataFiles[grep(pattern=modelName,x = rawDataFiles)]
    plotDataAll <- pbapply::pblapply(X = rawDataFiles, function(x){
      temp <- read.csv(x)
      temp
    })

    ## Combine and subset files:
    plotDataAll <- do.call(what = rbind,plotDataAll)

    plotDataTop <- plotDataAll[plotDataAll$TrespassTotal > 0.2,]
    plotDataPred <- plotDataPred[plotDataPred$Model_Prediction ==
                                   "TrespassHoles" |
                                   plotDataPred$Model_Prediction ==
                                   "TrespassPlants", ]

    ## Export Kmls:
    kmlMaker(kmlExportData = plotDataTop,
             filename = "MergedThresholdPositives",
             exportDir = exportDir)
    kmlMaker(kmlExportData = plotDataTop,
             filename = "MergedPredictionedPositives",
             exportDir = exportDir)
  }

}
