#' Flight Metric Tally
#'
#' @param homeDir Base Directory. Should have Model Output Directory.
#' @param modelName DIMEC Model version to extract
#' @param fileName Output file name.  Be sure to add .csv to end.
#' @param flightName Name of flight to summarize
#' @param summariseKMLs Logical. If true, calls kmlCompiler to produce summary
#'  kmls
#'
#' @return fileName.csv This file summarises the predictions at each tile for an
#' entire flight. The file contains several metrics of determining a "positive"
#' but they all seem about equivalent just with lower or higher sensitivity.
#'
#' @return kmls optionally the function can export flight-level merged kmls
#' by calling kmlCompiler
#'
#' @examples
#' \dontrun{
#' flightMetricTally(modelName="M17",
#'    flightName = "ca_dosrios_20170813")
#' }
#'
#' @export
flightMetricTally <- function(homeDir= getwd(),
                              modelName="M14",
                              flightName,
                              fileName = NULL,
                              summariseKMLs=FALSE){
  ##Create outputDir:
  outputDir <- file.path(homeDir, "Model Output Summary",
                       flightName, modelName)
  dir.create(outputDir, recursive=TRUE, showWarnings = FALSE)

  if(is.null(fileName)) fileName <- paste0("MetricTally-",modelName,".csv")
  ##CSV List:
  csvList <- fs::dir_ls(path = file.path(homeDir,"Model Output",flightName),
                        recursive = TRUE, glob = "*.csv")
  csvList <- csvList[grep(csvList,pattern = modelName)]

  ##This function reads the plotData csvs and summarises each one using a
  ##number of different thresholds
  dataFrameGenerator <- function(x) {
    temp <- read.csv(x)
    nclasses <- which(names(temp) == "Image")-1
    temp$evidenceRatio <- temp$TrespassTotal / apply(temp[,2:nclasses],1,sum)
    totalChips <- nrow(temp)
    modelPred <- sum((temp$Model_Prediction == "TrespassPlants" |
                        temp$Model_Prediction == "TrespassHoles"))
    threshold0.2 <- sum((temp$TrespassPlants >0.2 | temp$TrespassHoles > 0.2))
    threshold0.4 <- sum((temp$TrespassPlants >0.5 | temp$TrespassHoles > 0.5))
    thresholdEvidence0.5 <- sum((temp$evidenceRatio > 0.5))
    thresholdEvidence0.7 <- sum((temp$evidenceRatio > 0.7))
    ##peel off "plotData.csv" and modelDir and then grab the Tile dir name
    tileName <- basename(dirname(dirname(x)))
    result <- data.frame(Tile=tileName,
                         nChips = totalChips,
                         ModelPredictions = modelPred,
                         lowerThreshold=threshold0.2,
                         upperThreshold=threshold0.4,
                         lowerThresholdEvidence=thresholdEvidence0.5,
                         upperThresholdEvidence=thresholdEvidence0.7)
    return(result)
  }
  tileData <-pbapply::pblapply(X = csvList,dataFrameGenerator)
  tileData2 <- do.call(what = rbind,tileData)
  write.csv(x = tileData2,
            file = file.path(outputDir,fileName),
            row.names = FALSE)
  if(summariseKMLs){
    print("Collecting KMLs:")
    kmlCompiler(homeDir = homeDir, flightName=flightName, modelName=modelName,
                copyKMLs = FALSE, mergeKMLs = TRUE)
  }
}


