#' Flight Metric Tally
#'
#' @param flightDir Directory of multiple tile outputs
#' @param modelName DIMEC Model version to extract
#' @param outputDir Target location for output
#' @param fileName Output file name.  Be sure to add .csv to end.
#'
#' @return outputs a csv file that contains tallys of positive detections at different thresholds.
#'
#' @export
#'
#' @examples


flightMetricTally <- function(flightDir="Model Output",
                              modelName="M14",
                              outputDir="Model Summary Output",
                              fileName = "MetricTally.csv"){
  csvList <- list.files(flightDir,recursive=TRUE,pattern="*.csv",full.name=TRUE)
  csvList <- csvList[grep(csvList,pattern = modelName)]

  dataFrameGenerator <- function(x) {
    temp <- read.csv(x)
    nclasses <- which(names(temp) == "Image")-1
    temp$evidenceRatio <- temp$TrespassTotal / apply(temp[,2:nclasses],1,sum)
    modelPred <- (temp$Model_Prediction == "TrespassPlants" | temp$Model_Prediction == "TrespassHoles") %>% sum()
    threshold0.2 <- (temp$TrespassPlants >0.2 | temp$TrespassHoles > 0.2) %>% sum()
    threshold0.4 <- (temp$TrespassPlants >0.5 | temp$TrespassHoles > 0.5) %>% sum()
    thresholdEvidence0.5 <- (temp$evidenceRatio > 0.5) %>% sum()
    thresholdEvidence0.7 <- (temp$evidenceRatio > 0.7) %>% sum()
    quadName <- basename(dirname(dirname(x))) ##peel off "plotData.csv" and modelDir and then grab the Tile dir name
    result <- data.frame(Quad=quadName,
                         ModelPredictions = modelPred,
                         lowerThreshold=threshold0.2,
                         upperThreshold=threshold0.4,
                         lowerThresholdEvidence=thresholdEvidence0.5,
                         upperThresholdEvidence=thresholdEvidence0.7)
    return(result)
  }
  quadData <-pbapply::pblapply(X = csvList,dataFrameGenerator)
  quadData2 <- quadData %>% do.call(what = rbind)
  write.csv(x = quadData2,file = file.path(outputDir,modelName,fileName),row.names = FALSE)

}

flightMetricTally(modelName="M14")
