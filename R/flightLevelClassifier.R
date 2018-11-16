#' Title
#'
#' @param flightDir Directory of the chips to classify
#' @param modelName Model version to import for classification
#' @param exportResults Logical of whether you want the export the classificaiton results to the "Model Output" directory
#' @param returnPlotData Logical of whether you want to return the plotData classification data.
#'
#' @return
#' @export
#'
#' @examples
#'
#'


flightLevelClassifier <- function(flightDir=dirList[1],
                                  modelName=modelName,
                                  exportResults=TRUE,
                                  returnPlotData=FALSE){
  #source("F:/Adam Cummings/DimecV1/RScripts/Functions.R")
  print(flightDir)
  if(length(list.dirs(flightDir))==1) return(print(paste0("Make sure there is an Unclassified Dir at ", flightDir, ".")))

  filesToClassify <- list.files(flightDir,recursive = TRUE,pattern = "*.jpg")
  filesToClassifyLoc <- list.files(flightDir,recursive = TRUE,pattern = "*.jpg",full.names = TRUE)
  testSamples <- length(filesToClassify)

  flightName <- strsplit(flightDir,"/")[[1]][2]
  outputDir <- paste0("Model Output/",flightName,"/",modelName)
  if(dir.exists(outputDir)) return(print(paste0(flightName, " is already analyzed with ", modelName, ".")))


  test_generator <- keras::flow_images_from_directory(
    flightDir, #Test folder
    target_size = c(299, 299),
    color_mode = "rgb",
    #class_mode = "categorical",
    batch_size = 1,
    shuffle = FALSE,
    seed = 123)

  preds <- keras::predict_generator(model,
                             test_generator,
                             verbose = 1,
                             steps = testSamples,
                             max_queue_size = 1,
                             workers=4)


  plotData <- data.frame(preds)
  names(plotData) <- classes

  plotData %<>%
    mutate(Image = filesToClassify,
           #Actual_Class = substr(testfilenames,6,10),
           Model_Prediction = classes[apply(.,1,which.max)])
  plotData$TrespassTotal <- plotData$TrespassHoles + plotData$TrespassPlants
  ##OUTPUT
  if(exportResults){
    latLong <- filenameExtractor(as.character(plotData$Image))
    plotData <- cbind(plotData,latLong[,c(2,3)])



    dir.create(outputDir, recursive=TRUE)
    write.csv(plotData,file = paste0(outputDir,"/plotData.csv"))
    topImageExporter(plotData, "TrespassPlants",flightDir,outputDir,0.20)
    topImageExporter(plotData, "TrespassHoles",flightDir,outputDir,0.20)

    ###KML;

    kmlSubset <- plotData %>% subset(TrespassHoles >0.20 | TrespassPlants >0.20)
    kmlSubset2 <- plotData %>% subset(Model_Prediction == "TrespassPlants" | Model_Prediction == "TrespassHoles")

    kmlmaker(plotData,
             filename = paste0(flightName,"-FULL-",modelName),
             exportDir = outputDir,
             layerName = paste0("TotalExport--",modelName))

    if(nrow(kmlSubset) > 0){
      kmlmaker(kmlSubset,
               filename = paste0(flightName,"-",modelName),
               exportDir = outputDir,
               layerName = paste0("HighProbPoints--",modelName))
    }

    if(nrow(kmlSubset2) > 0){
      kmlmaker(kmlSubset2,
               filename = paste0(flightName,"-TrespassPredicted-",modelName),
               exportDir = outputDir,
               layerName = paste0("TrespassPredicted--",modelName))
    }


  }

  if(returnPlotData) return(plotData)
}


