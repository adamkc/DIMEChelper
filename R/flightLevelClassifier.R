#' Flight Level Classifier.
#'
#' @param flightDir Directory of the chips to classify
#' @param modelName Model version to import for classification
#' @param exportResults Logical of whether you want the export the classificaiton results to the "Model Output" directory
#' @param returnPlotData Logical of whether you want to return the plotData classification data.
#' @param classes Vector of class names.
#'
#' @return Returns either a data.frame of classifications, or outputs it to a plotData.csv, and two kml files, or both. Outputs to "getwd()/Model Output".
#' @return DirectoryofResults Optionally exports the results as a set of files in directories, including positive images, kml's of positives, and the full model results
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export


flightLevelClassifier <- function(flightDir=dirList[1],
                                  modelName=modelLabel,
                                  exportResults=TRUE,
                                  returnPlotData=FALSE,
                                  classes=classNames){
    print(flightDir)
  if(length(list.dirs(flightDir))==1) stop(paste0("Make sure there is an Unclassified Dir at ", flightDir, "."))

  filesToClassifyLoc <- fs::dir_ls(flightDir,recursive = TRUE,glob = "*.jpg")
  filesToClassify <- basename(filesToClassifyLoc)
  testSamples <- length(filesToClassify)

  flightName <- basename(dirname(flightDir))
  tileName <- basename(flightDir)
  outputDir <- file.path("Model Output",flightName,tileName,modelName)
  if(dir.exists(outputDir) & exportResults) return(paste0(flightName, " is already analyzed with ", modelName, "."))


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

  plotData <- setNames(data.frame(preds),classes)

  plotData$Image <- filesToClassify
  plotData$Model_Prediction = classes[apply(plotData[,1:length(classes)],1,which.max)]
  plotData$TrespassTotal <- plotData$TrespassHoles + plotData$TrespassPlants

  ##OUTPUT
  if(exportResults){

    latLong <- filenameExtractor(as.character(plotData$Image))

    plotData <- cbind(plotData,latLong[,c(2,3)])



    dir.create(outputDir, recursive=TRUE)
    write.csv(plotData,file = paste0(outputDir,"/plotData.csv"))
    topImageExporter(plotData,
                     "TrespassPlants",
                     flightName=flightName,
                     tileName = tileName,
                     threshold = 0.20)
    topImageExporter(plotData,
                     "TrespassHoles",
                     flightName=flightName,
                     tileName = tileName,
                     threshold = 0.20)

    ###KML;

    kmlSubset <-  subset(plotData,TrespassHoles >0.20 | TrespassPlants >0.20)
    kmlSubset2 <- subset(plotData, Model_Prediction == "TrespassPlants" | Model_Prediction == "TrespassHoles")

    kmlMaker(plotData,
             filename = paste0(flightName,"-FULL-",modelName),
             exportDir = outputDir,
             layerName = paste0("TotalExport--",modelName))

    if(nrow(kmlSubset) > 0){
      kmlMaker(kmlSubset,
               filename = paste0(flightName,"-",modelName),
               exportDir = outputDir,
               layerName = paste0("HighProbPoints--",modelName))
    }

    if(nrow(kmlSubset2) > 0){
      kmlMaker(kmlSubset2,
               filename = paste0(flightName,"-TrespassPredicted-",modelName),
               exportDir = outputDir,
               layerName = paste0("TrespassPredicted--",modelName))
    }


  }

  if(returnPlotData) return(plotData)
}

