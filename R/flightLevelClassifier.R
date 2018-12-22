#' Flight Level Classifier.
#'
#' @param flightDir Directory of the chips to classify
#' @param modelName Model version to import for classification
#' @param exportResults Logical of whether you want the export the classificaiton results to the "Model Output" directory
#' @param returnPlotData Logical of whether you want to return the plotData classification data.
#' @param classes Vector of class names.
#'
#' @return Returns either a data.frame of classifications, or outputs it to a plotData.csv, and two kml files, or both. Outputs to "getwd()/Model Output".
#' @export
#'
#' @examples
#' \dontrun{
#'
#' modelLabel <- "M14"
#' model <- keras::load_model_hdf5(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, paste0("ModelFile-",modelName,".h5")))
#' classNames <- read.table(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, "classes.txt"))[,1]
#'
#'
#' ###### Dos Rios Flight
#' setwd("F:/Adam Cummings/GoogleImagery/ca_dosrios_20170813")
#' dirList <- fs::dir_ls("Chips",recursive = FALSE) ##160 directories
#' flightLevelClassifier(flightDir = dirList[grep(x = dirList, pattern = "C07_R02-DosRios2016")],
#'                       modelName=modelLabel,
#'                       exportResults = TRUE,
#'                       returnPlotData = FALSE,
#'                       classes=classes)
#'
#' sapply(dirList,flightLevelClassifier,
#'        modelLabel,
#'        exportResults = TRUE,
#'        returnPlotData = FALSE)
#'}
#'


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
    topImageExporter(plotData, "TrespassPlants",flightDir,outputDir,threshold = 0.20)
    topImageExporter(plotData, "TrespassHoles",flightDir,outputDir,threshold = 0.20)

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



# modelLabel <- "M7"
# model <- keras::load_model_hdf5(file.path("F:/Adam Cummings/DimecV1/Model Files",modelLabel, paste0("ModelFile-",modelLabel,".h5")))
# classNames <- read.table(file.path("F:/Adam Cummings/DimecV1/Model Files",modelLabel, "classes.txt"))[,1]
#
#
###### Dos Rios Flight
# setwd("F:/Adam Cummings/GoogleImagery/ca_dosrios_20170813")
# dirList <- fs::dir_ls("Chips",recursive = FALSE) ##160 directories
# flightLevelClassifier(flightDir = dirList[grep(x = dirList, pattern = "C01_R07")],
#                       modelName=modelLabel,
#                       exportResults = TRUE,
#                       returnPlotData = FALSE,
#                       classes=classNames)
#
# sapply(dirList,flightLevelClassifier,
#        modelLabel,
#        exportResults = TRUE,
#        returnPlotData = FALSE)
#
#
# ###### Purchase2
# setwd("G:/GoogleImagery/Purchase2")
# dirList <- fs::dir_ls("Chips/or_goldbeach_20160703_rgb",recursive = FALSE) ##160 directories
# flightLevelClassifier(flightDir = dirList[grep(x = dirList, pattern = "C05_R02")],
#                       modelName=modelLabel,
#                       exportResults = TRUE,
#                       returnPlotData = FALSE,
#                       classes=classNames)
#
# sapply(dirList,flightLevelClassifier,
#        modelLabel,
#        exportResults = TRUE,
#        returnPlotData = FALSE)
