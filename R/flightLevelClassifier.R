#' Flight Level Classifier
#'
#' @param flightDir Directory of the chips to classify
#' @param modelName Model version to import for classification
#' @param exportResults Logical of whether you want the export the classificaiton results to the "Model Output" directory
#' @param returnPlotData Logical of whether you want to return the plotData classification data.
#' @param classes Vector of class names.
#'
#' @return Returns either a data.frame of classifications, or outputs it to a plotData.csv file, or both.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' modelName <- "M14"
#' model <- keras::load_model_hdf5(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, paste0("ModelFile-",modelName,".h5")))
#' classes <- read.table(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, "classes.txt"))[,1]
#'
#'
#' ###### Dos Rios Flight
#' setwd("F:/Adam Cummings/GoogleImagery/ca_dosrios_20170813")
#' dirList <- fs::dir_ls("Chips",recursive = FALSE) ##160 directories
#' flightLevelClassifier(flightDir = dirList[grep(x = dirList, pattern = "C07_R02-DosRios2016")],
#'                       modelName=modelName,
#'                       exportResults = TRUE,
#'                       returnPlotData = FALSE,
#'                       classes=classes)
#'
#' sapply(dirList,flightLevelClassifier,
#'        modelName,
#'        exportResults = TRUE,
#'        returnPlotData = FALSE)
#'}
#'


flightLevelClassifier <- function(flightDir=dirList[1],
                                  modelName=modelName,
                                  exportResults=TRUE,
                                  returnPlotData=FALSE,
                                  classes=classes){
    print(flightDir)
  if(length(list.dirs(flightDir))==1) stop(paste0("Make sure there is an Unclassified Dir at ", flightDir, "."))

  filesToClassifyLoc <- fs::dir_ls(flightDir,recursive = TRUE,glob = "*.jpg")
  filesToClassify <- basename(filesToClassifyLoc)
  testSamples <- length(filesToClassify)

  flightName <- basename(flightDir)
  outputDir <- file.path("Model Output",flightName,modelName)
  if(dir.exists(outputDir) & exportResults) stop(paste0(flightName, " is already analyzed with ", modelName, "."))


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



# modelName <- "M14"
# model <- keras::load_model_hdf5(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, paste0("ModelFile-",modelName,".h5")))
# classes <- read.table(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, "classes.txt"))[,1]
#
#
# ###### Dos Rios Flight
# setwd("F:/Adam Cummings/GoogleImagery/ca_dosrios_20170813")
# dirList <- fs::dir_ls("Chips",recursive = FALSE) ##160 directories
# flightLevelClassifier(flightDir = dirList[grep(x = dirList, pattern = "C01_R01-DosRios2016")],
#                       modelName=modelName,
#                       exportResults = FALSE,
#                       returnPlotData = FALSE,
#                       classes=classes)
#
# sapply(dirList,flightLevelClassifier,
#        modelName,
#        exportResults = TRUE,
#        returnPlotData = FALSE)

