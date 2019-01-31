#' Tile Level Classifier.
#'
#' Important: Requires a model loaded into the environment called "model".
#'
#' @param homeDir Base Directory.  Needs "Chips" folder.
#' @param flightName Name of flight to classify
#' @param tileName Name of tile to classify
#' @param modelName Model version to use in classification and labeling.
#' @param exportResults Logical.  Option to export results for later analysis
#' @param returnPlotData Logical. Option to return plotData from function.
#' @param classes Vector of class labels.
#'
#' @return Returns either a data.frame of classifications, or outputs it to a
#'  plotData.csv, and two kml files, or both. Outputs to "getwd()/Model Output".
#'   Optionally exports the results as a set of files in directories, including
#'   positive images, kml's of positives, and the full model results
#' @export
tileLevelClassifier <- function(tileName,
                                homeDir = getwd(),
                                flightName,
                                modelName,
                                exportResults=TRUE,
                                returnPlotData=FALSE,
                                classes=classNames){

  ##Problem Check:
  if(dir.exists(outputDir) & exportResults)
    return(print(paste0(file.path(flightName,tileName),
                        " is already analyzed with ", modelName, ".")))
  if(length(list.dirs(flightDir))==1)
    stop(paste0("Make sure there is an Unclassified Dir at ", flightDir, "."))


  outputDir <- file.path(homeDir,"Model Output",flightName,tileName,modelName)
  flightDir <- file.path(homeDir,"Chips",flightName,tileName)
  print(file.path(flightName,tileName))

  filesToClassifyLoc <- fs::dir_ls(flightDir,recursive = TRUE,glob = "*.jpg")
  filesToClassify <- basename(filesToClassifyLoc)
  testSamples <- length(filesToClassify)

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

  plotData <- stats::setNames(data.frame(preds),classes)

  plotData$Image <- filesToClassify
  plotData$Model_Prediction = classes[apply(plotData[,1:length(classes)],
                                            1,which.max)]
  plotData$TrespassTotal <- plotData$TrespassHoles + plotData$TrespassPlants

  ##OUTPUT
  if(exportResults){

    latLong <- filenameExtractor(as.character(plotData$Image))

    plotData <- cbind(plotData,latLong[,c(3,2)])



    dir.create(outputDir, recursive=TRUE)
    write.csv(plotData,file = file.path(outputDir,"plotData.csv"))
    topImageExporter(plotData,
                     "TrespassPlants",
                     flightName=flightName,
                     tileName = tileName,
                     modelName = modelName,
                     threshold = 0.20)
    topImageExporter(plotData,
                     "TrespassHoles",
                     flightName=flightName,
                     tileName = tileName,
                     modelName = modelName,
                     threshold = 0.20)
    ## KML:

    kmlSubset <-  subset(plotData,TrespassHoles >0.20 | TrespassPlants >0.20)
    kmlSubset2 <- subset(plotData, Model_Prediction == "TrespassPlants" |
                           Model_Prediction == "TrespassHoles")

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

#' @examples
#' \dontrun{
#'
#' modelLabel <- "M17"
#' model <- keras::load_model_hdf5(file.path("F:/Adam Cummings/DimecV1/Model Files", modelLabel, paste0("ModelFile-",modelLabel,".h5")))
#' classNames <- read.table(file.path("F:/Adam Cummings/DimecV1/Model Files",modelName, "classes.txt"))[,1]
#'
#'
#' ###### Dos Rios Flight
#' setwd("F:/Adam Cummings/GoogleImagery/FirstPurchase/")
#' dirList <- list.dirs("Chips/ca_dosrios_20170813",recursive = FALSE,full.names=FALSE) ##160 directories
#' tileLevelClassifier(flightName = "ca_dosrios_20170813",
#'                      tileName = "C15_R01-DosRios2016",
#'                       modelName=modelLabel,
#'                       exportResults = TRUE,
#'                       returnPlotData = FALSE,
#'                       classes=classNames)
#'
#' sapply(dirList,tileLevelClassifier,
#'        flightName = "ca_dosrios_20170813",
#'        modelName=modelLabel,
#'        exportResults = TRUE,
#'        returnPlotData = FALSE,
#'        classes=classNames)
#'}