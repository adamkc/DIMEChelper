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
#'
#' @examples
#' \dontrun{
#'
#' modelLabel <- "M17"
#' modelDir <- file.path("Model Files",
#'                        modelLabel,
#'                        paste0("ModelFile-",modelLabel,".h5"))
#' model <- keras::load_model_hdf5(file.path(modelDir,
#'                                      paste0("ModelFile-",modelLabel,".h5")))
#' classNames <- read.table(file.path(modelDir, "classes.txt"))[,1]
#'
#'
#' ## Dos Rios Flight
#' setwd("F:/Adam Cummings/GoogleImagery/FirstPurchase/")
#' dirList <- list.dirs("Chips/ca_dosrios_20170813",recursive = FALSE,
#' full.names=FALSE) ##160 directories
#' tileLevelClassifier(flightName = "ca_dosrios_20170813",
#'                      tileName = "C15_R01-DosRios2016",
#'                       modelName=modelLabel,
#'                       exportResults = TRUE,
#'                       returnPlotData = FALSE,
#'                       classes=classNames)
#'
#' ##Use flightLevelClassifier to classify many tiles at once:
#'
#' flightLevelClassifier(dirList,
#'        flightName = "ca_dosrios_20170813",
#'        modelName=modelLabel,
#'        exportResults = TRUE,
#'        returnPlotData = FALSE,
#'        classes=classNames)
#'
#' ## This also works:
#' sapply(dirList,tileLevelClassifier,
#'        flightName = "ca_dosrios_20170813",
#'        modelName=modelLabel,
#'        exportResults = TRUE,
#'        returnPlotData = FALSE,
#'        classes=classNames)
#'}
#'
#' @export
tileLevelClassifier <- function(tileName,
                                homeDir = getwd(),
                                chipsName = "Chips",
                                flightName,
                                modelName,
                                exportResults=TRUE,
                                returnPlotData=FALSE,
                                classes=classNames,
                                positiveClasses = c("TrespassPlants",
                                                    "TrespassHoles"),
                                filterThreshold = 0.2){

  outputDir <- file.path(homeDir,"Model Output",flightName,tileName,modelName)
  flightDir <- file.path(homeDir,chipsName,flightName,tileName)
  ##Problem Check:
  if(dir.exists(outputDir) & exportResults)
    return(print(paste0(file.path(flightName,tileName),
                        " is already analyzed with ", modelName, ".")))
  if(length(list.dirs(flightDir))==1)
    stop(paste0("Make sure there is an Unclassified Dir at ", flightDir, "."))



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
  plotData$Model_Prediction <- classes[apply(plotData[,seq_along(classes)],
                                            1,which.max)]
  if(all(positiveClasses %in% classes)){
    if(length(positiveClasses) > 1)
      plotData$PositiveTotal <- apply(plotData[,positiveClasses],1,sum)
    if(length(positiveClasses) == 1)
      plotData$PositiveTotal <- plotData[,positiveClasses]
  } else{
    plotData$PositiveTotal <- NA
  }


  ##OUTPUT
  if(exportResults){

    latLong <- filenameExtractor(as.character(plotData$Image))

    plotData <- cbind(plotData,latLong[,c(3,2)])

    dir.create(outputDir, recursive=TRUE)
    write.csv(plotData,file = file.path(outputDir,"plotData.csv"))

    if(all(positiveClasses %in% classes)){

    }
    sapply(positiveClasses,function(x) {
      topImageExporter(plotData,class = x,
                       flightName=flightName,
                       tileName = tileName,
                       modelName = modelName,
                       threshold = filterThreshold)
    })

    ## KML:
    if(length(positiveClasses)>1){
      kmlSubset <-  plotData[apply(plotData[,positiveClasses] > filterThreshold,
                                   1,any),]
    } else{
      kmlSubset <-  plotData[plotData[,positiveClasses] > filterThreshold,]
    }

    kmlSubset2 <- subset(plotData, Model_Prediction %in% positiveClasses)

    kmlMaker(plotData,
             fileName = paste0(flightName,"-FULL-",modelName),
             exportDir = outputDir,
             layerName = paste0("TotalExport--",modelName))

    if(nrow(kmlSubset) > 0){
      kmlMaker(kmlSubset,
               fileName = paste0(flightName,"-",modelName),
               exportDir = outputDir,
               layerName = paste0("HighProbPoints--",modelName))
    }

    if(nrow(kmlSubset2) > 0){
      kmlMaker(kmlSubset2,
               fileName = paste0(flightName,"-Predicted-",modelName),
               exportDir = outputDir,
               layerName = paste0("Predicted--",modelName))
    }


  }

  if(returnPlotData) return(plotData)
}


