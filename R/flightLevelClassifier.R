#' Flight Level Classifier.
#'
#' Iterates classification over a flight directory after giving computation
#' time.
#'
#' @param homeDir  Base directory.  Should have "Chips" folder.
#' @param flightName Name of flight to be classified.
#' @param modelName Name of loaded model for labeling.
#' @param classes vector of classes for model
#'
#' @return
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export


flightLevelClassifier <- function(homeDir = getwd(),
                                  flightName,
                                  modelName,
                                  classes){
  message("Assessing flight size:")
  tileList <- list.dirs(file.path("Chips",flightName),
                        recursive=FALSE,full.names = TRUE)
  tileSizes <- pbapply::pbsapply(X = tileList,
                                 FUN = function(x){
                                   length(list.files(x,recursive=TRUE))
                                 })
  stepTime <- 45
  computeTime = sum(tileSizes) * (stepTime/1000) / 3600
  endTime <- Sys.time() + computeTime * 3600
  message(sprintf("There are %s tiles with %s chips which will take %s hrs at
                %sms/step and should finish around %s.",length(tileList),
                  sum(tileSizes,na.rm=TRUE),
                  computeTime,
                  stepTime,
                  endTime))


  sapply(basename(tileList),function(x){
    tileLevelClassifier(tileName   = x,
                        flightName = flightName,
                        modelName  = modelName,
                        classes    = classes)
  })

   return(NULL)
}
  #   print(flightDir)
  # if(length(list.dirs(flightDir))==1)
  #   stop(paste0("Make sure there is an Unclassified Dir at ", flightDir, "."))
  #
  # filesToClassifyLoc <- fs::dir_ls(flightDir,recursive = TRUE,glob = "*.jpg")
  # filesToClassify <- basename(filesToClassifyLoc)
  # testSamples <- length(filesToClassify)
  #
  # flightName <- basename(dirname(flightDir))
  # tileName <- basename(flightDir)
  # outputDir <- file.path("Model Output",flightName,tileName,modelName)
  # if(dir.exists(outputDir) & exportResults)
  #   return(paste0(flightName, " is already analyzed with ", modelName, "."))
  #
  #
  # test_generator <- keras::flow_images_from_directory(
  #   flightDir, #Test folder
  #   target_size = c(299, 299),
  #   color_mode = "rgb",
  #   #class_mode = "categorical",
  #   batch_size = 1,
  #   shuffle = FALSE,
  #   seed = 123)
  #
  # preds <- keras::predict_generator(model,
  #                            test_generator,
  #                            verbose = 1,
  #                            steps = testSamples,
  #                            max_queue_size = 1,
  #                            workers=4)
  #
  # plotData <- setNames(data.frame(preds),classes)
  #
  # plotData$Image <- filesToClassify
  # plotData$Model_Prediction <- classes[apply(plotData[,1:length(classes)],
  #                                           1,which.max)]
  # plotData$TrespassTotal <- plotData$TrespassHoles + plotData$TrespassPlants
  #
  # ##OUTPUT
  # if(exportResults){
  #
  #   latLong <- filenameExtractor(as.character(plotData$Image))
  #
  #   plotData <- cbind(plotData,latLong[,c(2,3)])
  #
  #
  #
  #   dir.create(outputDir, recursive=TRUE)
  #   write.csv(plotData,file = paste0(outputDir,"/plotData.csv"))
  #   topImageExporter(plotData,
  #                    "TrespassPlants",
  #                    flightName=flightName,
  #                    tileName = tileName,
  #                    threshold = 0.20)
  #   topImageExporter(plotData,
  #                    "TrespassHoles",
  #                    flightName=flightName,
  #                    tileName = tileName,
  #                    threshold = 0.20)
  #
  #   ###KML;
  #
  #   kmlSubset <-  subset(plotData,TrespassHoles >0.20 | TrespassPlants >0.20)
  #   kmlSubset2 <- subset(plotData, Model_Prediction == "TrespassPlants" |
  #                          Model_Prediction == "TrespassHoles")
  #
  #   kmlMaker(plotData,
  #            filename = paste0(flightName,"-FULL-",modelName),
  #            exportDir = outputDir,
  #            layerName = paste0("TotalExport--",modelName))
  #
  #   if(nrow(kmlSubset) > 0){
  #     kmlMaker(kmlSubset,
  #              filename = paste0(flightName,"-",modelName),
  #              exportDir = outputDir,
  #              layerName = paste0("HighProbPoints--",modelName))
  #   }
  #
  #   if(nrow(kmlSubset2) > 0){
  #     kmlMaker(kmlSubset2,
  #              filename = paste0(flightName,"-TrespassPredicted-",modelName),
  #              exportDir = outputDir,
  #              layerName = paste0("TrespassPredicted--",modelName))
  #   }
  #
  #
  # }
  #
  # if(returnPlotData) return(plotData)
}

