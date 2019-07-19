#' Flight Level Classifier.
#'
#' Iterates classification over a flight directory after giving computation
#' time.
#'
#' @param homeDir  Base directory.  Should have "Chips" folder.
#' @param modelObj A DIMEC model object.
#' @param flightName Name of flight to be classified.
#' @param filterThreshold Adjustable threshold for classifying a positive hit.
#' @param assessFlightSize Spend a couple minutes getting a precise time estimate?
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
                                  modelObj,
                                  flightName,
                                  filterThreshold = 0.2,
                                  assessFlightSize = TRUE){


  tileList <- list.dirs(file.path(homeDir,"Chips",flightName),
                        recursive=FALSE,full.names = TRUE)

  if(assessFlightSize){
    message("Assessing flight size:")

    tileSizes <- pbapply::pbsapply(X = tileList,
                                   FUN = function(x){
                                     length(list.files(x,recursive=TRUE))
                                   })
  } else{
    tileSizes <- 6500
  }

  tileCount <- mean(tileSizes)*length(tileList)

  #Computational speed:
  stepTime <- 55

  computeTime = round(tileCount * (stepTime/1000) / 3600, 2)
  endTime <- Sys.time() + computeTime * 3600
  message(sprintf("There are %s tiles with ~%s chips.",
                  length(tileList),
                  tileCount,na.rm=TRUE))

  message(sprintf("Classification will take %s hrs
                    at %sms/step and should finish around %s.",
                  computeTime,
                  stepTime,
                  endTime))




  sapply(basename(tileList),function(x){
    tileLevelClassifier(tileName   = x,
                        homeDir = homeDir,
                        flightName = flightName,
                        modelObj  = modelObj,
                        filterThreshold = filterThreshold)
  })

   return(NULL)
}

