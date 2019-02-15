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
                                  classes,
                                  positiveClasses = c("TrespassPlants",
                                                      "TrespassHoles"),
                                  filterThreshold = 0.2){
  message("Assessing flight size:")
  tileList <- list.dirs(file.path("Chips",flightName),
                        recursive=FALSE,full.names = TRUE)
  tileSizes <- pbapply::pbsapply(X = tileList,
                                 FUN = function(x){
                                   length(list.files(x,recursive=TRUE))
                                 })
  stepTime <- 45
  computeTime = round(sum(tileSizes) * (stepTime/1000) / 3600, 2)
  endTime <- Sys.time() + computeTime * 3600
  message(sprintf("There are %s tiles with %s chips.",
                  length(tileList),
                  sum(tileSizes,na.rm=TRUE)))

  message(sprintf("Classification will take %s hrs
at %sms/step and should finish around %s.",
                  computeTime,
                  stepTime,
                  endTime))


  sapply(basename(tileList),function(x){
    tileLevelClassifier(tileName   = x,
                        flightName = flightName,
                        modelName  = modelName,
                        classes    = classes,
                        positiveClasses = positiveClasses,
                        filterThreshold = filterThreshold)
  })

   return(NULL)
}

