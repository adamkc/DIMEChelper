#' Title
#'
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' directory <- "F:/Adam Cummings/DimecV1/Model Files/M17"
#' modM17 <- loadDIMECModel(directory)
#' }


loadDIMECModel <- function(directory){
  classNames <- read.table(file.path(directory, "classes.txt"))[,1]
  positiveClasses <- read.table(file.path(directory, "PositiveClasses.txt"))[,1]
  modelLabel <- basename(directory)
  modelLocation <- list.files(directory,pattern = ".h5",full.names = TRUE)
  modelPtr <- keras::load_model_hdf5(modelLocation)
  if((reticulate::py_is_null_xptr(model))){
    message("Model load may have failed")
  }

  loadedModel <- list(classNames = classNames,
                      modelLabel = modelLabel,
                      modelLocation = modelLocation,
                      positiveClasses = positiveClasses,
                      modelPtr = modelPtr)

  class(loadedModel) <- "DIMEC"
  return(loadedModel)
}




#' Reload a broken python model pointer in a DIMEC object
#'
#' @param DIMECModel
#'
#' @return
#' @export
#'
#' @examples
#'
#'


reloadDIMECModel <- function(DIMECModel){
  DIMECModel$modelPtr <- keras::load_model_hdf5(modelLocation)
  return(DIMECModel)
}
