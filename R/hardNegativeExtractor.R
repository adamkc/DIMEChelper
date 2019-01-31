#' Hard negative extractor
#'
#' Grabs three random positives from a classified tile.  It can potentially grab
#' a true positive chip so "negative" is used loosely..
#'
#' @param tileDir Directory to extract hard negatives from
#' @param modelName Model verion name
#' @param outputDir Directory to export hard negative to.
#'
#' @return Three random positives from a classified tile copied to new dir
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
hardNegativeExtractor <- function(tileDir=dirList[1],
                                  modelName="M7",
                                  outputDir){

  ## Identify files:
  holes <- list.files(file.path(tileDir,modelName,"TrespassHoles"),
                      full.names = TRUE)
  if(length(holes)>3) holes <- sample(holes,3)

  plants <- list.files(file.path(tileDir,modelName,"TrespassPlants"),
                       full.names = TRUE)
  if(length(plants)>4) plants <- sample(plants,3)

  ##Create new dirs.
  dir.create(file.path(outputDir,
                       modelName,"/Hard negative holes"),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(outputDir,
                       modelName,"/Hard negative plants"),
             recursive = TRUE, showWarnings = FALSE)

  ## Export
  suppressMessages({
    file.copy(from = holes,
              file.path(outputDir,modelName,"/Hard negative holes"))
    file.copy(from = plants,
              file.path(outputDir,modelName,"/Hard negative plants"))
  })


}
##eg:
#Make Directories
#newDirs <- paste0(modelName,c("/","/Hard negative holes","/Hard negative plants","/KMLs top","/KMLs full"))
#sapply(paste0("F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Summary Output/",newDirs),dir.create)
#tileDirs <- list.dirs("F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Output",recursive=FALSE)
#sapply(tileDirs,hardNegativeExtractor)
#
# outputDir <- "F:/Adam Cummings/DimecV1/Data/M14 Hard Negatives"
# dirList <- list.dirs("F:/Adam Cummings/GoogleImagery/FirstPurchase/Model Output/ca_dosrios_20170813",recursive=FALSE)
# sapply(dirList,hardNegativeExtractor,modelName="M14",outputDir = outputDir)
