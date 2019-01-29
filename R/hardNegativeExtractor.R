##-------------------------------
##Grabs 3 random positive from each grid and combines into a folder
##
##----------------------------------
#' Title
#'
#' @param gridDir
#' @param modelName
#'
#' @return
#' @export
#'
#' @examples
hardNegativeExtractor <- function(gridDir=dirList[1],
                                  modelName="M7",
                                  outputDir){

  #Identify files:
  holes <- list.files(file.path(gridDir,modelName,"TrespassHoles"),full.names = TRUE)
  if(length(holes)>3) holes <- sample(holes,3)

  plants <- list.files(file.path(gridDir,modelName,"TrespassPlants"),full.names = TRUE)
  if(length(plants)>4) plants <- sample(plants,3)

  #Export
  dir.create(file.path(outputDir,modelName,"/Hard negative holes"),recursive = TRUE)
  dir.create(file.path(outputDir,modelName,"/Hard negative plants"),recursive = TRUE)

  suppressMessages({
    file.copy(from = holes,file.path(outputDir,modelName,"/Hard negative holes"))
    file.copy(from = plants,file.path(outputDir,modelName,"/Hard negative plants"))
  })


}
##eg:
#Make Directories
#newDirs <- paste0(modelName,c("/","/Hard negative holes","/Hard negative plants","/KMLs top","/KMLs full"))
#sapply(paste0("F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Summary Output/",newDirs),dir.create)
#gridDirs <- list.dirs("F:/Adam Cummings/GoogleImagery/ca_hayfork_20160529_4096mosaics/Model Output",recursive=FALSE)
#sapply(gridDirs,hardNegativeExtractor)
#
# outputDir <- "F:/Adam Cummings/DimecV1/Data/M14 Hard Negatives"
# dirList <- list.dirs("F:/Adam Cummings/GoogleImagery/FirstPurchase/Model Output/ca_dosrios_20170813",recursive=FALSE)
# sapply(dirList,hardNegativeExtractor,modelName="M14",outputDir = outputDir)
