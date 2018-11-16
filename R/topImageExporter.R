#' Top Image Export
#'
#' @param plotData data from which to extract the top images
#' @param class Target class
#' @param unclassifieddir location of chips
#' @param exportdir target directory for new files
#' @param threshold threshold for exporting the top images
#'
#' @return
#' @export
#'
#' @examples
#'


topImageExporter <- function(plotData,
                             class = "TrespassPlants",
                             unclassifieddir = searchdir,
                             exportdir = exportdir,
                             threshold = 0.90){
  temp <- plotData[plotData[,class] > threshold,]

  topimages <- temp$Image

  dir.create(paste0(exportdir,"/",class),recursive = TRUE)
  file.copy(paste0(unclassifieddir,"/",topimages),
            paste0(exportdir,"/",class,"/"))
  #file.rename(from = paste0(exportdir,"/",class,"/",topimages),
  #            to = paste0(exportdir,class,"/",topnewname,".jpg") )

}
