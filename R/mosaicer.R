#' Raster Mosaicer
#'
#' @param homeDir Base Directory. Mosaic and rgb folders important.
#' @param originalFolder Typically "rgb" but could be "original" or googletiles
#' @param flightName Name of the flight to mosaic.
#' @param mosaicLabel Short memorable
#'
#' @return Exports a mosaiced dataset with 25 small google tiles as one large
#'  tile
#' @export
##
# library(rgdal)
# library(gdalUtils)
# library(raster)
# library(tidyverse)
mosaicer <- function(homeDir = getwd(),
                     originalFolder = "rgb",
                     flightName,
                     mosaicLabel){
  inputFlight <- file.path(homeDir,originalFolder, flightName)

  outputDir <- file.path(homeDir,"Mosaics")

  #####
  dir.create(file.path(outputDir,flightName),
             recursive=TRUE,
             showWarnings = FALSE)


  #####Process and store extents of all tif images
  tiffNames <- list.files(inputFlight, full.names = TRUE,pattern="*.tif")

  GeoData <- data.frame(nrow = length(tiffNames), ncol = 5)

  for (i in 1:length(tiffNames)){ #collect extent info for each small .tif
    TheRaster <- raster::raster(tiffNames[i])
    rasExtent <- raster::extent(TheRaster)
    GeoData[i, 1] <- tiffNames[i]
    GeoData[i, 2] <- round(rasExtent[1], 3) #Xmin
    GeoData[i, 3] <- round(rasExtent[2], 3) #Xmax
    GeoData[i, 4] <- round(rasExtent[3], 3) #Ymin
    GeoData[i, 5] <- round(rasExtent[4], 3) #Ymax

    #print(i)
  }

  colnames(GeoData) <- c("File_Name", "XMin", "XMax", "YMin", "YMax")

  nCols <- ceiling(nrow(table(GeoData$XMin))/5)
  colLabs <- rep(1:nCols, each=5)
  nRows <- ceiling(nrow(table(GeoData$YMin))/5)
  rowLabs <- rep(nRows:1,each=5)

  colKey <- table(GeoData$XMin[order(GeoData$XMin)]) %>%
    data.frame %>%
    dplyr::mutate(colLab = colLabs[1:nrow(.)]) %>%
    .[,-2] %>%
    dplyr::rename(XMin = Var1) %>%
    dplyr::mutate(XMin = as.numeric(levels(XMin))[XMin])
  rowKey <- table(GeoData$YMin[order(GeoData$YMin,decreasing = TRUE)]) %>%
    data.frame %>%
    dplyr::mutate(rowLab = rowLabs[1:nrow(.)]) %>%
    .[,-2] %>%
    dplyr::rename(YMin = Var1) %>%
    dplyr::mutate(YMin = as.numeric(levels(YMin))[YMin])

  GeoData2 <- GeoData %>%
    dplyr::left_join(colKey) %>%
    dplyr::left_join(rowKey)
  print(table(GeoData2$rowLab,GeoData2$colLab))

  #####End Extent processing


  ##Create mosiacs:
  for(col in 1:nCols){
    print(paste0("Column: ",col))
    for(r in 1:nRows){
      print(paste0("Row: ",r))
      files <- GeoData2[GeoData2$colLab == col &
                          GeoData2$rowLab == r,"File_Name" ]
      if(length(files)>0){
        mosaicName <- paste0("C", stringr::str_pad(col,2,pad = "0"),
                             "_R", stringr::str_pad(r,2,pad = "0"),
                             "-",mosaicLabel,".tif")
        if (!file.exists(mosaicName)){
          suppressMessages(gdalUtils::mosaic_rasters(gdalfile=files,
                                          dst_dataset = file.path(outputDir,
                                                                  flightName,
                                                                  mosaicName)))
        }
      }
    }
  }
}
