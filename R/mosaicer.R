#' Title
#'
#' @param NOTBUILTASFUNCTIONYET
#'
#' @return
#' @export
#'
#' @examples


#####
library(rgdal)
library(gdalUtils)
library(raster)
library(tidyverse)


mosaicer <- function(NOTBUILTASFUNCTIONYET){
  #####UNUSED: convert DD to UTM
  LongLatToUTM<-function(x,y,zone){
    xy <- data.frame(ID = 1:length(x), X = x, Y = y)
    coordinates(xy) <- c("X", "Y")
    proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
    res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
    return(as.data.frame(res))
  }
  #####

  #####INPUTS
  #MosaicSize = 0.03 #decimal degree distance for edge of 5x5 grid, calculated using GeoData output in Excel.
  MosaicSize = 0.0275 #Average Jump in xMin and YMin

  #InputDir = "G:/ShastaT Search/ca_arcata_20160527/GoogleTiles"
  #InputDir = "G:/GoogleImagery/Purchase2/Mosiacs/ca_hayfork_20160529_rgb"
  InputDir = "G:/GoogleImagery/Purchase2/rgb/"
  #InputDir = "C:/Users/IERC Staff/Desktop/New Folder/" #Test Directory

  #outputDir = "G:/ShastaT Search/ca_arcata_20160527/Mosaics/"
  outputDir = "G:/GoogleImagery/Purchase2/Mosaics/"
  #outputDir = "C:/Users/IERC Staff/Desktop/New Folder (2)/" #Test Directory

  flightName = "or_medford_20170708_rgb"
  #####
  dir.create(file.path(outputDir,flightName))
  inputFlight = file.path(InputDir,flightName)
  #####Process and store extents of all tif images
  tiffNames <- list.files(inputFlight, full.names = TRUE,pattern="*.tif")

  GeoData = data.frame(nrow = length(tiffNames), ncol = 5)

  for (i in 1:length(tiffNames)){ #collect extent info for each small .tif
    TheRaster = raster(tiffNames[i])
    rasExtent = extent(TheRaster)
    GeoData[i, 1] = tiffNames[i]
    GeoData[i, 2] = round(rasExtent[1], 3) #Xmin
    GeoData[i, 3] = round(rasExtent[2], 3) #Xmax
    GeoData[i, 4] = round(rasExtent[3], 3) #Ymin
    GeoData[i, 5] = round(rasExtent[4], 3) #Ymax

    print(i)
  }

  colnames(GeoData) = c("File_Name", "XMin", "XMax", "YMin", "YMax")

  nCols <- ceiling(nrow(table(GeoData$XMin))/5)
  colLabs <- rep(1:nCols, each=5)
  nRows <- ceiling(nrow(table(GeoData$YMin))/5)
  rowLabs <- rep(nRows:1,each=5)

  colKey <- table(GeoData$XMin[order(GeoData$XMin)]) %>%
    data.frame %>% dplyr::mutate(colLab = colLabs[1:nrow(.)]) %>%
    .[,-2] %>% rename(XMin = Var1) %>% mutate(XMin = as.numeric(levels(XMin))[XMin])
  rowKey <- table(GeoData$YMin[order(GeoData$YMin,decreasing = TRUE)]) %>%
    data.frame %>% dplyr::mutate(rowLab = rowLabs[1:nrow(.)]) %>%
    .[,-2] %>% rename(YMin = Var1) %>% mutate(YMin = as.numeric(levels(YMin))[YMin])

  GeoData2 <- GeoData %>% dplyr::left_join(colKey) %>% dplyr::left_join(rowKey)
  table(GeoData2$rowLab,GeoData2$colLab)



  #write.csv(GeoData2, "C:/Users/IERC Staff/Desktop/Output.csv")
  #####End Extent processing
  flightLabel <- "-Medford2017"

  ##Create mosiacs:
  for(col in 1:nCols){
    print(paste0("Column: ",col))
    for(r in 1:nRows){
      print(paste0("Row: ",r))
      files <- GeoData2[GeoData2$colLab == col & GeoData2$rowLab == r,"File_Name" ]
      if(length(files)>0){
        mosaicName <-    paste0("C",stringr::str_pad(col,2,pad = "0"),"_R", stringr::str_pad(r,2,pad = "0"), flightLabel,".tif")
        if (!file.exists(mosaicName)){
          suppressMessages( mosaic_rasters(gdalfile=files,dst_dataset = file.path(outputDir,flightName,mosaicName)) )
        }
      }
    }
  }

  # #####Create mosaics. NOTE: CURRENTLY RUNS INFINITELY ON LAST MOSAIC!!
  # Xindex = min(GeoData$XMin) # control indexes to keep track of mosaic locations
  # Yindex = max(GeoData$YMax)
  #
  # while (Yindex > min(GeoData$YMax)){ #loop top to bottom
  #   print(paste("Yloop", Yindex))
  #
  #   while (Xindex < max(GeoData$XMin)){ #loop left to right
  #     print(paste("Xloop", Xindex))
  #
  #     ulCornerX = Xindex
  #     ulCornerY = Yindex
  #     brCornerX = Xindex + MosaicSize
  #     brCornerY = Yindex - MosaicSize
  #     ulCornerXformat = gsub("\\.", "_", as.character(abs(ulCornerX)))
  #     ulCornerYformat = gsub("\\.", "_", as.character(abs(ulCornerY)))
  #
  #     CurrentRasters = GeoData[GeoData$XMin>=ulCornerX & GeoData$XMax<=brCornerX & GeoData$YMax<=ulCornerY  & GeoData$YMin>=brCornerY, ] #Selects image records within 5x5 grid
  #
  #     if (nrow(CurrentRasters) > 1){
  #       Xindex = max(CurrentRasters$XMax)
  #       mosaic_rasters(CurrentRasters[,1],dst_dataset = paste0(outputDir, ulCornerXformat,"_", ulCornerYformat, "FINAL.tif"))
  #       }else{
  #         rvalues = GeoData[GeoData$YMax<=ulCornerY  & GeoData$YMin>=brCornerY, ] #moves the starting edge to match the first image on left when no tiles exist in original grid
  #         Xindex = min(rvalues$XMin)
  #     }
  #   }
  #   Xindex = min(GeoData$XMin) #reset Xindex to move window back to left like a typewriter
  #   Yindex = min(CurrentRasters$YMin) #sets y index to match bottom of upper row
  # }
  # ######
  #
  #
  #
  #



}
