#' retile
#'
#' This takes a raster of the class SpatialGridDataFrame and converts it into jpg
#' chips of the specified dimensions and overlap. Band color order can be
#' specified with arguments
#'
#' @param imageName Name of Tif to be chipped.
#' @param flightName name of flight to be chipped
#' @param red Dimension number for red band
#' @param green Dimension number for green band
#' @param blue Dimension number for blue band
#' @param dim Size of output jpg chip
#' @param overlap number of pixels to overlap the chips
#' @param outputFolder location that new chips are copied
#' @param exportChipKey Logical, export the chipkey csv or not.
#' @param outputJPG Logical.  Export jpg?
#' @param returnData Logical. return jpgs? For future use when chips not saved.
#'
#' @return Optionally exports a list of images that were created.
#' @return Chips jpg files into the outputFolder directory.
#' @return chipList csv file listing the filenames and coordinates for the exported jpgs.
#'
#'
#' @examples
#' \dontrun{
#' ###List of Tiffs to Crop:-----------------------------------------------
#' files <- list.files("G:/GoogleImagery/Purchase2/Mosaics",recursive=TRUE,pattern="*.tif",full.names = TRUE)
#' files <- files[grep(pattern = "crescentcity2015",x = files)]
#'
#' ###Crop one tiff:--------------------------------------------
#' retile(imageName = files[1],dim=299,overlap=60,outputFolder="Chips")
#'
#' ###Crop a list of tiffs:-------------------------------------------
#' setwd("G:/GoogleImagery/Purchase2")
#' sapply(files,FUN=retile,outputFolder="Chips",flightName = "ca_crescentcity_20150715_rgb")
#'
#' }
#' @export

retile <- function(imageName,
                   flightName="ca_hayfork_20160529_rgb",
                   red=1,green=2,blue=3,
                   dim=299,overlap=60,
                   outputFolder="Chips",
                   outputJPG = TRUE,
                   returnData = FALSE,
                   exportChipKey = FALSE){


  ## Create Output folder if necessary
  csvName <- substr(basename(imageName),start = 1,
                    stop = nchar(basename(imageName))-4)

  outputFolderFull <- file.path(outputFolder,flightName,csvName,"Unclassified")

  ## Bail early to avoid redundant work:
  if (dir.exists(outputFolderFull))
    return(print(paste0(imageName," is already clipped.")))
  if (!dir.exists(outputFolder))
    return(print(paste0("outputFolder '", outputFolder,
                        "' doesnt exist.  Setwd to correct Dir?.")))
  if(!file.exists(imageName))
    return(print(paste0("Cannot locate file ",imageName)))

  dir.create(outputFolderFull, recursive=TRUE)
  image <- rgdal::readGDAL(imageName)

  ## Extract variables from SpatialGridDataFrame
  pixelrows <- image@grid@cells.dim[2]
  pixelcols <-image@grid@cells.dim[1]
  df <- image@data
  startLat <- image@bbox[2,2]
  startLong <- image@bbox[1,1]
  cellsize <- image@grid@cellsize[1]


  ##Reproject Start Point
  if(!raster::compareCRS(image,raster::crs("+init=epsg:4326")) &
     !is.na(raster::crs(image))){
    xy <- sp::SpatialPoints(coords = data.frame(t(image@bbox)),
                  proj4string = raster::crs(image))
    wgs.xy <- sp::spTransform(xy,CRSobj = sp::CRS("+init=epsg:4326"))

  } else {
    wgs.xy <- sp::SpatialPoints(coords = data.frame(t(image@bbox)),
                            proj4string = raster::crs(image))
  }
  rm(image)

  ##df$col <- rep(1:pixelcols,times = pixelrows) #This label helps error check.
  ##df$row <- rep(1:pixelrows,each = pixelcols)

  # b1 <-  matrix(df[,red],nrow = pixelrows,byrow = TRUE)
  # b2 <-  matrix(df[,green], nrow = pixelrows,byrow = TRUE)
  # b3 <-  matrix(df[,blue], nrow = pixelrows,byrow = TRUE)
  m <-  array(c(matrix(df[,red],nrow = pixelrows,byrow = TRUE),
                matrix(df[,green], nrow = pixelrows,byrow = TRUE),
                matrix(df[,blue], nrow = pixelrows,byrow = TRUE)),
              dim=c(pixelrows,pixelcols,3))
  rm(df)
  ## Calculate number of images to be generated:
  nimagerows <- ceiling(pixelrows / (dim-overlap))
  nimagecols <- ceiling(pixelcols / (dim-overlap))

  print(sprintf("%s columns and %s rows gives %s expected images",nimagecols,
                nimagerows,nimagerows*nimagecols))

  ## Generate and export images:
  gridkey <- data.frame(Row=NULL,Col=NULL,File=NULL, Lat=NULL,Long=NULL)
  if(returnData) imageList <- list()
  for(r in 1:nimagerows){
    for(c in 1:nimagecols){
      ##Determine location of current crop:
      rowstart <- (((r-1)*dim)+1) - ((r-1)*overlap)
      rowend <- rowstart+dim -1
      if(rowend > pixelrows){ ##If at edge, set back from edge.
        rows <- (pixelrows-dim+1):pixelrows
      } else {
        rows <- rowstart:rowend
      }


      colstart <- (((c-1)*dim)+1) - ((c-1)*overlap)
      colend <- colstart+dim -1
      if(colend > pixelcols){ ##If at edge, set back from edge.
        cols <- (pixelcols-dim+1):pixelcols
      } else {
        cols <- colstart:colend
      }

      #Smear crops near edge so all output identical size:
      #rows[rows > pixelrows] <- pixelrows
      #cols[cols>pixelcols] <- pixelcols

      output <- m[rows,cols,c(1:3)]

      proportionBlack <- sum(output==0)/(dim*dim*3)

      if(proportionBlack <0.20){  ##Dont save black and edge pieces..
        ##
        #This method was used for all of the First purchase. Why?:
        # output <- output/max(output)
        #Isn't this a more reliable way to scale between 0 and 1?:
        output <- output/256

        # chipLat <- startLat - (round(mean(rows)) * cellsize)
        # chipLong <- startLong + (round(mean(cols)) * cellsize)

        chipLat <- wgs.xy@coords[2,2] - ((wgs.xy@coords[2,2] -
                                            wgs.xy@coords[1,2]) *
                                           (mean(rows)/pixelrows))
        chipLong <- wgs.xy@coords[1,1] + ((wgs.xy@coords[2,1] -
                                             wgs.xy@coords[1,1]) *
                                            (mean(cols)/pixelcols))

        ##
        chipName <- paste0(csvName,"_", round(chipLat,5),"_",
                           round(chipLong,5),".jpg")
        fileName <- paste0(outputFolderFull,"/",chipName)
        if (outputJPG) jpeg::writeJPEG(output, target = fileName,quality=0.95)
        if (returnData){
          imageNumber <- ((r-1)*nimagecols) + c
          imageList[[imageNumber]] <- output
        }
        ##GridKey:
        temp <- data.frame(Row=r,Col=c,File=chipName, Lat=chipLat,Long=chipLong)
        gridkey <- rbind(gridkey,temp)
      }
    }
    #csvName <- substr(imageName,start = 1,stop = nchar(imageName)-4)
    if(exportChipKey)
      write.csv(gridkey,
                file = paste0(csvName,"_ChipKey.csv"),row.names = FALSE)
  }

  ## Display count of chips and delete empty directory if no chips:
  if(outputJPG){
    exportedChipCount <- nrow(gridkey)
    message(paste0(exportedChipCount," chips created."))
    if(exportedChipCount==0){
      message("No valid chips exported. Deleting folder.")
      unlink(dirname(outputFolderFull),recursive=TRUE)
    }
  }
  if(returnData){
    imageList <- imageList[[which(!sapply(FUN = is.null,imageList))]]
    return(imageList)
  }

}
