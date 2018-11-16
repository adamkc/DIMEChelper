#' Title
#'
#' @param imageDir Folder for current operations
#' @param inputFolder Folder location of tiff file
#' @param imageName Name of tiff file to chip.
#' @param red Dimension number for red band
#' @param green Dimension number for green band
#' @param blue Dimension number for blue band
#' @param dim Size of output jpg chip
#' @param overlap number of pixels to overlap the chips
#' @param outputFolder location that new chips are copied
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ###List of Tiffs to Crop:-----------------------------------------------
#' files <- list.files("F:/Adam Cummings/GoogleImagery/ShastaT/Mosaics",recursive=FALSE)
#' files <- files[grep(pattern = "gold",x = files)]
#'
#' ###Crop one tiff:--------------------------------------------
#' retile("F:/Adam Cummings/GoogleImagery/ShastaT",
#'        imageName = files[2],
#'        dim=299,overlap=60,outputFolder="Chips",inputFolder="Mosaics")
#'
#' ###Crop a list of tiffs:-------------------------------------------
#' sapply(files,FUN=retile,
#'        imageDir = "G:/ShastaT Search/ca_arcata_20160527",
#'        dim=299,overlap=60,outputFolder="Chips",inputFolder="Mosaics")
#'
#' }


# library(rgdal)
# library(raster)
# library(magrittr)
# library(jpeg)


retile <- function(imageDir = getwd(),
                   inputFolder="Mosiacs",
                   imageName,
                   red=1,green=2,blue=3,
                   dim=299,overlap=60,
                   outputFolder="Tiles"){
  #This takes a raster of the class SpatialGridDataFrame and converts
  #it into jpg tiles of the specified dimensions and overlap. Band color order can be
  #specified with arguments

  ## Create Output folder if necessary
  csvName <- substr(imageName,start = 1,stop = nchar(imageName)-4)
  outputFolder <- paste0(imageDir,"/",outputFolder,"/",csvName,"/Unclassified")
  if (dir.exists(outputFolder)) return(print(paste0(imageName, " is already clipped."))) ##Bail early to avoid redundant work

  dir.create(outputFolder, recursive=TRUE)
  image = rgdal::readGDAL(paste0(imageDir,"/",inputFolder,"/",imageName))
  ## Extract variables from SpatialGridDataFrame
  pixelrows <- image@grid@cells.dim[2]
  pixelcols <-image@grid@cells.dim[1]
  df <- image@data
  startLat <- image@bbox[2,2]
  startLong <- image@bbox[1,1]
  cellsize <- image@grid@cellsize[1]
  ##df$col <- rep(1:pixelcols,times = pixelrows)  This labeling helped error check.
  ##df$row <- rep(1:pixelrows,each = pixelcols)

  b1 <-  matrix(df[,red],nrow = pixelrows,byrow = TRUE)
  b2 <-  matrix(df[,green], nrow = pixelrows,byrow = TRUE)
  b3 <-  matrix(df[,blue], nrow = pixelrows,byrow = TRUE)
  m <-  array(c(b1,b2,b3),dim=c(pixelrows,pixelcols,3))

  ## Calculate number of images to be generated:
  nimagerows <- ceiling(pixelrows / (dim-overlap))
  nimagecols <- ceiling(pixelcols / (dim-overlap))

  print(sprintf("%s columns and %s rows gives %s expected images",nimagecols,nimagerows,nimagerows*nimagecols))

  ## Generate and export images:
  gridkey <- data.frame(Row=NULL,Col=NULL,File=NULL, Lat=NULL,Long=NULL)
  for(r in 1:nimagerows){
    for(c in 1:nimagecols){
      ##Determine location of current crop:
      rowstart <- (((r-1)*dim)+1) - ((r-1)*overlap)
      rowend <- rowstart+dim -1
      if(rowend > pixelrows){ ##If at edge, set back from edge.
        rows <- (pixelrows-dim+1):pixelrows} else {
          rows <- rowstart:rowend}


      colstart <- (((c-1)*dim)+1) - ((c-1)*overlap)
      colend <- colstart+dim -1
      if(colend > pixelcols){ ##If at edge, set back from edge.
        cols <- (pixelcols-dim+1):pixelcols} else {
          cols <- colstart:colend}

      #Smear crops near edge so all output identical size:
      #rows[rows > pixelrows] <- pixelrows
      #cols[cols>pixelcols] <- pixelcols

      output <- m[rows,cols,c(1:3)]

      if(mean(output) != 0){  ##Only print and update if image has contents
        ##
        output <- output/max(output)
        tileLat <- startLat - (round(mean(rows)) * cellsize)
        tileLong <- startLong + (round(mean(cols)) * cellsize)
        ##
        tilename <- paste0(csvName,"_", round(tileLong,5),"_",round(tileLat,5),".jpg")
        filename <- paste0(outputFolder,"/",tilename)
        jpeg::writeJPEG(output, target = filename,quality=0.95)
        ##GridKey:
        temp <- data.frame(Row=r,Col=c,File=tilename, Lat=tileLat,Long=tileLong)
        gridkey <- rbind(gridkey,temp)
      }
    }
    csvName <- substr(imageName,start = 1,stop = nchar(imageName)-4)
    write.csv(gridkey,file = paste0(csvName,"_TileKey.csv"),row.names = FALSE)
  }

}
