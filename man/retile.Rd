% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retile.R
\name{retile}
\alias{retile}
\title{retile}
\usage{
retile(imageName, flightName = "ca_hayfork_20160529_rgb", red = 1,
  green = 2, blue = 3, dim = 299, overlap = 60,
  outputFolder = "Chips", outputJPG = TRUE, returnData = FALSE,
  exportChipKey = FALSE)
}
\arguments{
\item{imageName}{Name of Tif to be chipped.}

\item{flightName}{name of flight to be chipped}

\item{red}{Dimension number for red band}

\item{green}{Dimension number for green band}

\item{blue}{Dimension number for blue band}

\item{dim}{Size of output jpg chip}

\item{overlap}{number of pixels to overlap the chips}

\item{outputFolder}{location that new chips are copied}

\item{outputJPG}{Logical.  Export jpg?}

\item{returnData}{Logical. return jpgs? For future use when chips not saved.}

\item{exportChipKey}{Logical, export the chipkey csv or not.}
}
\value{
Optionally exports a list of images that were created.

Chips jpg files into the outputFolder directory.

chipList csv file listing the filenames and coordinates for the exported jpgs.
}
\description{
This takes a raster of the class SpatialGridDataFrame and converts it into jpg
chips of the specified dimensions and overlap. Band color order can be
specified with arguments
}
\examples{
\dontrun{
###List of Tiffs to Crop:-----------------------------------------------
files <- list.files("G:/GoogleImagery/Purchase2/Mosaics",recursive=TRUE,pattern="*.tif",full.names = TRUE)
files <- files[grep(pattern = "crescentcity2015",x = files)]

###Crop one tiff:--------------------------------------------
retile(imageName = files[1],dim=299,overlap=60,outputFolder="Chips")

###Crop a list of tiffs:-------------------------------------------
setwd("G:/GoogleImagery/Purchase2")
sapply(files,FUN=retile,outputFolder="Chips",flightName = "ca_crescentcity_20150715_rgb")

}
}
