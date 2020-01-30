#Setup
library(DIMEChelper)
directory <- "F:/Adam Cummings/GoogleImagery/ThirdPurchase"
flight <- "ca_burney_20160927"
shortFlightName <- "Burney2016"
modelDirectory <- "F:/Adam Cummings/DimecV1/Model Files/M17"
modM17 <- loadDIMECModel(modelDirectory)

setwd(directory)

#Download:
library(googleCloudStorageR)
downloadFlight(targetDir = file.path(directory,"rgb"),
               flightName = flight)
flightGridKML(rasterDir = file.path(directory,"rgb",flight))
#Mosaic:
mosaicer(flightName = flight,
         mosaicLabel = shortFlightName)
flightGridKML(rasterDir = file.path(directory,"Mosaics",flight))
#Retile:
files <- list.files(file.path(directory,"Mosaics",flight),
                    recursive=TRUE,pattern="*.tif",
                    full.names = TRUE)

sapply(files,FUN=retile,flightName = flight)

#Classify:
flightLevelClassifier(homeDir = directory,
                      flightName = flight,
                      modelObj = modM17,
                      assessFlightSize=FALSE)

#Summarize:
flightMetricTally(homeDir = directory,
                  modelObj = modM17,
                  flightName = flight,
                  summariseKMLs = TRUE)



