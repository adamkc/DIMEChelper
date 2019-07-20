#Setup
library(DIMEChelper)
directory <- "G:/GoogleImagery/Purchase2"
flight <- "or_medford_20170708_rgb"
shortFlightName <- "Medford2017"
modelDirectory <- "G:/DIMEC Model File Backup/Model Files/M17"
modM17 <- loadDIMECModel(modelDirectory)

setwd(directory)

#Download:
library(googleCloudStorageR)
downloadFlight(targetDir = file.path(directory,"rgb"),
               flightName = flight)

#Mosaic:
mosaicer(flightName = flight,
         mosaicLabel = shortFlightName)

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
                  modelName = modM17$modelLabel,
                  flightName = flight,
                  summariseKMLs = TRUE)



