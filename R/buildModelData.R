#' Build Model Data
#'
#' @description Create train, validation and training data from a pool of original class sorted data
#' This function creates a "Model Data" folder and necessary subfolders with and populated them with
#' @param data.folder
#' @param trainpercent
#' @param validatepercent
#' @param testpercent
#'
#' @return
#' @export
#'
#'
#' @examples
#' \dontrun{
#' buildModelData("F:/Adam Cummings/DimecV1/Data")
#' }
buildModelData <- function(data.folder = getwd(),
                           trainpercent = 0.85,
                           validatepercent = 0.15,
                           testpercent = 0.00){

  if((trainpercent + validationpercent + testpercent) != 1) return("Error: numbers don't add to 1")

  oldwd <- getwd()
  setwd(data.folder)

  sapply(c("Model Data","Model Data/training",
           "Model Data/validation","Model Data/testing","Model Data/testingexport",
           "Model Data/testing/Unclassified"),dir.create)
  classlist <- list.files(path = paste0(getwd(), "/Original"))
  minClass <- min(sapply(paste0(data.folder, "/Original/",classlist),function(x) length(list.files(x))))
  valSize <- round(minClass * validatepercent)
  sapply(classlist,function(x) dir.create(paste0("Model Data/training/",x)))
  sapply(classlist,function(x) dir.create(paste0("Model Data/Validation/",x)))
  labelandmoveoneclass <- function(targetclass = classlist[1]){
    classImages <- list.files(paste0(getwd(),"/Original/",targetclass))
    trainingImages <- sample(classImages,size = round(length(classImages)*trainpercent,0),replace = FALSE)
    valPool <- classImages[!(classImages %in% trainingImages)]
    validationImages <- sample(valPool,size = valSize,replace = FALSE)
    testImages <- valPool[!(valPool %in% validationImages)]
    file.copy(paste0(getwd(),"/Original/",targetclass,"/",trainingImages),paste0(getwd(),"/Model Data/training/",targetclass))
    file.copy(paste0(getwd(),"/Original/",targetclass,"/",validationImages),paste0(getwd(),"/Model Data/validation/",targetclass))
    file.copy(paste0(getwd(),"/Original/",targetclass,"/",testImages),paste0(getwd(),"/Model Data/testing/Unclassified"))

    print(paste0(targetclass, " copying is complete with ",
                 length(trainingImages)," training images and ",
                 length(validationImages)," validation images and ",
                 length(testImages), " test images"))
  }
  sapply(classlist,labelandmoveoneclass)
  file.rename(paste0(getwd(),"Model Data"),
              paste0(getwd(),"Model Data/",Sys.Date()))
  setwd(oldwd)
}
