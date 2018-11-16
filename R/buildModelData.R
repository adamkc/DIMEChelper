#' Build Model Data
#'
#' @description Create train, validation and training data from a pool of original class sorted data
#' This function creates a "Model Data" folder and necessary subfolders with and populated them with
#' @param data.folder Top directory that contains "Original" folder. "Model Data" will be added to this directory as well.
#' @param trainpercent Percent of data to add to the training dataset. These three need to add to 1.
#' @param validatepercent Percent of data to add to the validation dataset. These three need to add to 1.
#' @param testpercent Percent of data to add to the test dataset. These three need to add to 1.
#' @param balanceData logical of whether you'd like to balance the training data based on the class with the fewest examples
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
                           testpercent = 0.00,
                           balanceData = FALSE){

  if((trainpercent + validationpercent + testpercent) != 1)
    return("Error: numbers don't add to 1")

  #Generate folders:
  sapply(c("Model Data","Model Data/training",
           "Model Data/validation","Model Data/testing","Model Data/testingexport",
           "Model Data/testing/Unclassified","Model Data/testingExtra/Unclassified")
         ,dir.create)


  classlist <- list.files(path = paste0(data.folder, "/Original"))
  minClass <- min(sapply(paste0(data.folder, "/Original/",classlist),function(x) length(list.files(x))))
  valSize <- round(minClass * validatepercent)
  sapply(classlist,function(x) dir.create(paste0("Model Data/training/",x)))
  sapply(classlist,function(x) dir.create(paste0("Model Data/Validation/",x)))

  labelAndMoveOneClass <- function(targetclass = classlist[1]){
    classFolder <- paste0(data.folder,"/Original/",targetclass)
    classImages <- list.files(classFolder)
    classSize <- length(classImages)

    if(balanceData)
      classSize <- minClass

    trainingImages <- sample(classImages,size = round(classSize*trainpercent,0),replace = FALSE)
    valPool <- classImages[!(classImages %in% trainingImages)]
    validationImages <- sample(valPool,size = valSize,replace = FALSE)
    testPool <- valPool[!(valPool %in% validationImages)]
    testImages <- sample(testPool,size=round(classSize*testpercent,0),replace = FALSE)
    testExtraImages <- testPool[!(testPool %in% testImages)]

    file.copy(paste0(classFolder,"/",trainingImages),paste0(data.folder,"/Model Data/training/",targetclass))
    file.copy(paste0(classFolder,"/",validationImages),paste0(data.folder,"/Model Data/validation/",targetclass))
    file.copy(paste0(classFolder,"/",testImages),paste0(data.folder,"/Model Data/testing/Unclassified"))
    file.copy(paste0(classFolder,"/",testExtraImages),paste0(data.folder,"/Model Data/testingExtra/Unclassified"))

    print(paste0(targetclass, " copying is complete with ",
                 length(trainingImages)," training images and ",
                 length(validationImages)," validation images and ",
                 length(testImages), " test images"))
  }
  sapply(classlist,labelAndMoveOneClass)
}
