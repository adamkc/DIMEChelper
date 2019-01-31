#' Build Model Data
#'
#' @description Create train, validation and training data from a pool of
#'  original class sorted data
#' This function creates a "Model Data" folder and necessary subfolders with
#'  and populated them with
#' @param data.folder Top directory that contains "Original" folder.
#' "Model Data" will be added to this directory as well.
#' @param trainpercent Percent of data to add to the training dataset. These
#'  three need to add to 1.
#' @param validatepercent Percent of data to add to the validation dataset.
#'  These three need to add to 1.
#' @param testpercent Percent of data to add to the test dataset. These three
#'  need to add to 1.
#' @param balanceData logical of whether you'd like to balance the training
#'  data based on the class with the fewest examples
#'
#' @return Produces a random dataset with training, testing, and validation sets
#'
#' @examples
#' \dontrun{
#' buildModelData("F:/Adam Cummings/DimecV1/Data")
#' }
#'
#' @export
buildModelData <- function(data.folder = getwd(),
                           trainpercent = 0.85,
                           validatepercent = 0.15,
                           testpercent = 0.00,
                           balanceData = FALSE){

  if((trainpercent + validatepercent + testpercent) != 1)
    return("Error: numbers don't add to 1")
  oldwd <- getwd()
  setwd(data.folder)
  #Generate folders:
  sapply(file.path(c("Model Data/training",
                     "Model Data/validation",
                     "Model Data/testing/Unclassified",
                     "Model Data/testingExtra/Unclassified"))
         ,dir.create, recursive=TRUE)


  classlist <- list.files(path = file.path("Original"))
  minClass <- min(sapply(file.path("Original",classlist),
                         function(x) length(list.files(x))))
  valSize <- round(minClass * validatepercent)
  sapply(classlist,
         function(x) dir.create(file.path("Model Data/training",x)))
  sapply(classlist,
         function(x) dir.create(file.path("Model Data/validation",x)))

  labelAndMoveOneClass <- function(targetclass = classlist[1]){
    classFolder <- file.path("Original",targetclass)
    classImages <- list.files(classFolder)
    classSize <- length(classImages)

    if(balanceData)
      classSize <- minClass

    trainingImages <- sample(classImages,
                             size = round(classSize*trainpercent,0),
                             replace = FALSE)
    valPool <- classImages[!(classImages %in% trainingImages)]
    validationImages <- sample(valPool,
                               size = valSize,
                               replace = FALSE)
    testPool <- valPool[!(valPool %in% validationImages)]
    testImages <- sample(testPool,
                         size=round(classSize*testpercent,0),
                         replace = FALSE)
    testExtraImages <- testPool[!(testPool %in% testImages)]

    file.copy(file.path(classFolder,trainingImages),
              file.path("Model Data/training",targetclass))
    file.copy(file.path(classFolder,validationImages),
              file.path("Model Data/validation",targetclass))
    file.copy(file.path(classFolder,testImages),
              file.path("Model Data/testing/Unclassified"))
    file.copy(file.path(classFolder,testExtraImages),
              file.path("Model Data/testingExtra/Unclassified"))

    print(paste0(targetclass, " copying is complete with ",
                 length(trainingImages)," training images and ",
                 length(validationImages)," validation images and ",
                 length(testImages), " test images"))
  }
  sapply(classlist,labelAndMoveOneClass)
  setwd(oldwd)
}


