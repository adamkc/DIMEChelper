
#' Title
#'
#' @param data.folder
#' @param trainpercent
#' @param validatepercent
#' @param testpercent
#'
#' @return
#' @export
#'
#' @examples
#'
#'


buildBalancedModelData <- function(data.folder,
                                   trainpercent = 0.80,
                                   validatepercent = 0.10,
                                   testpercent = 0.10){
  # oldwd <- data.folder
  # setwd(paste0(data.folder,"/", data.folder))
  sapply(c("Model Data","Model Data/training","Model Data/validation",
           "Model Data/testing","Model Data/testingExtra",
           "Model Data/testing/Unclassified","Model Data/testingExtra/Unclassified"),dir.create)
  classlist <- list.files(path = paste0(data.folder, "/Original"))
  minClass <- min(sapply(paste0(data.folder, "/Original/",classlist),function(x) length(list.files(x))))
  sapply(classlist,function(x) dir.create(paste0("Model Data/training/",x)))
  sapply(classlist,function(x) dir.create(paste0("Model Data/Validation/",x)))
  labelandmoveoneclass <- function(targetclass = classlist[4]){
    classImages <- list.files(paste0(data.folder,"/Original/",targetclass))
    trainingImages <- sample(classImages,size = round(minClass*trainpercent,0),replace = FALSE)
    valPool <- classImages[!(classImages %in% trainingImages)]
    validationImages <- sample(valPool,size = round(minClass*validatepercent,0),replace = FALSE)
    testPool <- valPool[!(valPool %in% validationImages)]
    testImages <- sample(testPool,size=round(minClass*testpercent,0),replace = FALSE)
    testExtraImages <- testPool[!(testPool %in% testImages)]

    file.copy(paste0(data.folder,"/Original/",targetclass,"/",trainingImages),paste0(data.folder,"/Model Data/training/",targetclass))
    file.copy(paste0(data.folder,"/Original/",targetclass,"/",validationImages),paste0(data.folder,"/Model Data/validation/",targetclass))
    file.copy(paste0(data.folder,"/Original/",targetclass,"/",testImages),paste0(data.folder,"/Model Data/testing/Unclassified"))
    file.copy(paste0(data.folder,"/Original/",targetclass,"/",testExtraImages),paste0(data.folder,"/Model Data/testingExtra/Unclassified"))
    print(paste0(targetclass, " copying is complete with ",length(trainingImages),
                 " training images and ",length(testImages), " test images"))
  }
  sapply(classlist,labelandmoveoneclass)
  # file.rename(paste0(data.folder,"Model Data"),
  #             paste0(data.folder,"Model Data/",Sys.Date()))
  # setwd(oldwd)
}
