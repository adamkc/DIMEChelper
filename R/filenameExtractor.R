#' Filename Extractor
#'
#' @param fileNames a vector of filenames that you want to extract lat long
#'  values from. Similar to this:
#'    purchase1: or_goldbeach_20160703--Set2_-123.521_41.94.jpg
#'    purchase2: "C01_R01-Hayfork2016_41.1684_-123.5119.jpg"
#'    I flipped lat and long so they can be pasted into google earth easily
#'     but now two different extraction codes are necessary..
#'
#' @return Returns a dataframe that has three columns: Name, Longitude, Latitude
#'
#' @examples
#' filenameExtractor(c("C01_R07-Arcata2016_40.9977_-123.75258.jpg",
#' "C01_R07-Arcata2016_40.9977_-123.75322.jpg"))
#'
#' @export
filenameExtractor <- function(fileNames = kmlTiles$Image){
  ## TODO:
    ##Thows error if only a single file name is passed....

  ##Handle bad fileNames by returning dummy data:
  nperiods <- nchar(fileNames[1])-nchar(gsub(pattern = "\\.","",fileNames[1]))
  if(nperiods==1) return(data.frame(Name = fileNames,
                                    Longitude = -123,
                                    Latitude = 40))
  ## Purchase1 Filenames:
  temp <- fileNames %>% strsplit("-") %>% unlist %>%
    matrix(nrow=(length(fileNames)),byrow = TRUE) %>%
    .[,ncol(.)] %>% substr(1,nchar(.)-4) %>%
    strsplit("_") %>% unlist() %>%
    matrix(nrow=length(fileNames),byrow=TRUE)

  if(ncol(temp)>1) return(data.frame(Name = fileNames,
                       Longitude = -as.numeric(temp[,1]),
                       Latitude = as.numeric(temp[,2])))


  ## Purchase2 Filenames:
  temp <- fileNames %>% substr(1,nchar(.)-4) %>%
    strsplit("_") %>% unlist %>%
    matrix(nrow=(length(fileNames)),byrow = TRUE) %>%
    .[,c(3,4)]

  output <- data.frame(Name = fileNames,
                       Longitude = as.numeric(temp[,2]),
                       Latitude = as.numeric(temp[,1]))
  return(output)
}
