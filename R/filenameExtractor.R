#' Filename Extractor
#'
#' @param filelist a vector of filenames that you want to extract lat long values from. Similar to this:
#'    purchase1: or_goldbeach_20160703--Set2_-123.521_41.94.jpg
#'    purchase2: "C01_R01-Hayfork2016_41.1684_-123.5119.jpg"
#'    I flipped lat and long so they can be pasted into google earth easily but now
#'    two different extraction codes are necessary..
#'
#' @return
#' @export
#'
#' @examples
filenameExtractor <- function(filelist = kmlTiles$Image){
  #Purchase1 Filenames:
  temp <- filelist %>% strsplit("-") %>% unlist %>%
    matrix(nrow=(length(filelist)),byrow = TRUE) %>%
    .[,ncol(.)] %>% substr(1,nchar(.)-4) %>%
    strsplit("_") %>% unlist() %>%
    matrix(nrow=length(filelist),byrow=TRUE)
  output <- data.frame(Name = filelist,
                       Longitude = -as.numeric(temp[,1]),
                       Latitude = as.numeric(temp[,2]))
  if(length(output>1)) return(output)
  #Purchase2 Filenames:
  temp <- filelist %>% strsplit("_") %>% unlist %>%
    matrix(nrow=(length(filelist)),byrow = TRUE) %>%
    .[,c(3,4)]
  temp[1] <- as.numeric(temp[1])
  temp[2] <- temp[2] %>% substr(1,nchar(.)-4) %>% as.numeric()
  output <- data.frame(Name = filelist,
                       Longitude = -as.numeric(temp[,2]),
                       Latitude = as.numeric(temp[,1]))
  return(output)
}
