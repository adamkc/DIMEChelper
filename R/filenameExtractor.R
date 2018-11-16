#' Filename Extractor
#'
#' @param filelist a vector of filenames that you want to extract lat long values from. Similar to this:
#'     or_goldbeach_20160703--Set2_-123.521_41.94.jpg
#'
#' @return
#' @export
#'
#' @examples
filenameExtractor <- function(filelist = kmlTiles$Image){
  temp <- filelist %>% strsplit("-") %>% unlist %>%
    matrix(nrow=(length(filelist)),byrow = TRUE) %>%
    .[,ncol(.)] %>% substr(1,nchar(.)-4) %>%
    strsplit("_") %>% unlist() %>%
    matrix(nrow=length(filelist),byrow=TRUE)
  output <- data.frame(Name = filelist,
                       Longitude = -as.numeric(temp[,1]),
                       Latitude = as.numeric(temp[,2]))
  output
}
