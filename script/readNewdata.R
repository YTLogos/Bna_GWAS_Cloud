#' Title
#'
#' @param fileinfo
#'
#' @return
#' @export
#'
#' @examples
readNewData <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "sample_hair_gwas.txt", size = 1, type = "text/txt", datapath = "./data/sample_hair_gwas.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE)
  return(newdata)
}
