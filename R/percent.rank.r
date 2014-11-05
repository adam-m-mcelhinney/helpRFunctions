#' Calculate the rank of a value, as a percentile. Similar to Excel's \href{http://office.microsoft.com/en-us/excel-help/percentrank-HP005209212.aspx}{percentrank} function. 
#' Code apapted from \href{http://stats.stackexchange.com/questions/11924/computing-percentile-rank-in-r}{StackOverflow}.
#' 
#' 
#' @param x A vector (can be unsorted) to compute the percent rank.
#' @return The percentage rank for each of the observations.
#' @examples #Modified example from Excel's percentrank example.
#' 
#' y <- c(13, 12, 11, 8, 4, 3, 2, 1, 1, 1, NA)
#' percent.rank(y)
#' 
#' x <- c("A", 1, 2, 3, 4, NA)
#' #percent.rank(x) # Throws error
#' 

percent.rank<-function(x){
  if(!is.numeric(x)) stop("Input is not numeric")
  rx<-rle(sort(x))
  smaller<-cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger<-rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr<-smaller/(smaller+larger)
  rxpr[match(x, rx$values)]
}