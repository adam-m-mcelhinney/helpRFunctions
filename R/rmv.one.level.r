#' Removes variables that only contain one level, not counting NA.
#' 
#' 
#' @param df The data.frame you wish to have a list of the types for.
#' @param print.removed.cols Optional, defaults to TRUE. If TRUE, prints the column names of the deleted columns.
#' @return A data frame with variables only containing one level removed.
#' @examples 
#'df <- data.frame(matrix(rnorm(100), nrow = 10))
#'for (k in 1:10) {
#'  i = round(runif(n = 1, min = 0, max = 9))
#'  j = round(runif(n = 1, min = 0, max = 9))
#'  df[i, j] <- NA
#'}
#'df <- cbind(df, rep("A", nrow(df)))
#'df <- cbind(df, df[,1])
#'rmv.one.level(df)

# Vars with only one level, not including NA
rmv.one.level <- function(df, print.removed.cols = TRUE) {
  if(!is.data.frame(df)) stop('Must provide data frame')
  out <- lapply(df, function(x) length(table(x)))
  cols.to.remove <- unlist(which(!out > 1))
  if(print.removed.cols == TRUE) warning(paste0('Columns removed: ', names(cols.to.remove)))
  return (df[,-cols.to.remove])
}

