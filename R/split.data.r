#' Divides the data into training and testing sets, with a specified percentage. 
#' TODO: Extend this function to split into any number of groups with specified percentages in each group
#' TODO: Extend this function to work for matrices as well
#' 
#' @param y The column name of target variable, or variable of interest.
#' @param df The data frame to split
#' @param p Percent to put into the training data set. Optional, defaults to .7.
#' @return A list containing data frames with training as the first element and the testing as the second
#' @examples 
#' df <- data.frame(matrix(rnorm(100), nrow = 10))
#' t <- split.data(y = 'X1', df = df, p = .7)
#' training <- t[[1]]
#' testing <- t[[2]]
#' 


split.data <- function(y, df, p = .7){
  if(!is.data.frame(df)) stop('Must provide data frame')
  y = df[,y]
  part <- createDataPartition(y = y, p = p, list = FALSE)
  training <- data[part,]
  testing <- data[-part,]
  return (list(training, testing))
}

