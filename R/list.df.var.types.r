#' Provides a list containing the data.frame columns that are of a given type. 
#' This can be useful for functions that only support a given type.
#' 
#' 
#' @param df The data.frame you wish to have a list of the types for.
#' @return A list with names corresponding to the data type, and entries 
#' corresponding to the variable names that are of that type.
#' @examples 
#' df <- data.frame(matrix(rnorm(100), nrow = 10))
#' df <- cbind(df, rep("A", nrow(df)))
#' #cor(df) # This will return an error because the last column is not numeric.
#' varList <- list.df.var.types(df)
#' cor(df[,varList$numeric]) # No longer returns an error. Ignores the 
#' non-numeric columns.

list.df.var.types <- function(df) {
  if(!is.data.frame(df)) stop('Must provide data frame')
  var.types <- lapply(df, class) # Get all the var types for each variable
  types <- names(table(unlist(var.types))) # Determine the distinct var types that exist in that data set
  varList <- list() # Create empty list to store the names of the variables of each type
  for (j in types) { # TODO: Suspect there is a way to do this more efficiently
    l <- list(names(var.types[var.types == j]))
    names(l) <- j
    varList <- c(varList, l)
  }
  return (varList)
}