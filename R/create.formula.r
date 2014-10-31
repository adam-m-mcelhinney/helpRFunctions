#' Creates forumula from the given variables.
#' 
#' @param y_var The variable you are interested in predicting.
#' @param x_vars The variables you believe may predict the y_var.
#' @param intercept Optional, defaults to TRUE. If FALSE, remove the intercept term from the formula.
#' @return An R formula object.
#' @examples
#' x_vars <- c(as.character(paste0('X', c(1:10))), NA)
#' y_var <- 'Y'
#' create.formula(y_var, x_vars, intercept = FALSE)
 

create.formula <- function(y_var, x_vars, intercept = TRUE){
  x = paste0(na.omit(x_vars), collapse = ' + ') # Paste will coerce NA to "NA", so remove them
  y = as.character(y_var)
  raw = paste(y, x, sep = ' ~ ')
  if (intercept == FALSE){
    raw = paste0(raw, ' -1')
  }
  return (as.formula(raw))  
}




# df <- data.frame(matrix(rnorm(100), nrow = 10))
# x_vars = c(names(df[,-1]), NA)
# formula <- create.formula(y_var = 'X1', x_vars = x_vars, intercept = TRUE)
# l <- lm(formula, data = df) 
# summary(l)

