#' Calculates the amortization schedule for a given loan(s)
#' 
#' @param principal A vector of principal, or the amount of the loan.
#' @param rate A vector containing the annual interest rate on the loan, expressed as a decimal. For example, 12% = .012.
#' @param 
#' @return A list containing data frames named according to the set.names 
#' argument.
#' @examples 
#' df <- data.frame(matrix(rnorm(110), nrow = 11))

p <- c(1:5 * 100)
rate <- runif(n = 5, min = 0, max = .3)
