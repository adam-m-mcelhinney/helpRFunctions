#' Convert a character string containing the percent sign , to an actual numeric percentage
#' 
#' @param pctStr A string or vector of strings containing percentages.
#' @return A numeric value or vector containing actual percentages.
#' @examples
#' convert.pct(c("10%", "20.5%", "foo", "bar", NA)) #Returns two percentages and 3 NA's
#' 

convert.pct <- function(pctStr){
    if(!is.character(pctStr)) stop("Input is not a string or vector of strings")
    r = as.numeric(sub("%","", pctStr))/100.
    if(!is.numeric(r)) stop("Could not convert to numeric")
    return(r)
}