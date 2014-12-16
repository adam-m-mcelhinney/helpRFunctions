#' Appends the date and time to a string. Useful if you want to name a file based off when it was run, or something similar.
 
#' @param str The string to which you want the date appended. 
#' @param sep The charact with which to combine the string and the date-time. Defaults to underscore.
#' @param date.format Optional. The R date-time format you wish to append to the string. Defaults to "%Y_%m_%d_%H_%M_%S". 
#' Note that if you do not provide a date, currently this will not fail.
#' @return A string containing the date and time appended to it. 
#' @examples 
#' str <- 'test'
#' date.time.append(str)


date.time.append <- function(str, sep = '_', date.format ="%Y_%m_%d_%H_%M_%S") {
  stopifnot(is.character(str))
  return(paste(str, format(Sys.time(), date.format), sep = sep))  
}