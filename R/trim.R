#' Trims whitespace from each element in a chacter vector.
#' Adapted from this post on \href{http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r}{StackOverflow}
#' 
#' @param x A vector containing the strings to from which to remove white space. 
#' @param white.space Optional. Acceptable values are "leading", "trailing", or "both". Defaults to both. Do you want to trim the "leading" white space, the "trailing" white space, or "both" the leading and trailing white space.
#' @return A vector containing the bucketized values.
#' @examples 
#' 
#'x <-c(' leading whitespace', 'trailing whitespace ', ' both leading and trailing white space ')
#'trim(x)
#'trim(x, 'leading')
#'trim(x, 'trailing')
#'#trim(x, 'other') # throws error

trim <- function(x, white.space = 'both'){
	if(!(white.space  %in% c('leading', 'trailing', 'both'))) {
		stop(paste(white.space, ' is not an acceptable value for white.space.'))
		}
	if(white.space == 'both'){
		trim.str <- "^\\s+|\\s+$"
	} else if (white.space == 'leading'){
		trim.str <- "^\\s+"
	} else {
		trim.str = "\\s+$"
	}
	return (gsub(trim.str, "", x))
}




