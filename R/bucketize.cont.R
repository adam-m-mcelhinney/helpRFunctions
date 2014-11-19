#' Places a vector of continuous values into buckets of specified size. 
#' 
#' 
#' @param vals A vector of numeric values that you which to place into buckets.
#' @param bucket.size Optional. Defaults to 1. The size bucket you wish to 
#' create.
#' @return A vector containing the bucketized values.
#' @examples 
#' 
#'vals = c(runif(n = 100, 1, 10), NA)
#'t = bucketize.cont(vals)
#'table(t)
#'v = bucketize.cont(vals, bucket.size = .5)
#'table(v)

bucketize.cont <- function(vals, bucket.size = 1){
	if(!is.numeric(vals)) stop("Input is not numeric.")
	if(!is.numeric(bucket.size)) stop("bucket.size is not numeric.")
	
	n = length(vals)
	bucketized <- vector(length = n)
	for (i in 1:n){
		bucketized[i] <- ceiling(vals[i]/bucket.size) * bucket.size		
	}
	return(bucketized)
}





