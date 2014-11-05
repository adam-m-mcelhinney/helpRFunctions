#' Takes a vector of continuous data and puts it into buckets of specified size.
#' @param vals A vector of continous data (can contain NA)
#' @param bucket.size Optional. Defaults to 1. A number (integer or decimal) that specifies what size buckets you want to create.
#' @return A vector of data that has been placed into buckets of the specified size.
#' @examples 
#' 
#'vals = c(runif(n = 100, 1, 10), NA)
#'t = bucketize.cont(vals)
#'table(t)
#'v = bucketize.cont(vals, bucket.size = .5)
#'table(v)
#'vals <- c('A', 'B')
#'bucketize.cont(vals) # Throws error

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



