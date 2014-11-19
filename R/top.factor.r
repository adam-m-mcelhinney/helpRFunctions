#' Return the top X factors and for the rest, group them together.
#' 
#' @param df The incoming data frame
#' @param factor.vars Optional. Defaults to all columns in the df with names. 
#' The factor variables for which you want to group together.
#' @param max.factors Optional. Defaults to 10. The maximum number of 
#' factors you want to have for a given variable.
#' @return A data frame with the factor.vars columns grouped by the max factors
#' @examples
#'df <- data.frame(cbind(sample(letters, 100, replace = TRUE))
#'		,cbind(sample(letters, 100, replace = TRUE))
#'		,cbind(sample(letters, 100, replace = TRUE)))
#'df[1,1] <- NA
#'df <- cbind(df, df[,1])
#'levels(df[,ncol(df)]) <- c(levels(df[,ncol(df)]), 'another factor value', 'blah')
#'df[nrow(df),ncol(df)] <- 'another factor value'
#'df[nrow(df)-1,ncol(df)] <- 'blah'
#'names(df) <- 1:ncol(df)
#'top.factors(df)
#'top.factors(df, max.factors = 1000)
#'top.factors(df, max.factors = 27)


top.factors <- function(df, factor.vars = names(df), max.factors = 10){
	for(i in factor.vars){
		# TODO: Add try-except logic here
			counts <- sort(table(df[,i]), decreasing = TRUE)
			# Catch the case where the number of max.factors is greater than the actual number of factors.
			n.fac <- length(counts)
			if(n.fac < max.factors) warning(paste0(i, ' only contains ', n.fac, ' factors.'))
			top <- names(counts)[1:min(max.factors, n.fac)]
			new.name = paste0(i,"_top")
			
			# Need to add the new factor level, otherwise error
			levels(df[,i]) <- c(levels(df[,i]), 'other')
			df[(df[,i] %in% top) == FALSE,i] <- 'other' 	
			}
	return(df)
}