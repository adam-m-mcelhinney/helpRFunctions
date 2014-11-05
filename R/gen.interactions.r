#' Generates interaction variables for the specified columns
#' 
#' @param df The incoming data frame
#' @param interaction.vars Optional. Defaults to the names of the data frame columns. The names of the variables for which to generate the interactions.
#' @param return.all Optional. Defaults to FALSE. If true, the functon returns all of the columns in the data frame, including the interactions. If false, the function only returns the interaction columns.
#' @return A new data frame with either all of the columns, or just the new interaction columns (depending on vale of return.all parameter).
#' @examples 

df <- data.frame(matrix(rnorm(100), nrow = 10))
df <- cbind(df, rep('A', nrow(df)))
gen.interaction(df)

gen.interaction <- function(df, interaction.vars = names(df), return.all = FALSE) {
	
	n <- nrow(df)
	# Check is the variables are numeric. If not, ignore them.
	k <- length(interaction.vars)
	int.vars <- c() # Store the numeric variables only.
	for(i in 1:k){
		var <- names(df)[i]
		if(!is.numeric(df[,i])){
			warning(paste0(var, ' is not numeric. Ignoring this variable.'))
			} else {
				int.vars <- c(int.vars, var)
			}	
	}
	
	m <- length(int.vars)
	# Initialize the data frame for the required size.
	int.df <- data.frame(matrix(rep(NA, n * n * m), nrow = n))
	int.names <- c(rep('', m * m))
	z = 1 # Counter
	for(i in 1:j){
		for(q in 1:j) {
			int.names[z] <- paste0(int.vars[i],'.', int.vars[q])
			int.df[,z] <- df[,i] * df[,q]
			z <- z + 1
		}			
	}
	names(int.df) <- int.names
	
	if(return.all == FALSE) {
		return(int.df)
	} else {
		return(cbind(df, int.df))
	}
	
	
}