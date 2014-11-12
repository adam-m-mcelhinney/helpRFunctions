#' Generates interaction or quadratic term variables for the specified columns
#' 
#' @param df The incoming data frame
#' @param interaction.vars Optional. Defaults to the names of the data frame columns. The names of the variables for which to generate the interactions.
#' @param return.all Optional. Defaults to FALSE. If true, the functon returns all of the columns in the data frame, including the interactions. If false, the function only returns the interaction columns.
#' @param square.term Optional. Defaults to TRUE. If true, the function will return both interaction terms and square terms. If false, only interaction terms will be returned. If 'only', only square terms will be returned.
#' @return A new data frame with either all of the columns, or just the new interaction columns (depending on vale of return.all parameter).
#' @examples 
#' df <- data.frame(matrix(rnorm(100), nrow = 10))
#' df <- cbind(df, rep('A', nrow(df)))
#' df[1,1] <- NA
#' gen.interaction(df)
#' gen.interaction(df, interaction.vars = c("X1", "X2"), square.term=TRUE)
#' gen.interaction(df, interaction.vars = c("X1", "X2"), square.term=FALSE)
#' gen.interaction(df, interaction.vars = c("X1", "X2"), square.term='only')
#' gen.interaction(df, interaction.vars = c("X1", "X2"), return.all = TRUE)

gen.interaction <- function(df, interaction.vars = names(df), return.all = FALSE, square.term=c(TRUE, FALSE,'only')) {
  
  orig <- df # Save the original data frame somewhere
  df <- df[,interaction.vars] # Only get the columns we are interested in
  
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
  
  df <- df[,int.vars] # Get rid of the variables determined to be non-numeric
  w = length(int.vars)
  z = 1 # Counter
  if(square.term==T) {
    m <- sum(1:w)
    # Initialize the data frame for the required size.
    int.df <- data.frame(matrix(rep(NA, n * m), nrow = n))
    int.names <- c(rep('', m))
    for(i in 1:w){   
        for(q in i:w) {
          int.names[z] <- paste0(int.vars[i],'.', int.vars[q])
          int.df[,z] <- df[,i] * df[,q]
          z <- z + 1
        }			
      }
  }else if (square.term==F){
    m <- sum(1:w)-w
    # Initialize the data frame for the required size.
    int.df <- data.frame(matrix(rep(NA, n * m), nrow = n))
    int.names <- c(rep('', m))
    for(i in 1:(w-1)){   
        for(q in (i+1):w) {
          int.names[z] <- paste0(int.vars[i],'.', int.vars[q])
          int.df[,z] <- df[,i] * df[,q]
          z <- z + 1
        }  		
      }
  }else {
    m <- w
    # Initialize the data frame for the required size.
    int.df <- data.frame(matrix(rep(NA, n * m), nrow = n))
    int.names <- c(rep('', m))
    for(i in 1:w){    
      int.names[z] <- paste0(int.vars[i],'.', int.vars[i])
      int.df[,z] <- df[,i] * df[,i]
      z <- z + 1 		
    }    
  }
  names(int.df) <- int.names
  
  if(return.all == FALSE) {
    return(int.df)
  } else {
    return(cbind(orig, int.df))
  }
  
  
}
