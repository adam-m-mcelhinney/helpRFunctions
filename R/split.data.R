#' Divides the data into any specified number of buckets with specified names. 

#' @param df The data frame to split
#' @param pcts Optional. The percentage of observations to put into each bucket.
#' Extra observations due to rounding are placed in the last bucket.
#' @param set.names Optional. What to name the resulting data sets. This must
#' be the same length as the pcts vector.
#' @param seed Optional. Define a seed to use for sampling. Defaults to NULL 
#' which is just the normal random number generator in R. 
#' @return A list containing data frames named according to the set.names 
#' argument.
#' @examples 
#' df <- data.frame(matrix(rnorm(110), nrow = 11))
#' t <- split.data(df)
#' training <- t$training
#' testing <- t$testing
#' # A big example
#' \dontrun{
#' df <- data.frame(matrix(rnorm(110*1000000), nrow = 11*1000000))
#' system.time(split.data(df))
#' }



split.data <- function(  df
                        , pcts = c(.6, .3, .1)
                        , set.names = c('training', 'testing', 'validation')
                        , seed = NULL
                        ){
  if(!is.data.frame(df)) stop('Must provide data frame')
  if(sum(pcts) != 1) stop('The percentages specified do not sum to 1.')
  if(length(pcts) != length(set.names)) stop('The set.names and pcts are not the same length.')
  
  k = length(pcts)
  n = nrow(df)
  
  # Calculate the number of observations for each specified bucket
  qty <- sapply(pcts * n, floor)
  # Add any letover observations to the last bucket
  if (sum(qty) != n) qty[k] <- qty[k] + (n - sum(qty))
  
  # Assign the groups
  ind <- rep(set.names, qty)
  set.seed(seed); sampled.sets<- sample(ind);
  
  df['sampled.group'] <- sampled.sets
                  
  return(df)
  }
