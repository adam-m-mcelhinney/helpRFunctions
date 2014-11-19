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
#' 



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
  
  # Calculate the cummulative number of observations for each specified bucket
  cuts <- sapply(cumsum(pcts) * n, floor)
  set.seed(seed); sampled.rows <- sample(n, n); 
  data.sets <- list()
  for (i in 1:k){
    if(i == 1){
        start.cut <- 1
    } else {
        start.cut <- cuts[i-1] +1
    }
    
    selected.rows <- sampled.rows[start.cut: cuts[i]]
    l <- list(df[selected.rows,])
    names(l) <- set.names[i]
    data.sets <- c(data.sets, l)
                }
  # Verify the proper number of rows are returned
  rows <- rep(NA, length(data.sets))
  q = 1
  for (i in t){
    rows[q] <- nrow(i)
    q <- q+1
  }
  
  if(sum(rows) != nrow(df)) warning(paste0('The number of rows in the input data set was ', nrow(df), ' but the number of rows in the sampled data sets is ', sum(rows)))
  
  return(data.sets)
}
