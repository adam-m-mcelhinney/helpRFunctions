#' Removes highly correlated variables from your data frame. Ignores factor variables.
#' 
#' 
#' @param df The incoming data frame
#' @param verbose Optional. Defaults to FALSE. Is the verbose argument for the \href{http://cran.r-project.org/web/packages/caret/caret.pdf}{findCorrelation} function in caret.
#' @param cutoff Optional. Defaults to .75. Is the cutoff argument for the \href{http://cran.r-project.org/web/packages/caret/caret.pdf}{findCorrelation} function in caret.
#' @param use Optional. Defaults to pairwise.complete.obs. Is the use argument for the cor function.
#' @param print.removed.cols Optional. Defaults to TRUE. If TRUE, prints the names of the columns removed.
#' @return A new data frame with the highly correlated variables removed.
#' @examples 
#'df <- data.frame(matrix(rnorm(100), nrow = 10))
#'# Insert 10 random missing values
#'for (k in 1:10) {
#'  i = round(runif(n = 1, min = 0, max = 9))
#'  j = round(runif(n = 1, min = 0, max = 9))
#'  df[i, j] <- NA
#'}
#'df <- cbind(df, rep("A", nrow(df)))
#'df <- cbind(df, df[,1]) # TODO: This fails for this case! Name is null.
#'rmv.highly.correlated(df)


rmv.highly.correlated <- function(df, verbose = FALSE, cutoff = .75, use = "pairwise.complete.obs", print.removed.cols = TRUE) {
    if(!is.data.frame(df)) stop('Must provide data frame')
    varList <- list.df.var.types(df)
    # TODO: Ensure all the possible data types that cor function accepts are here
    if(is.null(varList$numeric) && is.null(varList$integer) 
			&& is.null(varList$double)) 
		stop("Data frame contains no numeric variables.")
    numeric.df <- df[,varList$numeric] # TODO: Fix this for the case of integers and doubles
    cor.matrix <- cor(numeric.df, use = use)
    highlyCorDescr <- findCorrelation(cor.matrix, cutoff = cutoff, verbose = verbose)
    cols.to.remove <- names(numeric.df[,highlyCorDescr]) # This is broke
    
    if (length(cols.to.remove) == 0){
        if(print.removed.cols == TRUE) warning('No columns removed')
        return(df)  
    } else {
            if(print.removed.cols == TRUE) warning(paste0('Columns removed: ', cols.to.remove))
            new.df <- df[, -which(names(df) %in% cols.to.remove)]
            return (new.df)
    }
}


