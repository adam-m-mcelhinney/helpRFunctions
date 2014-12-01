#' Imports all the csv's in a given directory.
#' 
#' 
#' @param ... Accepts all of the arguments for read.csv and list.files.
#' @return All of the csv files imported 
#' @examples 
#' \dontrun{
#' data1 <- data.frame(1:10)
#' data2 <- data.frame(10:15)
#' write.csv(data1, file = 'data1.csv')
#' write.csv(data2, file = 'data2.csv')
#' read.csv.dir()
#' }

read.csv.dir <- function(...){
  # TODO: Expand this program to handle any times of reads, as well as source()/
  # TODO: Add some type of logging and basic validation of the data
  files <- list.files(..., pattern = "*.csv", full.names = TRUE)
  for (i in files){
    print(paste0("Importing ",i))
    try(read.csv(i, ...))
                 }
  }




