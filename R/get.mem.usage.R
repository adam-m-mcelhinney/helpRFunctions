#' Displays the memory usage for all the objects in memory for R.
#' 
#' @param sort Optional, defaults to name. If you select 'name', then it returns the objects in alphabetical order. If you select 'size', it returns the objects in descending order of size. 
#' @param min.display.value Optional, defaults to NA. This parameter represents the smallest object that you want to display in your result set. For example, if you have hundreds of objects, but many of them are very small and inconsequential, you may want to select to min.display.value such that those objects are not shown.
#' @return A vector containing the objects and their associated sizes.
#' @examples 
#' 
#'get.mem.usage() # defaults to not filtering any values and sorting the results alphabetically.
#'get.mem.usage(sort = 'size') 
#'get.mem.usage(sort = 'size', min.display.value = 500)
 

get.mem.usage <- function(sort = 'name', min.display.value = NA) {
	sort.values = c('name', 'size')
	if(!(sort  %in% sort.values)) {
		stop(paste(sort, ' is not an acceptable value for sort.'))
	}
	t = sapply(ls(globalenv()),function(x){object.size(get(x))})
	
	# Check for empty list
	if(length(t) == 0) return(t)
	
	# Filter out results below the minimum display value
	if(!is.na(min.display.value)){
		t = t[t>=min.display.value]
	}
	
	# Order the data
	if(sort != 'name'){
		t = sort(t, decreasing = TRUE)
	} else {
		t = t[order(names(t))]
	}
	return (t)
}