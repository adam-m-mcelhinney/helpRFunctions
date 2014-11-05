#' Splits an email address, or vector of email address, of the form local-part@@domain, into the local and domain pieces.
#' 
#' @param emails An email address, or vector of email addresses.
#' @return A list containing the elements local and domain.
#' @examples 
#' 
#' emails = c('test323@@gmail.com', 'not an email address', 'test2@@email.org')
#' e = parse.email(emails)
#' e$local
#' e$domain
#' e = parse.email('test323@@gmail.com')
#' e$local
#' e$domain
#' 

parse.email <- function(emails) {
	email.list <- strsplit(emails, split = '@')
	# Preallocate the vectors
	n <- length(email.list)
	local <-  vector(length = n)
	domain <-  vector(length = n)
	for (i in 1:n){
		local[i] <- email.list[[i]][1]
		domain[i] <- email.list[[i]][2]
	}
	l <- list(local, domain)
	names(l) <- c('local', 'domain')
	return(l)
}


