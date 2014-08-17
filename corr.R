corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	correlation <- vector(mode="numeric")
	source("complete.R")
	files<-list.files(directory)
	all_the_complete_data <- complete("specdata", 1:332)
	complete_data_over_threshold <- all_the_complete_data[all_the_complete_data$nobs>threshold,]
	
	setwd(directory)

	for (i in complete_data_over_threshold[,1]){
		sonde <- read.csv(files[i])
		## store in a logical vector the number of completed cases for sulfate and then for nitrate
		observed_cases_nitrate <- !is.na(sonde$nitrate)
		observed_cases_sulfate <- !is.na(sonde$sulfate)
		
		## make a logical test is nitrate & sulfate are observed
		completely_observed_cases <- sonde[observed_cases_nitrate & observed_cases_sulfate,]		
		number_of_completely_observed_cases <- sum(observed_cases_nitrate & observed_cases_sulfate)
		
		append_corr <- cor(completely_observed_cases$nitrate,completely_observed_cases$sulfate)
		correlation <- c(correlation,append_corr)
	}
	
	## go back to previous directory
	setwd("../")
	return(correlation)
	}
