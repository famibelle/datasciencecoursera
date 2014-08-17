complete <- function(directory="specdata", id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	files<-list.files(directory)
	setwd(directory)
	
	## create an empty data.frame
	complete_cases <- data.frame()
	
	for (i in id){
		sonde <- read.csv(files[i])
		## store in a logical vector the number of completed cases for sulfate and then for nitrate
		nombreobservation_nitrate <- !is.na(sonde$nitrate)
		nombreobservation_sulfate <- !is.na(sonde$sulfate)
		
		## make a logical test is nitrate & sulfate are observed
		nombreobservation <- nombreobservation_nitrate & nombreobservation_sulfate
		
		## the number of TRUE give the number of observations
		nobs <- sum(nombreobservation)
		
		# add the result to the data.frame
		complete_cases <- rbind(complete_cases,c(i,nobs))
	}
	## names the columns
	names(complete_cases)<-c("id","nobs")
	
	## go back to previous directory
	setwd("../")

	## Return a data frame of the form:
	## id nobs## 1  117## 2  1041## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	return(complete_cases)
}
	## example complete("specdata", c(2, 4, 8, 10, 12))
	##   id nobs
	## 1  2 1041
	## 2  4  474
	## 3  8  192
	## 4 10  148
	## 5 12   96
	##complete("specdata", 1)
	##   id nobs
	## 1  1  117
