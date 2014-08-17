pollutantmean <- function(directory="specdata", pollutant="sulfate", id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)

	files<-list.files(directory)
	setwd(directory)
	
	sonde <- data.frame()
	for (i in id){
		sonde <- rbind(sonde ,read.csv(files[i]))
	}
	mean_of_the_pollutant <- colMeans(sonde[pollutant],na.rm=TRUE)

	setwd("../")
	return(mean_of_the_pollutant)
}
