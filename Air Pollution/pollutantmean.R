#Pollutant Mean Function
#Runs through a list of excel files and calculates the mean of a pollutant.

pollutantmean <- function(directory, pollutant, id = 1:332) {
	#Saves current working directory and changes new directory.
	oldwd <- getwd()
	setwd(directory)
	p_tot <- NULL
	
	#Loops over all the files specified.
	for (i in id){
	
		#Adds 00 or 0 for values not up to three digits.
		if (i < 10) {newi <- paste("00", i, sep="")}
		else if (i < 100) {newi <- paste("0", i, sep="")}
		else{newi <- i}
		
		#Reads file, removes NA, adds them up.
		file <- paste(newi, ".csv", sep = "")
		data <- read.csv(file)
		p_sum <- data[ ,pollutant]
		p_sum_clean <- p_sum[!is.na(p_sum)]
		p_tot <- c(p_tot, p_sum_clean)
	}
	#Switches back to old directory and prints the mean.
	setwd(oldwd)
	mean(p_tot)
}