#Complete Function
#Runs through a list of excel files and prints the complete cases for each ID.

complete <- function(directory, id = 1:332) {
	#Saves current working directory and changes new directory.
	oldwd <- getwd()
	setwd(directory)
	
	#Initializes nobs vector for data frame.
	nobs <- NULL
	
	#Loops over all the files specified.
	for (i in id){
		#Sets initial count to 0.
		count <- 0
	
		#Adds 00 or 0 for values not up to three digits.
		if (i < 10) {newi <- paste("00", i, sep="")}
		else if (i < 100) {newi <- paste("0", i, sep="")}
		else{newi <- i}
		
		#Reads files and saves sulfate and nitrate to vectors.
		file <- paste(newi, ".csv", sep = "")
		data <- read.csv(file)
		p_sul <- data[ ,"sulfate"]
		p_nit <- data[ ,"nitrate"]
		
		#Checks condition and adds count
		for(j in 1:length(p_sul)){
			if (!(is.na(p_sul[j])) & !(is.na(p_nit[j]))){
				count <- count + 1
			}
		}
		#Adds counts for each id to nobs vector.
		nobs <- c(nobs, count)
		
	}
	#Switches back to old directory and prints data frame.
	setwd(oldwd)
	print(data.frame(id, nobs))
}