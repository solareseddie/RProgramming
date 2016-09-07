#Correlation Function
#Runs through a list of excel files printing out correlation between 
#sulfate and nitrate for all observed cases above threshold value.

corr <- function(directory, threshold = 0) {
	#Saves current working directory and changes new directory.
	oldwd <- getwd()
	setwd(directory)
	
	corr_values <- NULL
	
	#Loops through all files.
	for(i in 1:332){
		#Resets count and vectors.
		count <- 0
		cval <- NULL
		obs_vector <- NULL
		sul_vect <- NULL
		nit_vect <- NULL
		
		#Adds 00 or 0 for values not up to three digits.
		if (i < 10) {newi <- paste("00", i, sep="")}
		else if (i < 100) {newi <- paste("0", i, sep="")}
		else{newi <- i}
		
		#Reads files and saves sulfate and nitrate to vectors.
		file <- paste(newi, ".csv", sep = "")
		data <- read.csv(file)
		p_sul <- data[ ,"sulfate"]
		p_nit <- data[ ,"nitrate"]
		
		#Checks condition, adds count, and attaches vectors.
		for(j in 1:length(p_sul)){
			if (!(is.na(p_sul[j])) & !(is.na(p_nit[j]))){
				count <- count + 1
				obs_vector <- c(obs_vector, j)
			}
		}
		
		#Checks for threshold condition, saves relevant vectors, and correlates.
		if(count > threshold){
			for(k in obs_vector){
				sul_vect <- c(sul_vect, p_sul[k])
				nit_vect <- c(nit_vect, p_nit[k])
				cval <- cor(sul_vect, nit_vect)
			}
		}
		
		corr_values <- c(corr_values, cval)
	}
	#Switches back to old directory and sends correlation vector.
	setwd(oldwd)
	corr_values
}