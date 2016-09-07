finder <- function(data, col, state) {
  #Creates an array from the desired state and outcome
  state_mat <- data[data[,7] == state, ]
  final_arr <- state_mat[, col]
  #Finds the minimum value and gives hospital with min value
  min <- min(final_arr, na.rm = T)
  ind <- which(final_arr == min)
  hosp <- state_mat[ind, 2]
  return(hosp)
}

best <- function(state, outcome) {
  #Reads the data file as characters in working directory
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Converts the data type to numeric
  data[,11] <- as.numeric(data[,11])
  data[,17] <- as.numeric(data[,17])
  data[,23] <- as.numeric(data[,23])
  
  #Valid Outcome Array
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  
  #Checks to see if state and outcome are valid
  if (!(state %in% data$State)) {
    print("invalid state")
  }
  else if (!(outcome %in%  valid_outcome)) {
    print("invalid outcome")
  }
  else {
    if (outcome == "heart attack") {
      hosp <- finder(data, 11, state)
    }
    else if (outcome == "heart failure") {
      hosp <- finder(data, 17, state)
    }
    else {
      hosp <- finder(data, 23, state)
    }
  return(hosp)
  }
}