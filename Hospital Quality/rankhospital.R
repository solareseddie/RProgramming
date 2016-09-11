get_rank <- function(data, col, state, num) {
  ##Creates an array from the desired state and outcome
  state_mat <- data[data[,7] == state, ]
  final_arr <- state_mat[, col]
  
  ##Calculates length without NA
  len <- dim(state_mat[!is.na(final_arr), ])[1]
  
  ##Finds and returns rank to rankhospital
  if (num == "best") {
    rank <- sorter(state_mat, final_arr, 1)
  }
  else if (num == "worst") {
    rank <- sorter(state_mat, final_arr, len)
  }
  else if (num > len) {
    rank <- NA
  }
  else {
    rank <- sorter(state_mat, final_arr, num)
  }
  return(rank)
}

sorter <- function(state_mat, final_arr, num) {
  ##Sorts out the hospital for desired rank
  rank <- state_mat[, 2][order(final_arr, state_mat[, 2])[num]]
  return(rank)
}

rankhospital <- function(state, outcome, num = "best") {
  #Reads the data file as characters in working directory
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Converts the data type to numeric
  data[,11] <- as.numeric(data[,11])
  data[,17] <- as.numeric(data[,17])
  data[,23] <- as.numeric(data[,23])

  ##Creates vector for valid outcomes
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    
  ##Check that state and outcome are valid, then returns rank
  if (!(state %in% data$State)) {
    print("invalid state")
  }
  else if (!(outcome %in% valid_outcome)) {
    print("invalid outcome")
  }
  else {
    if (outcome == "heart attack") {
      rank <- get_rank(data, 11, state, num)
    }
    else if (outcome == "heart failure") {
      rank <- get_rank(data, 17, state, num)
    }
    else {
      rank <- get_rank(data, 23, state, num)
    }
  }
  return(rank)
}