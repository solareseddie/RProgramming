num_helper <- function(state_sub, col, num) {
  outcome_arr <- as.numeric(state_sub[ , col])
  len <- dim(state_sub[!is.na(outcome_arr), ])[1]
  if (num == "best") {
    rank <- rank_helper(state_sub, outcome_arr, 1)
  } else if (num == "worst") {
    rank <- rank_helper(state_sub, outcome_arr, len)
  } else if (num > len) {
    rank <- NA
  } else {
    rank <- rank_helper(state_sub, outcome_arr, num)
  }
  return(rank)
}

rank_helper <- function(state_sub, outcome_arr, num) {
  getrank <- state_sub[ ,2][order(outcome_arr, state_sub[ ,2])[num]]
  return(getrank)
}

rankall <- function(outcome, num = "best") {
  ##Reads the data file as character files
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Creates vector for valid outcomes
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  
  ##Creates an array of every state, and an array with the length
  state_array <- sort(unique(data$State))
  state_len <- length(state_array)
  hospital <- rep("", state_len)
  
  ##For loop for filling hospital array
  if (!(outcome %in% valid_outcome)) {
    stop("invalid outcome")
  } else {
    for (i in 1:state_len) {
      state_sub <- data[data[ ,7] == state_array[i], ]
      if (outcome == "heart attack") {
        hospital[i] <- num_helper(state_sub, 11, num)
      } else if (outcome == "heart failure") {
        hospital[i] <- num_helper(state_sub, 17, num)
      } else {
        hospital[i] <- num_helper(state_sub, 23, num)
      }
    }
  }
  ##Creates and prints data frame
  dframe <- data.frame(hospital = hospital, state = state_array)
  print(dframe)
}