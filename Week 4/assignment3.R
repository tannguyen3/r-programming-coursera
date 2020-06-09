## 11 is death rate from "heart attack"
## 17 is death rate from "heart failure"
## 23 is death rate from "pneumonia"

best <- function(state, outcome){
  # verify outcome variable 
  outcomeCol <- NULL
  hospitalNameCol <- 2
  if (outcome == "heart attack") {
    outcomeCol <- 11
  } else if (outcome == "heart failure") {
    outcomeCol <- 17
  } else if (outcome == "pneumonia") {
    outcomeCol <- 23
  } else {
    stop("invalid outcome, should be either 'heart attack', 'heart failure' or 'pneumonia'")
  }
  
  # read the outcome data 
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  stateData <- outcome[which(outcome$State == state), c(hospitalNameCol, outcomeCol) ]
  
  if (nrow(stateData) == 0) {
    stop("invalid state")
  }
  deathRate <- as.numeric(stateData[, 2])
  stateData <- cbind(stateData, deathRate)
  lowestRate <- min(deathRate[!is.na(deathRate)])
  
  lowestRateFinding <- which(stateData[, 3] == lowestRate)
  lowestRateHospitals <- stateData[lowestRateFinding, 1]
  sort(lowestRateHospitals)[1]
}