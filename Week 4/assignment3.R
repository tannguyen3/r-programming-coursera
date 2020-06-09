## 11 is death rate from "heart attack"
## 17 is death rate from "heart failure"
## 23 is death rate from "pneumonia"
getOutcomeCol <- function(outcome ){
  if (outcome == "heart attack") {
    outcomeCol <- 11
  } else if (outcome == "heart failure") {
    outcomeCol <- 17
  } else if (outcome == "pneumonia") {
    outcomeCol <- 23
  } else {
    stop("invalid outcome, should be either 'heart attack', 'heart failure' or 'pneumonia'")
  }
  outcomeCol
}

## read data of the state based, on a outcome col, also remove the uncomplete cases
readStateData <- function(state, outcomeCol){
  hospitalNameCol <- 2
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stateData <- outcomeData[which(outcomeData$State == state), c(hospitalNameCol, outcomeCol) ]
  if (nrow(stateData) == 0) {
    stop("invalid state")
  }
  Rate <- as.numeric(stateData[, 2])
  stateData <- cbind(stateData, Rate)
  stateData <- stateData[which(complete.cases(stateData) ), ]
  stateData
}

best <- function(state, outcome){
  # get the outcome column from outcome 
  outcomeCol <- getOutcomeCol(outcome)
  
  # read the outcome data based on state 
  stateData <- readStateData(state, outcomeCol)
  lowestRate <- min(stateData[which(!is.na(stateData[, "Rate"])), "Rate"] )
  lowestRateFinding <- which(stateData[, "Rate"] == lowestRate)
  lowestRateHospitals <- stateData[lowestRateFinding, 1]
  sort(lowestRateHospitals)[1]
}

#Test: rankhospital("MN", "heart attack", 5000) => NA
#Test: rankhospital("MD", "heart attack", "worst")  => "HARFORD MEMORIAL HOSPITAL"
#Test: rankhospital("TX", "heart failure", 4) => "DETAR HOSPITAL NAVARRO"
rankhospital <- function(state, outcome, num = "best"){
  # get the outcome column from outcome 
  outcomeCol <- getOutcomeCol(outcome)
  
  # read the outcome data based on state 
  stateData <- readStateData(state, outcomeCol)
  ranking <- stateData[order(stateData$Rate, stateData$Hospital.Name), ]
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(ranking)
  } else if (!is.numeric(num)) {
    stop("invalid num")
  }
  
  ranking[num, 1]
}