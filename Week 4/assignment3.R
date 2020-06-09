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

#Test: rankhospital("MN", "heart attack", 5000) # NA
#Test: rankhospital("MD", "heart attack", "worst")  # "HARFORD MEMORIAL HOSPITAL"
#Test: rankhospital("TX", "heart failure", 4) # "DETAR HOSPITAL NAVARRO"
rankhospital <- function(state, outcome, num = "best"){
  # get the outcome column from outcome 
  outcomeCol <- getOutcomeCol(outcome)
  
  # read the outcome data based on state 
  stateData <- readStateData(state, outcomeCol)
  ranking <- stateData[order(stateData$Rate, stateData$Hospital.Name), ]
  
  num <- convertNumToRankingFunc(num)(ranking)
  
  ranking[num, 1]
}

#head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)

rankall <- function(outcome, num = "best"){
  # get the outcome column from outcome 
  outcomeCol <- getOutcomeCol(outcome)
  hospitalNameCol <- 2
  stateCol <- 7
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcomeData <- outcomeData[, c(stateCol, hospitalNameCol, outcomeCol) ]
  Rate <- as.numeric(outcomeData[, 3])
  outcomeData <- cbind(outcomeData, Rate)
  outcomeData <- outcomeData[which(complete.cases(outcomeData) ), ]
  
  ranking <- outcomeData[order(outcomeData$State, outcomeData$Rate, outcomeData$Hospital.Name), ]
  
  split <- split(ranking, ranking$State)
  convertFunc <- convertNumToRankingFunc(num)
  rankingByState <- lapply(split, function(x) x[convertFunc(x), ])
  
  result <- data.frame()
  
  
  for (item in rankingByState) {
    result <- rbind(result, c(item$Hospital.Name, item$State))
  }
  colnames(result) <- c("hospital", "state")
  result
}

convertNumToRankingFunc <- function(num){
  convertFunc <- function(data){
    if (num == "best") {
      num <- 1
    } else if (num == "worst") {
      num <- nrow(data)
    } else if (!is.numeric(num)) {
      stop("invalid num")
    }
    num
  }
  convertFunc
}

convertNumToRanking <- function(num, data){
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(data)
  } else if (!is.numeric(num)) {
    stop("invalid num")
  }
  num
}