pollutantmean <- function(directory, pollutant, id = 1:332){
  fileNames <- vector()
  data <- data.frame()
  for (i in id) {
    name <- nameById(i)
    filePath <- file.path(directory, name)
    csv <- read.csv(filePath, comment.char = "")
    data <- rbind(data, csv)
  }
  pollutantData <- data[, pollutant]
  pollutantData <- pollutantData[!is.na(pollutantData)]
  
  mean(pollutantData)
}

complete <- function(directory, id = 1:332){
  result <- data.frame()
  result <- rbind(result, c("id", "nobs"))
  for (i in id) {
    name <- nameById(i)
    filePath <- file.path(directory, name)
    csv <- read.csv(filePath, comment.char = "")
    nobs <- nrow(csv[complete.cases(csv), ])
    
    result<- rbind(result, c(i, nobs))
  }
  result
}

nameById <- function(id){
  if (id < 10) {
    name <- paste("00", id, ".csv", sep = "")
  }
  else if (id >= 10 & id < 100) {
    name <- paste("0", id, ".csv", sep = "")
  }
  else {
    name <- paste(id, ".csv", sep = "")
  }
  name
}