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
  for (i in id) {
    name <- nameById(i)
    filePath <- file.path(directory, name)
    csv <- read.csv(filePath, comment.char = "")
    nobs <- nrow(csv[complete.cases(csv), ])
    
    result<- rbind(result, c(i, nobs))
  }
  colnames(result )<-c("id", "nobs")
  result
}

corr <- function(directory, threshold = 0){
  result <- numeric()
  
  files <- dir(directory)
  for (file in files) {
    filePath <- file.path(directory, file)
    data <- read.csv(filePath, comment.char = "")
    data <- data[complete.cases(data), ]
    rows <- nrow(data)
    
    if (rows > threshold) {
        sulfate <- data[, 'sulfate']
        nitrate <- data[, 'nitrate']
        cor <- cor(sulfate, nitrate, use  = "complete.obs")
        result <- c(result, cor)
    }
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