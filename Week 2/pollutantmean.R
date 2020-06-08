pollutantmean <- function(directory, pollutant, id = 1:332){
  data <- data.frame()
  files <- list.files(directory, full.names = T)
  for (i in id) {
    csv <- read.csv(files[i], comment.char = "")
    data <- rbind(data, csv)
  }
  pollutantData <- data[, pollutant]
  pollutantData <- pollutantData[!is.na(pollutantData)]
  
  mean(pollutantData)
}

complete <- function(directory, id = 1:332){
  result <- data.frame()
  files <- list.files(directory, full.names = T)
  for (i in id) {
    name <- nameById(i)
    csv <- read.csv(files[i], comment.char = "")
    nobs <- nrow(csv[complete.cases(csv), ])
    
    result<- rbind(result, c(i, nobs))
  }
  colnames(result )<-c("id", "nobs")
  result
}

corr <- function(directory, threshold = 0){
  result <- numeric()
  
  files <- list.files(directory, full.names = T)
  for (file in files) {
    data <- read.csv(file, comment.char = "")
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