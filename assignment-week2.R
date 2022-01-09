
pollutantmean <- function(directory, pollutant, id=1:332) {
  if (directory == "specdata")
    directory = "/Users/sudhanshu/Downloads/specdata/"
  mean_pollutant <- c()
  spec_data_files <- as.character(list.files(directory))
  spec_data_paths <- paste(directory, spec_data_files, sep = "")
  for (i in id) {
    curr_file <- read.csv(spec_data_paths[i])
    removed_na <- curr_file[!is.na(curr_file[pollutant]), pollutant]
    mean_pollutant <- c(mean_pollutant, removed_na)
  }
  return(mean(mean_pollutant))
}

pollutantmean(directory="/Users/sudhanshu/Downloads/specdata/", pollutant = "sulfate", id=1:10)

complete <- function(directory, id=1:332) {
  if (directory == "specdata")
    directory = "/Users/sudhanshu/Downloads/specdata/"
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  spec_data_files <- as.character(list.files(directory))
  spec_data_paths <- paste(directory, spec_data_files, sep="")
  for(i in id) {
    curr_file <- read.csv(spec_data_paths[i])
    curr_file <- curr_file[(!is.na(curr_file$sulfate)),]
    curr_file <- curr_file[(!is.na(curr_file$nitrate)),]
    nobs <- nrow(curr_file)
    results <- rbind(results, data.frame(id=i, nobs=nobs))
  }
  results
}

corr <- function(directory, threshold=0) {
  if (directory == "specdata")
    directory = "/Users/sudhanshu/Downloads/specdata/"
  completed_cases <- complete(directory = directory)
  completed_cases <- completed_cases[(completed_cases$nobs >= threshold),]
  result <- numeric(0)
  if (nrow(completed_cases) > 0) {
    for(id in completed_cases$id) {
      path <- paste(directory, sprintf("%03d", id), ".csv", sep="")
      data <- read.csv(path)
      data <- data[(!is.na(data$sulfate)),]
      data <- data[(!is.na(data$nitrate)),]
      result <- c(result, cor(data$sulfate, data$nitrate))
    }
  }
  result
}

val <- corr(directory = "/Users/sudhanshu/Downloads/specdata/")
#print(val)

#data <- complete(directory = "/Users/sudhanshu/Downloads/specdata/", 1:10)
#print(data)


