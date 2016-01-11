pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  if(pollutant == "sulfate")
    columNR <- 2
  else if(pollutant == "nitrate")
    columNR <- 3
  else
    columNR <- -1
  ##error
  
  for(i in id)
  {
    filename <- sprintf("%s/%03d.csv", directory, i)
   ## print(a)
    tmp<-read.csv(filename, header = TRUE)
    
    tmp_filtered <-tmp[!is.na(tmp[pollutant]),]
    if (exists("dataset")){
      dataset<-rbind(dataset, tmp_filtered)
      rm(tmp_filtered)
    }
    else
    {
      dataset <-tmp_filtered
    }
    ##print(dim(tmp_filtered)[1])
  }
  ##print(dataset)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  pollution <- dataset[pollutant]
  ##print(class(pollution))
  ##print(pollution[,1])
  mean(pollution[,1])
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}