complete <- function(directory, id = 1:332) {
  ## Test

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  for(i in id)
  {
    filename <- sprintf("%s/%03d.csv", directory, i)
    ## print(a)
    tmp<-read.csv(filename, header = TRUE)
    
    tmp_filtered <-!is.na(tmp[2]) & !is.na(tmp[3])
    if (exists("dataset")){
      dataset<-rbind(dataset, c(i, sum(tmp_filtered)))
    }
    else
    {
      dataset<-data.frame(i, sum(tmp_filtered))
      names(dataset) <- c("id", "nobs")
    }
    
    rm(tmp_filtered)
    rm(tmp)
    
  }
  
  dataset
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}
