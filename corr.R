corr <- function(directory, threshold = 0, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  for(i in id)
  {
    filename <- sprintf("%s/%03d.csv", directory, i)
    ## print(a)
    tmp<-read.csv(filename, header = TRUE)
    
    tmp_filtered <-tmp[!is.na(tmp[2]) & !is.na(tmp[3]),]
    
    tmp_filtered2 <-!is.na(tmp[2]) & !is.na(tmp[3])
    ##print(tmp_filtered)
    if(sum(tmp_filtered2)>threshold)
      {
      if (exists("dataset")){
        dataset<-rbind(dataset,cor(tmp_filtered[2],tmp_filtered[3]))
      }
      else
      {
        dataset<-cor(tmp_filtered[2],tmp_filtered[3])
      }
    }
    rm(tmp_filtered)
    rm(tmp)
    
  }
  if (exists("dataset"))
  dataset
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}