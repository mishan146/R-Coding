

pollutantmean<- function(directory, pollutant, id=1:332){
  
  localmeans<-c()
  
  for(i in id){
  currentPath=paste(getwd(),"/",directory, "/", sprintf("%03d",i),".csv", sep="")
  checkingData<-read.csv(currentPath)
  localmeans<-c(localmeans,  checkingData[pollutant]
                [!is.na(checkingData[pollutant])])
  
  }
  mean(localmeans)
}

complete<-function(directory, id=1:332){
  
  ##'directory' is a character vector of length 1 indicating
  ##the location of the CSV files
  
  nobs<-c()
  for(i in id){
    currentPath=paste(getwd(),"/",directory, "/", sprintf("%03d",i),".csv", sep="")
    checkingData<-read.csv(currentPath)
    totalCC<-complete.cases(checkingData)
    CCD<-checkingData[totalCC,]
    nobs<-c(nobs, nrow(CCD) )
    
  }
  completeDataCases <- data.frame(id, nobs)
  completeDataCases
}

corr<-function(directory, threshold=0){
  cor_results <- numeric(0)
  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
  
  if(nrow(complete_cases)>0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      #print(path)
      monitor_data <- read.csv(path)
      #print(monitor_data)
      
      totalCC<-complete.cases(monitor_data)
      CCD<-monitor_data[totalCC,]
      
      sulfate_data <- CCD["sulfate"]
      nitrate_data <- CCD["nitrate"]
      cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
    }
  }
  cor_results
  
  
  
  
}



