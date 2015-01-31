rankhospital <- function(state = "AK", 
                         outcome = "heart attack", 
                         num = "best"){
  data1 <- read.csv("outcome-of-care-measures.csv", 
                    colClasses = "character")
  data2 <- data1[, c(2, 7, 11, 17, 23)]
  colnames(data2) <- c("Hospital.Name", "State", "heart attack", 
                       "heart failure", "pneumonia")
  
  if(any(state == data2[,"State"]) == FALSE){
    stop('invalid state')
    
  }
  if(any(outcome == colnames(data2[,3:5])) == FALSE){
      stop('invalid outcome')
      
    }
  data3 <- data2[,c("Hospital.Name", "State", outcome)]
  colnames(data3) <- c("Hospital.Name","State","Rate")
  data3[,3] <- suppressWarnings(as.numeric(data3[,3]))
  data3 <- na.omit(data3)
  data4 <- data3[data3$State == state,]
  datarows <- nrow(data4)
  data5 <- data4[order(data4[,3], data4[,1]), ]
  data6 <- cbind(data5$Hospital.Name, data5$Rate, 1:datarows)
  data6 <- data.frame(data6)
  data6[,1] <- suppressWarnings(as.character(data6[,1]))
  data6[,3] <- suppressWarnings(as.numeric(as.character(data6[,3])))
  colnames(data6) <- c("Hospital.Name","Rate","Rank")
  
  
  if(num == "best"){
    brank <- min(data6[,3])
    BH <- data6[which(data6[,3] == brank), "Hospital.Name"]
    return(BH)
    }
  else{
    if(num == "worst"){
      wrank <- max(data6[,3])
      BH <- data6[which(data6[,3] == wrank), "Hospital.Name"]
      return(BH)
    }
    else{
      if(datarows < num){
        return(NA)
      }
      else{
        BH <- data6[which(data6[,3] == num), "Hospital.Name"]
        return(BH)
      }
    }
  }
}
      
      
  
    

