
#' One hot encoding
#' One hot encoding function is used to convert the factor variables into multiple columns
#' based on the unique values

#' @param data frame is taken
#' @return data frame with the one hot encoding applied to all the variables with type as factor
#' @export

oneHotEncoding =function(data){
  for(i in 1:ncol(data))
  {
    #check whether the variable is factor or not
    if(is.factor(data[,i])){
      #get the column name
      columnName=colnames(data)[i]
      #get the unique values in the variable
      list <- unique(data[,i])
      #Number of colunms to be created
      encodingColumns <-length(list)

     # len=ncol(data)
      #Create individual column for every unique value in the variable
      for(j in 1:encodingColumns){
        newColumnName<-(paste(columnName,"_",list[j],sep = ""))
        data[,newColumnName] <- ifelse(data[,i]==list[j] & !is.na(data[,i]),TRUE,FALSE)
      }

    }
  }
  return(data)
}




