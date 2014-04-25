
#utilities for handling date and time
#library(lubridate)

nextWeek<-function(date,Nweek=1)
  return(date+weeks(Nweek))


#return the index of the columns that are Date
getColDateIdx<-function(X){
  if(!is.data.frame(X))
    cat("WARNINGS dates in matrix are converted as character therefore are not detected \n")
  
  if(is.vector(X))
    stop("vector not supported")
  
  colDate<-rep(FALSE,ncol(X))
  for(i in 1:ncol(X)){
    if(is.Date(X[,i]))
      colDate[i]<-TRUE
  }
  return(which(colDate==T))
}

#check if any column of X is of class Date
hasColDate<-function(X){
  
  if(is.null(X)) 
    return(FALSE)
  
  if(is.vector(X))
    return(is.Date(X))
  
  if(!is.data.frame(X))
    cat("WARNINGS dates in matrix are converted as character therefore are not detected \n")
  else{
    # X is a data.frame
    return(any(sapply(X,is.Date)))
  }
}


is.Date<-function(x){
  if(any(class(x)=="Date") | any(class(x)=="POSIXct") | any(class(x)=="POSIXt"))
    return(TRUE)
  else return(FALSE)
}


# d<-which(colnames(Xts)=="TX_DATE")
# # Xts[,d]<-factorCleanup(Xts[,d]) #delete unused levels
# # Xts[,d]<-as.Date(Xts[,d], "%d/%m/%Y")
# # TSdaysRange<-as.numeric(max(Xts[,d])-min(Xts[,d]))
# # NmonthsTS<-floor(TSdaysRange/30)
# 
# 
# TR_DATE<-X[,d]
# month<-as.numeric(substr(TR_DATE, 4, 5))
# monthsTR<-sort(unique(month))
# NmonthsTR<-length(monthsTR)
# X<-cbind(X,month)
# 
# TS_DATE<-Xts[,d]  
# month<-as.numeric(substr(TS_DATE, 4, 5))
# monthsTS<-sort(unique(month))
# NmonthsTS<-length(monthsTS)
# Xts<-cbind(Xts,month)