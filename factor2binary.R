
#transform factor variables with "l" levels into "l" binary variable
factor2binary=function(factor,factorName){
		binaryData=NULL
 		lev<-levels(factor)
 		for (i in 1:length(lev)) {
 			binaryVar<-as.numeric(factor==lev[i])
 			is.NA=which(is.na(binaryVar)==T)
 			binaryVar[is.NA]=0
 			binaryData<-cbind(binaryData,binaryVar)	
 			}
  		for(i in 1:length(lev))
 			colnames(binaryData)[i]<-paste(factorName,lev[i],sep="_")
		return(binaryData)
	}


