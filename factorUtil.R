#return the index of logicals of a dataset
is.logical=function(X){
  id.logical=which(sapply(X,class)=="logical")
  return(id.logical)
}

#return the index of the factors of a dataset
is.fac=function(X){
	id.fac=which(sapply(X,class)=="factor")
	return(id.fac)
	}

#return the index of the non-factors variables of a dataset
not.fac<-function(X){
	not.fac<-setdiff(1:ncol(X),is.fac(X))
	return(not.fac)
	}

#return the index of the numerical varibles of a dataset
is.num=function(X){
	id.fac=which(sapply(X,class)=="factor")
	id.num=setdiff(1:ncol(X),id.fac)
	return(id.num)
	}
	
#return the index of binary variables
is.bin=function(X){
	id.fac=is.fac(X)
	id.bin=id.fac[which(lapply((lapply(X[,id.fac],FUN=levels)),FUN=length)==2)]
	return(id.bin)
	}

#return the index of factors that are not binary variables
factorNotBin=function(X){
	id.fac=is.fac(X)
	id.bin=is.bin(X)
	id=setdiff(id.fac,id.bin)
	return(id)
}

#Drop unused factor levels from all factors in a data.frame
drop.levels <- function(dat){
	dat[] <- lapply(dat, function(x) x[,drop=TRUE])
	return(dat)
 }
 
 
#return the number of levels of a factor
n.levels=function(x){
	if(!is.factor(x)) stop("x not a factor")
	n.lev<-length(levels(x))
	return(n.lev)
	}


plotFactor<-function(x,name,...){
	if(!is.factor(x)) stop("x not a factor")
	tabSum <- table(x)
	barplot(tabSum,main=name,names.arg='',...)
}


#transform factor variables with "l" levels into "l" binary variable
factor2binary=function(factor,factorName){
		binaryData=NULL
 		lev<-levels(factor)
 		for (i in 1:length(lev)) {
 			binaryVar<-as.numeric(factor==lev[i])
			#if there are NA then set the value to zero
 			is.NA=which(is.na(binaryVar)==T)
 			binaryVar[is.NA]=0
 			binaryData<-cbind(binaryData,binaryVar)	
 			}
  		for(i in 1:length(lev))
 			colnames(binaryData)[i]<-paste(factorName,lev[i],sep="_")
		return(binaryData)
	}
	
#clean up factor variables after subset-ing, remove the levels unused
factorCleanup <- function(x) {
	if (is.factor(x)) {
		return(as.factor(as.character(x)))
	}
	if (is.data.frame(x) || is.matrix(x)) {
		for (i in 1:ncol(x)) {
			if(is.factor(x[,i])) { x[,i] <- as.factor(as.character(x[,i])) }
		}
		return(as.data.frame(x))
	}
}



#x is a factor vector
weighted.voting<-function(x,weights){
	if(is.factor(x)) stop("x is factor")
  if(round(sum(weights))!=1){ 
    cat("weights:",weights,"\n sum(weights):",sum(weights),"\n")
    stop("sum of weights must be 1")
  }
	l<-unique(x)
	if(!all(l %in% c(0,1))) stop("only for binary problems returning 0 or 1")
	i.1<-which(x==1)
	i.0<-which(x==0)
	#transfor 0 class in -1
	x[i.0]<- -1
	score<-x*weights
	if(sum(score)>=0) class=1
	else class=0
	return(class)	
}





#check if there are new levels in the testing set and in case add it to the training
#return the traininig with the added levels
addTestLevel=function(Xfac,Xtsfac){
	levelsListXfac=list()
	id.fac=factorNotBin(Xfac)
	
	i=0
	for(j in id.fac){
		i=i+1
		levelsListXfac[[i]]=levels(Xfac[,j])
		}
	colname=colnames(Xfac)[id.fac]
	names(levelsListXfac)=colname
	i=0
	for(j in id.fac){
		i=i+1
		jlevels=levels(Xtsfac[,j])
		newLevel=setdiff(jlevels,levelsListXfac[[i]])
		#if there is a new level in the test set
		if (length(newLevel)>0) {
			levelsListXfac[[i]]=c(levelsListXfac[[i]],newLevel)
			# add new level to j column
			Xfac[,j]<- factor(Xfac[,j], levels = c(levels(Xfac[,j]), newLevel))
			print(paste("new level in column",colnames(Xfac)[j],":",newLevel))
			}
		}
	save(file="levelsListXfac.Rdata",levelsListXfac)
	return(Xfac)
	}
	
#keep only the level that exits both in the training and testing set
keepIntersectionLevel=function(Xfac,Xtsfac,Y,Yts){
	
	id.fac=factorNotBin(Xfac)
	colname=colnames(Xfac)[id.fac]
	
	#delete the levels than has been added to the training with the function "addTestLevel"
	for(j in id.fac){
	  Xfac[,j]=factorCleanup(Xfac[,j])
	  Xtsfac[,j]=factorCleanup(Xtsfac[,j])
	  }
  
	#find training and testing levels
	levelsListXfac=list()
	levelsListXtsfac=list()
	i=0
	for(j in id.fac){
		i=i+1
		levelsListXfac[[i]]=levels(Xfac[,j])
		levelsListXtsfac[[i]]=levels(Xtsfac[,j])
		}
	names(levelsListXfac)=colname
	names(levelsListXtsfac)=colname
	
	#delete levels that appear only the testing and training set
	i=0
	for(j in id.fac){
		i=i+1
		TRlevels=levels(Xfac[,j])
		TRnewLevel=setdiff(TRlevels,levelsListXtsfac[[i]])
		TSlevels=levels(Xtsfac[,j])
		TSnewLevel=setdiff(TSlevels,levelsListXfac[[i]])
		#if there are new levels delete those observations
		if (length(TRnewLevel)>0) {
			for(s in 1:length(TRnewLevel)){
				id.s=which(Xfac[,j]==TRnewLevel[s])
				id.keep=setdiff(1:nrow(Xfac),id.s)
				Xfac=Xfac[id.keep,]
				Y=Y[id.keep]
				}			
			}
		if (length(TSnewLevel)>0) {
			for(l in 1:length(TSnewLevel)){
				id.l=which(Xtsfac[,j]==TSnewLevel[l])
				id.keep=setdiff(1:nrow(Xtsfac),id.l)
				Xtsfac=Xtsfac[id.keep,]
				Yts=Yts[id.keep]
				}			
			}
		}
	
	#remove unsed levels
	for(j in id.fac){
	  Xfac[,j]=factorCleanup(Xfac[,j])
	  Xtsfac[,j]=factorCleanup(Xtsfac[,j])
	  }
	
	#find the intersection
	intersectLevelsList=list()
	i=0
	for(j in id.fac){
		i=i+1
		intersectLevelsList[[i]]=intersect(levels(Xfac[,j]),levels(Xtsfac[,j]))
		}
	names(intersectLevelsList)=colname
	
	#delete levels that are in the intersection but not in training and testing 
	i=0
	for(j in id.fac){
		i=i+1
		TRlevels=levels(Xfac[,j])
		TRnewLevel=setdiff(TRlevels,intersectLevelsList[[i]])
		TSlevels=levels(Xtsfac[,j])
		TSnewLevel=setdiff(TSlevels,intersectLevelsList[[i]])
		#if there are new levels delete those observations
		if (length(TRnewLevel)>0) {
			for(s in 1:length(TRnewLevel)){
				id.s=which(Xfac[,j]==TRnewLevel[s])
				id.keep=setdiff(1:nrow(Xfac),id.s)
				Xfac=Xfac[id.keep,]
				Y=Y[id.keep]
				}			
			}
		if (length(TSnewLevel)>0) {
			for(l in 1:length(TSnewLevel)){
				id.l=which(Xtsfac[,j]==TSnewLevel[l])
				id.keep=setdiff(1:nrow(Xtsfac),id.l)
				Xtsfac=Xtsfac[id.keep,]
				Yts=Yts[id.keep]
				}			
			}
		}
	
	#remove unsed levels
	for(j in id.fac){
	  Xfac[,j]=factorCleanup(Xfac[,j])
	  Xtsfac[,j]=factorCleanup(Xtsfac[,j])
	  if(length(levels(Xfac[,j]))!=length(levels(Xtsfac[,j])))
		stop(paste("different number of levels in training and testing in factor",colname[j]))
	  }	
	
	if(nrow(Xfac)!=length(Y)) stop("nrow(Xfac)!=length(Y)")
	if(nrow(Xtsfac)!=length(Yts)) stop("nrow(Xtsfac)!=length(Y)")
	
	#print(intersectLevelsList)
	#save(file="intersectLevelsList.Rdata",intersectLevelsList)
	return(list(X=Xfac,Xts=Xtsfac,Y=Y,Yts=Yts))
	}


#transform factor "f" into a vectors "v" preserving the factor values
fact2num<-function(f,rm.na=F){
  if(!is.factor(f)) {
    stop("f is of class ",class(f),", but it must be a factor")
  }
  
  lev<-levels(f)
  asNumLev<-suppressWarnings(as.numeric(as.character(lev)))
  charLev<-lev[which(is.na(asNumLev))]
  numLev<-lev[which(!is.na(asNumLev))]
  #if all levels are numeric convert them to their corresponding number
  if(length(lev)==length(numLev)){
    v<-as.numeric(lev)[f]
    return(v)
  }
  else{
    if(nlevels(f)==2){
      
      #f<-as.numeric(as.logical(f))
      f<-as.numeric(f)
      if(any(is.na(f))){
        stop("factor 'f' contains NAs \n")
      }
      return(f)
    }
    
    #simplest
    v<-as.numeric(f)
    id.na<-which(is.na(v)==T)
    N.na<-length(id.na)
    if(N.na>0){
      lev.na<-as.character(unique(f[id.na]))
      if(rm.na==F) stop(paste("NAs created by converting factor to numeric for levels:",lev.na))
      if(rm.na==T) {
        v<-v[-id.na]
        cat("WARNING: levels:",lev.na,"deleted when converting to numeric (NAs created)  \n")
      }
    }
    return(v)
  }  
}
