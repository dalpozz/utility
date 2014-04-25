# wd<-getwd()
# root.wd<-unlist(strsplit(wd, split="/"))[2]
# platform<-NULL
# if(root.wd=="Users"){
# platform<-"mac"
# path<-"/Users/Andrea/Documents/Education/ULB/Phd/"
# }
# if(root.wd=="sulb5"){
# platform<-"hydra"
# path<-"/u/adalpozz/"
# }
# if(is.null(platform)){
# platform<-"atos"
# setwd("D:/R working directory/Temp")
# Sys.setlocale("LC_TIME","English_United States.1252")
# path<-"D:/R working directory/"
# lib.path<-"D:/R working directory/Rpackages/Rlibs"
# .libPaths(lib.path)
# }	


# #return the operating system of the computer used
# getOS=function(){
#   OpSystem=Sys.info()[['sysname']]
#   if(OpSystem=="Windows")
#     OS="WIN"
#   if(OpSystem=="Linux")
#     OS="LIN"
#   if(OpSystem=="Darwin")
#     OS="MAC"
#   return(OS)
# }
# 
# #set the size of a window before ploting
# set.dev.size <- function(width = 7, height = 7) { 
#   os<-getOS()
#   if (os=="LIN") 
#     x11(width=width, height=height)
#   if (os=="WIN") 
#     windows(width=width, height=height)
#   if (os=="MAC") 
#     quartz(width=width, height=height)
# }



#calculate the signed log 10
signedLog10<-function(x){
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# compute the frequency of each value in x
# e.g. freq(factor(letters[1:10]))
freq <- function(x){
  u<-unique(x)
  c.u<-numeric(length(u))
  for (i in 1:length(u))
    c.u[i]<-sum(x==u[i])
  names(c.u) <- u
  return(c.u)
}


#return the idx of obs for a cross validation
id.CV<-function(i,N.CV,N){
  i.ts<-((i-1)*N.CV+1):(i*N.CV)
  i.tr<-setdiff(c(1:N),i.ts)
  
  return(list(id.ts=i.ts,id.tr=i.tr))
}

#return the k-Training and k-testing data of a K-fold cross validation
data.CV<-function(data,k.CV,k){
  set.seed(2)
  
  #first check if data is a vecotr or a matrix
  if(is.vector(data) | is.factor(data))
    vectorData<-TRUE
  else vectorData<-FALSE
  
  if(!vectorData)
    N<-nrow(data)
  else N<-length(data)
  
  if(N==0) stop("N = 0 in data.CV")
  #resample
  id.sample=sample(1:N)
  if(!vectorData)
    data<-data[id.sample, ]
  else data<-data[id.sample]
  
  N.CV<-floor(N/k.CV)
  k.id<-id.CV(k,N.CV,N)
  
  if(!vectorData){
    k.TR=data[k.id$id.tr, ]
    k.TS=data[k.id$id.ts, ]
  } else{
    k.TR=data[k.id$id.tr]
    k.TS=data[k.id$id.ts]
  }
  
  return(list(TR=k.TR,TS=k.TS))	
}




#return a boostrap sample
#N.bs = number of boostrap sample to create
#k = the boostrap sample wanted
data.boostrap<-function(data,N.bs,k){
  set.seed(k)
  
  #first check if data is a vecotr or a matrix
  if(is.vector(data) | is.factor(data))
    vectorData<-TRUE
  else vectorData<-FALSE
  
  if(!vectorData)
    N<-nrow(data)
  else N<-length(data)
  
  if(N==0) stop("data.boostrap: N = 0")
  #resample
  id.sample=sample(1:N)
  if(!vectorData)
    data<-data[id.sample, ]
  else data<-data[id.sample]
  
  N.k<-floor(N/N.bs)
  id.k<-sample(1:N,N.k,replace=T)
  id.not.k<-setdiff(1:N,id.k)
  
  if(!vectorData){
    k.TR=data[id.not.k, ]
    k.TS=data[id.k, ]
  } else{
    k.TR=data[id.not.k]
    k.TS=data[id.k]
  }
  
  return(list(TR=k.TR,TS=k.TS))	
}


#return the inverse of rank
#where lower is ranked as first
inv.rank<-function(x,...)
  return(rank(-x,...))

# # check if the Required packages are already installed, if not install them
# package.list <- c("igraph", "network", "pixmap", "RCurl", "ReadImages", "rjson", "adimpro")

# for (i in 1:length(package.list)) {
# pkg <- package.list[i]
# if(!IsInstalled(pkg)) {  
# install.packages(pkg)
# }
# require(pkg, character.only = TRUE)
# }


#return the index of a column in a dataframe
getColIdx<-function(X,colname)
  return(which(colnames(X)==colname))


#for a given data.frame transform logical variable to factors
DFlogic2fac<-function(X){
  if(!is.data.frame(X))
    stop("X must be a data.frame")
  
  is.logic<-which(sapply(X,class)=="logical")
  for(i in is.logic)
    X[ ,i]<-logic2fac(X[ ,i])
  
  return(X)
}


#transform vectors of class logical to factors
logic2fac<-function(x){
  if(!is.logical(x))
    stop("x must be of class logical")
  
  x<-as.factor(x)
  
  return(x)  
}



#*************
#find rows in X that has at least one NA
#remove them form X and Y
#*************
removeNA<-function(X,Y){
  
  N<-nrow(X)
  id.na<-sapply(X, function(x) which(is.na(x)))
  id2remove<-unlist(id.na)
  id2keep<-setdiff(1:N,id2remove)
  
  #   id.na=NULL
  #   for(j in 1:ncol(X)){
  #     id.na<-c(id.na,which(is.na(X[,j])==T))
  #   }   
  #   id2keep<-setdiff(1:nrow(X),id.na)
  
  X<-X[id2keep, ]
  Y<-Y[id2keep]
  N.rm<-N-length(id2keep)
  
  return(list(X=X,Y=Y,nRemoved=N.rm))
}


#return TRUE if the vector has NA
vect.has.na<-function(x)
  return(any(is.na(x)))


# #return a vector of TRUE/FALSE (TRUE if the row of a matrix has an NA)
# row.has.na<-function(X)
#   return(apply(X,1,function(x){any(is.na(x))}))

#return the index of the row of a matrix/data.frame that has a least one NA
# much FASTER than row.has.na function!
id.row.has.na<-function(X)
  return(which(is.na(X)) %% nrow(X))

# #return TRUE if there is at least one row with an NA
# any.row.has.na<-function(X)
#   return(any(row.has.na(X)))
any.row.has.na<-function(X)
  return(length(id.row.has.na(X))>0)

#remove rows that have NAs
rm.row.has.na<-function(X,Y){
  
  #   row.is.na<-row.has.na(X)
  #   Xsub<-X[!row.is.na, ]
  #   Ysub<-Y[!row.is.na]
  #   nRemoved=sum(row.is.na)
  
  # faster !!
  row.na<-id.row.has.na(X)
  Xsub<-X[setdiff(1:nrow(X), row.na), ]
  Ysub<-Y[setdiff(1:nrow(X), row.na)]
  nRemoved=length(row.na)
  
  return(list(X=Xsub,Y=Ysub,nRemoved=nRemoved))
}


# row.has.nan<-function(X)
#   return(apply(X,1,function(x){any(is.nan(x))}))

#return the index of the row of a matrix/data.frame that has a least one NaN
# much FASTER than row.has.nan function!
id.row.has.nan <- function(X){
  m <- as.matrix(X)
  id.row.nan <- which(is.nan(m)) %% nrow(m)
  return(id.row.nan)
}

# any.row.has.nan<-function(X)
#   return(any(row.has.nan(X)))
any.row.has.nan<-function(X)
  return(length(id.row.has.nan(X))>0)

rows<-function(X){
  if(is.vector(X))
    N<-length(X)
  if(!is.vector(X))
    N<-nrow(X)
  if(is.null(X))
    N<-NA
  return(N)
}

cols<-function(X){
  if(is.vector(X))
    n<-1
  if(!is.vector(X))
    n<-ncol(X)
  if(is.null(X))
    n<-NA
  return(n)
}


is.const <- function(x)
  return(length(unique(x)) == 1)


is.constVector<-function(x){
  
  if(is.null(x))
    return(FALSE)
  
  if(is.vector(x)){
    if(vect.has.na(x)) {
      cat("WARNINGS: vector has NAs \n")
      if(all(is.na(x)))
        return(TRUE)
      else
        return(FALSE)
    }
    if (all(x==x[1]))
      return(TRUE)
    else 
      return(FALSE)
    
  } 
  if(!is.vector(x)){  
    
    if(is.Date(x)){
      if (all(x==x[1]))
        return(TRUE)
      else
        return(FALSE) 
    }
    
    if(is.array(x)){
      if (all(x==x[1]))
        return(TRUE)
      else
        return(FALSE)
    }
    
    cat("NOT a vector and not a date, class =",class(x),"\n")
    browser()
    
  }
  
}


#*****************
#find which column of a data.frame is constant
#return a vector of the same length of the number of column of the dataset
#the vector has value 1 is the corresponding column is constant
#*****************
constCol<-function(X){
  if(is.null(X))
    return(NA)
  
  constColumns=rep(FALSE,cols(X))
  constColumns<-apply(X,2,is.constVector)
  
  return(as.numeric(constColumns))
}

# constCol<-function(X){
# constColumns=rep(0,ncol(X))
# for(j in 1:ncol(X)){
# if (all(X[,j]==X[1,j]))
# constColumns[j]=1
# }

# return(constColumns)
# }


#*****************
#remove any constant variables
#return X
#*****************
remConstCol<-function(X,constColumns){
  if(is.null(X))
    return(X)
  
  if (sum(constColumns)>0){
    X=X[,constColumns==0]
  }
  return(X)
}


#check if the colnames of training and testing are the same to make sure there are the same variables
checkColnames<-function(tr,ts){
  col.tr<-colnames(tr)
  col.ts<-colnames(ts)
  if(!all(col.tr==col.ts)) stop("different variables between training and testing")
}


checkLength<-function(a,b){
  if(length(a)!=length(b)) stop("vectors have different lengths")
}


#return x/y as a percentage
Perc<-function(x,y){
  perc<-round(x/y*100,digit=2)
  return(perc)
}


#return the elapsed time of a system.time object
timeElapsed<-function(time){
  if(class(time)!="proc_time") stop("time given not supported")
  time<-as.numeric(time["elapsed"])
  time<-round(time,digit=2)	
  return(time)	
}




#code from Souhaib
#dyn.load(paste(path,"Code/distance.so",sep=""))

#MUST source distance.so with dyn.load before using Cdist

#x is a matrix
#y a vector
#return the distance between the matrix and the vector
Cdist<-function(x,y,result,N,d,p){
  dst<-.C("distance",as.double(x),as.double(y),result,N,d,as.double(p))
  return(dst[[3]])
}

#compute euclidean distance between a matrix (x) and a vector (y)
#return a vector
Euclidean<-function(x,y){
  x<-as.matrix(x)
  N<-NROW(x)
  d<-NCOL(x)
  p<-2
  d<-as.integer(d)
  N<-as.integer(N)
  p<-as.double(p)
  result<-double(N)
  dst<-Cdist(x,y,result,N,d,p)
  return(dst)
}


num2month<-function(i){
  if(i>12 | i<0) stop("month have to be between 1 and 12")
  
  if(i==1)
    month<-"January"
  if(i==2)
    month<-"February"
  if(i==3)
    month<-"March"
  if(i==4)
    month<-"April"
  if(i==5)
    month<-"May"
  if(i==6)
    month<-"June"
  if(i==7)
    month<-"July"
  if(i==8)
    month<-"August"
  if(i==9)
    month<-"September"
  if(i==10)
    month<-"October"
  if(i==11)
    month<-"November"
  if(i==12)
    month<-"December"
  
  return(month)
  
}


#used in racing
revertMetric<-function(value,metric){
  metrics<-c("Fmeasure","AP","AUC","Gini","Gmean","Recall","Precision","Accuracy","Mcc")
  if(metric %in% metrics)
    value<-1-value
  
  return(value)
}






# library(caret)
# pkg<-c("data.table","gplots","PerfMeas","ROCR")
# checkInstall(pkg)

#pkg<-"data.table"
loadPkg<-function(pkg){
  # Loading needed packages
  require(pkg)
  
  if(!require(pkg))
  {
    paste("You are missing the package", pkg, "we will now try to install it...")
    install.packages(pkg)    
    library(pkg)
  }
}


#read C4.5 file format
read.C45<-function(filename,filedir=getwd()){
  
  #check that both files exist
  files<-list.files(filedir)
  if(!(paste0(filename,".data") %in% files))
    stop(filename,".data does not exist in directory: \n",filedir,"\n")
  
  if(!(paste0(filename,".names") %in% files))
    stop(filename,".name does not exist in directory: \n",filedir,"\n")
  
  rowsTemp<-10000
  dataTemp<-read.table(paste0(filedir,filename,".data"),sep=",",nrows=rowsTemp,header=F)
  if(nrow(dataTemp)<rowsTemp)
    data<-dataTemp
  
  if(nrow(dataTemp)==rowsTemp){
    #extract class of columns to speed up the reading
    colClasses<-sapply(dataTemp,class)
    data<-read.table(paste0(filedir,filename,".data"),sep=",",colClasses=colClasses,header=F)
  }
  
  Input<-data[ ,-ncol(data)]
  Class<-data[ ,ncol(data)]
  
  name<-read.table(paste0(filedir,filename,".names"),sep=".",header=F)
  colsnameList<-sapply(name[,1], function(x) strsplit(as.character(x),":" )[1] )
  colsnameInput<-unlist(sapply(colsnameList,function(x) x[1])[-1])
  colnames(Input)<-colsnameInput
  
  colsValuesInput<-unlist(sapply(colsnameList,function(x) x[2])[-1])
  isFactorInput<-unlist(sapply(colsValuesInput,function(x) length(strsplit(as.character(x),",")[[1]])>1 ))
  
  #   TO DO:
  #     integer not converted well using as.factor
  #   Input[ ,isFactorInput]<-apply(Input[ ,isFactorInput],2, as.factor)
  #   check<-cbind(colClasses[-length(colClasses)],colsValuesInput)
  
  Class<-as.factor(Class)
  
  df<-data.frame(Input,Class)
  
  return(df)
}




#### progressBar ####
# pb   <- txtProgressBar(1, 100000, style=3)
# TIME <- Sys.time()
# for(i in SEQ){
#   Sys.sleep(0.00002)
#   if(i %% 1000 == 0){
#     setTxtProgressBar(pb, i)
#   }
# }
# Sys.time() - TIME



# ************************
# A Parallel boot Function
# uses foreach to distributed bootstrap processing to available workers. 
# It has 4 more arguments than the standard boot function: chunks, packages, export and verbose 
# Set verbose=TRUE to enabled back end worker process debugging. 
# The bootstrap resampling replicates will be divided into chunks tasks for processing by foreach.
# export: character vector of variables/functions  to export in foreach
# packages: character vector of packages that the tasks depend on. 
# Code originally from http://cran.r-project.org/web/packages/doRedis/vignettes/doRedis.pdf
# ************************
'bootForEach' <- function (data, statistic, R, sim = "ordinary", stype = "i",
                           strata = rep(1, n), L = NULL, m = 0, weights = NULL,
                           ran.gen = function(d, p) d, mle = NULL, simple=FALSE,
                           chunks = 1, packages = NULL, export = NULL, verbose=FALSE, ...) {
  
  thisCall <- match.call()
  n <- if (length(dim(data)) == 2) nrow(data)
  else length(data)
  if(R<2) 
    stop("R must be greater than 1")
  
  Rm1 <- R - 1
  RB <- floor(Rm1/chunks)
  combo <- function(...){
    al <- list(...)
    out <- al[[1]]
    t <- lapply(al, "[[", "t")
    out$t <- do.call("rbind", t)
    out$R <- R
    out$call <- thisCall
    class(out) <- "boot"
    out
  }
  # We define an initial bootstrap replicate locally. We use this
  # to set up all the components of the bootstrap output object
  # that don✬t vary from run to run. This is more efficient for
  # large data sets than letting the workers retun this information.
  binit <- boot(data, statistic, 1, sim = sim, stype = stype,
                strata = strata, L = L, m = m, weights = weights,
                ran.gen = ran.gen, mle=mle, ...)
  
  foreach(j=icount(chunks), .inorder=FALSE, .combine=combo, .init=binit, .export=export,
          .packages=c(packages, "boot","foreach"), .multicombine=TRUE, .verbose=verbose) %dopar% {
            if(j==chunks) 
              RB <- RB + Rm1 %% chunks
            res <- boot(data, statistic, RB, sim = sim, stype = stype,
                        strata = strata, L = L, m = m, weights = weights,
                        ran.gen = ran.gen, mle=mle, ...)
            list(t=res$t)
          }
  
}



# for unix (linux + mac)
# code from: http://rtm.wustl.edu/code/sendEmail.R
# vignette: http://rtm.wustl.edu/writings/htremail.pdf
# if file = TRUE, then the body of the message will just say have to include the path of the file",
# sendEmail(file = TRUE, text = "config.yml")
sendEmail <- function(subject = "Mail from R", 
                      text = "Body text.", 
                      address = "adalpozz@ulb.ac.be", 
                      file = FALSE){ #, filename = "MyRFile"
  
  if(file == FALSE){
    sys.arg <- paste("echo '", text, "' | mail -s ", subject,  " ", address, sep = "")		
  }else{
    # filename <- cat(filename, "." , unlist(strsplit(text, split="\\."))[2], sep = "")
    # sys.arg <- paste("uuencode ", text, " ", filename, "| mail -s ", subject, " ", address, sep = "")      
    sys.arg <- paste("mail -s ", subject, " ", address, " < ", text, sep = "") 
  }
  
  system(sys.arg)
}



# NOT working
#msg: Body text of message or a list containing mime_part objects
sendEmail2 <- function(subject = "Mail from R", msg = "It works!"){
  require(sendmailR)  
  from <- sprintf("<sendmailR@%s>", Sys.info()[4])
  to <- "Andrea Dal Pozzolo <dalpozz@gmail.com>"
  
  #sendmail(from, to, subject, msg, control=list(smtpServer="ASPMX.L.GOOGLE.COM",  verbose = TRUE))                                                  
  sendmail(from, to, subject, msg, control=list(smtpServer="smtp.ulb.ac.be",  verbose = TRUE))                                                  
}

# #only 20 email per day max, cannot include attachments
# library(mail)
# sendmail(recipient = "adalpozz@ulb.ac.be", subject="Notification from R", message="Calculation finished!", password="rmail")





# *************************
## Multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# *************************
# example
# library(ggplot2)
# # This example uses the ChickWeight dataset, which comes with ggplot2
# # First plot
# p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line() + ggtitle("Growth curve for individual chicks")
# # Second plot
# p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1) + ggtitle("Fitted growth curve per diet")
# # Third plot
# p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) + geom_density() + ggtitle("Final weight, by diet")
# # Fourth plot
# p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) + geom_histogram(colour="black", binwidth=50) +
#   facet_grid(Diet ~ .) + ggtitle("Final weight, by diet") + theme(legend.position="none") # No legend (redundant in this graph)    
# multiplot(p1, p2, p3, p4, cols=2)
# *************************
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
  
}
