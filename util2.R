#code from Gianluca Bontempi

mCRMRC<-function(CX,CY,nmax=10,lambda=0.5){

  n<-NCOL(CX)
  Iy<-cor2I2(CY)

  Ix<-cor2I2(CX)
  
  Ixx<-array(NA,c(n,n))
  
  for (i in 1:n)
    for (j in setdiff(1:n,i)){
      Ixx[i,j]<-cor2I2((CX[i,j]-CY[i]*CY[j])/(sqrt(1-CY[i]^2)*sqrt(1-CY[j]^2)))
      Ixx[j,i]<-Ixx[i,j]
    }

  cut<-numeric(n)
  for (i in 1:(n)){
    cut[i]<-(1-lambda)*Iy[i]-lambda*mean(Iy[-i]-Ix[i,-i]+Ixx[i,-i])
####                                 mean_j {I(xj;y| xi)}
  }
  
  subs<-sort(cut,decr=T,ind=T)$ix[1:nmax]
                                        # to be maximized
  
}

mimrC<-function(CX,CY,VX,VY,
                nmax=5,first=NULL,init=FALSE,lambda=0.95,fast.inter=0,back=FALSE,inter=FALSE,spouse.removal=FALSE,
                var.removal=FALSE){
  ## mimr filter

  n<-NCOL(CX)
  subset<-1:n
  if (var.removal)
    subset<-which(VX<=VY)
  
  Iy<-cor2I2(CY)
  Ix<-cor2I2(CX)
  Ixx<-array(NA,c(n,n))
  
  for (i in 1:n)
    for (j in setdiff(1:n,i)){
      Ixx[i,j]<-cor2I2((CX[i,j]-CY[i]*CY[j])/(sqrt(1-CY[i]^2)*sqrt(1-CY[j]^2)))
      Ixx[j,i]<-Ixx[i,j]
    }


  Inter<-array(NA,c(n,n))
  if (init){    
    max.kj<--Inf
    for (kk in 1:(n-1)){
      for (jj in (kk+1):n){
        Inter[kk,jj]<- Ixx[kk,jj]-Ix[kk,jj]
        
        Inter[jj,kk]<-Inter[kk,jj]
      
        if (Inter[kk,jj]>max.kj){
          max.kj<-Inter[kk,jj]
          subs<-c(kk,jj)
        }
      }
    }
  } else {
    subs<-which.max(Iy)
  }
  
  
  last.subs<-0
  for (j in length(subs):min(n-1,nmax-1)){
    mrmr<-numeric(n)-Inf
    if (length(subs)<(n-1)){
      if (length(subs)>1){
        mrmr[-subs]<- (1-lambda)*Iy[-subs]+lambda*apply(-Ix[subs,-subs]+Ixx[subs,-subs],2,mean)
      } else {
        
        mrmr[-subs]<- (1-lambda)*Iy[-subs]+lambda*(-Ix[subs,-subs]+Ixx[subs,-subs])
        
      }
    } else {
      mrmr[-subs]<-Inf
    }
    
    s<-which.max(mrmr)
    subs<-c(subs,s)
    
  }
 
  subset[subs[1:nmax]]
  
}


mimrC2<-function(CX,CY,VX,VY,nmax,lambda,init){
  subset<-mrmrC(CX,CY,lambda=lambda,nmax=2*nmax,init=init)

  CX<-CX[subset,subset]
 
  n<-NCOL(CX)
 
 
  Iy<-cor2I2(CY)
  CCx<-CX
  Ix<-cor2I2(CCx)
    Ixx<-array(NA,c(n,n))
  
  for (i in 1:n)
    for (j in setdiff(1:n,i)){
      Ixx[i,j]<-cor2I2((CX[i,j]-CY[i]*CY[j])/(sqrt(1-CY[i]^2)*sqrt(1-CY[j]^2)))
      Ixx[j,i]<-Ixx[i,j]
    }

  
 
  load("../../causal/classtri.Rdata")

 
  ind<-1:NROW(XX)
  XX<-XX[ind,]
  feat<-1:6
  YY<-factor(YY[ind])
 
  
  d<-data.frame(YY,XX[,feat])
  nn<-NCOL(d)-1
  names(d)[1]<-"Y"
  mod<-lda(Y~.,d)
 

  
  iscoll<-list()
  iscoll[[n+1]]<-0
  
  for (ii in 1:(n-1)){
    for (kk in (ii+1):n){
      xx<-c(VY-V[ii],VY-V[kk],Iy[ii],Iy[kk],Ix[ii,kk]-Ixx[ii,kk],Ix[ii,kk])
      xx2<-c(VY-V[kk],VY-V[ii],Iy[kk],Iy[ii],Ix[ii,kk]-Ixx[ii,kk],Ix[ii,kk])
      xx<-data.frame(rbind(xx[feat],xx2[feat]))
      colnames(xx)[1:(nn)]<-colnames(d)[2:(nn+1)]
      Y.hat<-predict(mod,xx)$posterior        
      iscoll[[ii]]<-c(iscoll[[ii]],mean(Y.hat[,2]))
      iscoll[[kk]]<-c(iscoll[[kk]],mean(Y.hat[,2]))
      
      
    }
    
  }

  U<-unlist(lapply(iscoll[1:n],max))
  if (length(which(U>0.5))>0){
    subset<-subset[which(U>0.5)]
    U<-U[which(U>0.5)]

  
    subs<-subset[sort(U,decr=T,ind=T)$ix][1:min(length(U),nmax)]
  
  } else
    subs<-subset[1:2]

 
  subs

  

}

mrmrC<-function(CX,CY,nmax=5,first=NULL,init=FALSE,lambda=0.95,fast.inter=0,back=FALSE,inter=FALSE,spouse.removal=FALSE){
  ## mrmr filter
  
  n<-NCOL(CX)
  Iy<-cor2I2(CY)
  Ix<-cor2I2(CX)
  Inter<-array(NA,c(n,n))

  subs<-which.max(Iy)
  
  
  
  last.subs<-0
  for (j in length(subs):min(n-1,nmax-1)){
    mrmr<-numeric(n)-Inf
    if (length(subs)<(n-1)){
      if (length(subs)>1){
        mrmr[-subs]<- (1-lambda)*Iy[-subs]+lambda*apply(-Ix[subs,-subs],2,mean)
      } else {
        
        mrmr[-subs]<- (1-lambda)*Iy[-subs]+lambda*(-Ix[subs,-subs])
        
      }
    } else {
      mrmr[-subs]<-Inf
    }
    
    s<-which.max(mrmr)
    subs<-c(subs,s)
    
  }
 
  subs[1:nmax]
  
}

howmany.notna<-function(x){

  length(which(!is.na(x)))

}

int7<-function(s1,s2,s3,s4,s5,s6,s7){

intersect(s1,intersect(s2,intersect(s3,intersect(s4,intersect(s5,intersect(s6,s7))))))

}


gseff<-function(X,Y,nmax,lambda=0.1){

  X<-scale(X)
  n<-NCOL(X)
  N<-NROW(X)
  sel<-NULL
  CY<-numeric(n)
  for (r in 1:min(50,N/2)){

    CY[setdiff(1:n,sel)]<-cor(X[,setdiff(1:n,sel)],Y)
   
  
    CY[sel]<-0
    
    if (r==1){
      IY<-cor2I2(CY)
      pv.CY<-1-2*ppears(CY,NROW(X)+numeric(NCOL(X)))
      ## probability that there is a correlation
      IIY<-NULL #IY
    }
    ##  if (r>1  && r<5)
    ##  IIY<-IIY+cor2I2(CY)
    if (r>1) {      
   ##   IIY<-cbind(IIY,IY-cor2I2(CY))
      pv.CY.c<-2*ppears(CY,NROW(X)+numeric(NCOL(X)),S=r)
      ## probability that there is no conditioned correlation
      IIY<-cbind(IIY,pv.CY.c)          
    }

    IIY2<-IIY
    IIY2[sel]<--Inf
    s<-which.max(cor2I2(CY))
    sel<-c(sel,s)
    x.sel<-X[,s]
    X<- X-x.sel%*%(x.sel%*%X)/as.numeric(x.sel%*%x.sel)
    Y<- Y-x.sel%*%(x.sel%*%Y)/as.numeric(x.sel%*%x.sel)
  
  }

##  notsel<-setdiff(1:n,sel)
##  c(sel,notsel[sort(apply(IIY,1,mean,na.rm=T)[notsel],d=T,ind=T)$ix])[1:nmax]
## browser()
 ## sort((1-lambda)*IY+lambda*apply(IIY,1,mean,na.rm=T),d=T,ind=T)$ix[1:nmax]
  sort((1-lambda)*pv.CY+lambda*apply(IIY,1,max,na.rm=T),d=T,ind=T)$ix[1:nmax]
}


    wrapeff<-function(X,Y,nmax=5,lambda=0.9,lag=-1,back=FALSE){
  
  X<-scale(X)
  
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)
  
  CY<-abs(cor(X,Y))
  
  ss<-sort(CY,decr=T,ind=T)

  L<-nn
  
  subset<-ss$ix
  X<-X[,subset[1:L]]
  n<-NCOL(X)
  subs<-1

  pd1<-numeric(n)+Inf
  for (i in 1:n)
      pd1[i]<-regrlin(X[,i],Y)$MSE.emp
  for (j in length(subs):(n-1)){

    if (lag<0)
      subs2<-NULL
    else
      subs2<-subs[max(1,j-lag):length(subs)]
    ##subs2<-NULL
    pd<-numeric(n)+Inf
    for (i in setdiff(1:n,subs))
      pd[i]<-regrlin(X[,c(subs2,i)],Y)$MSE.emp

    
    s<-which.min((1-lambda)*pd1+lambda*pd)
    subs<-c(subs,s)
  #  print(subs)
   
  }
  subset[1:L]<-subset[subs]

  subset[1:nmax]
}

wrapeff2<-function(X,Y,nmax=5,lambda=0.9,lag=3,back=FALSE){
  
  X<-scale(X) 
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)
  


  pd1<-numeric(n)
  pd<-array(NA,c(lag+1,n))
  for (i in 1:n)
    pd[1,i]<-regrlin(X[,i],Y)$MSE.emp

  pp<-pd[1,]
  s<-sort(pd[1,],decr=FALSE,ind=TRUE)$ix
  pr<-(max(pp)-pp)/sum(max(pp)-pp)
  ##pr<-(pp)/sum(pp)
  if (lag>0)
  for (j in 1:lag){
    
    for (i in 1:n){ 
      p<-NULL
      for (r in 1:50){
        #set.seed(r+j)
        S<-s[(r):(r+j-1)]
        #S<-sample(setdiff(1:n,i),j,prob=pr[-i]/sum(pr[-i]))
        #S<-sample(1:n,j,prob=pr/sum(pr))
       p<-c(p,regrlin(X[,c(i,S)],Y)$MSE.emp)
     }
      pd[j+1,i]<-quantile(p,0.4)
    }
     
   ## pp<-apply(pd[1:(1+j),],2,mean)
   ## pr<-(max(pp)-pp)/sum(max(pp)-pp)
  }

  
  subset<-sort((1-lambda)*pd[1,]+lambda*apply(pd[1:(lag+1),],2,min),decr=F,ind=T)$ix[1:nmax]
         

}


hotel.test<-function(X1,X2,m=1){

N1<-NROW(X1)
N2<-NROW(X2)
mu1<-apply(X1,2,mean)
mu2<-apply(X2,2,mean)

n<-NCOL(X1)

S1<-var(X1)
S2<-var(X2)

S=((N1-1)*S1+(N2-1)*S2)/(N1+N2-2)





t2 <- (mu1-mu2)%*%ginv(S)%*%(mu1-mu2)*N1*N2/(N1+N2)
F <- (N1+N2-n-1)*t2/((N1+N2-2)*n)
P<-1
M<-1
if (F>0){
  P <- 1-pf(F,n,N1+N2-n-1)
  M<- 1-pf(F,n,N1+N2-n-1)^m
}


list(pv=P,mpv=M,stat=F)
}
lda.test<-function(X,Y){

  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  mod<-lda(Y~.,d)
 
  mean(1-pf((mod$svd)^2,1,length(Y)))


}

Fsc<-function(pr,rec,beta=1){
  if (pr+rec==0)
    return(0)
  (1+beta^2)*pr*rec/((beta^2)*pr+rec)
}


seleff<-function(Ixx){
  n<-NCOL(Ixx)
  subs<-sort(apply(Ixx,2,min),dec=F,ind=T)$ix[1:2]
  
  
  
  for (i in length(subs):(n-2)){
    pd<-numeric(n)+Inf
    pd[-subs]<- apply(Ixx[subs,-subs],2,mean)
    subs<-c(subs,which.min(pd))
    
  }
  c(subs,setdiff(1:n,subs))
}

mimreff<-function(X,Y,lambda=0.5,lambda2=lambda,lag=1,nmax=10,back=FALSE,init=FALSE,mn=T){
  require(corpcor)
  n<-NCOL(X)
  i0<-(which(Y==0))
  i1<-(which(Y==1))
  
  w0<-length(i0)/(length(i0)+length(i1))
  w1<-length(i1)/(length(i0)+length(i1))
  Ixx<-w0*cor2I2(cor.shrink(X[i0,],lambda=0.5,v=F))+w1*cor2I2(cor.shrink(X[i1,],lambda=0.5,v=F))
  
  Ix<-cor2I2(cor(X)) ##array(0,c(n,n))
  
  Y[i0]<- -1
  
  Iy<-cor2I2(cor(X,Y))
 ## Iy<-numeric(n)
 ## for (i in 1:n)
   ### Iy[i]<-var2H(var(Y))-var2H(regrlin(X[,i],Y)$MSE.loo)
  n<-NCOL(Ixx)
  Inter<-array(NA,c(n,n))
  if (init){
    
    max.kj<--Inf
    for (kk in 1:(n-1)){
      for (jj in (kk+1):n){
        
        Inter[kk,jj]<- (Iy[kk]+Iy[jj])+lambda*(Ix[kk,jj]-Ixx[kk,jj])##
        Inter[jj,kk]<-Inter[kk,jj]
        if (is.na(Inter[jj,kk]))
          browser()
        if (Inter[kk,jj]>max.kj){
          max.kj<-Inter[kk,jj]
          subs<-c(kk,jj)
        }
      }
    }
  } else {
    subs<-which.max(Iy)
  }

  cut<-numeric(n)
  for (i in 1:n){
    cut[i]<-Iy[i]-lambda2*mean(Iy[-i]-Ix[i,-i]+Ixx[i,-i])
    
  }
  
  for (i in length(subs):min(nmax,(n-1))){
    pd<-numeric(n)-Inf
    if (length(subs)<(n-1)){
   
    
      if (length(subs)>1){
        if (mn)
          pd[-subs]<- (cut[-subs])+lambda*(apply(-Ix[subs,-subs]+Ixx[subs,-subs],2,mean))
        else
          pd[-subs]<- (cut[-subs])+lambda*(apply(-Ix[subs,-subs]+Ixx[subs,-subs],2,max))
     
        
      } else
        pd[-subs]<- (cut[-subs])+lambda*(+Ixx[subs,-subs]-Ix[subs,-subs])
    } else
    pd[-subs]<-Inf
    subs<-c(subs,which.max(pd))
#    browser()
    
  }
  c(subs,setdiff(1:n,subs))
  
  

  
  subs[1:nmax]
}






seldisc<-function(X,Y,lambda=0.5,lambda2=lambda,nmax=10,back=FALSE,init=FALSE){
  require(corpcor)
  n<-NCOL(X)
 
  
  Iy<-cor2I2(cor(cbind(X,Y)))
  diag(Iy)<-0
  ##  for (i in 1:n)
  ##  Iy[i]<-var2H(var(Y))-var2H(regrlin(X[,i],Y)$MSE.loo)

  Inter<-array(NA,c(n,n))

  
  max.kj<--Inf
  for (kk in 1:(n-1)){
    for (jj in (kk+1):n){
      
      Inter[kk,jj]<- sum(Iy[c(kk,jj,n+1),c(kk,jj,n+1)])
      Inter[jj,kk]<-Inter[kk,jj]
      if (is.na(Inter[jj,kk]))
        browser()
      if (Inter[kk,jj]>max.kj){
        max.kj<-Inter[kk,jj]
        subs<-c(kk,jj)
      }
    }
  }
 

  for (i in length(subs):min(nmax,(n-1))){
    pd<-numeric(n)-Inf
    for (j in setdiff(1:n,subs))
      pd[j]<-sum(Iy[c(subs,j,n+1),c(subs,j,n+1)])
    subs<-c(subs,which.max(pd))
   
    
  }
  
  subs
}

hoteff<-function(X,Y,nmax=10,back=FALSE){
  require(corpcor)
  n<-NCOL(X)

 i0<-(which(Y==0))
  i1<-(which(Y==1))
  Inter<-array(NA,c(n,n))
  
  
  max.kj<--Inf
  for (kk in 1:(n-1)){
    for (jj in (kk+1):n){
      Inter[kk,jj]<- hotel.test(X[i0,c(kk,jj)],X[i1,c(kk,jj)])$stat #Ix[kk,jj]-Ixx[kk,jj]##
      Inter[jj,kk]<-Inter[kk,jj]
      if (Inter[kk,jj]>max.kj){
        max.kj<-Inter[kk,jj]
        subs<-c(kk,jj)
      }
    }
  }

  

 
  for (i in length(subs):min(nmax,(n-1))){
    pd<-numeric(n)-Inf
    if (length(subs)<(n-1))
        if (length(subs)>1)
        pd[-subs]<- (apply(Inter[subs,-subs],2,mean))
        else
        pd[-subs]<- Inter[subs,-subs]
    else
        pd[-subs]<-Inf
    subs<-c(subs,which.max(pd))
  
   
  }
 
  subs
}




isel<-function(listgene,x){
 
is.element(x, unlist((listgene)))
}

ispatt<-function(x,p){

  all(x==p)
}
