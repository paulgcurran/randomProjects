###header text


scaledRangeItemTotal<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  
  dimen<-dim(Data1)
  nItems<-dimen[2]
  nPeople<-dimen[1]
  
  x<-0
  sumData1<-NULL
  itemTotal<-NULL
  rangeItemTotal<-NULL
  scaledRangeItemTotal<-NULL
  
  while (x<nPeople) {
    x<-x+1;
    sumData1[x]<-sum(Data1[x,],na.rm=TRUE)
  }
  
  rm(x)
  for (i in 1:nItems) {
    item<-Data1[,i];
    others<-(sumData1-item);
    itemTotal[i]<-cor(item,others,use="pairwise.complete.obs")
    rm(item,others)
  }
  minItemTotal<-NULL
  maxItemTotal<-NULL
  
  maxItemTotal<-max(itemTotal,na.rm=TRUE)
  minItemTotal<-min(itemTotal,na.rm=TRUE)
  rangeItemTotal<-(maxItemTotal-minItemTotal)
  scaledRangeItemTotal<-(rangeItemTotal/maxItemTotal)
  
  hist(itemTotal)
  return(scaledRangeItemTotal)
  
}