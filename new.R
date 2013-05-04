library(utils)
library(quantmod)
nasdaqstocks=file.choose()
stocklist=as.character(read.csv(nasdaqstocks, header=F)[,2])
pairs=combn(stocklist,2)
numpairs=ncol(pairs)

profits=rep(NA,numpairs)
correl=rep(NA,numpairs)
for (j in 1:numpairs){
  
  stock1=pairs[1,j]
  stock2=pairs[2,j]
  s1=getSymbols(stock1,from ="2010-04-01", to = "2013-04-01", auto.assign=FALSE)
  s2=getSymbols(stock2,from = "2010-04-01", to = "2013-04-01", auto.assign=FALSE)
  p1 = as.numeric(Ad(s1))
  p2 = as.numeric(Ad(s2))
  correl[j]=cor(p1,p2)
  p1.norm=(p1-runMean(p1,n=14))/runSD(p1,14)
  p2.norm=(p2-runMean(p2,n=14))/runSD(p2,14)
  ndiff=p1.norm-p2.norm
  
  numdays=length(ndiff)
  p1.traded=p2.traded=0
  current="neither"
  profit=0
  maxprofit=minprofit=numtrades=winners=0
  
  for(i in 14:numdays){
    
    if(ndiff[i]>2 & current=="neither"){
      p1.traded=(-10000/p1[i])
      p2.traded=(10000/p2[i])
      current="p2"
      numtrades=numtrades+1
    }
    if(ndiff[i]<(-2) & current=="neither"){
      p1.traded=(10000/p1[i])
      p2.traded=(-10000/p2[i])
      current="p1"
      numtrades=numtrades+1
    }
    if((ndiff[i]<0 & current=="p2") | (ndiff[i]>0 & current=="p1")){
      profit.temp=p1.traded*p1[i]+p2.traded*p2[i]
      profit=profit+profit.temp
      winners=winners+(profit.temp>0)
      maxprofit=max(maxprofit,profit.temp)
      minprofit=min(minprofit,profit.temp)
      p1.traded=0
      p2.traded=0
      current="neither"
    }
  }
  profits[j]=profit
}