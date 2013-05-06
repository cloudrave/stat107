##### Caching method with defaults from: http://www.mail-archive.com/r-sig-finance@r-project.org/msg02989/attachSymbolsWithArgs.R #####
DDB_Yahoo2 <- function(cache.dir=tempdir(), cacheOK=TRUE,
                       verbose=getOption("verbose"),
                       from="2007-01-01", 
                       to=Sys.Date(),
                       DDB.name="Yahoo") 
{
  db <- quantmod:::getSymbolsDB(cache.dir=tempdir(), cacheOK, verbose)
  db <- db[-grep("_|\\.", db)]
  structure(list(name=paste("DDB:", DDB.name, sep=""), src="yahoo", from=from, 
                 to=to, db=db), class="DDB")
}


attachSymbols2 <- function(DB=DDB_Yahoo2(), pos=2, prefix=NULL, postfix=NULL, 
                           mem.cache=TRUE, file.cache=!mem.cache, 
                           cache.dir=tempdir(),  ...) {
  attach(NULL, pos=pos, name=DB[["name"]])
  rsym <- function(x) x
  lsym <- function(x) paste(prefix, as.character(x), postfix, sep="")
  invisible(sapply(DB[["db"]], create.binding2, lsym=lsym, gsrc='yahoo',
                   mem.cache=mem.cache, file.cache=file.cache,
                   cache.dir=cache.dir, envir=DB[["name"]],
                   from=DB[["from"]],
                   to=DB[["to"]],
                   ...))
}

# Verbosity wrapper for getSymbols
GETSymbols <- function(...) {
  cat(list(...)[[1L]], "not in memory. fetching....\n") #using partial matching
  do.call("getSymbols", list(...))
}

create.binding2 <- function(s, lsym, rsym, gsrc, 
                            mem.cache=TRUE,
                            file.cache=!mem.cache, 
                            cache.dir=tempdir(),
                            envir, ...) {
  #if((mem.cache + file.cache) != 1) stop("only mem or file caching supported")
  # allow both to be set to FALSE, to force no caching
  if(missing(rsym) || !is.function(rsym)) {
    rsym <- function(x) x
  }
  if(missing(lsym) || !is.function(lsym)) {
    lsym <- function(x) x
  }
  
  if(file.cache) {
    f <- function(value) {
      if(missing(value)) {
        if(!file.exists(file.path(cache.dir, paste(s,"rda",sep=".")))) {
          assign(lsym(s), getSymbols(rsym(s), src=gsrc, auto.assign=FALSE, ...))
          save(list=lsym(s), file=file.path(cache.dir, paste(s,"rda",sep=".")))
          get(lsym(s))
        } else {
          load(file.path(cache.dir, paste(lsym(s),"rda",sep=".")))
          get(lsym(s))
        }
      } else {
        return(message("assignment not possible with 'DDB' databases"))
      }}
    makeActiveBinding(lsym(s), f, as.environment(envir))
  } else
    if(mem.cache) {
      envir <- as.environment(envir)
      delayedAssign(lsym(s), { 
        assign(lsym(s), do.call("GETSymbols", list(rsym(s), auto.assign=FALSE, 
                                                   src=gsrc, ...)), env=envir)
        get(lsym(s), env=envir) },
                    assign.env=envir)
    } else { # no cache
      f <- function(value) {
        if(missing(value)) {
          assign(lsym(s), getSymbols(rsym(s), src=gsrc, auto.assign=FALSE, ...))
          tmp <- get(lsym(s))
          rm(list=lsym(s))
          tmp
        } else return(message("assignment not possible with 'DDB' databases"))
      }
      makeActiveBinding(lsym(s), f, as.environment(envir))
    }
}


create.binding2 <- function(s, lsym, rsym, gsrc, 
                            mem.cache=TRUE,
                            file.cache=!mem.cache, 
                            cache.dir=tempdir(),
                            envir, ...) {
  #if((mem.cache + file.cache) != 1) stop("only mem or file caching supported")
  # allow both to be set to FALSE, to force no caching
  if(missing(rsym) || !is.function(rsym)) {
    rsym <- function(x) x
  }
  if(missing(lsym) || !is.function(lsym)) {
    lsym <- function(x) x
  }
  
  if(file.cache) {
    f <- function(value) {
      if(missing(value)) {
        if(!file.exists(file.path(cache.dir, paste(s,"rda",sep=".")))) {
          assign(lsym(s), getSymbols(rsym(s), src=gsrc, auto.assign=FALSE, ...))
          save(list=lsym(s), file=file.path(cache.dir, paste(s,"rda",sep=".")))
          get(lsym(s))
        } else {
          load(file.path(cache.dir, paste(lsym(s),"rda",sep=".")))
          get(lsym(s))
        }
      } else {
        return(message("assignment not possible with 'DDB' databases"))
      }}
    makeActiveBinding(lsym(s), f, as.environment(envir))
  } else
    if(mem.cache) {
      envir <- as.environment(envir)
      delayedAssign(lsym(s), { 
        assign(lsym(s), do.call("GETSymbols", list(rsym(s), auto.assign=FALSE, 
                                                   src=gsrc, ...)), env=envir)
        get(lsym(s), env=envir) },
                    assign.env=envir)
    } else { # no cache
      f <- function(value) {
        if(missing(value)) {
          assign(lsym(s), getSymbols(rsym(s), src=gsrc, auto.assign=FALSE, ...))
          tmp <- get(lsym(s))
          rm(list=lsym(s))
          tmp
        } else return(message("assignment not possible with 'DDB' databases"))
      }
      makeActiveBinding(lsym(s), f, as.environment(envir))
    }
}


##### Global Code #####

start.date = "2005-04-01"
end.date =   "2013-04-01"

# Prepares caching database with custom from and to dates.
DB <- DDB_Yahoo2(from=start.date, to=end.date, verbose=TRUE)
attachSymbols2(DB)

# Prepares pairs to use for both methods.
library(utils)
library(quantmod)
library(tseries)
nasdaqstocks=file.choose()
stocklist=as.character(read.csv(nasdaqstocks, header=FALSE)[,2])
pairs=combn(stocklist,2)
numpairs=ncol(pairs)

####### Difference Method ########


profits=rep(NA,numpairs)
correl=rep(NA,numpairs)
coint=rep(NA,numpairs)
for (j in 1:numpairs){
  
  stock1=pairs[1,j]
  stock2=pairs[2,j]
    
  all1 = get(stock1)
  unmerged.p1=Ad(all1[which(time(all1) >= start.date & time(all1) <= end.date)])
  all2 = get(stock2)
  unmerged.p2=Ad(all2[which(time(all2) >= start.date & time(all2) <= end.date)])
  merged = merge(unmerged.p1, unmerged.p2, all=FALSE)
  a1 = merged[,1]
  a2 = merged[,2]
  print(paste(stock2, time(a2[1])))
      
  p1 = as.numeric(a1)
  p2 = as.numeric(a2)
  correl[j]=cor(p1,p2)
  p1.norm=(p1-runMean(p1,n=14))/runSD(p1,14)
  p2.norm=(p2-runMean(p2,n=14))/runSD(p2,14)
  ndiff=p1.norm-p2.norm
  
  fit=lm(p1~p2+0)
  beta=coef(fit)[1]
  spread=p1-beta*p2
  spread.test= adf.test(spread, alternative="stationary", k=0)
  coint[j]=1-spread.test$p.value
  
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

fit1=lm(profits~correl)
summary(fit1)
plot(correl,profits, main= "Stock Correlation vs. Total Profits (Difference Method)", xlab= "Correlation", ylab="Profits")
abline(lm(profits~correl), col="blue")

fit2=lm(profits~coint)
summary(fit2)
plot(coint,profits, main= "Stock Cointegration vs. Total Profits (Difference Method)", xlab= "Cointegration (Taken from Dickey Fuller test statistic)", ylab="Profits")
abline(lm(profits~coint), col="red")

o=order(coint)
coint.order=coint[o]
profits.order=profits[o]
num.top.ten.percent = round(0.99 * numpairs) + 1
coint.high=coint.order[num.top.ten.percent:numpairs]
profits.high=profits.order[num.top.ten.percent:numpairs]
plot(coint.high,profits.high)
mean(profits.high)

p=order(correl)
correl.order=correl[p]
pprofits.order=profits[p]
correl.high=correl.order[num.top.ten.percent:numpairs]
pprofits.high=pprofits.order[num.top.ten.percent:numpairs]
plot(correl.high,pprofits.high)
mean(pprofits.high)

#stop("done")

###### Ratio Method #######

profits2=rep(NA,numpairs)
correl2=rep(NA,numpairs)
coint2=rep(NA,numpairs)
for (j in 1:numpairs){
  
  stock1=pairs[1,j]
  stock2=pairs[2,j]
  
  #print(paste("s1:", stock1, "s2:", stock2))
  
  all1 = get(stock1)
  unmerged.p1=Ad(all1[which(time(all1) >= start.date & time(all1) <= end.date)])
  all2 = get(stock2)
  unmerged.p2=Ad(all2[which(time(all2) >= start.date & time(all2) <= end.date)])
  merged = merge(unmerged.p1, unmerged.p2, all=FALSE)
  a1 = merged[,1]
  a2 = merged[,2]
  
  #print(paste("loaded s1:", stock1, "s2:", stock2))
  
  
  p1 = as.numeric(a1)
  p2 = as.numeric(a2)
  rat=p1/p2
  nrat=(rat-runMean(rat,14))/runSD(rat,14)
  
  numdays2=length(nrat)
  p1.traded2=p2.traded2=0
  current2="neither"
  profit2=0
  maxprofit2=minprofit2=numtrades2=winners2=0
  
  correl2[j]=cor(p1,p2)
  
  fit=lm(p1~p2+0)
  beta=coef(fit)[1]
  spread=p1-beta*p2
  spread.test= adf.test(spread, alternative="stationary", k=0)
  coint2[j]=1-spread.test$p.value
  
  for(i in 14:numdays2){
    
    if(nrat[i]>2 & current2=="neither"){
      p1.traded2=(-10000/p1[i])
      p2.traded2=(10000/p2[i])
      current2="p2"
      numtrades2=numtrades2+1
    }
    if(nrat[i]<(-2) & current2=="neither"){
      p1.traded2=(10000/p1[i])
      p2.traded2=(-10000/p2[i])
      current2="p1"
      numtrades2=numtrades2+1
    }
    if((nrat[i]<0 & current2=="p2") | (nrat[i]>0 & current2=="p1")){
      profit.temp2=p1.traded2*p1[i]+p2.traded2*p2[i]
      profit2=profit2+profit.temp2
      winners2=winners2+(profit.temp2>0)
      maxprofit2=max(maxprofit2,profit.temp2)
      minprofit2=min(minprofit2,profit.temp2)
      p1.traded2=0
      p2.traded2=0
      current2="neither"
    }
  }
  profits2[j]=profit2
}

o2=order(coint2)
coint.order2=coint2[o2]
profits.order2=profits2[o]
num.top.ten.percent = round(0.99 * numpairs) + 1
coint.high2=coint.order[num.top.ten.percent:numpairs]
profits.high2=profits.order[num.top.ten.percent:numpairs]
plot(coint.high2,profits.high2)
mean(profits.high2)

p2=order(correl2)
correl.order2=correl2[p]
pprofits.order2=profits2[p]
correl.high=correl.order[num.top.ten.percent:numpairs]
pprofits.high=pprofits.order[num.top.ten.percent:numpairs]
plot(correl.high2,pprofits.high2)
mean(pprofits.high2)

fit3=lm(profits2~correl2)
summary(fit3)
plot(correl2,profits2, main= "Stock Correlation vs. Total Profits (Ratio Method)", xlab= "Correlation", ylab="Profits")
abline(lm(profits2~correl2), col="blue")

fit4=lm(profits2~coint2)
summary(fit4)
plot(coint2,profits2, main= "Stock Cointegration vs. Total Profits (Ratio Method)", xlab= "Cointegration (Taken from Dickey Fuller test statistic)", ylab="Profits")
abline(lm(profits2~coint2), col="red")

difference.method=c(mean(pprofits.high),mean(profits.high))
ratio.method=c(mean(pprofits.high2),mean(profits.high2))
results=cbind(difference.method,ratio.method)
rownames(results)=c("Correlation", "Cointegration")
colnames(results)= c("Brazilian Method", "Pairslog Method")