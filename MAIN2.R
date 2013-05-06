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

# Set to FALSE to force database not to refresh.
# This is good for temporary efficiency, but must be set
# to TRUE if dates or selected stocks have been changed
# since last run!!
refresh.cache = TRUE

# Can be "2000" or "2008" or ...
my.time.choice = "long term"

refresh.cache = TRUE
if (exists("time.choice")) {
  if (time.choice == my.time.choice) {
    # time is unmodified
    refresh.cache = FALSE
  } else {
    print("Time has been modified... Clearing database.")
  }
}

time.choice = my.time.choice

if (time.choice == "2000") {
  # 2000-2002 recession
  date.1="1997-08-01"
  date.2="2000-03-01"
  date.3="2002-10-01"
  date.4="2005-05-01"
} else if (time.choice == "2008") {
  # 2008-2009 recession
  date.1="2007-08-01"
  date.2="2008-03-01"
  date.3="2009-10-01"
  date.4="2010-05-01"
} else if (time.choice == "early 2000s") {
  stop("Don't use these dates.")
  # based on https://www.google.com/finance?q=INDEXCBOE%3AVIX&sq=vix&sp=8&ei=peqHUZCGGKiylgPFuQE
  date.1="1986-05-01"
  date.2="1995-05-01"
  date.3="2004-05-01"
  date.4="2013-05-01"
} else if (time.choice == "long term") {
  # Note: Only the first time period should be used!
  #       This is to get a general perspective on
  #       pairs trading.
  date.1="2000-01-01"
  date.2="2012-12-31"
  date.3="2013-01-01"
  date.4="2013-03-01"
} else {
  stop("YOU MUST SPECIFY A PROPER TIME CHOICE!")
}

clear.cache = function() {
  DB <- DDB_Yahoo2(from=date.1, to=date.4, verbose=TRUE)
  attachSymbols2(DB)
}

# Prepares caching database with custom from and to dates.
if (refresh.cache) {
  clear.cache()
}

# Prepares pairs to use for both methods.
library(utils)
library(quantmod)
library(tseries)
if(refresh.cache) {
  nasdaqstocks=file.choose()
}
stocklist=as.character(read.csv(nasdaqstocks, header=FALSE)[,2])


# Check stocks' existence and load them into cache.
# Also populate list of stocks with full data.
valid.stocks = rep(NA, length(stocklist))
valid.count = 0
for (i in 1:length(stocklist)) {
  stock = stocklist[i]
  print(paste("checking", stock))
  s = get(stock)
  if (time(s[1]) == date.1) {
    valid.count = valid.count + 1
    valid.stocks[valid.count] = stock
  }
}

# remove NAs
valid.stocks = valid.stocks[!is.na(valid.stocks)]
print(paste("Number of valid stocks:", length(valid.stocks)))

pairs=combn(valid.stocks,2)
numpairs=ncol(pairs)

####### Difference Method ########


profits=rep(NA,numpairs)
correl=rep(NA,numpairs)
coint=rep(NA,numpairs)
for (j in 1:numpairs){
  
  stock1=pairs[1,j]
  stock2=pairs[2,j]
  
  all1 = get(stock1)
  unmerged.p1=Ad(all1[which(time(all1) >= date.1 & time(all1) <= date.4)])
  all2 = get(stock2)
  unmerged.p2=Ad(all2[which(time(all2) >= date.1 & time(all2) <= date.4)])
  merged = merge(unmerged.p1, unmerged.p2, all=FALSE)
  a1 = merged[,1]
  a2 = merged[,2]
  #print(paste(stock2, time(a2[1])))
  
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

#fit1=lm(profits~correl)
#summary(fit1)
#plot(correl,profits, main= "Stock Correlation vs. Total Profits (Difference Method)", xlab= "Correlation", ylab="Profits")
#abline(lm(profits~correl), col="blue")

#fit2=lm(profits~coint)
#summary(fit2)
#plot(coint,profits, main= "Stock Cointegration vs. Total Profits (Difference Method)", xlab= "Cointegration (Taken from Dickey Fuller test statistic)", ylab="Profits")
#abline(lm(profits~coint), col="red")

o=order(coint)
coint.order=coint[o]
profits.order=profits[o]
num.top.ten.percent = round(0.99 * numpairs) + 1
coint.high=coint.order[num.top.ten.percent:numpairs]
profits.high=profits.order[num.top.ten.percent:numpairs]
#plot(coint.high,profits.high)
mean(profits.high)

#p=order(correl)
#correl.order=correl[p]
#pprofits.order=profits[p]
#correl.high=correl.order[num.top.ten.percent:numpairs]
#pprofits.high=pprofits.order[num.top.ten.percent:numpairs]
#plot(correl.high,pprofits.high)
#mean(pprofits.high)

trade.pairs = function(stock1, stock2, date.1, date.2, return.val="profit") {
  all1 = get(stock1)
  unmerged.p1=Ad(all1[which(time(all1) >= date.1 & time(all1) <= date.2)])
  all2 = get(stock2)
  unmerged.p2=Ad(all2[which(time(all2) >= date.1 & time(all2) <= date.2)])
  merged = merge(unmerged.p1, unmerged.p2, all=FALSE)
  a1 = merged[,1]
  a2 = merged[,2]
  #print(paste(stock2, time(a2[1])))
  
  p1 = as.numeric(a1)
  p2 = as.numeric(a2)
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
  
  if (return.val == "numtrades")
    return(numtrades)
  else if (return.val == "profit")
    return(profit)
  else
    stop("MUST SPECIFY A PROPER RETURN TYPE!")
}

foo=o[num.top.ten.percent:numpairs]
top.pairs=pairs[,foo]
num.top.pairs=ncol(top.pairs)

top.profits.1=top.profits.2=top.profits.3=
  num.trades.1=num.trades.2=num.trades.3=rep(NA,num.top.pairs)
for (j in 1:num.top.pairs){

  s1 = top.pairs[1,j]
  s2 = top.pairs[2,j]
  
  top.profits.1[j] = trade.pairs(s1, s2, date.1, date.2, "profit")
  num.trades.1[j] = trade.pairs(s1, s2, date.1, date.2, "numtrades")
  
  top.profits.2[j] = trade.pairs(s1, s2, date.2, date.3, "profit")
  num.trades.2[j] = trade.pairs(s1, s2, date.2, date.3, "numtrades")

  top.profits.3[j] = trade.pairs(s1, s2, date.3, date.4, "profit")
  num.trades.3[j] = trade.pairs(s1, s2, date.3, date.4, "numtrades")
  
}

top.profits = cbind(top.profits.1, top.profits.2, top.profits.3)
num.trades = cbind(num.trades.1, num.trades.2, num.trades.3)

export.name = paste(substr(date.2,1,4), "-", substr(date.3,1,4), ".csv", sep="")

write.csv(x=top.profits, file=paste("exports/profits_", export.name, sep=""))
write.csv(x=num.trades, file=paste("exports/numtrades_", export.name, sep=""))

crimson = "#c90016"
my.hist = function(vector) {
  hist(vector, breaks=6, col=crimson)
}

my.hist(top.profits.1)
my.hist(top.profits.2)
my.hist(top.profits.3)

my.hist(num.trades.1)
my.hist(num.trades.2)
my.hist(num.trades.3)

