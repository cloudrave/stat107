stocks = c("AAPL", "CAT", "SUN")

# for testing only
getSymbols(stocks)

date.from = "2010-01-01"
date.to = "2013-01-01"

basis = getSymbols("AAPL", auto.assign=F, from=date.from, to=date.to)
dates = index(basis)

load.stock.data = function() {
  num.stocks = length(stocks)
  num.days = length(dates)
  stock.data = data.frame()
  # create matrix that allows for num.days days of trading data
  stock.data = matrix(nrow=num.days, ncol=num.stocks, dimnames=list(1:num.days, stocks))
  stock.data
  for (i in 1:num.stocks) {
    # load the adjusted price of each stock into the matrix
    adj = Ad(getSymbols(stocks[i], auto.assign=F, from=date.from, to=date.to))
    #### WATCH OUT! DATES DON'T MATCH ALWAYS. LOOK AT (SUN) ###
    print(length(adj))
    for (j in 1:num.days) {
      stock.data[j,i] = adj[j]
    }
  }
  print(stock.data)
  return(stock.data)
}

# load stock data into environment
stock.data = get.stock.data()

stop("Done")

pairs = combn(stocks, 2)

for (i in 1:ncol(pairs)) {
  pair = pairs[,i]
  print(pair)
  stock1 = getSymbols(pair[1], auto.assign=F)
  stock2 = getSymbols(pair[2], auto.assign=F)
  
  
}