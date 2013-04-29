stocks = c("AAPL", "CAT", "SUN")

load.stock.data.frame = function() {
  stock.data = c()
  num.stocks = length(stocks)
  for (i in 1:num.stocks) {
    stock.data = c(stock.data, getSymbols(stocks[i], auto.assign=F))
  }
  stock.data.frame = data.frame(stock.data[,1])
  print(stock.data.frame)
  return(stock.data.frame)
}

# load stock data into environment
stock.data.frame = get.stock.data.frame()

stop("Done")

pairs = combn(stocks, 2)

for (i in 1:ncol(pairs)) {
  pair = pairs[,i]
  print(pair)
  stock1 = getSymbols(pair[1], auto.assign=F)
  stock2 = getSymbols(pair[2], auto.assign=F)
  
  
}