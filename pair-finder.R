library(quantmod)
setwd("/home/shared/stat107/final-project")

# download ALL stock data

nyse.stocks <- read.csv("/home/shared/stat107/final-project/symbols/nyse-stocks.csv")[,1]
#symbols = rep(NA, length(all_symbols))


# keep valid symbols
i = 1
for (symb in all_symbols) {
  print (length(regexpr("\-", symb)))
  if (length(regexpr("\-", symb)) == 0) {
    print(symb)
    symbols[i] = symb
    i = i + 1
  }
}

# remove invalid symbols
i = 1
for (market in markets) {
  for (symb in market) {
    if (regexpr("-", symb) > 0) {
      symbols[i] = symb
      i = i + 1
    }
  }
}

for (market in all_stocks) {
  
  if (regexpr("-", market - in it then continue
  print(market)
  all_stocks = getSymbols(market, auto.assign=T)
}
