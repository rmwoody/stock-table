library(readr)
library(stringr)
library(dplyr)
stocks <- read_csv("./raw-data/historical_stock_prices.csv")

# Stock tickers to use
top_50 <- "AAPL:MSFT:AMZN:GOOG:FB:BRK.A:V:WMT:TSLA:JNJ:PG:NVDA:MA:UNH:HD:JPM:VZ:ADBE:PYPL:CRM:NFLX:INTC:DIS:KO:PFE:BAC:CMCSA:MRK:NKE:PEP:
T:ABT:TMO:ORCL:MCD:CSCO:COST:DHR:AVGO:ABBV:UPS:NEE:MDT:ACN:QCOM:XOM:UNP:CVX:TXN:LLY"
top_50 <- str_split(str_replace(top_50,"\n", ""), ":")[[1]]

# Original Dataset is 20,973,879 rows
nrow(stocks)
stocks_50 <- stocks %>% subset(str_trim(ticker) %in% top_50)
length(unique(stocks_50$ticker))
setdiff(top_50,unique(stocks_50$ticker))

# BRK.A does not seem to bee in there

if(!dir.exists("./data")){dir.create("./data")}

stocks_50 %>% write_csv("./data/stocks.csv")
stocks <- NULL
