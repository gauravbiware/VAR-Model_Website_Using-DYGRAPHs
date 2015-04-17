# GetSymbols.R
library(quantmod)
library(dygraphs)

GETSYBFUNC<- function(symbol,src,fromdate,todate){
  getSymbols(symbol,src=src,from=fromdate,to=todate,auto.assign=FALSE)
  }
  
  
GETSP500<- function(symbol,src,fromdate,todate){
  getSymbols("SPY",src=src,from=fromdate,to=todate,auto.assign=FALSE)
}

