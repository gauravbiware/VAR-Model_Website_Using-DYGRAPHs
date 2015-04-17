# server.R

#Date 24th  Feb 2015 

library(quantmod)
library(dygraphs)
source('GetSymbols.R', local=TRUE)
#Updated for dygraphs on 16th April 2015


shinyServer(function(input, output) {
 
  
  
  output$varplot<-renderDygraph({
    if(input$get==0)return(NULL)
    
   #Added on 16th April 2015
   stckclosedata_old<-GETSYBFUNC(input$symb1,src=input$radio,from=input$dates[1],to=input$dates[2])
   stckclosedata<-stckclosedata_old[,4]
   
   cont_returns<-diff(log(stckclosedata))*100
   VARCI<-as.numeric(input$VARConfInt)
   varpoint<-quantile(cont_returns,probs=VARCI,na.rm=TRUE)
   
   #Added on 16th April 2015
   SP500closedata_step1<-GETSP500(SPY,src=input$radio,from=input$dates[1],to=input$dates[2])
   SP500closedata<-SP500closedata_step1[,4]
    
   SP500cont_returns<-diff(log(SP500closedata))*100
   SP500var<-quantile(SP500cont_returns,probs=VARCI,na.rm=TRUE)
   
   #Added on 16th April 2015
   VARserieslength<-length(cont_returns)
   VARseries<-matrix(data=varpoint,ncol=1,nrow=VARserieslength)
   cont_returns_updated<-cbind(cont_returns,VARseries)
   colnames(cont_returns_updated)<-c("Stock Returns","VAR")
   
   dygraph(cont_returns_updated, main = "Daily Stock Returns and VAR over the chosen period") %>%
     dyRangeSelector() %>%
     dyRoller(rollPeriod = 1) %>%
     dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE) %>%
     dySeries("Stock Returns", label = "Daily Returns % is",color = "green")%>%
     dySeries("VAR", label = "VAR (in %) for this series is",drawPoints = FALSE, color = "blue")
   
   
   #colname1<-paste("VAR of",input$symb1)
   #plot(cont_returns,xlab="Period",ylab="Daily % Returns",main=paste("Plot showing Daily % returns for",input$symb1)) 
   #dnorm(cont_returns,mean=0,sd=1)
   #abline(h=varpoint,col="red",lwd=2,label="VAR")
   #text(0,-10, colname1, col = "red")
   #abline(h=SP500var,col="green",lwd=2)
   #text(0,-20, "VAR of S&P500", col = "green")
   
     })
  
  output$VARData<-renderTable({
    if(input$get==0)return(NULL)
    
    stckclosedata_old<-GETSYBFUNC(input$symb1,src=input$radio,from=input$dates[1],to=input$dates[2])
    stckclosedata<-stckclosedata_old[,4]
          
    
    cont_returns<-diff(log(stckclosedata))*100
    VARCI<-as.numeric(input$VARConfInt)
    varpoint<-quantile(cont_returns,probs=VARCI,na.rm=TRUE)
    
   
    SP500closedata_step1<-GETSP500(SPY,src=input$radio,from=input$dates[1],to=input$dates[2])
    SP500closedata<-SP500closedata_step1[,4]
      
    
    SP500cont_returns<-diff(log(SP500closedata))*100
    SP500var<-quantile(SP500cont_returns,probs=VARCI,na.rm=TRUE)
    newmat<-matrix(nrow=1,ncol=2)
    newmat[1,1]<-varpoint
    newmat[1,2]<-SP500var
    #colname1<-paste("VAR of",input$symb1)
    colnames(newmat)<-c(paste("VAR of",input$symb1),"VAR of S&P500")
    newmat
  })
  
 
  
  
})