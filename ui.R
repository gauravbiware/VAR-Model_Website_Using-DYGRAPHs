library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("Basic Value-At-Risk (VAR) Model"),
  
  sidebarLayout(
    sidebarPanel(
        
      radioButtons("radio",label="Select Source of Financial data",
                   c("Yahoo Finance"="yahoo","Google Finance"="google"),selected="yahoo"),
                   
             textInput("symb1", "Select the Stock using its Stock Symbol", "CSCO"),
      br(),
        
      dateRangeInput("dates", 
        "Select Period for VAR Analysis",
        start = "2014-01-01", 
        end = as.character(Sys.Date())),
      
      radioButtons("VARConfInt",label="Select VAR Confidence Interval",
                   c("90%"=0.1,"95%"=0.05,"99%"=0.01,"99.9%"=0.001),selected="95%"),
   
      actionButton("get", "Analyze"),
      
      br(),
      br()
      
      ),
      
    
    mainPanel(
      conditionalPanel(
        condition="input.get!='None'",
        tabsetPanel(
          
          tabPanel("VAR Model",
                   helpText("Below plot shows the distribution of the daily returns"),
                   helpText("In the same plot the blue horizontal line represents the VAR Value for the selected period"),
                   dygraphOutput("varplot"),
                   br(),
                   h4("Comparision of VAR for selected stock and S&P500 for the same period"),
                   tableOutput("VARData")
                  )
                                
               ))
  ))
))