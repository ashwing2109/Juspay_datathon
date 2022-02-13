library(shiny)
library(tidyverse)
data <- read.csv("C:/Users/Ashwini/Desktop/New folder (2)/data.csv")
log<- summarise(group_by(data,mid,pmt,pg,hr), t=sum(t),success = sum(success))
by_pmt<-summarise(group_by(log,mid,pmt,hr), t=sum(t),success = sum(success))
by_pmt<- mutate(by_pmt, Date=str_sub(hr,1,10),Hour=strtoi(str_sub(hr,-2,-1)),sr=success*100/t)
log<- mutate(log, Date=str_sub(hr,1,10),Hour=strtoi(str_sub(hr,-2,-1)),sr=success*100/t)
ui <- fluidPage(
  titlePanel("Data Visualisation"),
  sidebarPanel(
    selectInput(inputId="date",label="Choose Date",choices = c("February 12"="2020-02-12","February 13"="2020-02-13","February 14"="2020-02-14"),
                selected = "February 12",multiple = F),
    
    selectInput(inputId="xaxis",label="Choose X Variable",choices = c("Merchant"="mid","Payment Type"="pmt","Time"="hr"),
                selected = "hr",multiple = F),
    
    sliderInput(inputId = "range1",
                label = "Time Range",
                min = 00,
                max = 23,
                value = c(00,23)),
    plotOutput(outputId = "Table1"),
    width = 4
  ),
  mainPanel(
    textOutput("text1"),
    plotOutput(outputId = "Plot"),
    textOutput("text2"),
    plotOutput(outputId = "Plot1")
  )
  
)
server <- function(input, output, session) {
  output$text1 <- renderText({ 
    "Plot on basis of selected Parameters" 
  })
  output$Plot <- renderPlot({
    if(input$date=="2020-02-12"){
      x=filter(by_pmt, Date=="2020-02-12")
    }else if(input$date=="2020-02-13"){
      x=filter(by_pmt, Date=="2020-02-13")
    }else if(input$date=="2020-02-14"){
      x=filter(by_pmt, Date=="2020-02-14")
    }
  x <- filter(x,Hour >= input$range1[1] & Hour <= input$range1[2])
  if(input$xaxis == "mid"){
    ggplot(x)+ geom_point(aes(x=mid,y=sr,color=pmt),alpha=1/2)+  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=6,color="BLACK",face="bold"))+
      labs(x="Different Merchants",y="Success Rate",input$channel1,sep = " ")
  }else if(input$xaxis == "pmt"){
    ggplot(x)+ geom_point(aes(x=pmt,y=sr,color=mid),alpha=1/2)+  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=6,color="BLACK",face="bold"))+
      labs(x="Payment Methods",y="Success Rate",input$channel1,sep = " ")
  }else if(input$xaxis == "hr"){
    ggplot(x)+ geom_point(aes(x=Hour,y=sr,color=mid),alpha=1/2)+  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=6,color="BLACK",face="bold"))+
      labs(x="Hours in Day",y="Success Rate",input$channel1,sep = " ")
  }
  })
  output$text2 <- renderText({ 
    "Plot on basis of different Payment Gateways" 
  })
  output$Plot1 <- renderPlot({
    if(input$date=="2020-02-12"){
      x=filter(log, Date=="2020-02-12")
    }else if(input$date=="2020-02-13"){
      x=filter(log, Date=="2020-02-13")
    }else if(input$date=="2020-02-14"){
      x=filter(log, Date=="2020-02-14")
    }
    x <- filter(x,Hour >= input$range1[1] & Hour <= input$range1[2])
    if(input$xaxis == "mid"){
      ggplot(x)+ geom_point(aes(x=mid,y=sr,color=pmt),alpha=1/2) + facet_wrap(~pg)+  theme_bw()+
        theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
              axis.text = element_text(size=5,color="BLACK",face="bold"))+
        labs(x="Different Merchants",y="Success Rate",sep = " ")
    }else if(input$xaxis == "pmt"){
      ggplot(x)+ geom_point(aes(x=pmt,y=sr,color=mid),alpha=1/2) + facet_wrap(~pg)+   theme_bw()+
        theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
              axis.text = element_text(size=5,color="BLACK",face="bold"))+
        labs(x="Payment Methods",y="Success Rate",sep = " ")
    }else if(input$xaxis == "hr"){
      ggplot(x)+ geom_point(aes(x=Hour,y=sr,color=mid),alpha=1/2) + facet_wrap(~pg)+  theme_bw()+
        theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
              axis.text = element_text(size=5,color="BLACK",face="bold"))+
        labs(x="Hours",y="Success Rate",sep = " ")
    }
  })
  output$Table1 <- renderPlot({
    if(input$date=="2020-02-12"){
      x1=filter(log, Date=="2020-02-12")
    }else if(input$date=="2020-02-13"){
      x1=filter(log, Date=="2020-02-13")
    }else if(input$date=="2020-02-14"){
      x1=filter(log, Date=="2020-02-14")
    }
    x1 <- filter(x1,Hour >= input$range1[1] & Hour <= input$range1[2])
    x1 <- summarise(group_by(x1,mid,pmt), count=n()) 
    ggplot(data=x1, aes(pmt,count,fill=mid)) + geom_bar(stat="identity")+  theme_bw()+
      theme(axis.text.x = element_text(angle = 90),
            axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=5,color="BLACK",face="bold"))+
      labs(x="Payment Method",y="Number of transactions",sep = " ")
  },height = 250)
  
}
shinyApp(ui, server)