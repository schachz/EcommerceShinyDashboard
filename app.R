library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(bslib)
library(shinythemes)
library(rmarkdown)
library(httr)
library(jsonlite)
library(readxl)



url<-('https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx')
GET(url, write_disk(ECOMdata <- tempfile(fileext = ".xlsx")))
ECOMdata <- read_excel(ECOMdata)



totalprice <- ECOMdata$Quantity*ECOMdata$UnitPrice

total.revenue <- sum(totalprice)
totalorders <- length(unique(ECOMdata$InvoiceNo))


aov <- round(total.revenue/totalorders,digits = 2)
top.prod <- ECOMdata %>% group_by(Description) %>% summarise(value = sum(Quantity)) %>% filter(value==max(value))



unitsPerOrder <-  ECOMdata %>% group_by(InvoiceNo) %>% summarise(value = sum(Quantity))
avgUnitsPerOrder <- round(mean(unitsPerOrder$value),digits=2)
totalcustomers <- length(unique(ECOMdata$CustomerID))


top.products <- ECOMdata %>% group_by(StockCode) %>% summarise(value = sum(Quantity)) %>% arrange(desc(value))
top5prods <- top.products[1:5,]


topcountry <-  ECOMdata %>% group_by(Country) %>% summarise(value = sum(Quantity*UnitPrice)) %>% arrange(desc(value))

top5country <- topcountry[1:5,]
top5country$value <- round(top5country$value/1000,digits = 0)

#Dashboard header
header <- dashboardHeader(title = "E-commerce Dashboard") 

#Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Learn More", icon = icon("education",lib='glyphicon'), 
             href = "https://biostat.ufl.edu/"),
  menuItem("Github", icon = icon("hdd",lib='glyphicon'), 
           href = "https://github.com/schachz/EcommerceShinyDashboard")
))



frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Top 5 Products by Units"
    ,status = "success"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("top5products", height = "300px")
  )
  
  ,box(
    title = "Top 5 Countries by Revenue"
    ,status = "warning"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyCountry", height = "300px")
  ) 
  
)



frow3 <-  fluidRow(
  box(
    title = "Average Units per Orders", width = 4, solidHeader = TRUE, status = "info",
    avgUnitsPerOrder
  ),
  box(
    title = "Average Order Value", width = 4, solidHeader = TRUE,status = "warning",
    "$",aov
  ),
  box(
    title = "# of Customers", width = 4, solidHeader = TRUE, status = "success",
    totalcustomers
  )
)



# combining 3 fluid rows to make body of dashboard
body <- dashboardBody(frow1, frow2, frow3)


#Setting up User Interface
ui <- dashboardPage(title = 'Ecom Data Dashboard', header, sidebar, body, skin='black')




# Establishing serve input functions
server <- function(input, output) { 
  
  

#KPI Box Outputs
output$value1 <- renderValueBox({
    valueBox(
      formatC(top.prod$value, format="d", big.mark=',')
      ,paste('Top Product (Units Sold):',top.prod$Description)
      ,icon = icon("star",lib='glyphicon')
      ,color = "yellow")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Sales Revenue:'
      ,icon = icon("usd",lib='glyphicon')
      ,color = "blue")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(totalorders, format="d", big.mark=',')
      ,paste('Total Orders:')
      ,icon = icon("shopping-cart",lib='glyphicon')
      ,color = "red")
    
  })
  
  #Creating output for the plots
  
  output$top5products <- renderPlot({
    ggplot(data = top5prods, 
           aes(x=StockCode, y=value, fill=factor(StockCode))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Units Sold") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=20, face="bold")) + 
      ggtitle("Units Sold by Stock Code") + labs(fill = "StockCode")
  })
  
  
  output$revenuebyCountry <- renderPlot({
    ggplot(data = top5country, 
           aes(x=Country, y=value, fill=factor(Country))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue in USD (1000s)") + 
      xlab("Country") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=20, face="bold")) + 
      ggtitle("Revenue by Country") + labs(fill = "Country")
  })
  
  
  
}


shinyApp(ui, server)


