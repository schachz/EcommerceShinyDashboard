E-Commerce Sales Performance Dashboard Using R Shiny
================

## Author: Zach Schachter

### Introduction:

For any company, reporting is one of the most important tools for
monitoring sales performance, financials and overall health of the
business. Companies often compile reports on a monthly or quarterly
basis and this can often be a tedious task. Metrics and other KPIs (Key
Performance Indicators) are measured and displayed in concise and
easy-to-read dashboards for executives, stakeholders and other
employees.

The tools that companies use for reporting can often be expensive and
complicated. The goal of this project was to create a simple,
user-friendly and FREE app that can be used to display common e-commerce
sales metrics in a visually appealing dashboard. Based on my experience
working with e-commerce data, I wanted to design a dashboard that shows
the most important and commonly used KPIs from companies I have worked
with.

### Methods:

This dashboard was created using R Shiny in conjunction with ggplot2 and
Bootstrap components.

The header and sidebar include links to the UF Dept of Biostatistics
website and my Github page.

The icons used in the dashboard are from the Bootstrap Glyphicon
library.

``` r
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(shinyjs))
suppressMessages(library(bslib))
suppressMessages(library(shinythemes))
suppressMessages(library(rmarkdown))
suppressMessages(library(httr))
suppressMessages(library(jsonlite))
suppressMessages(library(readxl))


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
```

#### Fetching Data and Loading into R

``` r
url<-('https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx')
GET(url, write_disk(ECOMdata <- tempfile(fileext = ".xlsx")))
```

    ## Response [https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx]
    ##   Date: 2022-12-06 04:01
    ##   Status: 200
    ##   Content-Type: application/x-httpd-php
    ##   Size: 23.7 MB
    ## <ON DISK>  C:\Users\schac\AppData\Local\Temp\RtmpEJpBOZ\file6c1c67b1406e.xlsx

``` r
ECOMdata <- read_excel(ECOMdata)
```

#### Data Manipulation to calculate our KPIs

``` r
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
```

The dashboard body consists of 3 fluid rows housing our KPI displays and
bar chart plots.

#### Formatting the fluid rows:

``` r
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
```

The UI portion of the Shiny app combines our title, header, sidebar,
body, and skin.

``` r
# combining 3 fluid rows to make body of dashboard
body <- dashboardBody(frow1, frow2, frow3)


#Setting up User Interface
ui <- dashboardPage(title = 'Ecom Data Dashboard', header, sidebar, body, skin='black')
```

<figure>
<img src="DashboardUI.jpg" width="317" alt="Header and Sidebar" />
<figcaption aria-hidden="true">Header and Sidebar</figcaption>
</figure>

The Server portion of the Shiny app contains our functions to generate
our desired plots.

``` r
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
```

![The results from our plots!](DashboardPlots.jpg)

This function brings the UI and server portions together to get our
Shiny app!

``` r
# shinyApp(ui, server)
```

### Results:

The data used to test this app is actual data from a UK retailer
containing sales from 01/12/2010 to 09/12/2011.

This dataset was pulled from the UCI Machine Learning Repository and
made available by Dr Daqing Chen, Director of the Public Analytics group
at London South Bank University.

``` r
head(ECOMdata)
```

    ## # A tibble: 6 × 8
    ##   InvoiceNo StockC…¹ Descr…² Quant…³ InvoiceDate         UnitP…⁴ Custo…⁵ Country
    ##   <chr>     <chr>    <chr>     <dbl> <dttm>                <dbl>   <dbl> <chr>  
    ## 1 536365    85123A   WHITE …       6 2010-12-01 08:26:00    2.55   17850 United…
    ## 2 536365    71053    WHITE …       6 2010-12-01 08:26:00    3.39   17850 United…
    ## 3 536365    84406B   CREAM …       8 2010-12-01 08:26:00    2.75   17850 United…
    ## 4 536365    84029G   KNITTE…       6 2010-12-01 08:26:00    3.39   17850 United…
    ## 5 536365    84029E   RED WO…       6 2010-12-01 08:26:00    3.39   17850 United…
    ## 6 536365    22752    SET 7 …       2 2010-12-01 08:26:00    7.65   17850 United…
    ## # … with abbreviated variable names ¹​StockCode, ²​Description, ³​Quantity,
    ## #   ⁴​UnitPrice, ⁵​CustomerID
