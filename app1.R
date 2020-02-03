#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(reshape2)
library(ggplot2)
library(xts)
library(zoo)
library(shiny)
library(plotly)

house.data <- read.csv("kc_house_data.csv")

ui <- fluidPage(
  title = "King County House Sales History",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.panelSelection === "Overview"',
        sliderInput(inputId = "Pricerange", label = "Price Range:",
                    min = 75000, max = 7700000,
                    value = c(75000,7700000)),
        checkboxGroupInput(inputId = "show_vars", label = "Columns to show:",
                           names(house.data), selected = names(house.data))
      ),
      
      conditionalPanel(
        'input.panelSelection === "Location Analysis"',
        helpText("Display barchart for top ten locations with highest average price.")
      ),
      
      conditionalPanel(
        'input.panelSelection === "Location Analysis"',
        helpText("Display Chart .")
      ),
      
      conditionalPanel(
        'input.panelSelection === "Condition Analysis"',
        helpText("Display Chart .")
      ),
      
      conditionalPanel(
        'input.panelSelection === "Grade Analysis"',
        helpText("Display Chart .")
      ),
      
      conditionalPanel(
        'input.panelSelection === "Infraustructure Analysis"',
        helpText("Display Chart .")
      ),
      
      conditionalPanel(
        'input.panelSelection === "Price Prediction"',
        helpText("Tell us your preference"),
        textInput("txt", "Text input:", "general"),
        sliderInput("Nobedroom", "Number of bedroom:", min=1, max=100, value = 3),
        sliderInput("livingSqft", "living Space(sqft):", min=1, max=1000000, value =50000),
        selectInput("location", "Zip:", 
                    choices=unique(house.data$zipcode)),
        actionButton("action", "Search")
      )
      
    ),
    mainPanel(
      tabsetPanel(
        id = 'panelSelection',
        tabPanel(title = "Overview", DT::dataTableOutput(outputId = "mytable1")),
        tabPanel(title="Location Analysis", plotlyOutput(outputId = "myplot1")),
        tabPanel(title="Time Series Analysis", plotlyOutput(outputId = "myplot2")),
        tabPanel(title="Condition Analysis", plotlyOutput(outputId = "myplot3_a"),
                 plotlyOutput(outputId = "myplot3_b")),
        tabPanel(title="Grade Analysis", plotlyOutput(outputId = "myplot4")),
        tabPanel(title="Infrastructure Analysis", plotlyOutput(outputId = "myplot5_a"),
                                                             plotlyOutput(outputId = "myplot5_b"),
                                                                        plotlyOutput(outputId = "myplot5_c")),
        tabPanel(title = "Price Prediction")
        
      )
    )
  )
)

server <- function(input, output) {
  
  ##################################1 Data Table Overview################################################
  # choose columns to display
  house.data2 = house.data[sample(nrow(house.data), 1000), ]

  output$mytable1 <- DT::renderDataTable({
    #price_range = input$Pricerange
    house.dataOutput = house.data2 %>% filter(house.data2$price >= input$Pricerange[1] 
                                              & house.data2$price<=input$Pricerange[2])
    DT::datatable(house.dataOutput[, input$show_vars, drop = FALSE])
  })
  
  ####################################2 Location Analysis#################################################
  output$myplot1 <- renderPlotly({
    price_location <- house.data[,c(3,17,18,19)]
    zipCode_Price <- price_location[,c(1,2)]
    groupped <- melt(zipCode_Price,id.vars = c("price","zipcode")) %>% group_by(zipcode)
    priceByZipcode <- groupped %>% summarise( price = mean(price))
    priceByZipcode <- priceByZipcode[order(-priceByZipcode$price),]
    priceByZipcode$zipcode <- factor(priceByZipcode$zipcode, levels = priceByZipcode$zipcode)
    TopTenPrice <- head(priceByZipcode,n = 10)
    
    p1 <- ggplot(TopTenPrice, aes(x=TopTenPrice$zipcode, y=TopTenPrice$price)) +
      geom_bar(stat="identity", width=.5, fill="tomato3") +
      labs(title="Ordered Mean Price&Zip Chart",
           subtitle="Price Vs Zipcode",
           caption="source: US dollar") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    ggplotly(p1)
    
  })
  
  ########################################3 Time Series Analysis##########################################
  output$myplot2 <- renderPlotly({
    housePriceByTime <- house.data[,c(2,3)]
    housePriceByTime[,1]<- substr(housePriceByTime[,1],start = 1,stop = 8)
    housePriceByTime[,1]<- as.Date(housePriceByTime[,1],"%Y%m%d")
    housePriceByTime$price <- as.numeric(housePriceByTime$price)
    housePriceByTime <- xts(as.matrix(housePriceByTime[,-1]), order.by = housePriceByTime$date)
    xts5_quarterly <- split(housePriceByTime,f="quarters")
    quaterlyAvgPrice <- lapply(xts5_quarterly,FUN=mean)
    df <- data.frame("price" = round(as.numeric(quaterlyAvgPrice),2),"quarter"=c("2014Q2","2014Q3","2014Q4","2015Q1","2015Q2"))
    
    pt <- ggplot(data=df,mapping = aes(x = df$quarter,y = df$price))+
      geom_bar(stat="identity", width=.5, fill="tomato3") + 
      labs(title="Ordered Mean Price&Quarter", 
           subtitle="Price Vs quarter", 
           caption="source: US dollar") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    ggplotly(pt)
  })
  
  ########################################4 House Condition Analysis##########################################
  output$myplot3_a <- renderPlotly({
    HouseCondition <- house.data[,c(3,11,15,16)]
    pc <- ggplot(HouseCondition, aes(x=yr_built, y=price)) + 
      geom_point() + 
      geom_smooth(method="loess", se=F) + 
      labs(subtitle="Price Vs built year", 
           y="Price", 
           x="year Built", 
           title="Scatterplot" )
    ggplotly(pc)

  })
  
  output$myplot3_b <- renderPlotly({
    HouseCondition <- house.data[,c(3,11,15,16)]
    revocated_Yes <- HouseCondition[HouseCondition[,4]!= 0,]
    pc1 <- ggplot(revocated_Yes, aes(x=yr_renovated, y=price)) + 
      geom_point() + 
      geom_smooth(method="loess", se=F) + 
      labs(subtitle="Price Vs built year", 
           y="Price", 
           x="year renovated", 
           title="Scatterplot" )
    ggplotly(pc1)
  })
  
  ###########################################5 Grade Analysis################################################
  output$myplot4 <- renderPlotly({
    HouseGrade <- house.data[,c(3,12)]
    fit <- lm(HouseGrade$price~HouseGrade$grade)
    
    pg <- ggplot() +
      geom_point(aes(x = HouseGrade$grade, y =HouseGrade$price),
                 colour = 'red') +
      geom_line(aes(x = HouseGrade$grade, y = fit$coefficients[1]+fit$coefficients[2]*as.numeric(HouseGrade$grade)),
                colour = 'blue') +
      ggtitle('Grade vs Price') +
      xlab('Grade') +
      ylab('Price')
    ggplotly(pg)
  })
  #########################################6 Infrastructure Analysis#######################################
  output$myplot5_a <- renderPlot({
    house.infrastruct = house.data[,c(1,3,4,5,8,9,13,14,20,21)]
    find.aggregate <- function(agg.col,grpby.col) {
      
      tempDF <- aggregate(agg.col ~ grpby.col, house.infrastruct,mean)
      barplot(tempDF$agg.col,ylab = "House Price", names.arg = tempDF$grpby.col)
    }
    
    find.aggregate(house.infrastruct$price,house.infrastruct$bedrooms)
  })
  
  output$myplot5_b <- renderPlot({
    house.infrastruct = house.data[,c(1,3,4,5,8,9,13,14,20,21)]
    find.aggregate <- function(agg.col,grpby.col) {
      
      tempDF <- aggregate(agg.col ~ grpby.col, house.infrastruct,mean)
      barplot(tempDF$agg.col,ylab = "House Price", names.arg = tempDF$grpby.col)
    }
    find.aggregate(house.infrastruct$price,house.infrastruct$bathrooms)
  })
  
  output$myplot5_c <- renderPlot({
    house.infrastruct = house.data[,c(1,3,4,5,8,9,13,14,20,21)]
    find.aggregate <- function(agg.col,grpby.col) {
      
      tempDF <- aggregate(agg.col ~ grpby.col, house.infrastruct,mean)
      barplot(tempDF$agg.col,ylab = "House Price", names.arg = tempDF$grpby.col)
    }
    find.aggregate(house.infrastruct$price,house.infrastruct$floors)
  })
  
  #########################################6 Price Prediction#######################################
}

shinyApp(ui, server)
