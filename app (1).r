#Create Data Frame
#######################

library(readxl)

subscriptions <- read_excel(path = "/Users/sophia/subscriptions.xlsx", #change the path
                            sheet="subscriptions",
                            col_names=FALSE)

subCols= subscriptions[5,]
subscriptions = as.data.frame(subscriptions[6:nrow(subscriptions),])
colnames(subscriptions) = subCols


subscriptions = subscriptions[c("Country Name","1980":"2020")]
for(i in 2:ncol(subscriptions)) {
  subscriptions[which(is.na(subscriptions[,i])),i] <- 0
}

colnames(subscriptions)[1] = 'Country'

price <- as.data.frame(read_xlsx(path="/Users/sophia/Cost of 1GB of Data.xlsx", #change the path
                                 sheet="Sheet1"))
price = price[2:ncol(price)]

cellData = merge.data.frame(subscriptions,price,by="Country")

colnames(cellData)[ncol(cellData)] = "Price"
cellRows = cellData[,1]
cellData = cellData[2:ncol(cellData)]
rownames(cellData) = cellRows

#LP app ui
#######################
library(shiny)

ui = fluidPage(
  titlePanel('Mobile Cellular Subscriptions & Data Pricing Explorer'),
  sidebarLayout(
    sidebarPanel(
      h4("Filter the countries:"),
      sliderInput('minPrice', 'Set the min. data cost', value = 1, min = 0, max = 28),
      sliderInput('maxPrice', 'Set the max. data cost', value = 4, min = 0, max = 28),
      sliderInput('minSub', 'Set the min. mobile cellular subscription', value = 50, min = 12, max = 431),
      sliderInput('maxSub', 'Set the max. mobile cellular subscription', value = 80, min = 12, max = 431),
      selectInput('numCountry', 'Set the number of countries to invest in:', c(1:10)
    )
  ),
  mainPanel(
    column (9,
     h4("For countries satisfying the input conditions, below is the optimized country list as the top investment choices."),
     h6("Mobile cellular subscriptions trend by year (per 100 people)"),
     plotOutput('plot1', width = 600),
     h6("Data price"),
     verbatimTextOutput("txt1"))
    )
  )
)



#LP app server
#######################
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)
library(dplyr)
library(tidyr)

server <- function(input, output) {
  testData = cellData

  selectedData <- reactive({
    testData[which(testData["Price"] > input$minPrice & testData["Price"] < input$maxPrice &
                     testData["2020"] > input$minSub & testData["2020"] < input$maxSub),]
  })
  
  inputTopChoice = "Price"
  
  output$txt1 <- renderPrint({
    
    invest = make.lp(nrow=0,ncol=nrow(selectedData()))
    name.lp(invest,name="Countries to Invest In")
    
    for(i in 1:nrow(selectedData())) {
      set.type(invest,column=i,type = "binary")
      colnames(invest)[i] = rownames(selectedData())[i]
    }
    
    lp.control(invest,sense="max")
    set.objfn(invest,obj=selectedData()[, inputTopChoice])
    
    
    topCountryConst = matrix(data = 1,
                             nrow = 1,
                             ncol = ncol(invest))
    add.constraint(invest, topCountryConst, "<=", input$numCountry)
    
    solve(invest)
    objVal = get.objective(invest)
    objVar = get.variables(invest)
    
    priceString = "Cost of 1GB of Data for Selected Countries: \n"
    for(i in 1:nrow(selectedData()[objVar==1,])) {
      priceString = paste(priceString, rownames(selectedData()[objVar==1,])[i], ": ", selectedData()[objVar==1,][,"Price"][i], "\n")
    } 
    
    cat(priceString)
    
  })
  
  output$plot1 <- renderPlot({
    #objData = selectedData()[objVar==1,]
    #plotFrame = selectedData()[objVar==1,][1:(ncol(selectedData()[objVar==1,])-1)]
    
    invest = make.lp(nrow=0,ncol=nrow(selectedData()))
    name.lp(invest,name="Countries to Invest In")
    
    for(i in 1:nrow(selectedData())) {
      set.type(invest,column=i,type = "binary")
      colnames(invest)[i] = rownames(selectedData())[i]
    }
    
    lp.control(invest,sense="max")
    set.objfn(invest,obj=selectedData()[, inputTopChoice])
    
    
    topCountryConst = matrix(data = 1,
                             nrow = 1,
                             ncol = ncol(invest))
    add.constraint(invest, topCountryConst, "<=", input$numCountry)
    
    solve(invest)
    objVal = get.objective(invest)
    objVar = get.variables(invest)
    
    transPlotFrame = as.data.frame(t(selectedData()[objVar==1,][,1:(ncol(selectedData()[objVar==1,])-1)]))
    transPlotFrame$year = rownames(transPlotFrame)
    
    transPlotFrame %>%
    pivot_longer(!year, names_to = "country", values_to = "value") %>%
    ggplot(aes(x = year, y = value, group = country, color=country)) + geom_line()
    
  }) 
}

shinyApp(ui = ui, server = server)

