#Create Data Frame
#######################

library(readxl)

subscriptions <- read_excel(path = "subscriptions.xlsx",
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

price <- as.data.frame(read_xlsx(path="Cost of 1GB of Data.xlsx",
                                 sheet="Sheet1"))
price = price[2:ncol(price)]

cellData = merge.data.frame(subscriptions,price,by="Country")

colnames(cellData)[ncol(cellData)] = "Price"
cellRows = cellData[,1]
cellData = cellData[2:ncol(cellData)]
rownames(cellData) = cellRows



#Proof of Concept for LP 
#######################

library(lpSolve)
library(lpSolveAPI)

inputLowerPrice =1
inputHigherPrice = 4
inputLowerSubLevel = 50
inputHigherSubLevel = 80
inputNumCountries = 5
inputTopChoice = "Price"

testData = cellData

testData = as.data.frame(testData[testData["Price"]>inputLowerPrice,])
testData = as.data.frame(testData[testData["Price"]<inputHigherPrice,])
testData = as.data.frame(testData[testData["2020"]>inputLowerSubLevel,])
testData = as.data.frame(testData[testData["2020"]<inputHigherSubLevel,])

invest = make.lp(nrow=0,ncol=nrow(testData))
name.lp(invest,name="Countries to Invest In")

for(i in 1:nrow(testData)) {
  set.type(invest,column=i,type = "binary")
  colnames(invest)[i] = rownames(testData)[i]
}



lp.control(invest,sense="max")
set.objfn(invest,obj=testData[,inputTopChoice])


topCountryConst = matrix(data = 1,
                         nrow = 1,
                         ncol = ncol(invest))
add.constraint(invest, topCountryConst, "<=", inputNumCountries)

solve(invest)
objVal = get.objective(invest)
objVar = get.variables(invest)



  

objData = testData[objVar==1,]



objPrices = objData[,"Price"]



priceString = "Prices for Selected Countries: \n"
for(i in 1:nrow(objData)) {
  priceString = paste(priceString, rownames(objData)[i], ": ", objPrices[i], "\n")
} 

cat(priceString)


plotFrame = objData[1:(ncol(objData)-1)]

transPlotFrame = as.data.frame(t(plotFrame))
transPlotFrame$year = rownames(transPlotFrame)



library(ggplot2)
library(dplyr)
library(tidyr)

thisGraph =
  transPlotFrame %>%
  mutate(rn = row_name()) %>%
  pivot_longer(cols = -c(rn,year)) %>%
  ggplot(aes(x = year, y = value, color = name)) + 
  geom_line()


