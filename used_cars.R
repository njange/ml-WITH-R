install.packages("quantmod")
#install.packages("dplyr")
#install.packages("lubridate")

library(quantmod)
library(ggplot2)
library(dplyr)
library(lubridate)

getSymbols("^GSPC", src = "yahoo")

mydata <- data.frame(Date = index(GSPC), coredata(GSPC))

str(mydata)
summary(mydata)
summary(mydata$GSPC.Close)

mydata$Date <- as.Date(mydata$Date, "%Y-%m-%d")

str(mydata$Date)

ggplot(data=mydata, aes(x=Date, y=GSPC.Close)) + geom_line()
 
mydata<- select(mydata, Date, GSPC.Close)
mydata <- rename(mydata, sp500= GSPC.Close)

mydata <-filter(mydata, Date>=as.Date("202-01-01"))
mydata$sp500_lag <- lag(mydata$sp500)

mydata$sp500return <- (mydata$sp500 - mydata$sp500_lag)/mydata$sp500_lag*100
mydata <- arrange(mydata, sp500return)

mydata$month = month(mydata$Date)
monthly <- filter(mydata, mydata$month != lead(mydata$month))
monthly <- select(monthly, Date,month)
head(monthly)
