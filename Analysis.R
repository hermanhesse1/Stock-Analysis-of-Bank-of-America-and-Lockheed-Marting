data <- read.csv('homework.csv')
dim(data)
head(data)
head(data$RET)
head(data[,6])
names(data)
unique(data$PERMNO)
unique(data$TICKER)
unique(data$COMNAM)
america <- data[data$PERMNO == 59408,]
dim(america)
head(america)
tail(america)
unique(america$TICKER)
highest_america <- max(america$RET) * 100
paste0("Highest return for Bank of America: ", highest_america, "%")
lowest_america <- min(america$RET) * 100
paste0("Lowest return for Bank of America: ", lowest_america , "%")
install.packages("zoo")
install.packages("lubridate")
install.packages("tseries")
install.packages("rugarch")
install.packages("rmgarch")
install.packages("reshape2")
install.packages("car")
library(reshape2)
library(lubridate)
data <- read.csv("homework.csv")
head(data)
class(data)
data$Unadjusted_Prices <- data$PRC
data$Adjusted_Prices <- data$PRC / data$CFACPR
head(data)
Tickers= dcast(data, date ~ PERMNO, value.var = "TICKER")
Tickers=tail(Tickers,1)
Tickers
Prices = dcast(data, date ~ PERMNO, value.var = "Adjusted_Prices")
names(Prices) <- Tickers
names(Prices)[1]="date"
head(Prices)
dim(Prices)
UnAdjustedPrices = dcast(data, date ~ PERMNO, value.var = "PRC")
names(UnAdjustedPrices) <- Tickers
names(UnAdjustedPrices)[1]="date"
head(UnAdjustedPrices)
dim(UnAdjustedPrices)
simpleReturns <- dcast(data, date ~ PERMNO, value.var = "RET")
names(simpleReturns) <- Tickers
names(simpleReturns)[1]="date"
head(simpleReturns)
Returns <- log(1 + simpleReturns[,2:dim(simpleReturns)[2]])
Returns$date <- simpleReturns$date
head(Returns)
Returns =Returns[,c(dim(Returns)[2],1:(dim(Returns)[2]-1))]
head(Returns)
class(Returns$date)
date.ts <- ymd(Returns$date)
class(date.ts)
save(simpleReturns, file = "simpleReturns.RData")
save(Returns, file = "Returns.RData")
save(Prices, file = "Prices.RData")
save(UnAdjustedPrices, file = "UnAdjustedPrices.RData")
load("Prices.RData")
head(Prices)
write.csv(Prices, file = "Prices.csv")
write.csv(Returns, file = "Returns.csv")
plot(date.ts, Prices$BAC,
     type = "l",
     main = "Stock price of Bank of America",
     ylab = "Returns",
     xlab = "Date",
     col = "red",
     las = 1
)
plot(date.ts, Prices$LMT,
     type = "l",
     main = "Stock price of Lockheed Martin",
     ylab = "Returns",
     xlab = "Date",
     col = "green",
     las = 1
)
matplot(date.ts, Prices[,2:dim(Prices)[2]],
        type = "l",
        lty=1,
        main = "Prices ",
        ylab = "Price",
        xlab = "Date",
        col = 1:9,
        las=1
)
plot(date.ts, Returns$BAC,
     type = "l",
     main = "Returns of Bank of America",
     ylab = "Returns",
     xlab = "Date",
     col = "blue",
     las = 1
)
plot(date.ts, Returns$LMT,
     type = "l",
     main = "Returns of Lockheed Martin",
     ylab = "Returns",
     xlab = "Date",
     col = "black",
     las = 1
)
matplot(date.ts,Returns[,2:dim(Returns)[2]],
        type = "l",
        main = "Returns for our stocks",
        ylab = "Returns",
        lty = 1,
        las=1
)
legend("bottomright",
       legend = names(Returns[,2:dim(Returns)[2]]),
       col = c(1:6),
       lty=1,
       bty='n'
)
install.packages("tinytex")
tinytex::install_tinytex()
library(tseries)
library(car)
library(lubridate)
library(zoo)
library(moments)
y=Returns$LMT
mean(y)
sd(y)
skewness(y)
kurtosis(y)
jarque.bera.test(y)
Box.test(y, type = "Ljung-Box")
Box.test(y^2, type = "Ljung-Box")

z=Returns$BAC
mean(z)
sd(z)
skewness(z)
kurtosis(z)
jarque.bera.test(z)
Box.test(z, type = "Ljung-Box")
Box.test(z^2, type = "Ljung-Box")


