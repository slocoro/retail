library(tseries)
library(lubridate)

#READ DATA
radio <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/sales_radio_2.csv")
tv <- read.csv('/Users/laradaoud/retail_assign2/Assignment 2/sales_tv_2.csv')

plot(radio$sales,type = "o",  col="blue",
     ylab = "", xlab = "Time",main = "Sales")

sales_ts <- ts(radio$sales, frequency = 7) 
plot(stl(sales_ts, 'periodic'))


#CHECK FOR UNIT ROOTS
adf.test(radio$sales)
pp.test(radio$sales)
kpss.test(radio$sales)

adf.test(radio$visits)
pp.test(radio$visits)
kpss.test(radio$visits)
#no unit root


#ADD LAGGED SALES AND VISITS
radio$lag_sales <- c(NA, head(radio$sales, -1))
radio$lag_visits <- c(NA, head(radio$visits, -1))

radio$lag2_sales <- c(NA, NA, head(radio$sales, -2))
radio$lag2_visits <- c(NA, NA, head(radio$visits, -2))

radio$lag3_sales <- c(NA, NA, NA, head(radio$sales, -3))
radio$lag3_visits <- c(NA, NA, NA, head(radio$visits, -3))

radio$BUSINESS_DATE <- format(as.Date(radio$BUSINESS_DATE, "%m/%d/%y"))
radio$month <- as.character(month(as.Date(radio$BUSINESS_DATE)))

tv$lag1_sales <- c(NA, head(tv$sales, -1))
tv$lag1_visits <- c(NA, head(tv$visits, -1))

tv$lag2_sales <- c(NA, NA, head(tv$sales, -2))
tv$lag2_visits <- c(NA, NA, head(tv$visits, -2))

tv$lag3_sales <- c(NA, NA, NA, head(tv$sales, -3))
tv$lag3_visits <- c(NA, NA, NA, head(tv$visits, -3))

tv$BUSINESS_DATE <- format(as.Date(tv$BUSINESS_DATE, "%m/%d/%y"))
tv$month <- as.character(month(as.Date(tv$BUSINESS_DATE)))



#AGGREGATE RADIO 15 SECOND ADS
radio["X15.secondi"] <- 0

radio$X15.secondi <- rowSums(radio[,grep("X15.secondi", names(radio))])

#AGGREGATE RADIO 20 SECOND ADS
radio["X20.secondi"] <- 0

radio$X20.secondi <- rowSums(radio[,grep("X20.secondi", names(radio))])

#AGGREGATE RADIO 30 SECOND ADS
radio["X30.secondi"] <- 0

radio$X30.secondi <- rowSums(radio[,grep("X30.secondi", names(radio))])

#AGGREGATE TV ADS
tv$X10.secondi <- 0
tv$X15.secondi <- 0
tv$X20.secondi <- 0
tv$X30.secondi <- 0
tv$X45.secondi <- 0

tv$X10.secondi <- rowSums(tv[ ,grep("X10.secondi", names(tv))])
tv$X15.secondi <- rowSums(tv[ ,grep("X15.secondi", names(tv))])
tv$X20.secondi <- rowSums(tv[ ,grep("X20.secondi", names(tv))])
tv$X30.secondi <- rowSums(tv[ ,grep("X30.secondi", names(tv))])
tv$X45.secondi <- rowSums(tv[ ,grep("X45.secondi", names(tv))])


#ADD LOG AND SQR FOR EACH FORMAT
radio$log.15.secondi <- log(radio$X15.secondi) 
radio$sqr.15.secondi <- radio$X15.secondi^2

radio$log.20.secondi <- log(radio$X20.secondi) 
radio$sqr.20.secondi <- radio$X20.secondi^2

radio$log.30.secondi <- log(radio$X30.secondi) 
radio$sqr.30.secondi <- radio$X30.secondi^2


tv$log.10.secondi <- log(tv$X10.secondi) 
tv$sqr.10.secondi <- tv$X10.secondi^2

tv$log.15.secondi <- log(tv$X15.secondi) 
tv$sqr.15.secondi <- tv$X15.secondi^2

tv$log.20.secondi <- log(tv$X20.secondi) 
tv$sqr.20.secondi <- tv$X20.secondi^2

tv$log.30.secondi <- log(tv$X30.secondi) 
tv$sqr.30.secondi <- tv$X30.secondi^2

tv$log.45.secondi <- log(tv$X45.secondi) 
tv$sqr.45.secondi <- tv$X45.secondi^2



#ADD ADSTOCK AND LAGGED ADDSTOCK
#for linear model
radio$X15.secondi.adstock.1 <- 0 + radio$X15.secondi
radio$X20.secondi.adstock.1 <- 0 + radio$X20.secondi
radio$X30.secondi.adstock.1 <- 0 + radio$X30.secondi

radio$lag.adstock.15.1 <- c(NA, head(radio$X15.secondi.adstock.1, -1))
radio$lag.adstock.20.1 <- c(NA, head(radio$X20.secondi.adstock.1, -1))
radio$lag.adstock.30.1 <- c(NA, head(radio$X30.secondi.adstock.1, -1))

tv$X10.secondi.adstock.1 <- 0 + tv$X10.secondi
tv$X15.secondi.adstock.1 <- 0 + tv$X15.secondi
tv$X20.secondi.adstock.1 <- 0 + tv$X20.secondi
tv$X30.secondi.adstock.1 <- 0 + tv$X30.secondi
tv$X40.secondi.adstock.1 <- 0 + tv$X45.secondi

tv$lag.adstock.10.1 <- c(NA, head(tv$X10.secondi.adstock.1, -1))
tv$lag.adstock.15.1 <- c(NA, head(tv$X15.secondi.adstock.1, -1))
tv$lag.adstock.20.1 <- c(NA, head(tv$X20.secondi.adstock.1, -1))
tv$lag.adstock.30.1 <- c(NA, head(tv$X30.secondi.adstock.1, -1))
tv$lag.adstock.45.1 <- c(NA, head(tv$X45.secondi.adstock.1, -1))

#for concave model
radio$X15.secondi.adstock.2 <- 0 + radio$X15.secondi
radio$X20.secondi.adstock.2 <- 0 + radio$X20.secondi
radio$X30.secondi.adstock.2 <- 0 + radio$X30.secondi

radio$lag.adstock.15.2 <- c(NA, head(radio$X15.secondi.adstock.2, -1))
radio$lag.adstock.20.2 <- c(NA, head(radio$X20.secondi.adstock.2, -1))
radio$lag.adstock.30.2 <- c(NA, head(radio$X30.secondi.adstock.2, -1))

#for concave qudratic model
radio$X15.secondi.adstock.3 <- 0 + radio$X15.secondi
radio$X20.secondi.adstock.3 <- 0 + radio$X20.secondi
radio$X30.secondi.adstock.3 <- 0 + radio$X30.secondi

radio$lag.adstock.15.3 <- c(NA, head(radio$X15.secondi.adstock.3, -1))
radio$lag.adstock.20.3 <- c(NA, head(radio$X20.secondi.adstock.3, -1))
radio$lag.adstock.30.3 <- c(NA, head(radio$X30.secondi.adstock.3, -1))

tv$X10.secondi.adstock.3 <- 0 + tv$X10.secondi
tv$X15.secondi.adstock.3 <- 0 + tv$X15.secondi
tv$X20.secondi.adstock.3 <- 0 + tv$X20.secondi
tv$X30.secondi.adstock.3 <- 0 + tv$X30.secondi
tv$X40.secondi.adstock.3 <- 0 + tv$X45.secondi

tv$lag.adstock.10.3 <- c(NA, head(tv$X10.secondi.adstock.3, -1))
tv$lag.adstock.15.3 <- c(NA, head(tv$X15.secondi.adstock.3, -1))
tv$lag.adstock.20.3 <- c(NA, head(tv$X20.secondi.adstock.3, -1))
tv$lag.adstock.30.3 <- c(NA, head(tv$X30.secondi.adstock.3, -1))
tv$lag.adstock.45.3 <- c(NA, head(tv$X45.secondi.adstock.3, -1))

#complete models
radio <- radio[!is.na(radio$lag3_sales),]
tv <- tv[!is.na(tv$lag3_sales),]

#RADIO
#Without carryover
#linear
radio.1.sales <- lm(sales ~  weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.1.visits <- lm(visits ~ weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)

#concave
radio.2.sales <- lm(sales ~ weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.2.visits <- lm(visits ~ weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)

#concave qudratic
radio.3.sales <- lm(sales ~ weekday + month + X15.secondi + X20.secondi + X30.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)
radio.3.visits <- lm(visits ~ weekday + month + X15.secondi + X20.secondi + X30.secondi + + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)


#With carryover
#linear
radio.4.sales <- lm(sales ~  lag_sales + weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.4.visits <- lm(visits ~ lag_visits + weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)

#concave
radio.5.sales <- lm(sales ~ lag_sales + lag2_sales + lag3_sales + weekday + month + X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.5.visits <- lm(visits ~ lag_visits + lag2_visits + lag3_visits + weekday + month + log.15.secondi + log.20.secondi + log.30.secondi, data = radio)

#concave qudratic
summary(radio.6.sales <- lm(sales ~ lag_sales + lag2_sales + lag3_sales + weekday + month + X15.secondi + X20.secondi + X30.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio))
summary(radio.6.visits <- lm(visits ~ lag_visits + lag2_visits + lag3_visits + weekday + month +X15.secondi + X20.secondi + X30.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio))


#TV
#With carryover
#linear
summary(tv.4.sales <- lm(sales ~  lag1_sales + lag2_sales + lag3_sales + weekday + month + X10.secondi + X15.secondi +  X20.secondi + X30.secondi + X45.secondi, data = tv))
#significant vars for sales: lag1 (t-1), lag2 (t-2); days: mon, sat, thurs, wed; month: feb, dec; X.20.secondi
summary(tv.4.visits <- lm(visits ~ lag1_visits + lag2_visits + lag3_visits + weekday + month + X10.secondi + X15.secondi +  X20.secondi + X30.secondi + X45.secondi, data = tv))
#significant vars for visits: lag1 (t-1), lag2 (t-2); days: all; month: july, sep, dec; X.20.secondi, X15.secondi


#concave qudratic
summary(tv.6.sales <- lm(sales ~ lag1_sales + lag2_sales + lag3_sales + weekday + month + X10.secondi + X15.secondi + X20.secondi + X30.secondi + X45.secondi + sqr.10.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi + sqr.45.secondi, data = tv))
#significant vars for sales: lag1 (t-1), lag2 (t-2); days: mon, sat, thurs, wed; month: feb, dec; X.20.secondi, sqr.20.secondi
summary(tv.6.visits <- lm(visits ~ lag1_visits + lag2_visits + lag3_visits + weekday + month + X10.secondi + X15.secondi +  X20.secondi + X30.secondi + X45.secondi + sqr.10.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi + sqr.45.secondi, data = tv))
#significant vars for visits: lag1 (t-1), lag2 (t-2); days: all; month: july, sep, dec; X.20.secondi, sqr.20.secondi



#Adstock
alpha.values <- c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#model 1
grid.model1 <- list()


rmse <- function(error)
{
  sqrt(mean(error^2))
}

i <- 1
for (value in alpha.values){
  radio$X15.secondi.adstock.1 <- value * radio$lag.adstock.15.1 + radio$X15.secondi
  radio$lag.adstock.15.1 <- c(NA, head(radio$X15.secondi.adstock.1, -1))
  radio$X20.secondi.adstock.1 <- value * radio$lag.adstock.20.1 + radio$X20.secondi
  radio$lag.adstock.20.1 <- c(NA, head(radio$X20.secondi.adstock.1, -1))
  radio$X30.secondi.adstock.1 <- value * radio$lag.adstock.30.1 + radio$X30.secondi
  radio$lag.adstock.30.1 <- c(NA, head(radio$X30.secondi.adstock.1, -1))
  radio.1.2.adstock <- lm(sales ~  X15.secondi + X20.secondi + X30.secondi + X15.secondi.adstock.1 + X20.secondi.adstock.1 + X30.secondi.adstock.1, data = radio)
  grid.model1[i] <- rmse(fitted(radio.1.2.adstock) - radio$sales)
  i <- i+1
}

print(grid.model1)
#alpha 0.7



#model 2
grid.model2 <- list()


k <- 1
for (value in alpha.values){
  radio$X15.secondi.adstock.2 <- value * radio$lag.adstock.15.2 + radio$X15.secondi
  radio$lag.adstock.15.2 <- c(NA, head(radio$X15.secondi.adstock.2, -1))
  radio$X20.secondi.adstock.2 <- value * radio$lag.adstock.20.2 + radio$X20.secondi
  radio$lag.adstock.20.2 <- c(NA, head(radio$X20.secondi.adstock.2, -1))
  radio$X30.secondi.adstock.2 <- value * radio$lag.adstock.30.2 + radio$X30.secondi
  radio$lag.adstock.30.2 <- c(NA, head(radio$X30.secondi.adstock.2, -1))
  radio.2.2.adstock <- lm(sales ~  log.15.secondi + log.20.secondi + log.30.secondi + X15.secondi.adstock.2 + X20.secondi.adstock.2 + X30.secondi.adstock.2, data = radio)
  grid.model2[k] <- rmse(fitted(radio.2.2.adstock) - radio$sales)
  k <- k+1
}

print(grid.model2)


#model 3
grid.model3 <- list()


j <- 1
for (value in alpha.values){
  radio$X15.secondi.adstock.3 <- value * radio$lag.adstock.15.3 + radio$X15.secondi
  radio$lag.adstock.15.3 <- c(NA, head(radio$X15.secondi.adstock.3, -1))
  radio$X20.secondi.adstock.3 <- value * radio$lag.adstock.20.3 + radio$X20.secondi
  radio$lag.adstock.20.3 <- c(NA, head(radio$X20.secondi.adstock.3, -1))
  radio$X30.secondi.adstock.3 <- value * radio$lag.adstock.30.3 + radio$X30.secondi
  radio$lag.adstock.30.3 <- c(NA, head(radio$X30.secondi.adstock.3, -1))
  radio.3.2.adstock <- lm(sales ~ X15.secondi + X20.secondi + X30.secondi +  sqr.15.secondi + sqr.20.secondi + sqr.30.secondi + X15.secondi.adstock.3 + X20.secondi.adstock.3 + X30.secondi.adstock.3, data = radio)
  grid.model3[j] <- rmse(fitted(radio.3.2.adstock) - radio$sales)
  j <- j+1
}

print(grid.model3)
#alhpa 0.7


