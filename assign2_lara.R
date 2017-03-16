library(tseries)

#READ DATA
radio <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/sales_radio_2.csv")
tv <- read.csv('/Users/laradaoud/retail_assign2/Assignment 2/sales_tv_2.csv')

plot(radio$sales,type = "o",  col="blue",
     ylab = "", xlab = "Time",main = "Sales")

sales_ts<- ts(radio$sales, frequency = 7) 
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

tv$lag_sales <- c(NA, head(tv$sales, -1))
tv$lag_visits <- c(NA, head(tv$visits, -1))




#AGGREGATE RADIO 15 SECOND ADS
radio["X15.secondi"] <- 0

radio$X15.secondi <- rowSums(radio[,grep("X15.secondi", names(radio))])

#AGGREGATE RADIO 20 SECOND ADS
radio["X20.secondi"] <- 0

radio$X20.secondi <- rowSums(radio[,grep("X20.secondi", names(radio))])

#AGGREGATE RADIO 30 SECOND ADS
radio["X30.secondi"] <- 0

radio$X30.secondi <- rowSums(radio[,grep("X30.secondi", names(radio))])

#ADD LOG AND SQR FOR EACH FORMAT
radio$log.15.secondi <- log(radio$X15.secondi) 
radio$sqr.15.secondi <- radio$X15.secondi^2

radio$log.20.secondi <- log(radio$X20.secondi) 
radio$sqr.20.secondi <- radio$X20.secondi^2

radio$log.30.secondi <- log(radio$X30.secondi) 
radio$sqr.30.secondi <- radio$X30.secondi^2

#ADD ADSTOCK AND LAGGED ADDSTOCK
#for linear model
radio$X15.secondi.adstock.1 <- 0 + radio$X15.secondi
radio$X20.secondi.adstock.1 <- 0 + radio$X20.secondi
radio$X30.secondi.adstock.1 <- 0 + radio$X30.secondi

radio$lag.adstock.15.1 <- c(NA, head(radio$X15.secondi.adstock.1, -1))
radio$lag.adstock.20.1 <- c(NA, head(radio$X20.secondi.adstock.1, -1))
radio$lag.adstock.30.1 <- c(NA, head(radio$X30.secondi.adstock.1, -1))

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



#complete models
radio <- radio[!is.na(radio$lag_sales),]
tv <- tv[!is.na(tv$lag_sales),]


#Without carryover
#linear
radio.1.sales <- lm(sales ~  X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.1.visits <- lm(visits ~ weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)

#concave
radio.2.sales <- lm(sales ~ weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.2.visits <- lm(visits ~ weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)


#concave qudratic
radio.3.sales <- lm(sales ~ weekday +  X15.secondi + X20.secondi + X30.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)
radio.3.visits <- lm(visits ~ weekday +  X15.secondi + X20.secondi + X30.secondi + + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)


#With carryover
#linear
radio.4.sales <- lm(sales ~  lag_sales + weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.4.visits <- lm(visits ~ lag_visits + weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)

#concave
radio.5.sales <- lm(sales ~ lag_sales + weekday +  X15.secondi + X20.secondi + X30.secondi, data = radio)
radio.5.visits <- lm(visits ~ lag_visits + weekday +  log.15.secondi + log.20.secondi + log.30.secondi, data = radio)


#concave qudratic
radio.6.sales <- lm(sales ~ lag_sales + weekday +  X15.secondi + X20.secondi + X30.secondi + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)
radio.6.visits <- lm(visits ~ lag_visits + weekday +  X15.secondi + X20.secondi + X30.secondi + + sqr.15.secondi + sqr.20.secondi + sqr.30.secondi, data = radio)

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


