setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Data/Crackers")
library(readxl)
library(MASS)
library(dplyr)
library(tidyr)

# import data
product2 <- read_excel('Crackers.xlsx', sheet = 2, col_names = TRUE)
product3 <- read_excel('Crackers.xlsx', sheet = 3, col_names = TRUE)
product4 <- read_excel('Crackers.xlsx', sheet = 4, col_names = TRUE)
product5 <- read_excel('Crackers.xlsx', sheet = 5, col_names = TRUE)

# separate "Week" column
product2 <- product2 %>% 
  separate(Week, c("S", "week"), " ")
product3 <- product3 %>% 
  separate(Week, c("S", "week"), " ")
product4 <- product4 %>% 
  separate(Week, c("S", "week"), " ")
product5 <- product5 %>% 
  separate(Week, c("S", "week"), " ")


# convert week into data format
product2$week <- as.Date(product2$week, "%d/%m/%y")
product3$week <- as.Date(product3$week, "%d/%m/%y")
product4$week <- as.Date(product4$week, "%d/%m/%y")
product5$week <- as.Date(product5$week, "%d/%m/%y")

# add week number
product2$week_Number <- 1:nrow(product2)
product3$week_Number <- 1:nrow(product3)
product4$week_Number <- 1:nrow(product4)
product5$week_Number <- 1:nrow(product5)

# calculate prices and log prices
product2$price <- product2$Value_Euros / product2$Volume
product3$price <- product3$Value_Euros / product3$Volume
product4$price <- product4$Value_Euros / product4$Volume
product5$price <- product5$Value_Euros / product5$Volume
product2$log.p <- log(product2$price)
product2$log.q <- log(product2$Volume)

# add average price and threshold, first threshold is 1 sd below mean
product2$average_Price <- mean(product2$price)
product2$threshold1 <- mean(product2$price) - sd(product2$price)
product2$promo <- ifelse(product2$price < product2$threshold1, 1, 0)

product3$average_Price <- mean(product3$price)
product3$threshold1 <- mean(product3$price) - sd(product3$price)
product3$promo <- ifelse(product3$price < product3$threshold1, 1, 0)

product4$average_Price <- mean(product4$price)
product4$threshold1 <- mean(product4$price) - sd(product4$price)
product4$promo <- ifelse(product4$price < product4$threshold1, 1, 0)

product5$average_Price <- mean(product5$price)
product5$threshold1 <- mean(product5$price) - sd(product5$price)
product5$promo <- ifelse(product5$price < product5$threshold1, 1, 0)

# plot sales and price for each product

# product 2
par(mar=c(5,4,4,5)+.1)
plot(product2$week_Number, product2$price, type="l", col="blue", ylim=c(0,3))
#lines(product2$average_Price, lty=2, lwd=2)
lines(product2$threshold1, lty=2, lwd=2)
lines(product2$promo)   # c is the promotion dummy from the other RMD
par(new=TRUE)
plot(product2$week_Number, product2$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 3
par(mar=c(5,4,4,5)+.1)
plot(product3$week_Number, product3$price, type="l", col="blue", ylim=c(0,5.5))
lines(product3$threshold1, lty=2, lwd=2)
lines(product3$promo)
par(new=TRUE)
plot(product3$week_Number, product3$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 4
par(mar=c(5,4,4,5)+.1)
plot(product4$week_Number, product4$price, type="l", col="blue", ylim=c(0,7))
lines(product4$threshold1, lty=2, lwd=2)
lines(product4$promo)
par(new=TRUE)
plot(product4$week_Number, product4$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 5
par(mar=c(5,4,4,5)+.1)
plot(product5$week_Number, product5$price, type="l", col="blue", ylim=c(0,6))
lines(product5$threshold1, lty=2, lwd=2)
lines(product5$promo)
par(new=TRUE)
plot(product5$week_Number, product5$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")


### ratio of sd to mean
sd(product2$price)/mean(product2$price)
# 3.5% seems a bit low for a promotion. think 2 for 1...

# number of promotions during two year period, using 1 sd method
sum(product2$promo)/ nrow(product2)
# 15%, seems way too low

# average size of promotion ~24%
# http://www.mosaicmarketing.co.uk/single-post/2016/11/30/FMCG-brands-cut-price-promotions---a-bit

# possible method to identify the promotions periods:
# find price promotions with threshold method (need to find a way to define base price)
# then look at magnitude of sales and identify where sales have a similar magnitude to the 
# price promo and deduce that those are other types of promo (feat or aisle)

# attempt at adding the promotions manually (feature and aisle which is not visible in price)
# also set promo to one when GRP is available

# no GRP
product2[12:22, "promo"] <- 1
product2[45:59, "promo"] <- 1
product2[77:85, "promo"] <- 1

product3$promo <- ifelse(product3$GRP != 0, 1, 0)
product3[18:31, "promo"] <- 1
product3[48:55, "promo"] <- 1
product3[99:105, "promo"] <- 1

product4$promo <- ifelse(product4$GRP != 0, 1, 0)
product4[42:54, "promo"] <- 1

# no GRP
product5[17:35, "promo"] <- 1
product5[69:74, "promo"] <- 1

# view new graphs with added promotions periods

# product 2
par(mar=c(5,4,4,5)+.1)
plot(product2$week_Number, product2$price, type="l", col="blue", ylim=c(0,3))
#lines(product2$average_Price, lty=2, lwd=2)
lines(product2$threshold1, lty=2, lwd=2)
lines(product2$promo)   # c is the promotion dummy from the other RMD
par(new=TRUE)
plot(product2$week_Number, product2$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 3
par(mar=c(5,4,4,5)+.1)
plot(product3$week_Number, product3$price, type="l", col="blue", ylim=c(0,5.5))
lines(product3$threshold1, lty=2, lwd=2)
lines(product3$promo)
par(new=TRUE)
plot(product3$week_Number, product3$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 4
par(mar=c(5,4,4,5)+.1)
plot(product4$week_Number, product4$price, type="l", col="blue", ylim=c(0,7))
lines(product4$threshold1, lty=2, lwd=2)
lines(product4$promo)
par(new=TRUE)
plot(product4$week_Number, product4$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")

# product 5
par(mar=c(5,4,4,5)+.1)
plot(product5$week_Number, product5$price, type="l", col="blue", ylim=c(0,6))
lines(product5$threshold1, lty=2, lwd=2)
lines(product5$promo)
par(new=TRUE)
plot(product5$week_Number, product5$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Volume",side=4,line=3)
legend("topleft",col=c("blue","red"), lty=1, legend=c("Price","Volume"), cex=0.7, bty = "n")


# proportion of promotions during two year period, after adding manually
sum(product2$promo)/ nrow(product2)
sum(product3$promo)/ nrow(product3)
sum(product4$promo)/ nrow(product4)
sum(product5$promo)/ nrow(product5)
# between 30-42%

# add the baseline in excel and read the data again
# write.csv(product2, file = "product2.csv")
# write.csv(product3, file = "product3.csv")
# write.csv(product4, file = "product4.csv")
# write.csv(product5, file = "product5.csv")

# load files with baseline
setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail group")

prod2 <- read.csv("product2.csv")
prod3 <- read.csv("product3.csv")
prod4 <- read.csv("product4.csv")
prod5 <- read.csv("product5.csv")

# fill baseline column with regular volumes
prod2$baseline <- ifelse(is.na(prod2$baseline), prod2$Volume, prod2$baseline)
prod3$baseline <- ifelse(is.na(prod3$baseline), prod3$Volume, prod3$baseline)
prod4$baseline <- ifelse(is.na(prod4$baseline), prod4$Volume, prod4$baseline)
prod5$baseline <- ifelse(is.na(prod5$baseline), prod5$Volume, prod5$baseline)

# plot baseline and lift

# product 2
plot(prod2$week_Number, prod2$baseline, type="l", lwd=2, ylim = c(0, max(prod2$Volume)))
lines(prod2$weekly_lift + prod2$baseline, lwd=2, col = 11)

# product 3
plot(prod3$week_Number, prod3$baseline, type="l", lwd=2, ylim = c(0, max(prod3$Volume)))
lines(prod3$weekly_lift + prod3$baseline, lwd=2, col = 11)

# product 4
plot(prod4$week_Number, prod4$baseline, type="l", lwd=2, ylim = c(0, max(prod4$Volume)))
lines(prod4$weekly_lift + prod4$baseline, lwd=2, col = 11)

# product 5
plot(prod5$week_Number, prod5$baseline, type="l", lwd=2, ylim = c(0, max(prod5$Volume)))
lines(prod5$weekly_lift + prod5$baseline, lwd=2, col = 11)

# graph 2,3,5 look reasonably ok, graph 4 looks a bit strange because has negative lift
# and irregular promos
# graphs don't look exactly like hers which kinda sucks too



