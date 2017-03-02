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

product3$log.p <- log(product3$price)
product3$log.q <- log(product3$Volume)

product4$log.p <- log(product4$price)
product4$log.q <- log(product4$Volume)

product5$log.p <- log(product5$price)
product5$log.q <- log(product5$Volume)


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
product2[1:6, "promo"] <- 1
product2[11:15, "promo"] <- 1
product2[18:21, "promo"] <- 1
product2[24:26, "promo"] <- 1
product2[29:30, "promo"] <- 1
product2[63:65, "promo"] <- 1
product2[69:70, "promo"] <- 1
product2[77:85, "promo"] <- 1

product3$ad <- ifelse(product3$GRP != 0, 1, 0)
product3[11:15, "promo"] <- 1
product3[18:22, "promo"] <- 1
product3[25:32, "promo"] <- 1
product3[48:55, "promo"] <- 1
product3[80:89, "promo"] <- 1
product3[99:105, "promo"] <- 1

product4$ad <- ifelse(product4$GRP != 0, 1, 0)
product4[42:54, "promo"] <- 1
product4[64, "promo"] <- 1
product4[79:81, "promo"] <- 1
product4[98:102, "promo"] <- 1

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


## calculate baseline using a regression

# first eliminate all the promo weeks
p2noPromo <- filter(product2, promo == 0)
p3noPromo <- filter(product3, promo == 0)
p4noPromo <- filter(product4, promo == 0)
p5noPromo <- filter(product5, promo == 0)

# drop all columns that won't be used for the regression
# didn't include Value because its used to calculate price therefore seems wrong to include it
p2reg <- select(p2noPromo, 
                Volume,
                Average_Number_SKUs,
                Average_Distribution,
                week_Number,
                price)

p3reg <- select(p3noPromo, 
                Volume,
                Average_Number_SKUs,
                Average_Distribution,
                week_Number,
                price)

p4reg <- select(p4noPromo, 
                Volume,
                Average_Number_SKUs,
                Average_Distribution,
                week_Number,
                price)

p5reg <- select(p5noPromo, 
                Volume,
                Average_Number_SKUs,
                Average_Distribution,
                week_Number,
                price)


# baseline regressions

#### product 2
null_2 <- lm(Volume ~ 1, data = p2reg)
full_2 <- lm(Volume ~ ., data = p2reg)

step(null_2, scope = list (upper = full_2), data = p2reg, direction = "both")

# best
best2 <- lm(Volume ~ price + Average_Distribution + Average_Number_SKUs + week_Number, data = p2reg)
summary(best2)

#### product 3
null_3 <- lm(Volume ~ 1, data = p3reg)
full_3 <- lm(Volume ~ ., data = p3reg)

step(null_3, scope = list (upper = full_3), data = p3reg, direction = "both")

# best
best3 <- lm(Volume ~ Average_Distribution + week_Number + Average_Number_SKUs + price, data = p3reg)
summary(best3)

#### product 4
null_4 <- lm(Volume ~ 1, data = p4reg)
full_4 <- lm(Volume ~ ., data = p4reg)

step(null_4, scope = list (upper = full_4), data = p4reg, direction = "both")

# best
best4 <- lm(Volume ~ Average_Distribution + price + Average_Number_SKUs +  week_Number, data = p4reg)
summary(best4)

#### product 5
null_5 <- lm(Volume ~ 1, data = p5reg)
full_5 <- lm(Volume ~ ., data = p5reg)

step(null_5, scope = list (upper = full_5), data = p5reg, direction = "both")

# best
best5 <- lm(Volume ~ Average_Distribution + price + week_Number, data = p5reg)
summary(best5)


# calculate new baseline prices

# product 2
p2_int <- best2$coefficients[1]
p2_price_coeff <- best2$coefficients[2]
p2_avd_coeff <- best2$coefficients[3]
p2_avsku_coeff <- best2$coefficients[4]
p2_week_coeff <- best2$coefficients[5]

product2$baseline <- p2_int + 
  product2$price * p2_price_coeff +
  product2$Average_Distribution * p2_avd_coeff + 
  product2$Average_Number_SKUs * p2_avsku_coeff +
  product2$week_Number * p2_week_coeff

# product 3
p3_int <- best3$coefficients[1]
p3_price_coeff <- best3$coefficients[5]
p3_avd_coeff <- best3$coefficients[2]
p3_avsku_coeff <- best3$coefficients[4]
p3_week_coeff <- best3$coefficients[3]

product3$baseline <- p3_int + 
  product3$price * p3_price_coeff +
  product3$Average_Distribution * p3_avd_coeff + 
  product3$Average_Number_SKUs * p3_avsku_coeff +
  product3$week_Number * p3_week_coeff


# product 4
p4_int <- best4$coefficients[1]
p4_price_coeff <- best4$coefficients[3]
p4_avd_coeff <- best4$coefficients[2]
p4_avsku_coeff <- best4$coefficients[4]
p4_week_coeff <- best4$coefficients[5]

product4$baseline <- p4_int + 
  product4$price * p4_price_coeff +
  product4$Average_Distribution * p4_avd_coeff + 
  product4$Average_Number_SKUs * p4_avsku_coeff +
  product4$week_Number * p4_week_coeff

# product 5

p5_int <- best5$coefficients[1]
p5_price_coeff <- best5$coefficients[3]
p5_avd_coeff <- best5$coefficients[2]
p5_week_coeff <- best5$coefficients[4]

product5$baseline <- p5_int + 
  product5$price * p5_price_coeff +
  product5$Average_Distribution * p5_avd_coeff + 
  product5$week_Number * p5_week_coeff

# calculate weekly and total lift in Excel
# setwd("/Users/Steven/Desktop/Data")
# 
# write.csv(product2, file = "product2_newb.csv")
# write.csv(product3, file = "product3_newb.csv")
# write.csv(product4, file = "product4_newb.csv")
# write.csv(product5, file = "product5_newb.csv")

# reload data

setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail group/Data 4")

prod2 <- read.csv("product2_newb3.csv")
prod3 <- read.csv("product3_newb3.csv")
prod4 <- read.csv("product4_newb3.csv")
prod5 <- read.csv("product5_newb3.csv")

# use code from "visual_promotion_detection" to plot lift

# fill baseline column with regular volumes
prod2$baseline <- ifelse(prod2$promo == 0, prod2$Volume, prod2$baseline)
prod3$baseline <- ifelse(prod3$promo == 0, prod3$Volume, prod3$baseline)
prod4$baseline <- ifelse(prod4$promo == 0, prod4$Volume, prod4$baseline)
prod5$baseline <- ifelse(prod5$promo == 0, prod5$Volume, prod5$baseline)

# add column for sum of lift and volume for the plot
prod2$lift_plot <- ifelse(is.na(prod2$weekly_lift), prod2$Volume + 0,  prod2$Volume + prod2$weekly_lift)
prod3$lift_plot <- ifelse(is.na(prod3$weekly_lift), prod3$Volume + 0,  prod3$Volume + prod3$weekly_lift)
prod4$lift_plot <- ifelse(is.na(prod4$weekly_lift), prod4$Volume + 0,  prod4$Volume + prod4$weekly_lift)
prod5$lift_plot <- ifelse(is.na(prod5$weekly_lift), prod5$Volume + 0,  prod5$Volume + prod5$weekly_lift)

# plot baseline and lift

# product 2
plot(prod2$week_Number, prod2$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11)
lines(prod2$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")

# product 3
plot(prod3$week_Number, prod3$lift_plot, type="l", lwd=2, ylim = c(0, max(prod3$lift_plot)), col = 11)
lines(prod3$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")


# product 4
plot(prod4$week_Number, prod4$lift_plot, type="l", lwd=2, ylim = c(0, max(prod4$lift_plot)), col = 11)
lines(prod4$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")


# product 5
plot(prod5$week_Number, prod5$lift_plot, type="l", lwd=2, ylim = c(0, max(prod5$lift_plot)), col = 11)
lines(prod5$baseline, lwd=2)
legend("topright",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")








