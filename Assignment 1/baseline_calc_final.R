setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Data/Crackers")
library(readxl)
library(MASS)
library(dplyr)
library(tidyr)

# import data
product2 <- read_excel('Crackers.xlsx', sheet = 2, col_names = TRUE)
product3 <- read_excel('Crackers.xlsx', sheet = 3, col_names = TRUE)
product4 <- read_excel('Crackers.xlsx', sheet = 4, col_names = TRUE)
product6 <- read_excel('Crackers.xlsx', sheet = 6, col_names = TRUE)

# separate "Week" column
product2 <- product2 %>% 
  separate(Week, c("S", "week"), " ")
product3 <- product3 %>% 
  separate(Week, c("S", "week"), " ")
product4 <- product4 %>% 
  separate(Week, c("S", "week"), " ")
product6 <- product6 %>% 
  separate(Week, c("S", "week"), " ")


# convert week into data format
product2$week <- as.Date(product2$week, "%d/%m/%y")
product3$week <- as.Date(product3$week, "%d/%m/%y")
product4$week <- as.Date(product4$week, "%d/%m/%y")
product6$week <- as.Date(product6$week, "%d/%m/%y")

# add week number
product2$week_Number <- 1:nrow(product2)
product3$week_Number <- 1:nrow(product3)
product4$week_Number <- 1:nrow(product4)
product6$week_Number <- 1:nrow(product6)

# calculate prices and log prices
product2$price <- product2$Value_Euros / product2$Volume
product3$price <- product3$Value_Euros / product3$Volume
product4$price <- product4$Value_Euros / product4$Volume
product6$price <- product6$Value_Euros / product6$Volume

product2$log.p <- log(product2$price)
product2$log.q <- log(product2$Volume)

product3$log.p <- log(product3$price)
product3$log.q <- log(product3$Volume)

product4$log.p <- log(product4$price)
product4$log.q <- log(product4$Volume)

product6$log.p <- log(product6$price)
product6$log.q <- log(product6$Volume)


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

product6$average_Price <- mean(product6$price)
product6$threshold1 <- mean(product6$price) - sd(product6$price)
product6$promo <- ifelse(product6$price < product6$threshold1, 1, 0)

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

# product 6
par(mar=c(5,4,4,5)+.1)
plot(product6$week_Number, product6$price, type="l", col="blue", ylim=c(0,6))
lines(product6$threshold1, lty=2, lwd=2)
lines(product6$promo)
par(new=TRUE)
plot(product6$week_Number, product6$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
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
product6$ad <- ifelse(product6$GRP != 0, 1, 0)
product6[1:4, "promo"] <- 1
product6[11:18, "promo"] <- 1
product6[24:26, "promo"] <- 1
product6[30:31, "promo"] <- 1
product6[39, "promo"] <- 1
product6[46:52, "promo"] <- 1
product6[64:66, "promo"] <- 1
product6[76:79, "promo"] <- 1
product6[99, "promo"] <- 1
product6[104:105, "promo"] <- 1

# view new graphs with added promotions periods

# product 2
par(mar=c(5,4,4,5)+.1)
plot(product2$week_Number, product2$price, type="l", col="blue", ylim=c(0,max(product2$price)), lwd = 1.5,
     main = "Product 2 Volume and Price", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(1,11,18,24,29,34,46,49,58,63,69,77,103), 
     ybottom = -1, xright = c(6,15,21, 26,30,35,47,52,59,65,70,85,105), 
     ytop = 5, col = "lightgrey", border = NA, density = 100)
lines(product2$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product2$week_Number, product2$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5, cex.axis=0.6)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume", side=4,line=3,cex.lab=0.6, cex = 0.6)
legend("bottomright",col=c("blue","red", "black"), 
       lty=c(1,1,2), legend=c("Price","Volume", "P. Promo"), cex=0.6, bty = "n")

# product 3
par(mar=c(5,4,4,5)+.1)
plot(product3$week_Number, product3$price, type="l", col="blue", ylim=c(0,max(product3$price)), lwd = 1.5,
     main = "Product 3 Volume and Price", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(0.5, 11,18,25,48,70,80,96), 
     ybottom = -1, xright = c(1.5, 15,22,32,55,71,89,105), 
     ytop = 7, col = "lightgrey", border = NA, density = 100)
lines(product3$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product3$week_Number, product3$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)
legend("bottomright",col=c("blue","red", "black"), 
       lty=c(1,1,2), legend=c("Price","Volume", "P. Promo"), cex=0.6, bty = "n")

# product 4
par(mar=c(5,4,4,5)+.1)
plot(product4$week_Number, product4$price, type="l", col="blue", ylim=c(0,max(product4$price)), lwd = 1.5,
     main = "Product 4 Volume and Price", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(4.5,11,19,24,29,42,63.5,78,88.5,97), 
     ybottom = -1, xright = c(5.5,15,21,26,30,54,64.5,81,89.5,103), 
     ytop = 8, col = "lightgrey", border = NA, density = 100)
lines(product4$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product4$week_Number, product4$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)
legend("bottomright",col=c("blue","red", "black"), 
       lty=c(1,1,2), legend=c("Price","Volume", "P. Promo"), cex=0.6, bty = "n")

# product 6
par(mar=c(5,4,4,5)+.1)
plot(product6$week_Number, product6$price, type="l", col="blue", ylim=c(0,max(product6$price)), lwd = 1.5,
     main = "Product 6 Volume and Price", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(1,11,24,30,38.5,46,64,76,90.5,98.5,104), 
     ybottom = -1, xright = c(4,18,26,31,39.5,52,66,79,91.5,99.5,105), 
     ytop = 7, col = "lightgrey", border = NA, density = 100)
lines(product6$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product6$week_Number, product6$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)
legend("bottomright",col=c("blue","red", "black"), 
       lty=c(1,1,2), legend=c("Price","Volume", "P. Promo"), cex=0.6, bty = "n")


# proportion of promotions during two year period, after adding manually
sum(product2$promo)/ nrow(product2)
sum(product3$promo)/ nrow(product3)
sum(product4$promo)/ nrow(product4)
sum(product6$promo)/ nrow(product6)


## calculate baseline using a regression

# first eliminate all the promo weeks
p2noPromo <- filter(product2, promo == 0)
p3noPromo <- filter(product3, promo == 0)
p4noPromo <- filter(product4, promo == 0)
p6noPromo <- filter(product6, promo == 0)

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

p6reg <- select(p6noPromo, 
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
null_6 <- lm(Volume ~ 1, data = p6reg)
full_6 <- lm(Volume ~ ., data = p6reg)

step(null_6, scope = list (upper = full_6), data = p6reg, direction = "both")

# best
best6 <- lm(formula = Volume ~ Average_Number_SKUs + price + Average_Distribution + 
              week_Number, data = p6reg)
summary(best6)


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

# product 6

p6_int <- best6$coefficients[1]
p6_price_coeff <- best6$coefficients[3]
p6_avd_coeff <- best6$coefficients[4]
p6_avsku_coeff <- best6$coefficients[2]
p6_week_coeff <- best6$coefficients[5]

product6$baseline <- p6_int + 
  product6$price * p6_price_coeff +
  product6$Average_Distribution * p6_avd_coeff + 
  product6$week_Number * p6_week_coeff +
  product6$Average_Number_SKUs * p6_avsku_coeff


# calculate weekly and total lift in Excel
# setwd("/Users/Steven/Desktop/Data")
# 
# write.csv(product2, file = "product2_newb.csv")
# write.csv(product3, file = "product3_newb.csv")
# write.csv(product4, file = "product4_newb.csv")
# write.csv(product6, file = "product6_newb.csv")

# reload data

setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail group/Data 5")

prod2 <- read.csv("product2_newb4.csv")
prod3 <- read.csv("product3_newb4.csv")
prod4 <- read.csv("product4_newb4.csv")
prod6 <- read.csv("product6_newb4.csv")

# use code from "visual_promotion_detection" to plot lift

# fill baseline column with regular volumes
prod2$baseline <- ifelse(prod2$promo == 0, prod2$Volume, prod2$baseline)
prod3$baseline <- ifelse(prod3$promo == 0, prod3$Volume, prod3$baseline)
prod4$baseline <- ifelse(prod4$promo == 0, prod4$Volume, prod4$baseline)
prod6$baseline <- ifelse(prod6$promo == 0, prod6$Volume, prod6$baseline)

# add column for sum of lift and volume for the plot
prod2$lift_plot <- ifelse(is.na(prod2$weekly_lift), prod2$Volume + 0,  prod2$Volume + prod2$weekly_lift)
prod3$lift_plot <- ifelse(is.na(prod3$weekly_lift), prod3$Volume + 0,  prod3$Volume + prod3$weekly_lift)
prod4$lift_plot <- ifelse(is.na(prod4$weekly_lift), prod4$Volume + 0,  prod4$Volume + prod4$weekly_lift)
prod6$lift_plot <- ifelse(is.na(prod6$weekly_lift), prod6$Volume + 0,  prod6$Volume + prod6$weekly_lift)

# plot baseline and lift

# product 2
plot(prod2$week_Number, prod2$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11, 
     main= "Product 2 Baseline Sales and Lift",
     xlab= "Week", ylab= "Volume")
lines(prod2$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")

# product 3
plot(prod3$week_Number, prod3$lift_plot, type="l", lwd=2, ylim = c(0, max(prod3$lift_plot)), col = 11, 
     main= "Product 3 Baseline Sales and Lift", 
     xlab= "Week", ylab= "Volume")
lines(prod3$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")


# product 4
plot(prod4$week_Number, prod4$lift_plot, type="l", lwd=2, ylim = c(0, max(prod4$lift_plot)), col = 11, 
     main= "Product 4 Baseline Sales and Lift", 
     xlab= "Week", ylab= "Volume")
lines(prod4$baseline, lwd=2)
legend("bottomleft",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")


# product 5
plot(prod6$week_Number, prod6$lift_plot, type="l", lwd=2, ylim = c(0, max(prod6$lift_plot)), col = 11, 
     main= "Product 6 Baseline Sales and Lift", 
     xlab= "Week", ylab= "Volume")
lines(prod6$baseline, lwd=2)
legend("topright",col=c("black", 11), lty=1, legend=c("Baseline","Lift"), cex=0.7, bty = "n")



# make 2x2 plot for price, volume, promotion

par(mfrow=c(2,2))

# product 2
par(mar=c(5,4,4,5)+.1)
plot(product2$week_Number, product2$price, type="l", col="blue", ylim=c(0,max(product2$price)), lwd = 1.5,
     main = "Product 2", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(1,11,18,24,29,34,46,49,58,63,69,77,103), 
     ybottom = -1, xright = c(6,15,21, 26,30,35,47,52,59,65,70,85,105), 
     ytop = 8, col = "lightgrey", border = NA, density = 100)
lines(product2$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product2$week_Number, product2$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5, cex.axis=0.6)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume", side=4,line=3,cex.lab=0.6, cex = 0.6)


# product 3
par(mar=c(5,4,4,5)+.1)
plot(product3$week_Number, product3$price, type="l", col="blue", ylim=c(0,max(product3$price)), lwd = 1.5,
     main = "Product 3", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(0.5, 11,18,25,48,70,80,96), 
     ybottom = -1, xright = c(1.5, 15,22,32,55,71,89,105), 
     ytop = 8, col = "lightgrey", border = NA, density = 100)
lines(product3$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product3$week_Number, product3$Volume,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)


# product 4
par(mar=c(5,4,4,5)+.1)
plot(product4$week_Number, product4$price, type="l", col="blue", ylim=c(0,max(product4$price)), lwd = 1.5,
     main = "Product 4", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(4.5,11,19,24,29,42,63.5,78,88.5,97), 
     ybottom = -1, xright = c(5.5,15,21,26,30,54,64.5,81,89.5,103), 
     ytop = 8, col = "lightgrey", border = NA, density = 100)
lines(product4$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product4$week_Number, product4$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)


# product 6
par(mar=c(5,4,4,5)+.1)
plot(product6$week_Number, product6$price, type="l", col="blue", ylim=c(0,max(product6$price)), lwd = 1.5,
     main = "Product 6", xlab = "Week", ylab = "Price", cex.axis=0.6, cex.lab=0.6)
rect(xleft = c(1,11,24,30,38.5,46,64,76,90.5,98.5,104), 
     ybottom = -1, xright = c(4,18,26,31,39.5,52,66,79,91.5,99.5,105), 
     ytop = 8, col = "lightgrey", border = NA, density = 100)
lines(product6$threshold1, lty=2, lwd=2)
par(new=TRUE)
plot(product6$week_Number, product6$Volume, type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="", lwd = 1.5)
axis(4, cex.axis=0.6, cex.lab=0.6)
mtext("Volume",side=4,line=3, cex = 0.6)


# 2x2 plot of baseline and lift

par(mfrow=c(2,2))

# product 2
plot(prod2$week_Number, prod2$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11, 
     main= "Product 2",
     xlab= "Week", ylab= "Volume")
lines(prod2$baseline, lwd=2)


# product 3
plot(prod3$week_Number, prod3$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11, 
     main= "Product 3", 
     xlab= "Week", ylab= "Volume")
lines(prod3$baseline, lwd=2)


# product 4
plot(prod4$week_Number, prod4$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11, 
     main= "Product 4", 
     xlab= "Week", ylab= "Volume")
lines(prod4$baseline, lwd=2)


# product 5
plot(prod6$week_Number, prod6$lift_plot, type="l", lwd=2, ylim = c(0, max(prod2$lift_plot)), col = 11, 
     main= "Product 6", 
     xlab= "Week", ylab= "Volume")
lines(prod6$baseline, lwd=2)



