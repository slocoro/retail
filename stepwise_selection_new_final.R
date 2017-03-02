library(knitr)
# new data for prodcute three with updated promotional periods
# added "week" variable to regression to account for trend
# maybe we need to adjust what we see as significant
# http://marketingland.com/statistical-significance-business-114834
# this one uses volume (not baseline)
# uses product 6 instead of product 5

setwd("/Users/Steven/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail group/Data 5")

prod2 <- read.csv('product2_newb4.csv')
prod3 <- read.csv('product3_newb4.csv')
prod4 <- read.csv('product4_newb4.csv')
prod6 <- read.csv('product6_newb4.csv')

#Add product identifier for column names
colnames(prod2) <- c("X_2","S_2","week_2","Volume_2","Value_Euros_2","Average_Number_SKUs_2","Average_Distribution_2","Volume_Baseline_2","Value_Baseline_2","week_Number_2","price_2","log.p_2", "log.q_2", "average_Price_2","threshold1_2","promo_2", "baseline_2")
colnames(prod3) <- c("X_3","S_3","week_3","Volume_3","Value_Euros_3","Average_Number_SKUs_3","Average_Distribution_3","Volume_Baseline_3","Value_Baseline_3", "GRP_3","week_Number_3","price_3","log.p_3","log.q_3" , "average_Price_3","threshold1_3","promo_3", "ad_3", "baseline_3")
colnames(prod4) <- c("X_4","S_4","week_4","Volume_4","Value_Euros_4","Average_Number_SKUs_4","Average_Distribution_4","Volume_Baseline_4","Value_Baseline_4", "GRP_4","week_Number_4","price_4","log.p_4", "log.q_4", "average_Price_4","threshold1_4","promo_4", "ad_4", "baseline_4")
colnames(prod6) <- c("X_6","S_6","week_6","Volume_6","Value_Euros_6","Average_Number_SKUs_6","Average_Distribution_6","Volume_Baseline_6","Value_Baseline_6","GRP_6", "week_Number_6","price_6","log.p_6", "log.q_6", "average_Price_6","threshold1_6","promo_6", "ad_6", "baseline_6")


#Create single dataframe with all products
products <- as.data.frame(cbind(prod2, prod3, prod4, prod6))

# add log of baseline
products$log_baseline2 <- ifelse(products$baseline_2 > 0, log(products$baseline_2), 0)
products$log_baseline3 <- ifelse(products$baseline_3 > 0, log(products$baseline_3), 0)
products$log_baseline4 <- ifelse(products$baseline_4 > 0, log(products$baseline_4), 0)
products$log_baseline6 <- ifelse(products$baseline_6 > 0, log(products$baseline_6), 0)

#MODEL SELECTION - STEPWISE 

#create function for RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}


#product 2 - linear
null_2_linear <- lm(Volume_2 ~ 1, data = products)
full_2_linear <- lm(Volume_2 ~ price_2 + price_3 + price_4 + price_6 +
                      promo_2 + promo_3 + promo_4 + promo_6 +
                      ad_3 + ad_4 + ad_6 +
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                      week_Number_2, data = products)

step(null_2_linear, scope = list (upper = full_2_linear), data = products, direction = "both")

p2_linear <- lm(formula = Volume_2 ~ promo_2 + price_2 + Average_Distribution_6 + 
                  Average_Distribution_3 + price_6 + Average_Number_SKUs_2 + 
                  promo_6 + Average_Number_SKUs_6 + price_4, data = products)

#product 2 - semilog
null_2_semilog <- lm(log.q_2 ~ 1, data = products)
full_2_semilog <- lm(log.q_2 ~ price_2 + price_3 + price_4 + price_6 +
                       promo_2 + promo_3 + promo_4 + promo_6 +
                       ad_3 + ad_4 + ad_6 +
                       Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                       Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                       week_Number_2, data = products)

step(null_2_semilog, scope = list (upper = full_2_semilog), data = products, direction = "both")

p2_semilog <- lm(formula = log.q_2 ~ promo_2 + Average_Distribution_3 + Average_Distribution_6 + 
                   price_2 + price_6 + Average_Number_SKUs_2 + Average_Number_SKUs_6 + 
                   promo_6 + price_4 + ad_3, data = products)

#product 2 - log log
null_2_log <- lm(log.q_2 ~ 1, data = products)
full_2_log <- lm(log.q_2 ~ log.p_2 + log.p_3 + log.p_4 + log.p_6 +
                   promo_2 + promo_3 + promo_4 + promo_6 +
                   ad_3 + ad_4 + ad_6 +
                   Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                   Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                   week_Number_2, data = products)

step(null_2_log, scope = list (upper = full_2_log), data = products, direction = "both")

p2_log <- lm(formula = log.q_2 ~ promo_2 + Average_Distribution_3 + Average_Distribution_6 + 
               log.p_2 + Average_Number_SKUs_2 + log.p_6 + Average_Number_SKUs_6 + 
               promo_6 + log.p_4 + ad_3, data = products)

#product 2 - errors
rmse(fitted(p2_linear) - products$Volume_2)
rmse(exp(fitted(p2_semilog)) - products$Volume_2) 
rmse(exp(fitted(p2_log)) - products$Volume_2) 

#Linear model has lowest error so we will use that one

#product 3 - linear
null_3_linear <- lm(Volume_3 ~ 1, data = products)
full_3_linear <- lm(Volume_3 ~ price_2 + price_3 + price_4 + price_6 +
                      promo_2 + promo_3 + promo_4 + promo_6 +
                      ad_3 + ad_4 + ad_6 +
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                      week_Number_2, data = products)

step(null_3_linear, scope = list (upper = full_3_linear), data = products, direction = "both")

p3_linear <- lm(formula = Volume_3 ~ Average_Distribution_3 + Average_Distribution_6 + 
                  price_3 + Average_Number_SKUs_6 + promo_3 + Average_Number_SKUs_2 + 
                  price_6 + Average_Number_SKUs_3 + week_Number_2 + ad_4 + 
                  promo_2, data = products)

#product 3 - semilog
null_3_semilog <- lm(log.q_3 ~ 1, data = products)
full_3_semilog <- lm(log.q_3 ~ price_2 + price_3 + price_4 + price_6 +
                       promo_2 + promo_3 + promo_4 + promo_6 +
                       ad_3 + ad_4 + ad_6 +
                       Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                       Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                       week_Number_2, data = products)

step(null_3_semilog, scope = list (upper = full_3_semilog), data = products, direction = "both")

p3_semilog <- lm(formula = log.q_3 ~ Average_Distribution_3 + Average_Distribution_6 + 
                   price_3 + Average_Number_SKUs_3 + week_Number_2 + promo_2 + 
                   price_6 + Average_Number_SKUs_6 + promo_3 + Average_Number_SKUs_2 + 
                   Average_Distribution_4, data = products)


#product 3 - log log
null_3_log <- lm(log.q_3 ~ 1, data = products)
full_3_log <- lm(log.q_3 ~ log.p_2 + log.p_3 + log.p_4 + log.p_6 +
                   promo_2 + promo_3 + promo_4 + promo_6 +
                   ad_3 + ad_4 + ad_6 +
                   Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                   Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                   week_Number_2, data = products)

step(null_3_log, scope = list (upper = full_3_log), data = products, direction = "both")

p3_log <- lm(formula = log.q_3 ~ Average_Distribution_3 + Average_Distribution_6 + 
               log.p_3 + Average_Number_SKUs_3 + week_Number_2 + promo_2 + 
               log.p_6 + Average_Number_SKUs_6 + promo_3 + Average_Number_SKUs_2 + 
               Average_Distribution_4, data = products)

#product 3 - errors
rmse(fitted(p3_linear) - products$Volume_3)
rmse(exp(fitted(p3_semilog)) - products$Volume_3)
rmse(exp(fitted(p3_log)) - products$Volume_3)

#linear model has lowest error so we will use it 

#product 4 - linear
null_4_linear <- lm(Volume_4 ~ 1, data = products)
full_4_linear <- lm(Volume_4 ~ price_2 + price_3 + price_4 + price_6 +
                      promo_2 + promo_3 + promo_4 + promo_6 +
                      ad_3 + ad_4 + ad_6 +
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                      week_Number_2, data = products)

step(null_4_linear, scope = list (upper = full_4_linear), data = products, direction = "both")

p4_linear <- lm(formula = Volume_4 ~ Average_Distribution_4 + price_4 + promo_4 + 
                   Average_Number_SKUs_4 + Average_Distribution_6 + ad_3 + Average_Number_SKUs_6 + 
                   ad_4 + week_Number_2 + Average_Number_SKUs_2 + Average_Distribution_2 + 
                   ad_6, data = products)

#product 4 - semilog
null_4_semilog <- lm(log.q_4 ~ 1, data = products)
full_4_semilog <- lm(log.q_4 ~ price_2 + price_3 + price_4 + price_6 +
                       promo_2 + promo_3 + promo_4 + promo_6 +
                       ad_3 + ad_4 + ad_6 +
                       Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                       Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                       week_Number_2, data = products)

step(null_4_semilog, scope = list (upper = full_4_semilog), data = products, direction = "both")

p4_semilog <- lm(formula = log.q_4 ~ Average_Distribution_4 + price_4 + Average_Number_SKUs_2 + 
                   Average_Distribution_3 + Average_Number_SKUs_4 + Average_Distribution_6 + 
                   Average_Number_SKUs_6 + ad_3 + Average_Distribution_2 + ad_6 + 
                   promo_3, data = products)


#product 4 - log log
null_4_log <- lm(log.q_4 ~ 1, data = products)
full_4_log <- lm(log.q_4 ~ log.p_2 + log.p_3 + log.p_4 + log.p_6 +
                   promo_2 + promo_3 + promo_4 + promo_6 +
                   ad_3 + ad_4 + ad_6 +
                   Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                   Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                   week_Number_2, data = products)

step(null_4_log, scope = list (upper = full_4_log), data = products, direction = "both")

p4_log <- lm(formula = log.q_4 ~ Average_Distribution_4 + log.p_4 + Average_Number_SKUs_3 + 
               Average_Distribution_3 + Average_Number_SKUs_2 + Average_Number_SKUs_4 + 
               Average_Distribution_6 + Average_Number_SKUs_6 + ad_3 + Average_Distribution_2 + 
               ad_6, data = products)

#product 4 - errors
rmse(fitted(p4_linear) - products$Volume_4)
rmse(exp(fitted(p4_semilog)) - products$Volume_4)
rmse(exp(fitted(p4_log)) - products$Volume_4)

#linear model has lowest error so we will use it

#product 6 - linear
null_6_linear <- lm(Volume_6 ~ 1, data = products)
full_6_linear <- lm(Volume_6 ~ price_2 + price_3 + price_4 + price_6 +
                      promo_2 + promo_3 + promo_4 + promo_6 +
                      ad_3 + ad_4 + ad_6 +
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                      week_Number_2, data = products)

step(null_6_linear, scope = list (upper = full_6_linear), data = products, direction = "both")

p6_linear <- lm(formula = Volume_6 ~ promo_6 + Average_Number_SKUs_6 + price_6 + 
                  ad_3 + Average_Distribution_6 + Average_Distribution_4 + 
                  ad_6 + ad_4 + Average_Number_SKUs_4, data = products)

#product 6 - semilog
null_6_semilog <- lm(log.q_6 ~ 1, data = products)
full_6_semilog <- lm(log.q_6 ~ price_2 + price_3 + price_4 + price_6 +
                       promo_2 + promo_3 + promo_4 + promo_6 +
                       ad_3 + ad_4 + ad_6 +
                       Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                       Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                       week_Number_2, data = products)

step(null_6_semilog, scope = list (upper = full_6_semilog), data = products, direction = "both")

p6_semilog <- lm(formula = log.q_6 ~ promo_6 + Average_Number_SKUs_6 + price_6 + 
                   Average_Distribution_4 + Average_Distribution_6 + ad_3 + 
                   ad_6, data = products)

#product 6 - log log
null_6_log <- lm(log.q_6 ~ 1, data = products)
full_6_log <- lm(log.q_6 ~ log.p_2 + log.p_3 + log.p_4 + log.p_6 +
                   promo_2 + promo_3 + promo_4 + promo_6 +
                   ad_3 + ad_4 + ad_6 +
                   Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                   Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 +Average_Distribution_6 +
                   week_Number_2, data = products)

step(null_6_log, scope = list (upper = full_6_log), data = products, direction = "both")

p6_log <- lm(formula = log.q_6 ~ promo_6 + Average_Number_SKUs_6 + log.p_6 + 
               Average_Distribution_4 + Average_Distribution_6 + ad_3 + 
               ad_6, data = products)

rmse(fitted(p6_linear) - products$Volume_6)
rmse(exp(fitted(p6_semilog)) - products$Volume_6)
rmse(exp(fitted(p6_log)) - products$Volume_6)

# elasticity / cross elasticity matrix

elasticity.22 <- summary(p2_linear)$coefficients[3] * mean(products$price_2) / mean(products$Volume_2)
elasticity.23 <- 0
elasticity.24 <- summary(p2_linear)$coefficients[10] * mean(products$price_4) / mean(products$Volume_2)
elasticity.26 <- summary(p2_linear)$coefficients[6] * mean(products$price_6) / mean(products$Volume_2)

elasticity.32 <- 0
elasticity.33 <- summary(p3_semilog)$coefficients[4] * mean(products$price_3)
elasticity.34 <- 0
elasticity.36 <- summary(p3_semilog)$coefficients[8] * mean(products$price_6)

elasticity.42 <- 0
elasticity.43 <- 0
elasticity.44 <- summary(p4_log)$coefficients[3]
elasticity.46 <- 0

elasticity.62 <- 0
elasticity.63 <- 0
elasticity.64 <- 0
elasticity.66 <- summary(p6_linear)$coefficients[4] * mean(products$price_6) / mean(products$Volume_6)

elasticity.matrix <- matrix(c(elasticity.22, elasticity.23, elasticity.24, elasticity.26,
                              elasticity.32, elasticity.33, elasticity.34, elasticity.36,
                              elasticity.42, elasticity.43, elasticity.44, elasticity.46,
                              elasticity.62, elasticity.63, elasticity.64, elasticity.66),
                            nrow = 4, ncol = 4, byrow = TRUE)

colnames(elasticity.matrix) <- c('2','3','4','6')
rownames(elasticity.matrix) <- c('2','3','4','6')


kable(elasticity.matrix)

# clout vulnerability table and plot

clout.2 <- elasticity.32 + elasticity.42 + elasticity.62
clout.3 <- elasticity.23 + elasticity.43 + elasticity.63
clout.4 <- elasticity.24 + elasticity.34 + elasticity.64
clout.6 <- elasticity.26 + elasticity.36 + elasticity.46

vulnerability.2 <- elasticity.23 + elasticity.24 + elasticity.26
vulnerability.3 <- elasticity.32 + elasticity.34 + elasticity.36
vulnerability.4 <- elasticity.42 + elasticity.43 + elasticity.46
vulnerability.6 <- elasticity.26 + elasticity.62 + elasticity.64

par(mfrow=c(1,2))

product = c(2,3,4,6)
vulnerability = c(vulnerability.2, vulnerability.3, vulnerability.4, vulnerability.6)
clout = c(clout.2, clout.3, clout.4, clout.6)
avg_volume = c(mean(products$Volume_2), mean(products$Volume_3), mean(products$Volume_4), mean(products$Volume_6))
df <- data.frame(product, vulnerability, clout, avg_volume)

radius <- sqrt(df$avg_volume / pi)
symbols(df$vulnerability, df$clout, circles = radius, inches = 0.35, fg="white", bg="red", xlim=c(-0.5,1.5), ylim=c(-1,2), xlab="Vulnerability", ylab="Clout", main = "Crackers Market")
text(df$vulnerability, df$clout, df$product, cex=1.5)

# running regression using all predictors to see how it compares

p2_linear_all <- lm(Volume_2 ~ price_2 + price_3 + price_4 + price_6 +  
                      promo_2 + promo_3 + promo_4 + promo_6 + 
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 + Average_Distribution_6 + 
                      ad_3 + ad_4 + ad_6, data = products)
p3_linear_all <- lm(Volume_3 ~ price_2 + price_3 + price_4 + price_6 +  
                      promo_2 + promo_3 + promo_4 + promo_6 + 
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 + Average_Distribution_6 + 
                      ad_3 + ad_4 + ad_6, data = products)
p4_linear_all <- lm(Volume_4 ~ price_2 + price_3 + price_4 + price_6 +  
                      promo_2 + promo_3 + promo_4 + promo_6 + 
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 + Average_Distribution_6 + 
                      ad_3 + ad_4 + ad_6, data = products)
p6_linear_all <- lm(Volume_6 ~ price_2 + price_3 + price_4 + price_6 +  
                      promo_2 + promo_3 + promo_4 + promo_6 + 
                      Average_Number_SKUs_2 + Average_Number_SKUs_3 + Average_Number_SKUs_4 + Average_Number_SKUs_6 +
                      Average_Distribution_2 + Average_Distribution_3 + Average_Distribution_4 + Average_Distribution_6 + 
                      ad_3 + ad_4 + ad_6, data = products)

# elasticity and cross elasticity calculations

elasticity.22.all <- summary(p2_linear_all)$coefficients[2] * mean(products$price_2) / mean(products$Volume_2)
elasticity.23.all <- summary(p2_linear_all)$coefficients[3] * mean(products$price_3) / mean(products$Volume_2)
elasticity.24.all <- summary(p2_linear_all)$coefficients[4] * mean(products$price_4) / mean(products$Volume_2)
elasticity.26.all <- summary(p2_linear_all)$coefficients[5] * mean(products$price_6) / mean(products$Volume_2)
elasticity.32.all <- summary(p3_linear_all)$coefficients[2] * mean(products$price_2) / mean(products$Volume_3)
elasticity.33.all <- summary(p3_linear_all)$coefficients[3] * mean(products$price_3) / mean(products$Volume_3)
elasticity.34.all <- summary(p3_linear_all)$coefficients[4] * mean(products$price_4) / mean(products$Volume_3)
elasticity.36.all <- summary(p3_linear_all)$coefficients[5] * mean(products$price_6) / mean(products$Volume_3)
elasticity.42.all <- summary(p4_linear_all)$coefficients[2] * mean(products$price_2) / mean(products$Volume_3)
elasticity.43.all <- summary(p4_linear_all)$coefficients[3] * mean(products$price_3) / mean(products$Volume_4)
elasticity.44.all <- summary(p4_linear_all)$coefficients[4] * mean(products$price_4) / mean(products$Volume_4)
elasticity.46.all <- summary(p4_linear_all)$coefficients[5] * mean(products$price_6) / mean(products$Volume_4)
elasticity.62.all <- summary(p6_linear_all)$coefficients[2] * mean(products$price_2) / mean(products$Volume_6)
elasticity.63.all <- summary(p6_linear_all)$coefficients[3] * mean(products$price_3) / mean(products$Volume_6)
elasticity.64.all <- summary(p6_linear_all)$coefficients[4] * mean(products$price_4) / mean(products$Volume_6)
elasticity.66.all <- summary(p6_linear_all)$coefficients[5] * mean(products$price_6) / mean(products$Volume_6)



elasticity.matrix.all <- matrix(c(elasticity.22.all, elasticity.23.all, elasticity.24.all, elasticity.26.all,
                                  elasticity.32.all, elasticity.33.all, elasticity.34.all, elasticity.36.all,
                                  elasticity.42.all, elasticity.43.all, elasticity.44.all, elasticity.46.all,
                                  elasticity.62.all, elasticity.63.all, elasticity.64.all, elasticity.66.all),
                                nrow = 4, ncol = 4, byrow = TRUE)


colnames(elasticity.matrix.all) <- c('2','3','4','6')
rownames(elasticity.matrix.all) <- c('2','3','4','6')
kable(elasticity.matrix.all)

# clout vulnerability table and plot

clout.2.all <- elasticity.32.all + elasticity.42.all + elasticity.62.all
clout.3.all <- elasticity.23.all + elasticity.43.all + elasticity.63.all
clout.4.all <- elasticity.24.all + elasticity.34.all + elasticity.64.all
clout.6.all <- elasticity.26.all + elasticity.36.all + elasticity.46.all

vulnerability.2.all <- elasticity.23.all + elasticity.24.all + elasticity.26.all
vulnerability.3.all <- elasticity.32.all + elasticity.34.all + elasticity.36.all
vulnerability.4.all <- elasticity.42.all + elasticity.43.all + elasticity.46.all
vulnerability.6.all <- elasticity.26.all + elasticity.62.all + elasticity.64.all

product = c(2,3,4,6)

vulnerability.all = c(vulnerability.2.all, vulnerability.3.all, vulnerability.4.all, vulnerability.6.all)
clout.all = c(clout.2.all, clout.3.all, clout.4.all, clout.6.all)

avg_volume = c(mean(products$Volume_2), mean(products$Volume_3), mean(products$Volume_4), mean(products$Volume_6))
df <- data.frame(product, vulnerability.all, clout.all, avg_volume)

radius <- sqrt(df$avg_volume / pi)
symbols(df$vulnerability.all, df$clout.all, circles = radius, inches = 0.35, fg="white", bg="red", xlim=c(-0.5,1.5), ylim=c(-1,2), xlab="Vulnerability", ylab="Clout", main = "Crackers Market with all Variables")
text(df$vulnerability.all, df$clout.all, df$product, cex=1.5)

kable(list(elasticity.matrix, elasticity.matrix.all))



