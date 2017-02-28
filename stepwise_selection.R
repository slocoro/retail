prod2 <- read.csv('product2.csv')
prod3 <- read.csv('product3.csv')
prod4 <- read.csv('product4.csv')
prod5 <- read.csv('product5.csv')

#Add product identifier for column names
colnames(prod2) <- c("X_2","S_2","week_2","Volume_2","Value_Euros_2","Average_Number_SKUs_2","Average_Distribution_2","Volume_Baseline_2","Value_Baseline_2","week_Number_2","price_2","log.p_2", "log.q_2", "average_Price_2","threshold1_2","promo_2")
colnames(prod3) <- c("X_3","S_3","week_3","Volume_3","Value_Euros_3","Average_Number_SKUs_3","Average_Distribution_3","Volume_Baseline_3","Value_Baseline_3", "GRP_3","week_Number_3","price_3","log.p_3","log.q_3" , "average_Price_3","threshold1_3","promo_3", "ad_3")
colnames(prod4) <- c("X_4","S_4","week_4","Volume_4","Value_Euros_4","Average_Number_SKUs_4","Average_Distribution_4","Volume_Baseline_4","Value_Baseline_4", "GRP_4","week_Number_4","price_4","log.p_4", "log.q_4", "average_Price_4","threshold1_4","promo_4", "ad_4")
colnames(prod5) <- c("X_5","S_5","week_5","Volume_5","Value_Euros_5","Average_Number_SKUs_5","Average_Distribution_5","Volume_Baseline_5","Value_Baseline_5","week_Number_5","price_5","log.p_5", "log.q_5", "average_Price_5","threshold1_5","promo_5")

#Create single dataframe with all products
products <- as.data.frame(cbind(prod2, prod3, prod4, prod5))

#MODEL SELECTION - STEPWISE 

#create function for RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}


#product 2 - linear
null_2_linear <- lm(Volume_2 ~ 1, data = products)
full_2_linear <- lm(Volume_2 ~ price_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + price_3 + price_4 + price_5 + promo_3 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_2_linear, scope = list (upper = full_2_linear), data = products, direction = "both")

p2_linear <-lm(formula = Volume_2 ~ price_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + price_4 + ad_3, data = products)


#product 2 - semilog
null_2_semilog <- lm(log.q_2 ~ 1, data = products)
full_2_semilog <- lm(log.q_2 ~ price_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + price_3 + price_4 + price_5 + promo_3 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_2_semilog, scope = list (upper = full_2_semilog), data = products, direction = "both")

p2_semilog <- lm(formula = log.q_2 ~ price_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + price_4 + ad_3, data = products)


#product 2 - log log
null_2_log <- lm(log.q_2 ~ 1, data = products)
full_2_log <- lm(log.q_2 ~ log.p_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + log.p_3 + log.p_4 + log.p_5 + promo_3 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_2_log, scope = list (upper = full_2_log), data = products, direction = "both")

p2_log <- lm(formula = log.q_2 ~ log.p_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + log.p_4 + ad_3, data = products)


#product 2 - errors
rmse(fitted(p2_linear) - products$Volume_2)
rmse(exp(fitted(p2_semilog)) - products$Volume_2) 
rmse(exp(fitted(p2_log)) - products$Volume_2) 

#Linear model has lowest error so we will use that one

#product 3 - linear
null_3_linear <- lm(Volume_3 ~ 1, data = products)
full_3_linear <- lm(Volume_3 ~ price_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + price_2 + price_4 + price_5 + promo_2 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_3_linear, scope = list (upper = full_3_linear), data = products, direction = "both")

p3_linear <- lm(formula = Volume_3 ~ price_3 + Average_Distribution_3 + promo_3 + promo_5 + promo_2 + Average_Number_SKUs_3, data = products)


#product 3 - semilog
null_3_semilog <- lm(log.q_3 ~ 1, data = products)
full_3_semilog <- lm(log.q_3 ~ price_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + price_2 + price_4 + price_5 + promo_2 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_3_semilog, scope = list (upper = full_3_semilog), data = products, direction = "both")

p3_semilog <- lm(formula = log.q_3 ~ price_3 + Average_Distribution_3 + promo_5 + promo_2 + promo_3 + Average_Number_SKUs_3 + promo_4, data = products)

#product 3 - log log
null_3_log <- lm(log.q_3 ~ 1, data = products)
full_3_log <- lm(log.q_3 ~ log.p_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + log.p_2 + log.p_4 + log.p_5 + promo_2 + promo_4 + promo_5 + ad_3 + ad_4, data = products)

step(null_3_log, scope = list (upper = full_3_log), data = products, direction = "both")

p3_log <- lm(formula = log.q_3 ~ log.p_2 + Average_Distribution_3 + promo_5 + promo_2 + promo_3 + Average_Number_SKUs_3 + promo_4, data = products)


#product 3 - errors
rmse(fitted(p3_linear) - products$Volume_3)
rmse(exp(fitted(p3_semilog)) - products$Volume_3)
rmse(exp(fitted(p3_log)) - products$Volume_3)

#linear model has lowest error so we will use it 


#product 4 - linear
null_4_linear <- lm(Volume_4 ~ 1, data = products)
full_4_linear <- lm(Volume_4 ~ price_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + price_3 + price_2 + price_5 + promo_3 + promo_2 + promo_5 + ad_3 + ad_4, data = products)

step(null_4_linear, scope = list (upper = full_4_linear), data = products, direction = "both")

p4_linear <- lm(formula = Volume_4 ~ price_4 + Average_Distribution_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + ad_4 + promo_5, data = products)


#product 4 - semilog
null_4_semilog <- lm(log.q_4 ~ 1, data = products)
full_4_semilog <- lm(log.q_4 ~ price_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + price_2 + price_3 + price_5 + promo_2 + promo_3 + promo_5 + ad_3 + ad_4, data = products)

step(null_4_semilog, scope = list (upper = full_4_semilog), data = products, direction = "both")

p4_semilog <- lm(formula = log.q_4 ~ Average_Distribution_4 + price_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + price_2 + ad_4, data = products)

#product 4 - log log
null_4_log <- lm(log.q_4 ~ 1, data = products)
full_4_log <- lm(log.q_4 ~ log.p_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + log.p_2 + log.p_3 + log.p_5 + promo_2 + promo_3 + promo_5 + ad_3 + ad_4, data = products)

step(null_4_log, scope = list (upper = full_4_log), data = products, direction = "both")

p4_log <- lm(formula = log.q_4 ~ log.p_4 + Average_Distribution_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + ad_4 + log.p_2, data = products)
  
#product 4 - errors
rmse(fitted(p4_linear) - products$Volume_4)
rmse(exp(fitted(p4_semilog)) - products$Volume_4)
rmse(exp(fitted(p4_log)) - products$Volume_4)

#linear model has lowest error so we will use it
  
  #product 5 - linear
null_5_linear <- lm(Volume_5 ~ 1, data = products)
full_5_linear <- lm(Volume_5 ~ price_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + price_3 + price_2 + price_4 + promo_3 + promo_2 + promo_4 + ad_3 + ad_4, data = products)

step(null_5_linear, scope = list (upper = full_5_linear), data = products, direction = "both")

p5_linear <- lm(formula = Volume_5 ~ price_5 + Average_Distribution_5 + promo_5 + promo_3 + price_4, data = products)

#product 5 - semilog
null_5_semilog <- lm(log.q_5 ~ 1, data = products)
full_5_semilog <- lm(log.q_5 ~ price_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + price_2 + price_3 + price_4 + promo_2 + promo_3 + promo_4 + ad_3 + ad_4, data = products)

step(null_5_semilog, scope = list (upper = full_5_semilog), data = products, direction = "both")

p5_semilog <- lm(formula = log.q_5 ~ price_5 + Average_Distribution_5 + ad_4 + price_5 + 
                   price_4 + promo_4 + promo_3 + price_2, data = products)

#product 5 - log log
null_5_log <- lm(log.q_5 ~ 1, data = products)
full_5_log <- lm(log.q_5 ~ log.p_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + log.p_2 + log.p_3 + log.p_4 + promo_2 + promo_3 + promo_4 + ad_3 + ad_4, data = products)

step(null_5_log, scope = list (upper = full_5_log), data = products, direction = "both")

p5_log <- lm(formula = log.q_5 ~ log.p_5 + Average_Distribution_5 + ad_4 + 
               log.p_4 + promo_4 + promo_3 + log.p_2, data = products)


rmse(fitted(p5_linear) - products$Volume_5)
rmse(exp(fitted(p5_semilog)) - products$Volume_5)
rmse(exp(fitted(p5_log)) - products$Volume_5)

# linear model has lowest error so we will use it

elasticity.2 <- summary(p2_linear)$coefficients[2] * mean(products$price_2) / mean(products$Volume_2)
elasticity.3 <- summary(p3_linear)$coefficients[2] * mean(products$price_3) / mean(products$Volume_3)
elasticity.4 <- summary(p4_linear)$coefficients[2] * mean(products$price_4) / mean(products$Volume_4)
elasticity.5 <- summary(p5_linear)$coefficients[2] * mean(products$price_5) / mean(products$Volume_5)

