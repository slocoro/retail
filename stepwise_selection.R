#Add product identifier for column names
colnames(prod2) <- c("X_2","S_2","week_2","Volume_2","Value_Euros_2","Average_Number_SKUs_2","Average_Distribution_2","Volume_Baseline_2","Value_Baseline_2","week_Number_2","price_2","log.p_2", "log.q_2", "average_Price_2","threshold1_2","promo_2")
colnames(prod3) <- c("X_3","S_3","week_3","Volume_3","Value_Euros_3","Average_Number_SKUs_3","Average_Distribution_3","Volume_Baseline_3","Value_Baseline_3", "GRP_3","week_Number_3","price_3","log.p_3","log.q_3" , "average_Price_3","threshold1_3","promo_3", "ad_3")
colnames(prod4) <- c("X_4","S_4","week_4","Volume_4","Value_Euros_4","Average_Number_SKUs_4","Average_Distribution_4","Volume_Baseline_4","Value_Baseline_4", "GRP_4","week_Number_4","price_4","log.p_4", "log.q_4", "average_Price_4","threshold1_4","promo_4", "ad_4")
colnames(prod5) <- c("X_5","S_5","week_5","Volume_5","Value_Euros_5","Average_Number_SKUs_5","Average_Distribution_5","Volume_Baseline_5","Value_Baseline_5","week_Number_5","price_5","log.p_5", "log.q_5", "average_Price_5","threshold1_5","promo_5")

#Create single dataframe with all products
prods <- cbind(prod2, prod3, prod4, prod5)


#MODEL SELECTION - STEPWISE 

#create function for RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}


#product 2 - linear
null_2_linear <- lm(Volume_2 ~ 1, data = prods)
full_2_linear <- lm(Volume_2 ~ price_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + price_3 + price_4 + price_5 + promo_3 + promo_4 + promo_5, data = prods)

step(null_2_linear, scope = list (upper = full_2_linear), data = product2, direction = "both")

p2_linear <-lm(formula = Volume_2 ~ price_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + price_4, data = prods)


#product 2 - semilog
null_2_semilog <- lm(log.q_2 ~ 1, data = prods)
full_2_semilog <- lm(log.q_2 ~ price_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + price_3 + price_4 + price_5 + promo_3 + promo_4 + promo_5, data = prods)

step(null_2_semilog, scope = list (upper = full_2_semilog), data = product2, direction = "both")

p2_semilog <- lm(formula = log.q_2 ~ price_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + price_4, data = prods)


#product 2 - log log
null_2_log <- lm(log.q_2 ~ 1, data = prods)
full_2_log <- lm(log.q_2 ~ log.p_2 + Average_Number_SKUs_2 +  Average_Distribution_2 + promo_2 + log.p_3 + log.p_4 + log.p_5 + promo_3 + promo_4 + promo_5, data = prods)

step(null_2_log, scope = list (upper = full_2_log), data = product2, direction = "both")

p2_log <- lm(formula = log.q_2 ~ log.p_2 + Average_Distribution_2 + Average_Number_SKUs_2 + promo_4 + log.p_4, data = prods)


#product 3 - linear
null_3_linear <- lm(Volume_3 ~ 1, data = prods)
full_3_linear <- lm(Volume_3 ~ price_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + price_2 + price_4 + price_5 + promo_2 + promo_4 + promo_5, data = prods)

step(null_3_linear, scope = list (upper = full_3_linear), data = product3, direction = "both")

p3_linear <- lm(formula = Volume_3 ~ Average_Distribution_3 + promo_3 + promo_5 + promo_2 + Average_Number_SKUs_3, data = prods)


#product 3 - semilog
null_3_semilog <- lm(log.q_3 ~ 1, data = prods)
full_3_semilog <- lm(log.q_3 ~ price_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + price_2 + price_4 + price_5 + promo_2 + promo_4 + promo_5, data = prods)

step(null_3_semilog, scope = list (upper = full_3_semilog), data = product3, direction = "both")

p3_semilog <- lm(formula = log.q_3 ~ Average_Distribution_3 + promo_5 + promo_2 + promo_3 + Average_Number_SKUs_3 + promo_4, data = prods)

#product 3 - log log
null_3_log <- lm(log.q_3 ~ 1, data = prods)
full_3_log <- lm(log.q_3 ~ log.p_3 + Average_Number_SKUs_3 +  Average_Distribution_3 + promo_3 + log.p_2 + log.p_4 + log.p_5 + promo_2 + promo_4 + promo_5, data = prods)

step(null_3_log, scope = list (upper = full_3_log), data = product3, direction = "both")

p3_log <- lm(formula = log.q_3 ~ Average_Distribution_3 + promo_5 + promo_2 + promo_3 + Average_Number_SKUs_3 + promo_4, data = prods)



#product 4 - linear
null_4_linear <- lm(Volume_4 ~ 1, data = prods)
full_4_linear <- lm(Volume_4 ~ price_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + price_3 + price_2 + price_5 + promo_3 + promo_2 + promo_5, data = prods)

step(null_4_linear, scope = list (upper = full_4_linear), data = product4, direction = "both")

p4_linear <- lm(formula = Volume_4 ~ Average_Distribution_4 + price_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + price_3, data = prods)


#product 4 - semilog
null_4_semilog <- lm(log.q_4 ~ 1, data = prods)
full_4_semilog <- lm(log.q_4 ~ price_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + price_2 + price_3 + price_5 + promo_2 + promo_3 + promo_5, data = prods)

step(null_4_semilog, scope = list (upper = full_4_semilog), data = product4, direction = "both")

p4_semilog <- lm(formula = log.q_4 ~ Average_Distribution_4 + price_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + price_2, data = prods)

#product 4 - log log
null_4_log <- lm(log.q_4 ~ 1, data = prods)
full_4_log <- lm(log.q_4 ~ log.p_4 + Average_Number_SKUs_4 +  Average_Distribution_4 + promo_4 + log.p_2 + log.p_3 + log.p_5 + promo_2 + promo_3 + promo_5, data = prods)

step(null_4_log, scope = list (upper = full_4_log), data = product4, direction = "both")

p4_log <- lm(formula = log.q_4 ~ Average_Distribution_4 + log.p_4 + Average_Number_SKUs_4 + promo_3 + promo_4 + log.p_2, data = prods)
  
  
  #product 5 - linear
null_5_linear <- lm(Volume_5 ~ 1, data = prods)
full_5_linear <- lm(Volume_5 ~ price_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + price_3 + price_2 + price_4 + promo_3 + promo_2 + promo_4, data = prods)

step(null_5_linear, scope = list (upper = full_5_linear), data = product5, direction = "both")

p5_linear <- lm(formula = Volume_5 ~ Average_Distribution_5 + promo_5 + promo_3 + price_5 + price_4, data = prods)

#product 5 - semilog
null_5_semilog <- lm(log.q_5 ~ 1, data = prods)
full_5_semilog <- lm(log.q_5 ~ price_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + price_2 + price_3 + price_4 + promo_2 + promo_3 + promo_4, data = prods)

step(null_5_semilog, scope = list (upper = full_5_semilog), data = product5, direction = "both")

p5_semilog <- lm(formula = log.q_5 ~ Average_Distribution_5 + price_5 + promo_2 + price_4 + promo_4, data = prods)

#product 5 - log log
null_5_log <- lm(log.q_5 ~ 1, data = prods)
full_5_log <- lm(log.q_5 ~ log.p_5 + Average_Number_SKUs_5 +  Average_Distribution_5 + promo_5 + log.p_2 + log.p_3 + log.p_4 + promo_2 + promo_3 + promo_4, data = prods)

step(null_5_log, scope = list (upper = full_5_log), data = product5, direction = "both")

p5_log <- lm(formula = log.q_5 ~ Average_Distribution_5 + log.p_5 + promo_2 + log.p_4 + promo_4, data = prods)