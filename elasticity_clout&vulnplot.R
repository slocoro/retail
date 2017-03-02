
# own-price elasticity
elasticity.2 <- summary(p2_linear)$coefficients[2] * mean(products$price_2) / mean(products$Volume_2)
elasticity.3 <- summary(p3_linear)$coefficients[2] * mean(products$price_3) / mean(products$Volume_3)
elasticity.4 <- summary(p4_linear)$coefficients[2] * mean(products$price_4) / mean(products$Volume_4)
elasticity.5 <- summary(p5_linear)$coefficients[2] * mean(products$price_5) / mean(products$Volume_5)

# cross-price elasticity
elasticity.2.3 <- 0
elasticity.2.4 <- summary(p2_linear)$coefficients[6] * mean(products$price_4) / mean(products$Volume_2)
elasticity.2.5 <- 0

elasticity.3.2 <- 0
elasticity.3.4 <- 0
elasticity.3.5 <- 0

elasticity.4.2 <- 0
elasticity.4.3 <- 0
elasticity.4.5 <- 0

elasticity.5.2 <- 0
elasticity.5.3 <- 0
elasticity.5.4 <- summary(p5_linear)$coefficients[6] * mean(products$price_4) / mean(products$Volume_5)

# clout vulnerability
vulnerability.2 <- elasticity.2.3 + elasticity.2.4 + elasticity.2.5
vulnerability.3 <- elasticity.3.2 + elasticity.3.4 + elasticity.3.5
vulnerability.4 <- elasticity.4.2 + elasticity.4.3 + elasticity.4.5
vulnerability.5 <- elasticity.5.2 + elasticity.5.3 + elasticity.5.4

clout.2 <- elasticity.3.2 + elasticity.4.2 + elasticity.5.2
clout.3 <- elasticity.2.3 + elasticity.4.3 + elasticity.5.3
clout.4 <- elasticity.2.4 + elasticity.3.4 + elasticity.5.4
clout.5 <- elasticity.2.5 + elasticity.3.5 + elasticity.4.5

# plot clout and vulnerability
product = c(2,3,4,5)
vulnerability = c(vulnerability.2, vulnerability.3, vulnerability.4, vulnerability.5)
clout = c(clout.2, clout.3, clout.4, clout.5)
avg_volume = c(mean(products$Volume_2), mean(products$Volume_3), mean(products$Volume_4), mean(products$Volume_5))
df <- data.frame(product, vulnerability, clout, avg_volume)

radius <- sqrt(df$avg_volume / pi)
symbols(df$vulnerability, df$clout, circles = radius, inches = 0.35, fg="white", bg="red", xlim=c(-0.7,0.2), ylim=c(-1.7,1), xlab="Vulnerability", ylab="Clout", main = "Crackers Market")
text(df$vulnerability, df$clout, df$product, cex=1.5)

  
  

