library(dplyr)
library(reshape2)
library(dummies)
library(chron)
library(lubridate)

rmse <- function(error) {
  sqrt(mean(error^2)) }


# load data for  assignment 2
data.campaign <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/Chain_Campaign_Details.csv", fileEncoding = "latin1", stringsAsFactors=FALSE)
data.store <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/Chain_Store_Performance_2015_2016.csv", fileEncoding = "latin1", stringsAsFactors=FALSE)
data.googletrends <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/GoogleTrends.csv")
data.grp <- read.csv("/Users/laradaoud/retail_assign2/Assignment 2/Chain_GRPS_2015_2016.csv")

# convert all entries to lower case
data.store$RESTAURANT_CODE <- tolower(data.store$RESTAURANT_CODE)

# check frequency count and filter
t <- data.frame(table(data.store$RESTAURANT_CODE))
filter <- as.character(t[t$Freq == 640, "Var1"])

data.store.new <- subset(data.store, RESTAURANT_CODE %in% filter)


# convert BUSINESS_DATE column to date format
data.store.new$BUSINESS_DATE <- ifelse(nchar(data.store.new$BUSINESS_DATE) < 10, 
                                       as.character(as.Date(data.store.new$BUSINESS_DATE, "%d/%m/%y")),
                                       as.character(as.Date(data.store.new$BUSINESS_DATE, "%d/%m/%Y")))

# check if all dates are present in same frequency
d <- data.frame(table(data.store.new$BUSINESS_DATE))
which(d$Freq != 396)

# aggregate data by day
agg.sales <- data.store.new %>%
  group_by(BUSINESS_DATE) %>%
  summarize(sales = sum(NET_SALES, na.rm = TRUE),
            visits = sum(RECEIPTS_COUNT, na.rm = TRUE))

# add day of the week to later model seasonality
agg.sales$weekday <- weekdays(as.Date(agg.sales$BUSINESS_DATE))
agg.sales$month <- months(as.Date(agg.sales$BUSINESS_DATE))


# plot sales
plot(agg.sales$sales,
     type="o",  col="blue",
     ylab = "", xlab = "Time",
     main = "Daily Sales")

# decomposition of sales
sales.ts <- ts(agg.sales$sales, frequency = 365, start=c(2015,5,15))
plot(stl(sales.ts))

agg.sales$lag_visits1 <- c(NA, head(agg.sales$visits, -1))
agg.sales$lag_visits2 <- c(NA, NA, head(agg.sales$visits, -2))
agg.sales$lag_visits3 <- c(NA, NA, NA, head(agg.sales$visits, -3))

#--------------------------------------------------------------------------------------------------

# fix date column in campaign data
data.campaign$Date_out_start <- ifelse(nchar(data.campaign$Date_out_start) < 10, 
                                       as.character(as.Date(data.campaign$Date_out_start, "%d/%m/%y")),
                                       as.character(as.Date(data.campaign$Date_out_start, "%d/%m/%Y")))

# fix date column in grp data 
data.grp$Date <- as.character(as.Date(data.grp$Date, "%d/%m/%Y"))

# find number of media formats
media.table <- data.frame(table(data.campaign$MEDIA))

# group medias into new column
data.campaign$media_grouped <- ifelse(grepl("Tv", data.campaign$MEDIA), "tv", 
                                      ifelse(grepl("St", data.campaign$MEDIA), "print",
                                      ifelse(grepl("Radio", data.campaign$MEDIA), "radio", 
                                      ifelse(grepl("M.L.", data.campaign$MEDIA), "special",
                                      ifelse(grepl("Internet", data.campaign$MEDIA), "internet", 0)))))

# subset data by different medias
campaign.print <- data.campaign[data.campaign$media_grouped == "print", ]
campaign.tv <- data.campaign[data.campaign$media_grouped == "tv", ]
campaign.radio <- data.campaign[data.campaign$media_grouped == "radio", ]
campaign.special <- data.campaign[data.campaign$media_grouped == "special", ]
campaign.internet <- data.campaign[data.campaign$media_grouped == "internet", ]

# weekly and monthly media 
weekly.media <- c("Chi", "D La Repubblica delle Donne", "Dipi",
                 "Donna Moderna", "F", "Famiglia Cristiana",
                 "Grazie", "Io Donna",
                 "Oggi", "Sette", "Sorrisi e Canzoni TV",
                 "Sport Week", "Starbene")

monthly.media <- c("Il Giornale Stile", "Natural Style", "Zero Network")

# add column with length of ad
campaign.print$length <- ifelse(campaign.print$CHANNEL_VEHICLE %in% weekly.media, 7,
                         ifelse(campaign.print$CHANNEL_VEHICLE %in% monthly.media, 30, 1))

# add daily cost for add
campaign.print$daily_cost <- round(campaign.print$NET_COST / campaign.print$length, 2)


# distribute cost over length of campaign
col.names <- colnames(campaign.print)
new.data <- setNames(data.frame(matrix(ncol = length(col.names), nrow = 0)), col.names)

count <- 0

for (i in 1:nrow(campaign.print)) {
  if (campaign.print$length[i] == 1) {
    new.data[nrow(new.data) + 1, ] <- campaign.print[i, ]
  } else {
    for (j in 1:campaign.print[i, "length"]) {
      if (j == 1) {
        new.data[nrow(new.data) + 1, ] <- campaign.print[i, ]
      } else {
        new.data[nrow(new.data) + 1, ] <- campaign.print[i, ]
        new.data$Date_out_start[nrow(new.data)] <- as.character(as.Date(campaign.print$Date_out_start[i]) + j - 1)
      }
    }
  }
}


# clean data in preparation to merge with aggregated sales data
campaign.print.final <- select(new.data, FORMAT, Date_out_start, daily_cost)
campaign.internet.final <- select(campaign.internet, FORMAT, NET_COST, Date_out_start)
campaign.tv.final <- select(campaign.tv, FORMAT, NETWORK, ISSUE_TIME, NET_COST, Date_out_start)
campaign.radio.final <- select(campaign.radio, FORMAT, NETWORK, ISSUE_TIME, NET_COST, Date_out_start)
campaign.special.final <- select(campaign.special, FORMAT, NETWORK, ISSUE_TIME, NET_COST, Date_out_start)


# aggregate all the data daily
campaign.print.final.agg <- campaign.print.final %>%
  group_by(Date_out_start) %>%
  summarize(print_inv = sum(daily_cost, na.rm = TRUE))

campaign.internet.final.agg <- campaign.internet.final %>%
  group_by(Date_out_start) %>%
  summarize(internet_inv = sum(NET_COST, na.rm = TRUE))

campaign.tv.final.agg <- campaign.tv.final %>%
  group_by(Date_out_start) %>%
  summarize(tv_inv = sum(NET_COST, na.rm = TRUE))

campaign.radio.final.agg <- campaign.radio.final %>%
  group_by(Date_out_start) %>%
  summarize(radio_inv = sum(NET_COST, na.rm = TRUE))

campaign.special.final.agg <- campaign.special.final %>%
  group_by(Date_out_start) %>%
  summarize(special_inv = sum(NET_COST, na.rm = TRUE))


# check how many gifted campaigns there were in each media
gift.print <- sum(campaign.print.final.agg$print_inv == 0); gift.print
gift.internet <- sum(campaign.internet.final.agg$internet_inv == 0); gift.internet
gift.tv <- sum(campaign.tv.final.agg$tv_inv == 0); gift.tv
gift.radio <- sum(campaign.radio.final.agg$radio_inv == 0); gift.radio
gift.special <- sum(campaign.special.final.agg$special_inv == 0); gift.special

# replace gifted (0 investment) values using the GRP and linear regression (for tv)
#tv.grp <- full_join(campaign.tv.final.agg, data.grp, by = c("Date_out_start" = "Date"))
tv.grp <- left_join(campaign.tv.final.agg, data.grp, by = c("Date_out_start" = "Date"))
tv.grp[is.na(tv.grp)] <- 0  # replaced NAs in GRP with 0 

cor.tv.grp <- cor(tv.grp$tv_inv, tv.grp$TV_GRPS); cor.tv.grp

model.tv.grp <- lm(tv_inv ~ TV_GRPS, data = tv.grp); summary(model.tv.grp)
tv.grp$tv_inv <- ifelse(tv.grp$tv_inv == 0 & tv.grp$TV_GRPS != 0, 
                        model.tv.grp$coefficients[1] + model.tv.grp$coefficients[2] * tv.grp$TV_GRPS, tv.grp$tv_inv)
# after using this method there is one row with value 0

campaign.tv.final.agg <- tv.grp[, c("Date_out_start", "tv_inv")]

# not possible to replace 0 values for radio because they don't have a GRP associated to them
# instead replace with average investment value for radio
mean.radio.inv <- mean(campaign.radio.final.agg$radio_inv, na.rm = TRUE)
campaign.radio.final.agg$radio_inv <- ifelse(campaign.radio.final.agg$radio_inv == 0,
                                             mean.radio.inv,
                                             campaign.radio.final.agg$radio_inv)

# replace 0s in other media with mean as well
mean.print.inv <- mean(campaign.print.final.agg$print_inv, na.rm = TRUE); mean.print.inv
campaign.print.final.agg$print_inv <- ifelse(campaign.print.final.agg$print_inv == 0,
                                             mean.print.inv,
                                             campaign.print.final.agg$print_inv)

mean.internet.inv <- mean(campaign.internet.final.agg$internet_inv, na.rm = TRUE); mean.internet.inv
campaign.internet.final.agg$internet_inv <- ifelse(campaign.internet.final.agg$internet_inv == 0,
                                             mean.internet.inv,
                                             campaign.internet.final.agg$internet_inv)

mean.special.inv <- mean(campaign.special.final.agg$special_inv, na.rm = TRUE); mean.special.inv
campaign.special.final.agg$special_inv <- ifelse(campaign.special.final.agg$special_inv == 0,
                                                   mean.special.inv,
                                                   campaign.special.final.agg$special_inv)


# test general regression to see which media has the biggest effect on sales
agg.sales.all.media <- 
  left_join(agg.sales, campaign.print.final.agg, by = c("BUSINESS_DATE" = "Date_out_start")) %>%
  left_join(., campaign.internet.final.agg, by = c("BUSINESS_DATE" = "Date_out_start")) %>%
  left_join(., campaign.tv.final.agg, by = c("BUSINESS_DATE" = "Date_out_start")) %>%
  left_join(., campaign.radio.final.agg, by = c("BUSINESS_DATE" = "Date_out_start")) %>%
  left_join(., campaign.special.final.agg, by = c("BUSINESS_DATE" = "Date_out_start"))


# aggregated data has a lot of NAs because company doesn't always use all media at the same time
# replaced NAs with 0. Gifted campaigns were taken care of before
agg.sales.all.media[is.na(agg.sales.all.media)] <- 0

# Test which media is most efficient
agg.sales.all.media$trend <- 1:nrow(agg.sales.all.media)
model.allmedia <- lm(sales ~ visits + weekday + trend + print_inv + internet_inv + tv_inv + radio_inv + special_inv, agg.sales.all.media); summary(model.allmedia)

cor(agg.sales.all.media[, c("sales","visits","print_inv", "internet_inv", "tv_inv", "radio_inv")])

# missing data needs to be replaced in dissagregated data
tv.grp.dis <- left_join(campaign.tv.final, data.grp, by = c("Date_out_start" = "Date"))
tv.grp.dis[is.na(tv.grp.dis)] <- 0  # replaced NAs in GRP with 0 

cor.tv.grp.dis <- cor(tv.grp.dis$NET_COST, tv.grp.dis$TV_GRPS); cor.tv.grp.dis # very low correlation at disaggregate level

# better to replace with mean value (for tv and radio)
mean.tv.inv.dis <- mean(campaign.tv.final$NET_COST, na.rm = TRUE); mean.tv.inv.dis
campaign.tv.final$NET_COST <- ifelse(campaign.tv.final$NET_COST == 0,
                                     mean.tv.inv.dis,
                                     campaign.tv.final$NET_COST)

mean.radio.inv.dis <- mean(campaign.radio.final$NET_COST, na.rm = TRUE); mean.radio.inv.dis
campaign.radio.final$NET_COST <- ifelse(campaign.radio.final$NET_COST == 0,
                                     mean.radio.inv.dis,
                                     campaign.radio.final$NET_COST)


# focus on TV and radio as these are the only statistically significant ones
# different approach
# aggregate data by date, format, network
agg.tv.new <- campaign.tv.final %>%
  group_by(Date_out_start, FORMAT, NETWORK) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE))

agg.radio.new <- campaign.radio.final %>%
  group_by(Date_out_start, FORMAT, NETWORK) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE))

tv.data <- dcast(agg.tv.new, Date_out_start ~ FORMAT + NETWORK)
radio.data <- dcast(agg.radio.new, Date_out_start ~ FORMAT + NETWORK)

# merge media data with aggregated data
agg.sales.tv <- 
  left_join(agg.sales, tv.data, by = c("BUSINESS_DATE" = "Date_out_start"))
agg.sales.tv$trend <- 1:nrow(agg.sales.tv)

agg.sales.radio <- 
  left_join(agg.sales, radio.data, by = c("BUSINESS_DATE" = "Date_out_start"))
agg.sales.radio$trend <- 1:nrow(agg.sales.radio)

# fill all NAs with 0
agg.sales.tv[is.na(agg.sales.tv)] <- 0
agg.sales.radio[is.na(agg.sales.radio)] <- 0

# add sales per visit column
agg.sales.tv$sales_per_visit <- agg.sales.tv$sales / agg.sales.tv$visits
agg.sales.radio$sales_per_visit <- agg.sales.radio$sales / agg.sales.radio$visits


# save data to csv
#write.csv(agg.sales.tv, file = "sales_tv_3.csv")
#write.csv(agg.sales.radio, file = "sales_radio_3.csv")

#-------------------------------------------------------------------
# test regression
t <- agg.sales.radio[, -c(1)]
mod1 <- lm(sales ~ ., t); summary(mod1)
summary.mod1 <- data.frame(summary(mod1)$coefficients)
summary.mod1$names <- row.names(summary.mod1)
rownames(summary.mod1) <- NULL

tt <- agg.sales.tv[, -c(1)]
mod2 <- lm(sales ~ ., tt); summary(mod2)
summary.mod2 <- data.frame(summary(mod2)$coefficients)
summary.mod2$names <- row.names(summary.mod2)
rownames(summary.mod2) <- NULL


# column count of non-zero values
count.tv <- data.frame(colSums(agg.sales.tv[, 5:(ncol(agg.sales.tv)-1)] != 0)); count.tv
count.tv$names <- row.names(count.tv)
rownames(count.tv) <- NULL

count.radio <- data.frame(colSums(agg.sales.radio[, 5:(ncol(agg.sales.radio)-1)] != 0)); count.radio
count.radio$names <- row.names(count.radio)
rownames(count.radio) <- NULL

# join the count of adds
count.radio$names <- paste0("`", count.radio$names, "`")
summary.mod11 <- left_join(summary.mod1, count.radio, by = c("names" = "names"))

count.tv$names <- paste0("`", count.tv$names, "`")
summary.mod22 <- left_join(summary.mod2, count.tv, by = c("names" = "names"))


#-------------------------------------------------------------------

# data for the formats only
agg.tv.new1 <- campaign.tv.final %>%
  group_by(Date_out_start, FORMAT) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE))

agg.radio.new1 <- campaign.radio.final %>%
  group_by(Date_out_start, FORMAT) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE))

tv.data.format <- dcast(agg.tv.new1, Date_out_start ~ FORMAT)
radio.data.format <- dcast(agg.radio.new1, Date_out_start ~ FORMAT)

# merge media data with aggregated data
agg.sales.tv1 <- 
  left_join(agg.sales, tv.data.format, by = c("BUSINESS_DATE" = "Date_out_start"))
agg.sales.tv1$trend <- 1:nrow(agg.sales.tv1)

agg.sales.radio1 <- 
  left_join(agg.sales, radio.data.format, by = c("BUSINESS_DATE" = "Date_out_start"))
agg.sales.radio1$trend <- 1:nrow(agg.sales.radio1)

# fill all NAs with 0
agg.sales.tv1[is.na(agg.sales.tv1)] <- 0
agg.sales.radio1[is.na(agg.sales.radio1)] <- 0

# add sales per visit column
agg.sales.radio1$sales_per_visit <- agg.sales.radio1$sales / agg.sales.radio1$visits
agg.sales.radio1$sales_per_visit.lag1 <- c(NA, head(agg.sales.radio1$sales_per_visit, -1))
agg.sales.radio1$sales_per_visit.lag2 <- c(NA, NA, head(agg.sales.radio1$sales_per_visit, -2))
agg.sales.radio1$sales_per_visit.lag3 <- c(NA, NA, NA, head(agg.sales.radio1$sales_per_visit, -3))

agg.sales.radio1$`15 secondi.lag1` <- c(NA, head(agg.sales.radio1$`15 secondi`, -1))
agg.sales.radio1$`15 secondi.lag2` <- c(NA, NA, head(agg.sales.radio1$`15 secondi`, -2))
agg.sales.radio1$`15 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.radio1$`15 secondi`, -3))

agg.sales.radio1$`20 secondi.lag1` <- c(NA, head(agg.sales.radio1$`20 secondi`, -1))
agg.sales.radio1$`20 secondi.lag2` <- c(NA, NA, head(agg.sales.radio1$`20 secondi`, -2))
agg.sales.radio1$`20 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.radio1$`20 secondi`, -3))

agg.sales.radio1$`30 secondi.lag1` <- c(NA, head(agg.sales.radio1$`30 secondi`, -1))
agg.sales.radio1$`30 secondi.lag2` <- c(NA, NA, head(agg.sales.radio1$`30 secondi`, -2))
agg.sales.radio1$`30 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.radio1$`30 secondi`, -3))

agg.sales.radio1$`15secondi.sq` <- agg.sales.radio1$`15 secondi`^2
agg.sales.radio1$`20secondi.sq` <- agg.sales.radio1$`20 secondi`^2
agg.sales.radio1$`30secondi.sq` <- agg.sales.radio1$`30 secondi`^2

agg.sales.radio1$`15secondi.adstock` <- 0 + agg.sales.radio1$`15 secondi`
agg.sales.radio1$`20secondi.adstock` <- 0 + agg.sales.radio1$`20 secondi`
agg.sales.radio1$`30secondi.adstock` <- 0 + agg.sales.radio1$`30 secondi`

agg.sales.tv1$sales_per_visit <- agg.sales.tv1$sales / agg.sales.tv1$visits
agg.sales.tv1$sales_per_visit.lag1 <- c(NA, head(agg.sales.tv1$sales_per_visit, -1))
agg.sales.tv1$sales_per_visit.lag2 <- c(NA, NA, head(agg.sales.tv1$sales_per_visit, -2))
agg.sales.tv1$sales_per_visit.lag3 <- c(NA, NA, NA, head(agg.sales.tv1$sales_per_visit, -3))

agg.sales.tv1$`10 secondi.lag1` <- c(NA, head(agg.sales.tv1$`10 secondi`, -1))
agg.sales.tv1$`10 secondi.lag2` <- c(NA, NA, head(agg.sales.tv1$`10 secondi`, -2))
agg.sales.tv1$`10 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.tv1$`10 secondi`, -3))

agg.sales.tv1$`15 secondi.lag1` <- c(NA, head(agg.sales.tv1$`15 secondi`, -1))
agg.sales.tv1$`15 secondi.lag2` <- c(NA, NA, head(agg.sales.tv1$`15 secondi`, -2))
agg.sales.tv1$`15 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.tv1$`15 secondi`, -3))

agg.sales.tv1$`20 secondi.lag1` <- c(NA, head(agg.sales.tv1$`20 secondi`, -1))
agg.sales.tv1$`20 secondi.lag2` <- c(NA, NA, head(agg.sales.tv1$`20 secondi`, -2))
agg.sales.tv1$`20 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.tv1$`20 secondi`, -3))

agg.sales.tv1$`30 secondi.lag1` <- c(NA, head(agg.sales.tv1$`30 secondi`, -1))
agg.sales.tv1$`30 secondi.lag2` <- c(NA, NA, head(agg.sales.tv1$`30 secondi`, -2))
agg.sales.tv1$`30 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.tv1$`30 secondi`, -3))

agg.sales.tv1$`45 secondi.lag1` <- c(NA, head(agg.sales.tv1$`45 secondi`, -1))
agg.sales.tv1$`45 secondi.lag2` <- c(NA, NA, head(agg.sales.tv1$`45 secondi`, -2))
agg.sales.tv1$`45 secondi.lag3` <- c(NA, NA, NA, head(agg.sales.tv1$`45 secondi`, -3))

agg.sales.tv1$`10secondi.sq` <- agg.sales.tv1$`10 secondi`^2
agg.sales.tv1$`15secondi.sq` <- agg.sales.tv1$`15 secondi`^2
agg.sales.tv1$`20secondi.sq` <- agg.sales.tv1$`20 secondi`^2
agg.sales.tv1$`30secondi.sq` <- agg.sales.tv1$`30 secondi`^2
agg.sales.tv1$`45secondi.sq` <- agg.sales.tv1$`45 secondi`^2

agg.sales.tv1$`10secondi.adstock` <- 0 + agg.sales.tv1$`10 secondi`
agg.sales.tv1$`15secondi.adstock` <- 0 + agg.sales.tv1$`15 secondi`
agg.sales.tv1$`20secondi.adstock` <- 0 + agg.sales.tv1$`20 secondi`
agg.sales.tv1$`30secondi.adstock` <- 0 + agg.sales.tv1$`30 secondi`
agg.sales.tv1$`45secondi.adstock` <- 0 + agg.sales.tv1$`45 secondi`

# things that would be nice to have 
# indication about what kind of audience the channel targets. this would allow to
# cluster channels by type (family, sports, film, ...)

#Ad Stock Alphas
alpha.values <- c(seq(0.8, 1, by = 0.01))

adstock.table.radio <- data.frame(Alpha = numeric(0),
                            RMSE = numeric(0))

j <- 1
for (value in alpha.values){
  agg.sales.radio1$`15secondi.adstock` <- numeric(length(agg.sales.radio1$`15 secondi`))
  agg.sales.radio1$`20secondi.adstock` <- numeric(length(agg.sales.radio1$`20 secondi`))
  agg.sales.radio1$`30secondi.adstock` <- numeric(length(agg.sales.radio1$`30 secondi`))
  
  agg.sales.radio1$`15secondi.adstock`[1] <- agg.sales.radio1$`15 secondi`[1]
  agg.sales.radio1$`20secondi.adstock`[1] <- agg.sales.radio1$`20 secondi`[1]
  agg.sales.radio1$`30secondi.adstock`[1] <- agg.sales.radio1$`30 secondi`[1]
  
  for (i in 2:length(agg.sales.radio1$`15secondi.adstock`)){
    agg.sales.radio1$`15secondi.adstock`[i] = agg.sales.radio1$`15 secondi`[i] + value * agg.sales.radio1$`15secondi.adstock`[i-1]
  }
  
  for (i in 2:length(agg.sales.radio1$`20secondi.adstock`)){
    agg.sales.radio1$`20secondi.adstock`[i] = agg.sales.radio1$`20 secondi`[i] + value * agg.sales.radio1$`20secondi.adstock`[i-1]
  }
  
  for (i in 2:length(agg.sales.radio1$`30secondi.adstock`)){
    agg.sales.radio1$`30secondi.adstock`[i] = agg.sales.radio1$`30 secondi`[i] + value * agg.sales.radio1$`30secondi.adstock`[i-1]
  }
  
  radio.1.2.adstock <- lm(visits ~  weekday + month + trend + 
                          `15secondi.adstock` + `20secondi.adstock` + `30secondi.adstock` , 
                          data = agg.sales.radio1) 
  
  adstock.table.radio <- rbind(adstock.table.radio, c(value, rmse(fitted(radio.1.2.adstock) - agg.sales.radio1$visits)))
  
  j <- j+1
}

colnames(adstock.table.radio) <- c("Alpha", "RMSE")
adstock.alpha.radio <- adstock.table.radio$Alpha[which.min(adstock.table.radio$RMSE)]

agg.sales.radio1$`15secondi.adstock` <- numeric(length(agg.sales.radio1$`15 secondi`))
agg.sales.radio1$`20secondi.adstock` <- numeric(length(agg.sales.radio1$`20 secondi`))
agg.sales.radio1$`30secondi.adstock` <- numeric(length(agg.sales.radio1$`30 secondi`))

agg.sales.radio1$`15secondi.adstock`[1] <- agg.sales.radio1$`15 secondi`[1]
agg.sales.radio1$`20secondi.adstock`[1] <- agg.sales.radio1$`20 secondi`[1]
agg.sales.radio1$`30secondi.adstock`[1] <- agg.sales.radio1$`30 secondi`[1]

for (i in 2:length(agg.sales.radio1$`15secondi.adstock`)){
  agg.sales.radio1$`15secondi.adstock`[i] = agg.sales.radio1$`15 secondi`[i] + adstock.alpha.radio * agg.sales.radio1$`15secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.radio1$`20secondi.adstock`)){
  agg.sales.radio1$`20secondi.adstock`[i] = agg.sales.radio1$`20 secondi`[i] + adstock.alpha.radio * agg.sales.radio1$`20secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.radio1$`30secondi.adstock`)){
  agg.sales.radio1$`30secondi.adstock`[i] = agg.sales.radio1$`30 secondi`[i] + adstock.alpha.radio * agg.sales.radio1$`30secondi.adstock`[i-1]
}

agg.sales.all.media$radio.adstock <- numeric(length(agg.sales.all.media$radio_inv))
agg.sales.all.media$radio.adstock[1] <- agg.sales.all.media$radio_inv[1]

for (i in 2:length(agg.sales.all.media$radio.adstock)){
  agg.sales.all.media$radio.adstock[i] = agg.sales.all.media$radio_inv[i] + adstock.alpha.radio * agg.sales.all.media$radio.adstock[i-1]
}

#Ad Stock Alphas TV
alpha.values <- c(seq(0.8, 1, by = 0.01))

adstock.table.tv <- data.frame(Alpha = numeric(0),
                                  RMSE = numeric(0))

j <- 1
for (value in alpha.values){
  agg.sales.tv1$`10secondi.adstock` <- numeric(length(agg.sales.tv1$`10 secondi`))
  agg.sales.tv1$`15secondi.adstock` <- numeric(length(agg.sales.tv1$`15 secondi`))
  agg.sales.tv1$`20secondi.adstock` <- numeric(length(agg.sales.tv1$`20 secondi`))
  agg.sales.tv1$`30secondi.adstock` <- numeric(length(agg.sales.tv1$`30 secondi`))
  agg.sales.tv1$`45secondi.adstock` <- numeric(length(agg.sales.tv1$`45 secondi`))
  
  agg.sales.tv1$`10secondi.adstock`[1] <- agg.sales.tv1$`10 secondi`[1]
  agg.sales.tv1$`15secondi.adstock`[1] <- agg.sales.tv1$`15 secondi`[1]
  agg.sales.tv1$`20secondi.adstock`[1] <- agg.sales.tv1$`20 secondi`[1]
  agg.sales.tv1$`30secondi.adstock`[1] <- agg.sales.tv1$`30 secondi`[1]
  agg.sales.tv1$`45secondi.adstock`[1] <- agg.sales.tv1$`45 secondi`[1]
  
  for (i in 2:length(agg.sales.tv1$`10secondi.adstock`)){
    agg.sales.tv1$`10secondi.adstock`[i] = agg.sales.tv1$`10 secondi`[i] + value * agg.sales.tv1$`10secondi.adstock`[i-1]
  }
  
  for (i in 2:length(agg.sales.tv1$`15secondi.adstock`)){
    agg.sales.tv1$`15secondi.adstock`[i] = agg.sales.tv1$`15 secondi`[i] + value * agg.sales.tv1$`15secondi.adstock`[i-1]
  }
  
  for (i in 2:length(agg.sales.tv1$`20secondi.adstock`)){
    agg.sales.tv1$`20secondi.adstock`[i] = agg.sales.tv1$`20 secondi`[i] + value * agg.sales.tv1$`20secondi.adstock`[i-1]
  }

  for (i in 2:length(agg.sales.tv1$`30secondi.adstock`)){
    agg.sales.tv1$`30secondi.adstock`[i] = agg.sales.tv1$`30 secondi`[i] + value * agg.sales.tv1$`30secondi.adstock`[i-1]
  }
  
  for (i in 2:length(agg.sales.tv1$`45secondi.adstock`)){
    agg.sales.tv1$`45secondi.adstock`[i] = agg.sales.tv1$`45 secondi`[i] + value * agg.sales.tv1$`45secondi.adstock`[i-1]
  }
  
  tv.1.2.adstock <- lm(visits ~  weekday + month + trend + 
                          `10secondi.adstock` + `15secondi.adstock` + `20secondi.adstock` + `30secondi.adstock` + `45secondi.adstock` , 
                          data = agg.sales.tv1) 
  
  adstock.table.tv <- rbind(adstock.table.tv, c(value, rmse(fitted(tv.1.2.adstock) - agg.sales.tv1$visits)))
  
  j <- j+1
}

colnames(adstock.table.tv) <- c("Alpha", "RMSE")
adstock.alpha.tv <- adstock.table.tv$Alpha[which.min(adstock.table.tv$RMSE)]

agg.sales.tv1$`10secondi.adstock` <- numeric(length(agg.sales.tv1$`10 secondi`))
agg.sales.tv1$`15secondi.adstock` <- numeric(length(agg.sales.tv1$`15 secondi`))
agg.sales.tv1$`20secondi.adstock` <- numeric(length(agg.sales.tv1$`20 secondi`))
agg.sales.tv1$`30secondi.adstock` <- numeric(length(agg.sales.tv1$`30 secondi`))
agg.sales.tv1$`45secondi.adstock` <- numeric(length(agg.sales.tv1$`45 secondi`))

agg.sales.tv1$`10secondi.adstock`[1] <- agg.sales.tv1$`10 secondi`[1]
agg.sales.tv1$`15secondi.adstock`[1] <- agg.sales.tv1$`15 secondi`[1]
agg.sales.tv1$`20secondi.adstock`[1] <- agg.sales.tv1$`20 secondi`[1]
agg.sales.tv1$`30secondi.adstock`[1] <- agg.sales.tv1$`30 secondi`[1]
agg.sales.tv1$`45secondi.adstock`[1] <- agg.sales.tv1$`45 secondi`[1]

for (i in 2:length(agg.sales.tv1$`10secondi.adstock`)){
  agg.sales.tv1$`10secondi.adstock`[i] = agg.sales.tv1$`10 secondi`[i] + adstock.alpha.tv * agg.sales.tv1$`10secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.tv1$`15secondi.adstock`)){
  agg.sales.tv1$`15secondi.adstock`[i] = agg.sales.tv1$`15 secondi`[i] + adstock.alpha.tv * agg.sales.tv1$`15secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.tv1$`20secondi.adstock`)){
  agg.sales.tv1$`20secondi.adstock`[i] = agg.sales.tv1$`20 secondi`[i] + adstock.alpha.tv * agg.sales.tv1$`20secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.tv1$`30secondi.adstock`)){
  agg.sales.tv1$`30secondi.adstock`[i] = agg.sales.tv1$`30 secondi`[i] + adstock.alpha.tv * agg.sales.tv1$`30secondi.adstock`[i-1]
}

for (i in 2:length(agg.sales.tv1$`45secondi.adstock`)){
  agg.sales.tv1$`45secondi.adstock`[i] = agg.sales.tv1$`45 secondi`[i] + adstock.alpha.tv * agg.sales.tv1$`45secondi.adstock`[i-1]
}


agg.sales.all.media$tv.adstock <- numeric(length(agg.sales.all.media$tv_inv))
agg.sales.all.media$tv.adstock[1] <- agg.sales.all.media$tv_inv[1]

for (i in 2:length(agg.sales.all.media$tv.adstock)){
  agg.sales.all.media$tv.adstock[i] = agg.sales.all.media$tv_inv[i] + adstock.alpha.tv * agg.sales.all.media$tv.adstock[i-1]
}

#Ad Stock Alphas Print
alpha.values <- c(seq(0.5, 0.96, by = 0.01))

adstock.table.print <- data.frame(Alpha = numeric(0),
                                  RMSE = numeric(0))
j <- 1
for (value in alpha.values){
  agg.sales.all.media$print.adstock <- numeric(length(agg.sales.all.media$print_inv))
  
  agg.sales.all.media$print.adstock[1] <- agg.sales.all.media$print_inv[1]
  
  for (i in 2:length(agg.sales.all.media$print.adstock)){
    agg.sales.all.media$print.adstock[i] = agg.sales.all.media$print_inv[i] + value * agg.sales.all.media$print.adstock[i-1]
  }
  print.1.2.adstock <- lm(visits ~ weekday + month + trend + print.adstock , data = agg.sales.all.media) 
  adstock.table.print <- rbind(adstock.table.print, c(value, rmse(fitted(print.1.2.adstock) - agg.sales.all.media$visits)))
  j <- j+1
}

colnames(adstock.table.print) <- c("Alpha", "RMSE")
adstock.alpha.print <- adstock.table.print$Alpha[which.min(adstock.table.print$RMSE)]

agg.sales.all.media$print.adstock <- numeric(length(agg.sales.all.media$print_inv))
agg.sales.all.media$print.adstock[1] <- agg.sales.all.media$print_inv[1]

for (i in 2:length(agg.sales.all.media$print.adstock)){
  agg.sales.all.media$print.adstock[i] = agg.sales.all.media$print_inv[i] + adstock.alpha.print * agg.sales.all.media$print.adstock[i-1]
}

#Ad Stock Alphas Internet
alpha.values <- c(seq(0.5, .99, by = 0.01))

adstock.table.internet <- data.frame(Alpha = numeric(0),
                                  RMSE = numeric(0))
j <- 1
for (value in alpha.values){
  agg.sales.all.media$internet.adstock <- numeric(length(agg.sales.all.media$internet_inv))
  
  agg.sales.all.media$internet.adstock[1] <- agg.sales.all.media$internet_inv[1]
  
  for (i in 2:length(agg.sales.all.media$internet.adstock)){
    agg.sales.all.media$internet.adstock[i] = agg.sales.all.media$internet_inv[i] + value * agg.sales.all.media$internet.adstock[i-1]
  }
  internet.1.2.adstock <- lm(visits ~ weekday + month + trend + internet.adstock , data = agg.sales.all.media) 
  adstock.table.internet <- rbind(adstock.table.internet, c(value, rmse(fitted(internet.1.2.adstock) - agg.sales.all.media$visits)))
  j <- j+1
}

colnames(adstock.table.internet) <- c("Alpha", "RMSE")
adstock.alpha.internet <- adstock.table.internet$Alpha[which.min(adstock.table.internet$RMSE)]

agg.sales.all.media$internet.adstock <- numeric(length(agg.sales.all.media$internet_inv))
agg.sales.all.media$internet.adstock[1] <- agg.sales.all.media$internet_inv[1]

for (i in 2:length(agg.sales.all.media$internet.adstock)){
  agg.sales.all.media$internet.adstock[i] = agg.sales.all.media$internet_inv[i] + adstock.alpha.internet * agg.sales.all.media$internet.adstock[i-1]
}


#Ad Stock Alphas Special
alpha.values <- c(seq(0.5, 1, by = 0.01))

adstock.table.special <- data.frame(Alpha = numeric(0),
                                     RMSE = numeric(0))
j <- 1
for (value in alpha.values){
  agg.sales.all.media$special.adstock <- numeric(length(agg.sales.all.media$special_inv))
  
  agg.sales.all.media$special.adstock[1] <- agg.sales.all.media$special_inv[1]
  
  for (i in 2:length(agg.sales.all.media$special.adstock)){
    agg.sales.all.media$special.adstock[i] = agg.sales.all.media$special_inv[i] + value * agg.sales.all.media$special.adstock[i-1]
  }
  special.1.2.adstock <- lm(visits ~ weekday + month + trend + special.adstock , data = agg.sales.all.media) 
  adstock.table.special <- rbind(adstock.table.special, c(value, rmse(fitted(special.1.2.adstock) - agg.sales.all.media$visits)))
  j <- j+1
}

colnames(adstock.table.special) <- c("Alpha", "RMSE")
adstock.alpha.special <- adstock.table.special$Alpha[which.min(adstock.table.special$RMSE)]

agg.sales.all.media$special.adstock <- numeric(length(agg.sales.all.media$special_inv))
agg.sales.all.media$special.adstock[1] <- agg.sales.all.media$special_inv[1]

for (i in 2:length(agg.sales.all.media$special.adstock)){
  agg.sales.all.media$special.adstock[i] = agg.sales.all.media$special_inv[i] + adstock.alpha.special * agg.sales.all.media$special.adstock[i-1]
}


agg.sales.all.media$print.adstock.log    <- log(agg.sales.all.media$print.adstock)
agg.sales.all.media$internet.adstock.log <- log(agg.sales.all.media$internet.adstock)
agg.sales.all.media$tv.adstock.log       <- log(agg.sales.all.media$tv.adstock)
agg.sales.all.media$radio.adstock.log    <- log(agg.sales.all.media$radio.adstock)
agg.sales.all.media$special.adstock.log  <- log(agg.sales.all.media$special.adstock)

agg.sales.all.media$internet.adstock.log[is.infinite(agg.sales.all.media$internet.adstock.log)] <- mean(agg.sales.all.media$internet.adstock.log[is.finite(agg.sales.all.media$internet.adstock.log)])
agg.sales.all.media$radio.adstock.log[is.infinite(agg.sales.all.media$radio.adstock.log)]       <- mean(agg.sales.all.media$radio.adstock.log[is.finite(agg.sales.all.media$radio.adstock.log)])
agg.sales.all.media$special.adstock.log[is.infinite(agg.sales.all.media$special.adstock.log)] <- mean(agg.sales.all.media$special.adstock.log[is.finite(agg.sales.all.media$special.adstock.log)])

agg.sales.radio1 <- cbind(agg.sales.radio1, agg.sales.all.media$print.adstock.log, agg.sales.all.media$internet.adstock.log,
                          agg.sales.all.media$tv.adstock.log, agg.sales.all.media$special.adstock.log)

agg.sales.tv1 <- cbind(agg.sales.tv1, agg.sales.all.media$print.adstock.log, agg.sales.all.media$internet.adstock.log,
                       agg.sales.all.media$radio.adstock.log, agg.sales.all.media$special.adstock.log)

agg.sales.radio1$`15secondi.adstock.log` <- log(agg.sales.radio1$`15secondi.adstock`)
agg.sales.radio1$`20secondi.adstock.log` <- log(agg.sales.radio1$`20secondi.adstock`)
agg.sales.radio1$`30secondi.adstock.log` <- log(agg.sales.radio1$`30secondi.adstock`)

agg.sales.radio1$`15secondi.adstock.log`[is.infinite(agg.sales.radio1$`15secondi.adstock.log`)] <- 0
agg.sales.radio1$`20secondi.adstock.log`[is.infinite(agg.sales.radio1$`20secondi.adstock.log`)] <- 0
agg.sales.radio1$`30secondi.adstock.log`[is.infinite(agg.sales.radio1$`30secondi.adstock.log`)] <- 0

agg.sales.radio1$lag_visits1 <- log(agg.sales.radio1$lag_visits1)
agg.sales.radio1$lag_visits2 <- log(agg.sales.radio1$lag_visits2)
agg.sales.radio1$lag_visits3 <- log(agg.sales.radio1$lag_visits3)

agg.sales.radio1$lag_visits1[is.infinite(agg.sales.radio1$lag_visits1)] <- NA
agg.sales.radio1$lag_visits2[is.infinite(agg.sales.radio1$lag_visits2)] <- NA
agg.sales.radio1$lag_visits3[is.infinite(agg.sales.radio1$lag_visits3)] <- NA

agg.sales.radio1$sales_per_visit.lag1 <- log(agg.sales.radio1$lag_visits1)
agg.sales.radio1$sales_per_visit.lag2 <- log(agg.sales.radio1$lag_visits2)
agg.sales.radio1$sales_per_visit.lag3 <- log(agg.sales.radio1$lag_visits3)




agg.sales.tv1$`10secondi.adstock.log` <- log(agg.sales.tv1$`10secondi.adstock`)
agg.sales.tv1$`15secondi.adstock.log` <- log(agg.sales.tv1$`15secondi.adstock`)
agg.sales.tv1$`20secondi.adstock.log` <- log(agg.sales.tv1$`20secondi.adstock`)
agg.sales.tv1$`30secondi.adstock.log` <- log(agg.sales.tv1$`30secondi.adstock`)
agg.sales.tv1$`45secondi.adstock.log` <- log(agg.sales.tv1$`45secondi.adstock`)

agg.sales.tv1$`10secondi.adstock.log`[is.infinite(agg.sales.tv1$`10secondi.adstock.log`)] <- 0
agg.sales.tv1$`15secondi.adstock.log`[is.infinite(agg.sales.tv1$`15secondi.adstock.log`)] <- 0
agg.sales.tv1$`20secondi.adstock.log`[is.infinite(agg.sales.tv1$`20secondi.adstock.log`)] <- 0
agg.sales.tv1$`30secondi.adstock.log`[is.infinite(agg.sales.tv1$`30secondi.adstock.log`)] <- 0
agg.sales.tv1$`45secondi.adstock.log`[is.infinite(agg.sales.tv1$`45secondi.adstock.log`)] <- 0


agg.sales.tv1$lag_visits1 <- log(agg.sales.tv1$lag_visits1)
agg.sales.tv1$lag_visits2 <- log(agg.sales.tv1$lag_visits2)
agg.sales.tv1$lag_visits3 <- log(agg.sales.tv1$lag_visits3)

agg.sales.tv1$lag_visits1[is.infinite(agg.sales.tv1$lag_visits1)] <- NA
agg.sales.tv1$lag_visits2[is.infinite(agg.sales.tv1$lag_visits2)] <- NA
agg.sales.tv1$lag_visits3[is.infinite(agg.sales.tv1$lag_visits3)] <- NA

agg.sales.tv1$sales_per_visit.lag1 <- log(agg.sales.tv1$lag_visits1)
agg.sales.tv1$sales_per_visit.lag2 <- log(agg.sales.tv1$lag_visits2)
agg.sales.tv1$sales_per_visit.lag3 <- log(agg.sales.tv1$lag_visits3)





#grouping issue times into time_of_day_buckets
campaign.tv.final$ISSUE_TIME <- as.numeric(gsub("\\D+","",substr(campaign.tv.final$ISSUE_TIME,0,2)))
campaign.tv.final$timeofday <- ifelse(campaign.tv.final$ISSUE_TIME > 5 & campaign.tv.final$ISSUE_TIME < 10,
                                      "morning",
                                      ifelse(campaign.tv.final$ISSUE_TIME >= 10 & campaign.tv.final$ISSUE_TIME < 17,
                                             "daytime",
                                             ifelse(campaign.tv.final$ISSUE_TIME >= 17 & campaign.tv.final$ISSUE_TIME < 22,
                                                    "primetime",
                                                    "night")))

tv.time.data.prep <- campaign.tv.final[c(1,4:6)]

tv.time.data <- dcast(tv.time.data.prep, Date_out_start ~ FORMAT + timeofday, value.var = "NET_COST", fun.aggregate = sum)
tv.time.data2 <- tv.time.data[c(1:5,7:10,12:15,17:20,22:25)]

tv.time.data2$`10 secondi_daytime.sq`   <- tv.time.data2$`10 secondi_daytime`^2
tv.time.data2$`10 secondi_morning.sq`   <- tv.time.data2$`10 secondi_morning`^2
tv.time.data2$`10 secondi_night.sq`     <- tv.time.data2$`10 secondi_night`^2
tv.time.data2$`10 secondi_primetime.sq` <- tv.time.data2$`10 secondi_primetime`^2

tv.time.data2$`15 secondi_daytime.sq`   <- tv.time.data2$`15 secondi_daytime`^2
tv.time.data2$`15 secondi_morning.sq`   <- tv.time.data2$`15 secondi_morning`^2
tv.time.data2$`15 secondi_night.sq`     <- tv.time.data2$`15 secondi_night`^2
tv.time.data2$`15 secondi_primetime.sq` <- tv.time.data2$`15 secondi_primetime`^2

tv.time.data2$`20 secondi_daytime.sq`   <- tv.time.data2$`20 secondi_daytime`^2
tv.time.data2$`20 secondi_morning.sq`   <- tv.time.data2$`20 secondi_morning`^2
tv.time.data2$`20 secondi_night.sq`     <- tv.time.data2$`20 secondi_night`^2
tv.time.data2$`20 secondi_primetime.sq` <- tv.time.data2$`20 secondi_primetime`^2

tv.time.data2$`30 secondi_daytime.sq`   <- tv.time.data2$`30 secondi_daytime`^2
tv.time.data2$`30 secondi_morning.sq`   <- tv.time.data2$`30 secondi_morning`^2
tv.time.data2$`30 secondi_night.sq`     <- tv.time.data2$`30 secondi_night`^2
tv.time.data2$`30 secondi_primetime.sq` <- tv.time.data2$`30 secondi_primetime`^2

tv.time.data2$`45 secondi_daytime.sq`   <- tv.time.data2$`45 secondi_daytime`^2
tv.time.data2$`45 secondi_morning.sq`   <- tv.time.data2$`45 secondi_morning`^2
tv.time.data2$`45 secondi_night.sq`     <- tv.time.data2$`45 secondi_night`^2
tv.time.data2$`45 secondi_primetime.sq` <- tv.time.data2$`45 secondi_primetime`^2

agg.sales$trend <- 1:nrow(agg.sales)

tv.time.data.final <- merge(agg.sales, tv.time.data2, 
                            by.x = "BUSINESS_DATE", by.y = "Date_out_start")

tv.time.data.final$sales_per_visit <- tv.time.data.final$sales / tv.time.data.final$visits


#ADD LAG FOR EACH TIME AND SECOND
#10 SECONDI
tv.time.data.final$`10 secondi_daytime.lag1` <- c(NA, head(tv.time.data.final$`10 secondi_daytime`, -1))
tv.time.data.final$`10 secondi_daytime.lag2` <- c(NA, NA, head(tv.time.data.final$`10 secondi_daytime`, -2))
tv.time.data.final$`10 secondi_daytime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`15 secondi_daytime`, -3))

tv.time.data.final$`10 secondi_morning.lag1` <- c(NA, head(tv.time.data.final$`10 secondi_morning`, -1))
tv.time.data.final$`10 secondi_morning.lag2` <- c(NA, NA, head(tv.time.data.final$`10 secondi_morning`, -2))
tv.time.data.final$`10 secondi_morning.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`10 secondi_morning`, -3))

tv.time.data.final$`10 secondi_night.lag1` <- c(NA, head(tv.time.data.final$`10 secondi_night`, -1))
tv.time.data.final$`10 secondi_night.lag2` <- c(NA, NA, head(tv.time.data.final$`10 secondi_night`, -2))
tv.time.data.final$`10 secondi_night.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`10 secondi_night`, -3))

tv.time.data.final$`10 secondi_primetime.lag1` <- c(NA, head(tv.time.data.final$`10 secondi_primetime`, -1))
tv.time.data.final$`10 secondi_primetime.lag2` <- c(NA, NA, head(tv.time.data.final$`10 secondi_primetime`, -2))
tv.time.data.final$`10 secondi_primetime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`10 secondi_primetime`, -3))

#15 SECONDI
tv.time.data.final$`15 secondi_daytime.lag1` <- c(NA, head(tv.time.data.final$`15 secondi_daytime`, -1))
tv.time.data.final$`15 secondi_daytime.lag2` <- c(NA, NA, head(tv.time.data.final$`15 secondi_daytime`, -2))
tv.time.data.final$`15 secondi_daytime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`15 secondi_daytime`, -3))

tv.time.data.final$`15 secondi_morning.lag1` <- c(NA, head(tv.time.data.final$`15 secondi_morning`, -1))
tv.time.data.final$`15 secondi_morning.lag2` <- c(NA, NA, head(tv.time.data.final$`15 secondi_morning`, -2))
tv.time.data.final$`15 secondi_morning.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`15 secondi_morning`, -3))

tv.time.data.final$`15 secondi_night.lag1` <- c(NA, head(tv.time.data.final$`15 secondi_night`, -1))
tv.time.data.final$`15 secondi_night.lag2` <- c(NA, NA, head(tv.time.data.final$`15 secondi_night`, -2))
tv.time.data.final$`15 secondi_night.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`15 secondi_night`, -3))

tv.time.data.final$`15 secondi_primetime.lag1` <- c(NA, head(tv.time.data.final$`15 secondi_primetime`, -1))
tv.time.data.final$`15 secondi_primetime.lag2` <- c(NA, NA, head(tv.time.data.final$`15 secondi_primetime`, -2))
tv.time.data.final$`15 secondi_primetime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`15 secondi_primetime`, -3))

#20 SECONDI
tv.time.data.final$`20 secondi_daytime.lag1` <- c(NA, head(tv.time.data.final$`20 secondi_daytime`, -1))
tv.time.data.final$`20 secondi_daytime.lag2` <- c(NA, NA, head(tv.time.data.final$`20 secondi_daytime`, -2))
tv.time.data.final$`20 secondi_daytime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`20 secondi_daytime`, -3))

tv.time.data.final$`20 secondi_morning.lag1` <- c(NA, head(tv.time.data.final$`20 secondi_morning`, -1))
tv.time.data.final$`20 secondi_morning.lag2` <- c(NA, NA, head(tv.time.data.final$`20 secondi_morning`, -2))
tv.time.data.final$`20 secondi_morning.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`20 secondi_morning`, -3))

tv.time.data.final$`20 secondi_night.lag1` <- c(NA, head(tv.time.data.final$`20 secondi_night`, -1))

tv.time.data.final$`20 secondi_night.lag2` <- c(NA, NA, head(tv.time.data.final$`20 secondi_night`, -2))
tv.time.data.final$`20 secondi_night.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`20 secondi_night`, -3))

tv.time.data.final$`20 secondi_primetime.lag1` <- c(NA, head(tv.time.data.final$`20 secondi_primetime`, -1))
tv.time.data.final$`20 secondi_primetime.lag2` <- c(NA, NA, head(tv.time.data.final$`20 secondi_primetime`, -2))
tv.time.data.final$`20 secondi_primetime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`20 secondi_primetime`, -3))

#30 SECONDI
tv.time.data.final$`30 secondi_daytime.lag1` <- c(NA, head(tv.time.data.final$`30 secondi_daytime`, -1))
tv.time.data.final$`30 secondi_daytime.lag2` <- c(NA, NA, head(tv.time.data.final$`30 secondi_daytime`, -2))
tv.time.data.final$`30 secondi_daytime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`30 secondi_daytime`, -3))

tv.time.data.final$`30 secondi_morning.lag1` <- c(NA, head(tv.time.data.final$`30 secondi_morning`, -1))
tv.time.data.final$`30 secondi_morning.lag2` <- c(NA, NA, head(tv.time.data.final$`30 secondi_morning`, -2))
tv.time.data.final$`30 secondi_morning.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`30 secondi_morning`, -3))

tv.time.data.final$`30 secondi_night.lag1` <- c(NA, head(tv.time.data.final$`30 secondi_night`, -1))
tv.time.data.final$`30 secondi_night.lag2` <- c(NA, NA, head(tv.time.data.final$`30 secondi_night`, -2))
tv.time.data.final$`30 secondi_night.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`30 secondi_night`, -3))

tv.time.data.final$`30 secondi_primetime.lag1` <- c(NA, head(tv.time.data.final$`30 secondi_primetime`, -1))
tv.time.data.final$`30 secondi_primetime.lag2` <- c(NA, NA, head(tv.time.data.final$`30 secondi_primetime`, -2))
tv.time.data.final$`30 secondi_primetime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`30 secondi_primetime`, -3))

#45 SECONDI
tv.time.data.final$`45 secondi_daytime.lag1` <- c(NA, head(tv.time.data.final$`45 secondi_daytime`, -1))
tv.time.data.final$`45 secondi_daytime.lag2` <- c(NA, NA, head(tv.time.data.final$`45 secondi_daytime`, -2))
tv.time.data.final$`45 secondi_daytime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`45 secondi_daytime`, -3))

tv.time.data.final$`45 secondi_morning.lag1` <- c(NA, head(tv.time.data.final$`45 secondi_morning`, -1))
tv.time.data.final$`45 secondi_morning.lag2` <- c(NA, NA, head(tv.time.data.final$`45 secondi_morning`, -2))
tv.time.data.final$`45 secondi_morning.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`45 secondi_morning`, -3))

tv.time.data.final$`45 secondi_night.lag1` <- c(NA, head(tv.time.data.final$`45 secondi_night`, -1))
tv.time.data.final$`45 secondi_night.lag2` <- c(NA, NA, head(tv.time.data.final$`45 secondi_night`, -2))
tv.time.data.final$`45 secondi_night.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`45 secondi_night`, -3))

tv.time.data.final$`45 secondi_primetime.lag1` <- c(NA, head(tv.time.data.final$`45 secondi_primetime`, -1))
tv.time.data.final$`45 secondi_primetime.lag2` <- c(NA, NA, head(tv.time.data.final$`45 secondi_primetime`, -2))
tv.time.data.final$`45 secondi_primetime.lag3` <- c(NA, NA, NA, head(tv.time.data.final$`45 secondi_primetime`, -3))

#SALES PER VISIT LAG
tv.time.data.final$sales_per_visit.lag1 <- c(NA, head(tv.time.data.final$sales_per_visit, -1))
tv.time.data.final$sales_per_visit.lag2 <- c(NA, NA, head(tv.time.data.final$sales_per_visit, -2))
tv.time.data.final$sales_per_visit.lag3 <- c(NA, NA, NA, head(tv.time.data.final$sales_per_visit, -3))

#TV with times adstock 
tv.time.data.final$`10secondi_daytime.adstock`   <- numeric(length(tv.time.data.final$`10 secondi_daytime`))
tv.time.data.final$`10secondi_morning.adstock`   <- numeric(length(tv.time.data.final$`10 secondi_morning`))
tv.time.data.final$`10secondi_night.adstock`     <- numeric(length(tv.time.data.final$`10 secondi_night`))
tv.time.data.final$`10secondi_primetime.adstock` <- numeric(length(tv.time.data.final$`10 secondi_primetime`))

tv.time.data.final$`15secondi_daytime.adstock`   <- numeric(length(tv.time.data.final$`15 secondi_daytime`))
tv.time.data.final$`15secondi_morning.adstock`   <- numeric(length(tv.time.data.final$`15 secondi_morning`))
tv.time.data.final$`15secondi_night.adstock`     <- numeric(length(tv.time.data.final$`15 secondi_night`))
tv.time.data.final$`15secondi_primetime.adstock` <- numeric(length(tv.time.data.final$`15 secondi_primetime`))

tv.time.data.final$`20secondi_daytime.adstock`   <- numeric(length(tv.time.data.final$`20 secondi_daytime`))
tv.time.data.final$`20secondi_morning.adstock`   <- numeric(length(tv.time.data.final$`20 secondi_morning`))
tv.time.data.final$`20secondi_night.adstock`     <- numeric(length(tv.time.data.final$`20 secondi_night`))
tv.time.data.final$`20secondi_primetime.adstock` <- numeric(length(tv.time.data.final$`20 secondi_primetime`))

tv.time.data.final$`30secondi_daytime.adstock`   <- numeric(length(tv.time.data.final$`30 secondi_daytime`))
tv.time.data.final$`30secondi_morning.adstock`   <- numeric(length(tv.time.data.final$`30 secondi_morning`))
tv.time.data.final$`30secondi_night.adstock`     <- numeric(length(tv.time.data.final$`30 secondi_night`))
tv.time.data.final$`30secondi_primetime.adstock` <- numeric(length(tv.time.data.final$`30 secondi_primetime`))

tv.time.data.final$`45secondi_daytime.adstock`   <- numeric(length(tv.time.data.final$`45 secondi_daytime`))
tv.time.data.final$`45secondi_morning.adstock`   <- numeric(length(tv.time.data.final$`45 secondi_morning`))
tv.time.data.final$`45secondi_night.adstock`     <- numeric(length(tv.time.data.final$`45 secondi_night`))
tv.time.data.final$`45secondi_primetime.adstock` <- numeric(length(tv.time.data.final$`45 secondi_primetime`))


tv.time.data.final$`10secondi_daytime.adstock`[1]   <- tv.time.data.final$`10 secondi_daytime`[1]
tv.time.data.final$`10secondi_morning.adstock`[1]   <- tv.time.data.final$`10 secondi_morning`[1]
tv.time.data.final$`10secondi_night.adstock`[1]     <- tv.time.data.final$`10 secondi_night`[1]
tv.time.data.final$`10secondi_primetime.adstock`[1] <- tv.time.data.final$`10 secondi_primetime`[1]

tv.time.data.final$`15secondi_daytime.adstock`[1] <- tv.time.data.final$`15 secondi_daytime`[1]
tv.time.data.final$`15secondi_morning.adstock`[1] <- tv.time.data.final$`15 secondi_morning`[1]
tv.time.data.final$`15secondi_night.adstock`[1] <- tv.time.data.final$`15 secondi_night`[1]
tv.time.data.final$`15secondi_primetime.adstock`[1] <- tv.time.data.final$`15 secondi_primetime`[1]

tv.time.data.final$`20secondi_daytime.adstock`[1] <- tv.time.data.final$`20 secondi_daytime`[1]
tv.time.data.final$`20secondi_morning.adstock`[1] <- tv.time.data.final$`20 secondi_morning`[1]
tv.time.data.final$`20secondi_night.adstock`[1] <- tv.time.data.final$`20 secondi_night`[1]
tv.time.data.final$`20secondi_primetime.adstock`[1] <- tv.time.data.final$`20 secondi_primetime`[1]

tv.time.data.final$`30secondi_daytime.adstock`[1] <- tv.time.data.final$`30 secondi_daytime`[1]
tv.time.data.final$`30secondi_morning.adstock`[1] <- tv.time.data.final$`30 secondi_morning`[1]
tv.time.data.final$`30secondi_night.adstock`[1] <- tv.time.data.final$`30 secondi_night`[1]
tv.time.data.final$`30secondi_primetime.adstock`[1] <- tv.time.data.final$`30 secondi_primetime`[1]

tv.time.data.final$`45secondi_daytime.adstock`[1] <- tv.time.data.final$`45 secondi_daytime`[1]
tv.time.data.final$`45secondi_morning.adstock`[1] <- tv.time.data.final$`45 secondi_morning`[1]
tv.time.data.final$`45secondi_night.adstock`[1] <- tv.time.data.final$`45 secondi_night`[1]
tv.time.data.final$`45secondi_primetime.adstock`[1] <- tv.time.data.final$`45 secondi_primetime`[1]


for (i in 2:length(tv.time.data.final$`10secondi_daytime.adstock`)){
  tv.time.data.final$`10secondi_daytime.adstock`[i] = tv.time.data.final$`10 secondi_daytime`[i] + 
                                   adstock.alpha.tv * tv.time.data.final$`10secondi_daytime.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`10secondi_morning.adstock`)){
  tv.time.data.final$`10secondi_morning.adstock`[i] = tv.time.data.final$`10 secondi_morning`[i] + adstock.alpha.tv * tv.time.data.final$`10secondi_morning.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`10secondi_night.adstock`)){
  tv.time.data.final$`10secondi_night.adstock`[i] = tv.time.data.final$`10 secondi_night`[i] + adstock.alpha.tv * tv.time.data.final$`10secondi_night.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`10secondi_primetime.adstock`)){
  tv.time.data.final$`10secondi_primetime.adstock`[i] = tv.time.data.final$`10 secondi_primetime`[i] + adstock.alpha.tv * tv.time.data.final$`10secondi_primetime.adstock`[i-1]
}



for (i in 2:length(tv.time.data.final$`15secondi_daytime.adstock`)){
  tv.time.data.final$`15secondi_daytime.adstock`[i] = tv.time.data.final$`15 secondi_daytime`[i] + adstock.alpha.tv * tv.time.data.final$`15secondi_daytime.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`15secondi_morning.adstock`)){
  tv.time.data.final$`15secondi_morning.adstock`[i] = tv.time.data.final$`15 secondi_morning`[i] + adstock.alpha.tv * tv.time.data.final$`15secondi_morning.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`15secondi_night.adstock`)){
  tv.time.data.final$`15secondi_night.adstock`[i] = tv.time.data.final$`15 secondi_night`[i] + adstock.alpha.tv * tv.time.data.final$`15secondi_night.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`15secondi_primetime.adstock`)){
  tv.time.data.final$`15secondi_primetime.adstock`[i] = tv.time.data.final$`15 secondi_primetime`[i] + adstock.alpha.tv * tv.time.data.final$`15secondi_primetime.adstock`[i-1]
}




for (i in 2:length(tv.time.data.final$`20secondi_daytime.adstock`)){
  tv.time.data.final$`20secondi_daytime.adstock`[i] = tv.time.data.final$`20 secondi_daytime`[i] + adstock.alpha.tv * tv.time.data.final$`20secondi_daytime.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`20secondi_morning.adstock`)){
  tv.time.data.final$`20secondi_morning.adstock`[i] = tv.time.data.final$`20 secondi_morning`[i] + adstock.alpha.tv * tv.time.data.final$`20secondi_morning.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`20secondi_night.adstock`)){
  tv.time.data.final$`20secondi_night.adstock`[i] = tv.time.data.final$`20 secondi_night`[i] + adstock.alpha.tv * tv.time.data.final$`20secondi_night.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`20secondi_primetime.adstock`)){
  tv.time.data.final$`20secondi_primetime.adstock`[i] = tv.time.data.final$`20 secondi_primetime`[i] + adstock.alpha.tv * tv.time.data.final$`20secondi_primetime.adstock`[i-1]
}



for (i in 2:length(tv.time.data.final$`30secondi_daytime.adstock`)){
  tv.time.data.final$`30secondi_daytime.adstock`[i] = tv.time.data.final$`30 secondi_daytime`[i] + adstock.alpha.tv * tv.time.data.final$`30secondi_daytime.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`30secondi_morning.adstock`)){
  tv.time.data.final$`30secondi_morning.adstock`[i] = tv.time.data.final$`30 secondi_morning`[i] + adstock.alpha.tv * tv.time.data.final$`30secondi_morning.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`30secondi_night.adstock`)){
  tv.time.data.final$`30secondi_night.adstock`[i] = tv.time.data.final$`30 secondi_night`[i] + adstock.alpha.tv * tv.time.data.final$`30secondi_night.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`30secondi_primetime.adstock`)){
  tv.time.data.final$`30secondi_primetime.adstock`[i] = tv.time.data.final$`30 secondi_primetime`[i] + adstock.alpha.tv * tv.time.data.final$`30secondi_primetime.adstock`[i-1]
}



for (i in 2:length(tv.time.data.final$`45secondi_daytime.adstock`)){
  tv.time.data.final$`45secondi_daytime.adstock`[i] = tv.time.data.final$`45 secondi_daytime`[i] + adstock.alpha.tv * tv.time.data.final$`45secondi_daytime.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`45secondi_morning.adstock`)){
  tv.time.data.final$`45secondi_morning.adstock`[i] = tv.time.data.final$`45 secondi_morning`[i] + adstock.alpha.tv * tv.time.data.final$`45secondi_morning.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`45secondi_night.adstock`)){
  tv.time.data.final$`45secondi_night.adstock`[i] = tv.time.data.final$`45 secondi_night`[i] + adstock.alpha.tv * tv.time.data.final$`45secondi_night.adstock`[i-1]
}
for (i in 2:length(tv.time.data.final$`45secondi_primetime.adstock`)){
  tv.time.data.final$`45secondi_primetime.adstock`[i] = tv.time.data.final$`45 secondi_primetime`[i] + adstock.alpha.tv * tv.time.data.final$`45secondi_primetime.adstock`[i-1]
}


tv.time.data.final$`10secondi_daytime.adstock.log` <- log(tv.time.data.final$`10secondi_daytime.adstock`)
tv.time.data.final$`10secondi_morning.adstock.log` <- log(tv.time.data.final$`10secondi_morning.adstock`)
tv.time.data.final$`10secondi_night.adstock.log` <- log(tv.time.data.final$`10secondi_night.adstock`)
tv.time.data.final$`10secondi_primetime.adstock.log` <- log(tv.time.data.final$`10secondi_primetime.adstock`)

tv.time.data.final$`15secondi_daytime.adstock.log` <- log(tv.time.data.final$`15secondi_daytime.adstock`)
tv.time.data.final$`15secondi_morning.adstock.log` <- log(tv.time.data.final$`15secondi_morning.adstock`)
tv.time.data.final$`15secondi_night.adstock.log` <- log(tv.time.data.final$`15secondi_night.adstock`)
tv.time.data.final$`15secondi_primetime.adstock.log` <- log(tv.time.data.final$`15secondi_primetime.adstock`)

tv.time.data.final$`20secondi_daytime.adstock.log` <- log(tv.time.data.final$`20secondi_daytime.adstock`)
tv.time.data.final$`20secondi_morning.adstock.log` <- log(tv.time.data.final$`20secondi_morning.adstock`)
tv.time.data.final$`20secondi_night.adstock.log` <- log(tv.time.data.final$`20secondi_night.adstock`)
tv.time.data.final$`20secondi_primetime.adstock.log` <- log(tv.time.data.final$`20secondi_primetime.adstock`)

tv.time.data.final$`30secondi_daytime.adstock.log` <- log(tv.time.data.final$`30secondi_daytime.adstock`)
tv.time.data.final$`30secondi_morning.adstock.log` <- log(tv.time.data.final$`30secondi_morning.adstock`)
tv.time.data.final$`30secondi_night.adstock.log` <- log(tv.time.data.final$`30secondi_night.adstock`)
tv.time.data.final$`30secondi_primetime.adstock.log` <- log(tv.time.data.final$`30secondi_primetime.adstock`)

tv.time.data.final$`45secondi_daytime.adstock.log` <- log(tv.time.data.final$`45secondi_daytime.adstock`)
tv.time.data.final$`45secondi_morning.adstock.log` <- log(tv.time.data.final$`45secondi_morning.adstock`)
tv.time.data.final$`45secondi_night.adstock.log` <- log(tv.time.data.final$`45secondi_night.adstock`)
tv.time.data.final$`45secondi_primetime.adstock.log` <- log(tv.time.data.final$`45secondi_primetime.adstock`)


tv.time.data.final$`10secondi_daytime.adstock.log`[is.infinite(tv.time.data.final$`10secondi_daytime.adstock.log`)] <- 0
tv.time.data.final$`10secondi_morning.adstock.log`[is.infinite(tv.time.data.final$`10secondi_morning.adstock.log`)] <- 0
tv.time.data.final$`10secondi_night.adstock.log`[is.infinite(tv.time.data.final$`10secondi_night.adstock.log`)] <- 0
tv.time.data.final$`10secondi_primetime.adstock.log`[is.infinite(tv.time.data.final$`10secondi_primetime.adstock.log`)] <- 0

tv.time.data.final$`15secondi_daytime.adstock.log`[is.infinite(tv.time.data.final$`15secondi_daytime.adstock.log`)] <- 0
tv.time.data.final$`15secondi_morning.adstock.log`[is.infinite(tv.time.data.final$`15secondi_morning.adstock.log`)] <- 0
tv.time.data.final$`15secondi_night.adstock.log`[is.infinite(tv.time.data.final$`15secondi_night.adstock.log`)] <- 0
tv.time.data.final$`15secondi_primetime.adstock.log`[is.infinite(tv.time.data.final$`15secondi_primetime.adstock.log`)] <- 0

tv.time.data.final$`20secondi_daytime.adstock.log`[is.infinite(tv.time.data.final$`20secondi_daytime.adstock.log`)] <- 0
tv.time.data.final$`20secondi_morning.adstock.log`[is.infinite(tv.time.data.final$`20secondi_morning.adstock.log`)] <- 0
tv.time.data.final$`20secondi_night.adstock.log`[is.infinite(tv.time.data.final$`20secondi_night.adstock.log`)] <- 0
tv.time.data.final$`20secondi_primetime.adstock.log`[is.infinite(tv.time.data.final$`20secondi_primetime.adstock.log`)] <- 0

tv.time.data.final$`30secondi_daytime.adstock.log`[is.infinite(tv.time.data.final$`30secondi_daytime.adstock.log`)] <- 0
tv.time.data.final$`30secondi_morning.adstock.log`[is.infinite(tv.time.data.final$`30secondi_morning.adstock.log`)] <- 0
tv.time.data.final$`30secondi_night.adstock.log`[is.infinite(tv.time.data.final$`30secondi_night.adstock.log`)] <- 0
tv.time.data.final$`30secondi_primetime.adstock.log`[is.infinite(tv.time.data.final$`30secondi_primetime.adstock.log`)] <- 0

tv.time.data.final$`45secondi_daytime.adstock.log`[is.infinite(tv.time.data.final$`45secondi_daytime.adstock.log`)] <- 0
tv.time.data.final$`45secondi_morning.adstock.log`[is.infinite(tv.time.data.final$`45secondi_morning.adstock.log`)] <- 0
tv.time.data.final$`45secondi_night.adstock.log`[is.infinite(tv.time.data.final$`45secondi_night.adstock.log`)] <- 0
tv.time.data.final$`45secondi_primetime.adstock.log`[is.infinite(tv.time.data.final$`45secondi_primetime.adstock.log`)] <- 0


tv.time.data.final <- merge(tv.time.data.final, agg.sales.all.media, by="BUSINESS_DATE")

##NEW MODELS
#TV
tv.sales.model.1 <- lm(log(sales) ~ weekday + month + trend + 
                          `10secondi.adstock.log` + `15secondi.adstock.log` + 
                          `20secondi.adstock.log` + `30secondi.adstock.log` + `45secondi.adstock.log` + log(visits) +
                          agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                          agg.sales.all.media$special.adstock.log + agg.sales.all.media$radio.adstock.log, 
                          data = agg.sales.tv1)
summary(tv.sales.model.1)


tv.visits.model.2 <- lm(log(visits) ~ weekday + month + trend + 
                          `10secondi.adstock.log` + `15secondi.adstock.log` + 
                          `20secondi.adstock.log` + `30secondi.adstock.log` + `45secondi.adstock.log` +
                          agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                          agg.sales.all.media$special.adstock.log + agg.sales.all.media$radio.adstock.log, 
                        data = agg.sales.tv1)
summary(tv.visits.model.2)


tv.sales.visits.model.3 <- lm(log(sales_per_visit) ~ weekday + month + trend + 
                          `10secondi.adstock.log` + `15secondi.adstock.log` + 
                          `20secondi.adstock.log` + `30secondi.adstock.log` + `45secondi.adstock.log` +
                          agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                          agg.sales.all.media$special.adstock.log + agg.sales.all.media$radio.adstock.log, 
                        data = agg.sales.tv1)
summary(tv.sales.visits.model.3)


#TV with times
tv.times.sales.model.1 <- lm(log(sales.x) ~ weekday.x + month.x + trend.x + 
                         `10secondi_daytime.adstock.log` + `10secondi_morning.adstock.log` + `10secondi_night.adstock.log` +
                           `10secondi_primetime.adstock.log` + `15secondi_daytime.adstock.log` + `15secondi_morning.adstock.log` + `15secondi_night.adstock.log` +
                           `15secondi_primetime.adstock.log` +`20secondi_daytime.adstock.log` + `20secondi_morning.adstock.log` + `20secondi_night.adstock.log` +
                           `20secondi_primetime.adstock.log` +`30secondi_daytime.adstock.log` + `30secondi_morning.adstock.log` + `30secondi_night.adstock.log` +
                           `30secondi_primetime.adstock.log` + `45secondi_daytime.adstock.log` + `45secondi_morning.adstock.log` + `45secondi_night.adstock.log` +
                           `45secondi_primetime.adstock.log` + log(visits.x) +
                         print.adstock.log + internet.adstock.log +
                         special.adstock.log + radio.adstock.log, 
                       data = tv.time.data.final)
summary(tv.times.sales.model.1)


tv.times.visits.model.2 <- lm(log(visits.x) ~ weekday.x + month.x + trend.x + 
                               `10secondi_daytime.adstock.log` + `10secondi_morning.adstock.log` + `10secondi_night.adstock.log` +
                               `10secondi_primetime.adstock.log` + `15secondi_daytime.adstock.log` + `15secondi_morning.adstock.log` + `15secondi_night.adstock.log` +
                               `15secondi_primetime.adstock.log` +`20secondi_daytime.adstock.log` + `20secondi_morning.adstock.log` + `20secondi_night.adstock.log` +
                               `20secondi_primetime.adstock.log` +`30secondi_daytime.adstock.log` + `30secondi_morning.adstock.log` + `30secondi_night.adstock.log` +
                               `30secondi_primetime.adstock.log` + `45secondi_daytime.adstock.log` + `45secondi_morning.adstock.log` + `45secondi_night.adstock.log` +
                               `45secondi_primetime.adstock.log` +
                               print.adstock.log + internet.adstock.log +
                               special.adstock.log + radio.adstock.log, 
                             data = tv.time.data.final)
summary(tv.times.visits.model.2)


tv.times.sales.visits.model.3 <- lm(log(sales_per_visit) ~ weekday.x + month.x + trend.x + 
                                `10secondi_daytime.adstock.log` + `10secondi_morning.adstock.log` + `10secondi_night.adstock.log` +
                                `10secondi_primetime.adstock.log` + `15secondi_daytime.adstock.log` + `15secondi_morning.adstock.log` + `15secondi_night.adstock.log` +
                                `15secondi_primetime.adstock.log` +`20secondi_daytime.adstock.log` + `20secondi_morning.adstock.log` + `20secondi_night.adstock.log` +
                                `20secondi_primetime.adstock.log` +`30secondi_daytime.adstock.log` + `30secondi_morning.adstock.log` + `30secondi_night.adstock.log` +
                                `30secondi_primetime.adstock.log` + `45secondi_daytime.adstock.log` + `45secondi_morning.adstock.log` + `45secondi_night.adstock.log` +
                                `45secondi_primetime.adstock.log` +
                                print.adstock.log + internet.adstock.log +
                                special.adstock.log + radio.adstock.log, 
                              data = tv.time.data.final)
summary(tv.times.sales.visits.model.3)



#RADIO
radio.sales.model.1 <- lm(log(sales) ~ weekday + month + trend + 
                          `15secondi.adstock.log` + `20secondi.adstock.log` + 
                          `30secondi.adstock.log` + log(visits) + 
                            agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                            agg.sales.all.media$special.adstock.log + agg.sales.all.media$tv.adstock.log, 
                        data = agg.sales.radio1)
summary(radio.sales.model.1)


radio.visits.model.2 <- lm(log(visits) ~ weekday + month + trend + 
                          `15secondi.adstock.log` + `20secondi.adstock.log` + 
                          `30secondi.adstock.log` + agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                            agg.sales.all.media$special.adstock.log + agg.sales.all.media$tv.adstock.log , 
                        data = agg.sales.radio1)
summary(radio.visits.model.2)


radio.sales.visits.model.3 <- lm(log(sales_per_visit) ~ weekday + month + trend + 
                          `15secondi.adstock.log` + `20secondi.adstock.log` + 
                          `30secondi.adstock.log` + agg.sales.all.media$print.adstock.log + agg.sales.all.media$internet.adstock.log +
                            agg.sales.all.media$special.adstock.log + agg.sales.all.media$tv.adstock.log, 
                        data = agg.sales.radio1)
summary(radio.sales.visits.model.3)

