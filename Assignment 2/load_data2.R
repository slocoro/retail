library(dplyr)
library(reshape2)
library(dummies)

# load data for  assignment 2
data.campaign <- read.csv("~/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail/Assignment 2/Chain_Campaign_Details.csv", fileEncoding = "latin1", stringsAsFactors=FALSE)
data.store <- read.csv("~/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail/Assignment 2/Chain_Store_Performance_2015_2016.csv", fileEncoding = "latin1", stringsAsFactors=FALSE)
data.googletrends <- read.csv("~/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail/Assignment 2/GoogleTrends.csv")
data.grp <- read.csv("~/Google Drive/Imperial College London/Term 3/Retail and Marketing Analytics/Team assignment/retail/Assignment 2/Chain_GRPS_2015_2016.csv")

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

# plot sales
plot(agg.sales$sales,
     type="o",  col="blue",
     ylab = "", xlab = "Time",
     main = "Daily Sales")

# decomposition of sales
sales.ts <- ts(agg.sales$sales, frequency = 365, start=c(2015,5,15))
plot(stl(sales.ts))

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
# write.csv(agg.sales.tv, file = "sales_tv_3.csv")
# write.csv(agg.sales.radio, file = "sales_radio_3.csv")

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
agg.sales.tv1$sales_per_visit <- agg.sales.tv1$sales / agg.sales.tv1$visits
agg.sales.radio1$sales_per_visit <- agg.sales.radio1$sales / agg.sales.radio1$visits

# things that would be nice to have 
# indication about what kind of audience the channel targets. this would allow to
# cluster channels by type (family, sports, film, ...)



