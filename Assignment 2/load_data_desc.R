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

# check which media has the biggest investment
agg.media <- data.campaign %>%
  group_by(media_grouped) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE))

# check which media is used most often
agg.media.count <- data.campaign %>%
  group_by(media_grouped) %>%
  summarize(n = n())

# add year variable from date
data.campaign$year <- format(as.Date(data.campaign$Date_out_start),"%Y")

# check which media is used most often by year, range of dates, and investment per year
agg.media.count.year <- data.campaign %>%
  group_by(media_grouped, year) %>%
  summarize(investment = sum(NET_COST, na.rm = TRUE),
            n = n(),
            date_from = min(as.Date(Date_out_start)),
            date_to = max(as.Date(Date_out_start)))
# Internet includes some ads that don't have a year, we drop them

# add average invesment value of campaign
agg.media.count.year$average.inv <- agg.media.count.year$investment / agg.media.count.year$n

# write.csv(agg.media.count.year, file = "data_desc.csv")


