holidays.data <- read.csv("national_holidays.csv")

# create column with full data
holidays.data$date_full <- paste(as.character(holidays.data$date), 
                                 as.character(holidays.data$year),
                                 sep = "-")

# convert new column to date
holidays.data$date_full2 <- as.character(as.Date(holidays.data$date_full, "%d-%b-%Y"))

# drop unnecessary columns
holidays.data <- holidays.data[, c(2,3,7)]

# save file
# write.csv(holidays.data, file = "national_holiday_final.csv")