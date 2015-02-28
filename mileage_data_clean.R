# Remove some location info and create a basic data set for publication

library(lubridate)

mileagefile = 'data//mileage.csv'
mileage <- read.csv(mileagefile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
mileage$Timestamp <- mdy_hms(mileage$Timestamp, tz = tz)
mileage <- mileage[order(mileage$Timestamp), ]

# Calculate some useful values

write.csv(mileage, file = 'output/mileage.csv')
