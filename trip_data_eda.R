# Initial analysis of @automatic data

library(lubridate)
library(ggplot2)

tripsfile = 'data//Volkswagen Jetta trips - Sheet 1.csv'
trips <- read.csv(tripsfile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
trips$Trip.Started.At <- mdy_hm(trips$Trip.Started.At, tz = tz)
trips$Trip.Ended.At <- mdy_hm(trips$Trip.Ended.At, tz = tz)
trips$Fuel.Cost.USD <- as.numeric(sub('\\$', '', trips$Fuel.Cost.USD))

# Calculate some useful values
trips$trip.minutes <- as.integer(trips$Trip.Ended.At - trips$Trip.Started.At)/60
# Zero-minute trips rounded up to one minute because Infinity
trips$avg.mph <- trips$Trip.Distance.Miles / max(1, trips$trip.minutes) * 60.0
trips$calc.gallons <- trips$Trip.Distance.Miles / trips$Average.MPG
trips$cost.per.gallon <- trips$Fuel.Cost.USD / trips$calc.gallons

# EDA
hist(trips$avg.mph)
tot.minutes <- sum(trips$trip.minutes)
tot.miles <- sum(trips$Trip.Distance.Miles)
tot.gallons <- sum(trips$calc.gallons)
tot.cost <- sum(trips$Fuel.Cost.USD)


daily.miles <- aggregate(trips$Trip.Distance.Miles, 
                         by = list(substr(trips$Trip.Started.At, 1, 10)), 
                         FUN = sum)

# Weekly data

week.list <- list(floor_date(as.Date(trips$Trip.Started.At, tz = tz), 'week'))
weekly.stats <- aggregate(trips$Trip.Distance.Miles, 
                          by = week.list, 
                          FUN = sum)
names(weekly.stats) <- c('week.num', 'miles')
weekly.stats$minutes <- aggregate(trips$trip.minutes, 
                                  by = week.list, 
                                  FUN = sum)[,2]
weekly.stats$gallons <- aggregate(trips$calc.gallons, 
                                  by = week.list, 
                                  FUN = sum)[,2]
weekly.stats$cost <- aggregate(trips$Fuel.Cost.USD, 
                               by = week.list, 
                               FUN = sum)[,2]
weekly.stats$count <- aggregate(Trip.Started.At ~ floor_date(Trip.Started.At, 
                                                             'week'), 
                                data = trips, 
                                FUN = length)[,2]

week.num <- data.frame(seq(min(week.list[[1]]), max(week.list[[1]]), 'weeks'))
names(week.num) <- c('week.num')
weekly.stats <- merge(week.num, 
                      weekly.stats,  
                      by = 'week.num',
                      all = TRUE)

