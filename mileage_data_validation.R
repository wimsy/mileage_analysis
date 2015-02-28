# mileage_data_validation.R

library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)

# Load trip data

tripsfile = 'data//raw_trips.csv'
trips <- read.csv(tripsfile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
trips$Start.Time <- ymd_hm(trips$Start.Time, tz = tz)
trips$End.Time <- ymd_hm(trips$End.Time, tz = tz)
trips <- trips[order(trips$Start.Time), ]

# Load mileage data

mileagefile = 'data//mileage.csv'
mileage <- read.csv(mileagefile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
mileage$Timestamp <- mdy_hms(mileage$Timestamp, tz = tz)
mileage <- mileage[order(mileage$Timestamp), ]
mileage$diff.miles[2:nrow(mileage)] <- diff(mileage$Miles)

# Look at trips

tdf <- trips %>%
    select(Start.Time, 
           Distance..mi., 
           Fuel.Cost..USD., 
           Fuel.Volume..gal.) %>%
    group_by(month=paste(as.character(year(Start.Time)), 
                         sprintf('%02d', month(Start.Time)), 
                         sep='-')) %>%
    summarize(miles=sum(Distance..mi.), 
              cost=sum(Fuel.Cost..USD.), 
              gallons=sum(Fuel.Volume..gal.), 
              mpg=miles/gallons, 
              cpg=cost/gallons, 
              cpm=cost/miles) %>% 
    mutate(datasource='Automatic')

mdf <- mileage %>%
    group_by(month=paste(as.character(year(Timestamp)), 
                         sprintf('%02d', month(Timestamp)), 
                         sep='-')) %>%
    summarize(miles=sum(diff.miles), 
              gallons=sum(Gallons), 
              cost=sum(Price), 
              mpg=miles/gallons, 
              cpg=cost/gallons, 
              cpm=cost/miles) %>%
    mutate(datasource='Mileage records')

# Monthly comparisons

compdf <- rbind(mdf, tdf) %>%
    arrange(month) %>%
    na.omit() %>%
    filter(month>'2014-08')

compdf %>%
    ggplot(aes(x=month, y=miles, group=datasource)) + 
    geom_line(aes(color=datasource)) + 
    scale_y_continuous(limits=c(0,2500))

compdf %>%
    ggplot(aes(x=month, y=gallons, group=datasource)) + 
    geom_line(aes(color=datasource)) + 
    scale_y_continuous(limits=c(0,100))

compdf %>%
    ggplot(aes(x=month, y=mpg, group=datasource)) + 
    geom_line(aes(color=datasource)) + 
    scale_y_continuous(limits=c(23, 29))

compdf %>%
    ggplot(aes(x=month, y=cpg, group=datasource)) + 
    geom_line(aes(color=datasource)) + 
    scale_y_continuous(limits=c(2.0, 4.0))

# Rolled up comparison

rollupdf <- compdf %>%
    group_by(datasource) %>%
    summarize(miles=sum(miles), 
              gallons=sum(gallons), 
              cost=sum(cost), 
              mpg=miles/gallons, 
              cpg=cost/gallons, 
              cpm=cost/miles) 

