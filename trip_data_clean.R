# Remove some location info and create a basic data set for publication

library(lubridate)
library(dplyr)

tripsfile = 'data//raw_trips.csv'
trips <- read.csv(tripsfile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
trips$Start.Time <- ymd_hm(trips$Start.Time, tz = tz)
trips$End.Time <- ymd_hm(trips$End.Time, tz = tz)
trips <- trips[order(trips$Start.Time), ]
# trips$Fuel.Cost..USD. <- as.numeric(sub('\\$', '', trips$Fuel.Cost..USD.))

# Calculate some useful values
# trips$trip.minutes <- as.integer(trips$End.Time - trips$Start.Time)/60
# Zero-minute trips rounded up to one minute because Infinity
trips$avg.mph <- trips$Distance..mi. / trips$Duration..min. * 60.0
# trips$calc.gallons <- trips$Distance..mi. / trips$Average.MPG
trips$cost.per.gallon <- trips$Fuel.Cost..USD. / trips$Fuel.Volume..gal.

trips <- subset(trips, select = -c(Vehicle, 
                                   Duration.Over.70.mph..secs., 
                                   Duration.Over.75.mph..secs.,
                                   Duration.Over.80.mph..secs.,
                                   Start.Location.Lon,
                                   Start.Location.Lat,
                                   End.Location.Lon,
                                   End.Location.Lat, 
                                   Start.Location.Accuracy..meters., 
                                   End.Location.Accuracy..meters., 
                                   Path
))

write.csv(trips, file = 'output/trips.csv')


    