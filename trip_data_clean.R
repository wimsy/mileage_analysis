# Remove some location info and create a basic data set for publication

library(lubridate)

tripsfile = 'data//Volkswagen Jetta trips - Sheet 1.csv'
trips <- read.csv(tripsfile, stringsAsFactors = FALSE)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
trips$Trip.Started.At <- mdy_hm(trips$Trip.Started.At, tz = tz)
trips$Trip.Ended.At <- mdy_hm(trips$Trip.Ended.At, tz = tz)
trips <- trips[order(trips$Trip.Started.At), ]
trips$Fuel.Cost.USD <- as.numeric(sub('\\$', '', trips$Fuel.Cost.USD))

# Calculate some useful values
trips$trip.minutes <- as.integer(trips$Trip.Ended.At - trips$Trip.Started.At)/60
# Zero-minute trips rounded up to one minute because Infinity
trips$avg.mph <- trips$Trip.Distance.Miles / max(1, trips$trip.minutes) * 60.0
trips$calc.gallons <- trips$Trip.Distance.Miles / trips$Average.MPG
trips$cost.per.gallon <- trips$Fuel.Cost.USD / trips$calc.gallons

trips <- subset(trips, select = -c(Vehicle.Name, 
                                   Duration.Over.70.MPH, 
                                   Duration.Over.75.MPH,
                                   Duration.Over.80.MPH,
                                   Trip.Path.Image.Map.URL,
                                   Start.Location.Lon,
                                   Start.Location.Lat,
                                   Start.Location.Map.URL,
                                   End.Location.Lon,
                                   End.Location.Lat,
                                   End.Location.Map.URL
))

write.csv(trips, file = 'output/trips.csv')
