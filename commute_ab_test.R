# commute_ab_test.R

library(dplyr)
library(lubridate)
library(ggplot2)

df <- read.csv('data//ab_route_data.txt', sep = "|", stringsAsFactors=F) %>%
    mutate(timestamp = parse_date_time(timestamp, 
                                       "%m %d, %y, %I:%M %p", 
                                       locale = Sys.getlocale("LC_TIME"), 
                                       tz = Sys.timezone()), 
           route = as.factor(route))
levels(df$route) <- c("long", "short")

# Analyze "fairness" of random algorithm
trial_counts <- table(df$route)
long_likelihood <- 
    trial_counts['long']/(trial_counts['long'] + trial_counts['short'])
Z <- 2
E <- Z / (2*sqrt(length(df$route)))
range <- c(long_likelihood - E, long_likelihood + E)  # 95% confidence interval

df %>% 
    group_by(route) %>% 
    summarize(n = n(), 
              mean.time = mean(time), 
              st.dev = sd(time))

qplot(route, time, data=df, geom="boxplot")

# Play with maps

library(leaflet)

road.signs <- data.frame(lng = c(-73.274525, -73.275758), 
                         lat = c(40.772942, 40.772796),  
                         label = c('stop sign', 'traffic light'))

road.icons <- icons(
    iconUrl = ifelse(road.signs$label == 'stop sign', 
                     "images/noun_51713_cc.svg", 
                     "images/noun_53685_cc.svg"),
    iconWidth = 19, iconHeight = 45,
    iconAnchorX = 0, iconAnchorY = 0)

short.route <- matrix(c(-73.273977, 40.770891, 
                        -73.274192, 40.770866, 
                        -73.274728, 40.773036, 
                        -73.275919, 40.772759), 
                      ncol=2, 
                      byrow=TRUE)

long.route <- matrix(c(-73.273977, 40.770891, 
                       -73.274192, 40.770866, 
                       -73.273634, 40.768798, 
                       -73.274675, 40.768644, 
                       -73.275780, 40.772796, 
                       -73.275919, 40.772759), 
                     ncol=2, 
                     byrow=TRUE)

m <- leaflet() %>% setView(lng = -73.274774, lat = 40.771, zoom = 16)
m %>% addProviderTiles("CartoDB.Positron") %>% 
    addMarkers(data = road.signs, 
               lat = ~lat, 
               lng = ~lng, 
               icon = road.icons) %>% 
    addPolylines(data = short.route, 
                weight = 7, 
                fillColor = 'transparent',
                color = '#1b9e77', 
                popup = "Short route: 0.2 miles") %>% 
    addPolylines(data = long.route, 
                 weight = 7, 
                 fillColor = 'transparent',
                 color = '#d95f02', 
                 popup = "Long route: 0.5 miles")

# Add attribution for icons
# Stop sign and traffic light icons by Jonathan Li from the Noun Project
# with links to each icon and the Noun Project


