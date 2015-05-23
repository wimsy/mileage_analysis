# commute_analysis.R

library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(fields)
library(qcc)

trips <- read.csv('data//automatic-trips-2015-04-02.csv', stringsAsFactors=F)

# Note, I know I didn't leave the Easern time zone during this period 
# with this car.
tz = 'America/New_York'
trips$Start.Time <- ymd_hm(trips$Start.Time, tz = tz)
trips$End.Time <- ymd_hm(trips$End.Time, tz = tz)
trips <- trips[order(trips$Start.Time), ]

home.lon.lat <- data.frame(-73.923640, 40.762608)
office.lon.lat <- data.frame(-73.265673, 40.766484)
start.matrix <- trips %>% select(Start.Location.Lon, Start.Location.Lat)
end.matrix <- trips %>% select(End.Location.Lon, End.Location.Lat)
start.home.dist <- rdist.earth(start.matrix, home.lon.lat)
start.office.dist <- rdist.earth(start.matrix, office.lon.lat)
end.home.dist <- rdist.earth(end.matrix, home.lon.lat)
end.office.dist <- rdist.earth(end.matrix, office.lon.lat)

trips$commute <- "none"
trips$commute[start.home.dist < 1.0 & end.office.dist < 1.0] <- 'AM'
trips$commute[start.office.dist < 1.0 & end.home.dist < 1.0] <- 'PM'
trips$commute <- as.factor(trips$commute)

df <- trips %>% select(Start.Time, 
                       End.Time, 
                       Distance..mi., 
                       Duration..min., 
                       Average.MPG, 
                       commute) %>%
    filter(commute == 'AM' | commute == 'PM')

# PDF of commute times (AM & PM)
ggplot(df, aes(Duration..min., color=commute)) + 
    geom_density()

# Boxplot commute time by hour - morning
df %>%
    filter(commute == 'AM') %>%
    ggplot(aes(factor(hour(Start.Time)), Duration..min.)) + 
    geom_boxplot()

# Boxplot commute time by hour - evening
df %>%
    filter(commute == 'PM') %>%
    ggplot(aes(factor(hour(Start.Time)), Duration..min.)) + 
    geom_boxplot()

# Histogram of start times (morning & evening)
ggplot(df, aes(hour(Start.Time), fill=commute)) + 
    geom_histogram()

pathdf <- trips %>% select(Start.Time, 
                           commute, 
                           Path) %>%
    filter(commute == 'AM' | commute == 'PM')

#write.csv(pathdf, 'paths.csv')

gate_results <- read.csv('gate_results.csv', stringsAsFactors=T)
gatedefs <- read.csv('gates.csv', stringsAsFactors = F)
gatedefs$label <- LETTERS[1:nrow(gatedefs)]
gate_results$Gate.List <- as.factor(gate_results$Gate.List)
route.freq.AM <- 
    data.frame(table(gate_results$Gate.List[gate_results$commute == 'AM'])) %>%
    arrange(-Freq) %>%
    rename(route = Var1, freq = Freq)
route.freq.PM <- 
    data.frame(table(gate_results$Gate.List[gate_results$commute == 'PM'])) %>%
    arrange(-Freq) %>%
    rename(route = Var1, freq = Freq)
route.AM.top3 <- route.freq.AM$route[1:3]
route.PM.top3 <- route.freq.PM$route[1:3]

gate_results$Start.Time <- ymd_hms(gate_results$Start.Time, tz = tz)
commutedf <- merge(trips, gate_results, by='Start.Time') %>% 
    select(start.time = Start.Time, 
           end.time = End.Time, 
           distance = Distance..mi., 
           duration = Duration..min., 
           start.lat = Start.Location.Lat, 
           start.lon = Start.Location.Lon, 
           end.lat = End.Location.Lat, 
           end.lon = End.Location.Lon, 
           path = Path, 
           commute = commute.x, 
           gates = Gate.List)

# Paths weren't plotting properly, but I found this: 
# http://stackoverflow.com/a/22865250/1397476

#commutedf$path <- URLencode(commutedf$path)

map_path <- function(path, size, weight, color) {
    enc <- path
    base_url <- 'https://maps.googleapis.com/maps/api/staticmap?'
    map_url <- URLencode(paste(base_url, 
                              'size=', 
                              toString(size), 
                              'x', 
                              toString(floor(size/4)),
                              '&maptype=terrain', 
                              '&path=weight:', 
                              toString(weight), 
                              '|color:', 
                              color, 
                              '|enc:', 
                              enc, 
                              sep = ""))
    return(map_url)
}

size = 800
weight = 5
path.color = 'red'

commutedf <- commutedf %>% 
    mutate(path_url = map_path(commutedf$path, size, weight, path.color)) %>%
    select(-path)


top3 <- commutedf %>% 
    filter(commute == 'AM' & gates %in% route.AM.top3 | 
           commute == 'PM' & gates %in% route.PM.top3) %>% 
    droplevels()

# Gate list                               | cmute | friendly name
# GCP2,CIP1,LIE2,BQE2,LIE1,LIE4           | AM,PM | CIP-LIE
# GCP2,GCP1,LIE2,BQE2,LIE1,LIE6,LIE4,LIE5 | AM,PM | All LIE
# GCP2,GCP1,NSP3,NSP2,BQE2,LIE1           | AM    | GCP-NSP and LIE to Sag
# GCP2,NSP1,CIP1,LIE2,BQE2,LIE4           | PM    | NSP to 37A then LIE-CIP

top3$gates <-revalue(top3$gates, 
        c("GCP2,CIP1,LIE2,BQE2,LIE1,LIE4" = "CIP-LIE", 
          "GCP2,GCP1,LIE2,BQE2,LIE1,LIE6,LIE4,LIE5" = "All LIE", 
          "GCP2,GCP1,NSP3,NSP2,BQE2,LIE1" = "GCP-NSP and LIE to Sag", 
          "GCP2,NSP1,CIP1,LIE2,BQE2,LIE4" = "NSP to 37A then LIE-CIP"))
    
# https://maps.googleapis.com/maps/api/staticmap?size=400x400&path=weight:3%7Ccolor:orange%7Cenc:'

# Scatter plots with colors for routes
top3 %>% 
    filter(commute == 'AM') %>% 
    ggplot(aes(x=hour(start.time) + minute(start.time)/60, 
               y=duration, 
               color=gates)) + 
    geom_point() + 
    scale_x_continuous(breaks = 6:11, "start time") +
    scale_y_continuous("duration(min)") + 
    ggtitle('Morning commute') + 
    scale_color_discrete(name = "Route") + 
    geom_smooth()

top3 %>% 
    filter(commute == 'PM') %>% 
    ggplot(aes(x=hour(start.time) + minute(start.time)/60, 
               y=duration, 
               color=gates)) + 
    geom_point() + 
    scale_x_continuous(breaks = 14:22, "start time") + 
    scale_y_continuous("duration(min)") + 
    ggtitle('Evening commute') + 
    scale_color_discrete(name = "Route") + 
    xlim(14,22)

# Look at commute times for all the top 3
top3 %>% 
    filter(commute == 'AM') %>% 
    ggplot(aes(x=hour(start.time) + minute(start.time)/60, 
               y=duration)) + 
    geom_point() + 
    scale_x_continuous(breaks = 6:11, "start time") +
    scale_y_continuous("duration(min)") + 
    ggtitle('Morning commute') + 
    scale_color_discrete(name = "Route") + 
    geom_smooth()

top3 %>% 
    filter(commute == 'PM') %>% 
    ggplot(aes(x=hour(start.time) + minute(start.time)/60, 
               y=duration)) + 
    geom_point() + 
    scale_x_continuous(breaks = 14:22, "start time") + 
    scale_y_continuous("duration(min)") + 
    ggtitle('Evening commute') + 
    scale_color_discrete(name = "Route") + 
    xlim(14,22) + 
    geom_smooth()

# PDFs for all route-commute combos
ggplot(top3, aes(duration)) + 
    geom_density() + 
    facet_grid(gates ~ commute)

ggplot(top3, aes(hour(start.time) + minute(start.time)/60)) + 
    geom_density() + 
    facet_grid(gates ~ commute)

ggplot(top3, aes(hour(start.time) + minute(start.time)/60, 
                 color=gates)) + 
    geom_density() + 
    facet_grid(commute ~ .)

