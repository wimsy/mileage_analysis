# waze_eta.R

# Probably not going to work because Waze stopped sending SMS in June 2014.

# Exploring Waze ETA accuracy

library(DBI)
library(tidyr)

# Pull all SMS with "waze" in the body and select critical columns
con <- dbConnect(RSQLite::SQLite(), "data//chat.db")
wazeres <- dbSendQuery(con, "SELECT * FROM message WHERE text LIKE '%waze.to%'")
wazedf <- dbFetch(wazeres) %>% 
    filter(is_from_me == 1) %>%
    select(ROWID, 
           text, 
           handle_id, 
           date) %>% 
    filter(grepl("I'm on my way to ", text)) %>% 
    separate(text, 
             into = c("trip"), 
             sep = "\\. Y", 
             extra = "drop", 
             remove = TRUE) %>%
    separate(trip, 
             into = c("destination.string", "eta.string"), 
             sep = ", arriving at ", 
             remove = TRUE) %>% 
    separate(destination.string, 
             into = c("subject", "destination"), 
             sep = "on my way to ", 
             remove = TRUE) %>% 
    mutate(timestamp = as.POSIXct(date, origin="2001-01-01 00:00:00")) %>% 
    select(-subject, -date)
dbClearResult(wazeres)
dbDisconnect(con)

