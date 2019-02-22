# read station data
stations <- read.csv('rawData/Public_Transit_Stations.csv')
stations$X <- NULL
stations$Y <- NULL
stations$ID <- NULL
stations$OBJECTID <- NULL
stations$STATIONURL <- NULL
stations$SYS_AGENCY <- NULL
stations$NTDID = NULL
stations$UA <- NULL
stations$UACODE <- NULL
stations$SOURCE <- NULL
stations$SOURCEDATE <- NULL
stations$ADDRESS <- NULL

# read route data
routes <- read.csv('rawData/Routes_and_Stations.csv')
routes$OBJECTID <- NULL

# read tax data
raw_taxes <- read.csv('rawData/16zpallagi.csv')
taxes <- data.frame('zip' = raw_taxes$zipcode, 'count' = raw_taxes$N02650, 'total' = raw_taxes$A02650)
taxes <- taxes[taxes$zip != 0,]

# sum up totals for each zip code
library(dplyr)
taxes <- taxes %>% 
  group_by(zip) %>% 
  summarise_all(sum)

# compute average income for the zip code
taxes$avg <- taxes$total / taxes$count * 1000

# append tax data to station data
stations$income <- apply(stations, 1, function(row) paste(taxes[taxes$zip == row['ZIP'], 'avg'], collapse=' '))

# find matched routes for the stations
stations$RTE_NAME <- apply(stations, 1, function(row) paste(routes[routes$STA_ID == row['STA_ID'], 'RTE_ID_SYS'], collapse=','))

# # check if there are unique stations between the 2 sets
# tStations <- unique(stations$STA_ID)
# rStations <- unique(routes$STA_ID)
# aStations <- unique(c(tStations, rStations))
# length(tStations)
# length(rStations)
# length(aStations)

# save the data to transit.csv
write.csv(stations, file='transit.csv')