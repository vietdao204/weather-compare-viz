## Data Cleaning #
## Viet Dao ######
## May 2020 ######

require(tidyverse)
require(dplyr)

#########################################
#### Hanoi ####
#########################################
hanoi_orig <- read.csv('./data/hanoi.csv', header = TRUE, stringsAsFactors = FALSE)
keepCols_hanoi <- c('NAME', 'DATE', 'PRCP', 'TAVG', 'TMAX', 'TMIN')
hanoi <- hanoi_orig[, keepCols_hanoi]

# no of NAs in each row
# sapply(hanoi, function(x) sum(is.na(x)));

# calculate NA TMIN and TMAX from TAVG and each other
hanoi[is.na(hanoi$TMIN) & !is.na(hanoi$TMAX), 'TMIN'] <- hanoi[is.na(hanoi$TMIN) & !is.na(hanoi$TMAX), 'TAVG']*2 - hanoi[is.na(hanoi$TMIN) & !is.na(hanoi$TMAX), 'TMAX']
hanoi[!is.na(hanoi$TMIN) & is.na(hanoi$TMAX), 'TMAX'] <- hanoi[!is.na(hanoi$TMIN) & is.na(hanoi$TMAX), 'TAVG']*2 - hanoi[!is.na(hanoi$TMIN) & is.na(hanoi$TMAX), 'TMIN']
# 3 rows: both TMAX and TMIN are NA, replace by TAVG
hanoi[is.na(hanoi$TMIN) & is.na(hanoi$TMAX), 'TMIN'] <- hanoi[is.na(hanoi$TMIN) & is.na(hanoi$TMAX), 'TAVG']
hanoi[is.na(hanoi$TMAX), 'TMAX'] <- hanoi[is.na(hanoi$TMAX), 'TAVG']

# replace 1 NA PRCP by 0
hanoi['PRCP'][is.na(hanoi['PRCP'])] <- 0.0

# set DATE to Date object
hanoi$DATE <- as.Date(hanoi$DATE)
hanoi <- hanoi[order(hanoi$DATE),]

# change name from 'HA DONG' to 'HANOI' for simplicity
hanoi$NAME <- 'HANOI'

# add SNOW and SNWD columns
hanoi$SNOW <- 0
hanoi$SNWD <- 0

# dont need TAVG
hanoi <- hanoi[, !(names(hanoi) %in% ('TAVG'))]
hanoi <- hanoi[, c('NAME', 'DATE', 'PRCP', 'SNOW', 'SNWD', 'TMAX', 'TMIN')]


#############################################
#### St. Peter ####
#############################################
stpeter_orig <- read.csv('./data/stpeter.csv', header = TRUE, stringsAsFactors = FALSE)
keepCols_stpeter <- c('NAME', 'DATE', 'PRCP', 'SNOW', 'SNWD', 'TMAX', 'TMIN')
stpeter <- stpeter_orig[, keepCols_stpeter];
# stpeter %>% group_by(NAME) %>% summarise(n = n())

# set DATE to Date object
stpeter$DATE <- as.Date(stpeter$DATE)
stpeter <- stpeter[order(stpeter$DATE),]

# fill NAs
stpeter <- aggregate(stpeter, by=list(DATE_ID=stpeter$DATE), min, na.rm = TRUE)
stpeter <- stpeter[, !(names(stpeter) %in% ('DATE_ID'))]
stpeter['SNWD'][stpeter$SNOW==0 & !is.finite(stpeter$SNWD)] <- 0

stpeter$PRCP[!is.finite(stpeter$PRCP)] <- NA
stpeter$SNWD[!is.finite(stpeter$SNWD)] <- NA
stpeter$TMAX[!is.finite(stpeter$TMAX)] <- NA
stpeter$TMIN[!is.finite(stpeter$TMIN)] <- NA

# sapply(stpeter, function(x) sum(is.infinite(x)))

# few NA is okay
# View(stpeter %>% filter(is.na(PRCP)|is.na(SNWD)|is.na(TMAX)|is.na(TMIN)))

# rename for simplicty
stpeter$NAME <- 'STPETER'

stpeter <- stpeter[stpeter$DATE >= '2016-06-01',]


#################################################
#### San Francisco (2017-06-01 - 2019-09-05) ####
#################################################
sf_orig <- read.csv('./data/sf.csv', header = TRUE, stringsAsFactors = FALSE);
keepCols_sf <- c('NAME', 'DATE', 'PRCP', 'SNOW', 'SNWD', 'TMAX', 'TMIN')
sf <- sf_orig[, keepCols_sf]

sapply(sf, function(x) sum(is.na(x)))

sf$DATE <- as.Date(sf$DATE)
sf <- sf[order(sf$DATE),]

sf <- sf %>% filter(NAME == 'SAN FRANCISCO DOWNTOWN, CA US')
sf$NAME <- 'SF'

sf[c('SNOW', 'SNWD')][is.na(sf[c('SNOW', 'SNWD')])] <- 0


###########################################
#### Oakland (2018-05-01 - 2019-09-05) ####
###########################################
oakland_orig <- read.csv('./data/oakland.csv', header = TRUE, stringsAsFactors = FALSE)
oakland <- oakland_orig[, keepCols_sf]
sapply(oakland, function(x) sum(is.na(x)))

oakland$DATE <- as.Date(oakland$DATE)
oakland <- oakland[order(oakland$DATE),]

oakland <- oakland[oakland$NAME %in% c('OAKLAND METROPOLITAN, CA US', 'OAKLAND MUSEUM, CA US'),]

oakland[c('SNOW', 'SNWD')][is.na(oakland[c('SNOW', 'SNWD')])] <- 0.0
# oakland <- oakland[!is.na(oakland$TMAX), ]

oakland$ID <- seq.int(nrow(oakland))
ids_to_drop <- oakland[oakland$NAME == 'OAKLAND METROPOLITAN, CA US' & oakland$DATE > '2018-06-20',]$ID
oakland <- oakland[!(oakland$ID %in% ids_to_drop), ];
oakland <- oakland[, !(colnames(oakland) == "ID")];

oakland$NAME <- 'OAKLAND'
# rearrage index column
row.names(oakland) <- NULL


##############################################
#### Swarthmore (2019-06-15 - 2020-06-14) ####
##############################################
swarthmore_orig <- read.csv('./Data/swarthmore.csv', header = TRUE, stringsAsFactors = FALSE)

# swarthmore_orig %>% group_by(NAME) %>% summarise(n = n())
# sapply(swarthmore, function(x) sum(is.na(x)))
swarthmore <- swarthmore_orig[, keepCols_sf]

swarthmore$DATE <- as.Date(swarthmore$DATE)
swarthmore <- swarthmore[order(swarthmore$DATE),]

# Use data from Philadelphia International Airport Station, which is closest to Swarthmore.
swarthmore <- swarthmore[swarthmore$NAME %in% c('PHILADELPHIA INTERNATIONAL AIRPORT, PA US'),]

swarthmore['SNWD'][is.na(swarthmore['SNWD'])] <- 0

swarthmore$NAME <- 'SWARTHMORE'
row.names(swarthmore) <- NULL


############################################
#### Victoria (2019-06-16 - 2020-06-15) ####
############################################
victoria_orig <- read.csv('./data/victoria.csv', header = TRUE, stringsAsFactors = FALSE)

# victoria_orig %>% group_by(NAME) %>% summarise(n = n())
# sapply(victoria_orig, function(x) sum(is.na(x)))

victoria <- victoria_orig[, keepCols_sf]

victoria$DATE <- as.Date(victoria$DATE)
victoria <- victoria[order(victoria$DATE),]
victoria <- victoria %>% filter(DATE >= '2019-06-16')

victoria %>% group_by(NAME) %>% summarise(prcp_na=sum(is.na(PRCP)),snow_na=sum(is.na(SNOW)), snwd_na=sum(is.na(SNWD)), tmax_na=sum(is.na(TMAX)), tmin_na=sum(is.na(TMIN)))
victoria <- victoria %>% filter(NAME %in% c('VICTORIA UNIVERSITY CS, BC CA', 'VICTORIA 6.0 NNW, CA'))
victoria$NAME <- 'VICTORIA'

victoria <- aggregate(victoria, by=list(DATE_ID=victoria$DATE), min, na.rm = TRUE)
victoria <- victoria[, !(colnames(victoria)=='DATE_ID')]

# sapply(victoria, function(x) sum(!is.finite(x)))
victoria[is.infinite(victoria$SNWD),]['SNWD'] <- 0
victoria[is.infinite(victoria$SNOW),]['SNOW'] <- 0
victoria$TMAX[!is.finite(victoria$TMAX)] <- NA
victoria$TMIN[!is.finite(victoria$TMIN)] <- NA