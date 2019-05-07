library(sf)
library(rio)
library(tidyverse)

input.dir <- 'R:/Gill/spatial-fisheries-analysis/tables/raw/'
log.data <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                   which = 1, skip =1) 

log.data1 <- select(log.data,Rec_ID:"No catch")

summary(log.data)
unique(log.data$Gear)
hist(log.data$`Weight_(Lbs)`)
summary(log.data$`Weight_(Lbs)`)
