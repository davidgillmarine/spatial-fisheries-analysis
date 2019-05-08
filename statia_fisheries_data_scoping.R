library(sf)
library(rio)
install_formats()
library(tidyverse)


########################### Import the Data ##############################3
input.dir <- '~/OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/'
log.data <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                   which = 1, skip =1) 

########################### select only the columns we are interested in and remove spaces ############
log.data1 <- select(log.data,Rec_ID:"No catch") #between Rec_ID and "No Catch are our interesting columns
names(log.data1)
names(log.data1) <- gsub(" ","_",names(log.data1)) #get rid of the names between spaces

###########Clean and tidy up the data by replaceing rogue names#############
unique(log.data1$Year) #check
log.data1$Year <- ifelse(log.data1$Year==2004,2014,log.data1$Year) ## edit the years 
log.data1$Year <- ifelse(log.data1$Year==2011,2017,log.data1$Year) ## edit the years
unique(log.data1$Year) #check 

unique(log.data1$Gear) #check
log.data1$Gear <- ifelse(log.data1$Gear=="pt","PT",log.data1$Gear) # edit the gear types
unique(log.data1$Gear) #check

unique(log.data1$Landings) #check
log.data1$Landings <- ifelse(log.data1$Landings %in% c("Fish","fish","FIsh"), "Fish",log.data1$Landings) # edit for Fish


gear.trips <- log.data1 %>%
  group_by(Year, Gear) %>%
  select(Trip_ID, Year, Gear) %>%
  summarize(n_distinct(Trip_ID))
log.data1 %>% 
  filter()
  
log.data1 <- ifelse(log.data1$Year==2004,2014,log.data1$Year)

species <- log.data1 %>%
  filter(Landings=="Fish" | Landings == "fish" | Landings=="FIsh") %>%
  group_by(Month, Gear)%>%
  summarize(weight_landings = sum(`Weight_(Lbs)`))

# filter(!Landings %in% c("Fish","fish","FIsh"))

# ifelse(log.data1$Landings %in% c("Fish","fish","FIsh"), "Fish",)

summary(log.data1)


names(log.data1) <- gsub(" ","_",names(log.data1))
# gsub(".*_",/,n)


