library(sf)
library(rio)
install_formats()
library(tidyverse)


########################### Import the Data ##############################3
input.dir <- '~/OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/'
log.data.total <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                   which = 1, skip =1) 

########################### select only the columns we are interested in and remove spaces ############
log.data <- select(log.data.total,Rec_ID:"No catch") #between Rec_ID and "No Catch are our interesting columns
names(log.data)
names(log.data) <- gsub(" ","_",names(log.data)) #get rid of the names between spaces
names(log.data) <- gsub("No.lobster/fish_etc.","Num_ind",names(log.data)) # get rid of weid lobster name
###########Clean and tidy up the data by replaceing rogue names#############
unique(log.data$Year) #check
log.data$Year <- ifelse(log.data$Year==2004,2014,log.data$Year) ## edit the years 
log.data$Year <- ifelse(log.data$Year==2011,2017,log.data$Year) ## edit the years
unique(log.data$Year) #check 

unique(log.data$Gear) #check
log.data$Gear <- ifelse(log.data$Gear=="pt","PT",log.data$Gear) # edit the gear types
unique(log.data$Gear) #check

unique(log.data$Landings) #check
log.data$Landings <- ifelse(log.data$Landings %in% c("Fish","fish","FIsh"), "Fish",log.data$Landings) # edit for Fish
log.data$Landings <- ifelse(log.data$Landings %in% c("Queen Conch","Conch","conch"), "Queen Conch",log.data$Landings) # edit for Conch
log.data$Landings <- ifelse(log.data$Landings %in% c("Whelk","Whelks"), "Whelk",log.data$Landings) # edit for Whelk
unique(log.data$Landings) #check

############################### Breaking out data by year #############
trips.year <- log.data %>% # looking at the number of trips per year 
  group_by(Year) %>%
  select(Year, Trip_ID) %>%
  summarize(n_distinct(Trip_ID))
  
trips.month <- log.data %>% # looking at the number of trips per month for each year
  group_by(Year, Month) %>%
  select (Year, Month, Trip_ID) %>%
  summarize(n_distinct(Trip_ID))

gear.trips.year <- log.data %>% #looking at the types of gear used, and the number used per year
  group_by(Year, Gear) %>%
  select(Trip_ID, Year, Gear) %>%
  summarize(n_distinct(Trip_ID))

gear.trips.month <- log.data %>% #looking at the types of gear used, and the amount used per month
  group_by(Year, Month, Gear) %>%
  select(Year, Month, Trip_ID, Gear) %>%
  summarize(n_distinct(Trip_ID))

fish.weight.year <- log.data %>% # looking at the amount of fish caught per year
  group_by(Year, Landings)%>%
  filter(Landings=="Fish") %>%
  select(Year,Landings,`Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.weight.month <- log.data %>% # looking at the amount of fish caught per year
  group_by(Year, Month, Landings)%>%
  filter(Landings=="Fish") %>%
  select(Year, Month, Landings,`Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear<- log.data %>% # looking at the amount of fish caught per gear
  group_by(Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.year <- log.data %>% # looking at the amount of fish caught per gear per year
  group_by(Year, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Year,Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.month <- log.data %>% # looking at the amount of fish caught per gear per month
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Year,Month,Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

lobster.number.year <- log.data %>%
  group_by(Year, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year, Landings, Num_ind)
  #merge (Lob_num<-as.numeric(Num_ind, na.rm=TRUE))
  #summarize(, lobster_num = (sum(Lob_num, na.rm=TRUE)))
  
####################### joining logbook entries for fish, lobster, and conch #############
log.data.Fish <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 2, skip =0)   #import sheet for fish data
names(log.data.Fish)
names(log.data.Fish) <- gsub(" ","_",names(log.data.Fish)) #get rid of the spaces between names
names(log.data.Fish) <- gsub("/","_",names(log.data.Fish)) #get rid of the / between names

log.data.Lobster <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 3, skip =0) # import sheet for lobster data

log.data.Conch <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 4, skip =0) #import sheet for conch data

####################### cleaning logbooks and renaming ####################################

log.data.F <- log.data.Fish %>%
  filter(!is.na(Species_common_name))

names(log.data.F)
names(log.data.F) <- gsub(" ","_",names(log.data.F)) #get rid of the spaces between names
names(log.data.F) <- gsub("/","_",names(log.data.F)) #get rid of the / between names
#names(log.data.F) <- gsub("(","",names(log.data.F)) #get rid of the names between spaces

joined.fish <- log.data %>% 
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Fish")%>%
  left_join(log.data.F, by ="Trip_ID")

log.data.L <-log.data.Lobster
names(log.data.L)
names(log.data.L) <- gsub(" ","_",names(log.data.L)) #get rid of the spaces between names
names(log.data.L) <- gsub("/","_",names(log.data.L)) #get rid of the / between names
log.data.L$Trip_ID <-type.convert(log.data.L$Trip_ID, as.is =TRUE)

lob.trip.trans <- log.data.L %>% 
  mutate(Trip_ID = as.character(Trip_ID))
  
joined.lobster <- log.data %>%
  filter(Landings =="Spiny Lobster")%>%
  left_join(log.data.L, by ="Trip_ID")

log.data.C <- log.data.Conch
names(log.data.C)
names(log.data.C) <- gsub(" ","_",names(log.data.C)) #get rid of the spaces between names
names(log.data.C) <- gsub("/","_",names(log.data.C)) #get rid of the / between names
names(log.data.C) <- gsub("Rec_ID","Trip_ID",names(log.data.C)) #get rid of the / between names

joined.Conch <- log.data %>% 
  filter(Landings =="Queen Conch")%>%
  left_join(log.data.C, by ="Trip_ID")



  
#band_members %>% left_join(band_instruments)
  
#log.data <- ifelse(log.data$Year==2004,2014,log.data$Year)
# filter(!Landings %in% c("Fish","fish","FIsh"))
# ifelse(log.data$Landings %in% c("Fish","fish","FIsh"), "Fish",)
#summary(log.data)
#names(log.data) <- gsub(" ","_",names(log.data))
# gsub(".*_",/,n)


