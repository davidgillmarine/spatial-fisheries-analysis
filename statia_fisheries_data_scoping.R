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
names(log.data) <- gsub("No.lobster/fish_etc.","Num_ind",names(log.data)) # get rid of weird lobster name
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

############################### Breaking out data by year, and month, and by gear #############
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
  group_by(Year, Landings, Num_ind) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year, Landings, Trip_ID, Num_ind)%>%
  summarize(n_distinct(Trip_ID))
  
  #merge (Lob_num<-as.numeric(Num_ind, na.rm=TRUE))
  #summarize(, lobster_num = (sum(Lob_num, na.rm=TRUE)))
  
####################### reading logbook entries for fish, lobster, and conch #############
log.data.Fish <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 2, skip =0, .name_repair="universal")   #import sheet for fish data
names(log.data.Fish)
names(log.data.Fish) <- gsub(" ","_",names(log.data.Fish)) #get rid of the spaces between names
names(log.data.Fish) <- gsub("/","_",names(log.data.Fish)) #get rid of the / between names

log.data.Lobster <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 3, skip =0, .name_repair="universal",col_types=c("numeric")) # import sheet for lobster data
names(log.data.Lobster)
#names(log.data.Lobster) <- gsub(" ","_",names(log.data.L)) #get rid of the spaces between names
#names(log.data.Lobster) <- gsub("/","_",names(log.data.L)) #get rid of the / between names

log.data.Conch <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 4, skip =0,.name_repair="universal") #import sheet for conch data
names(log.data.Conch)
names(log.data.C) <- gsub(" ","_",names(log.data.C)) #get rid of the spaces between names
names(log.data.C) <- gsub("/","_",names(log.data.C)) #get rid of the / between names


####################### cleaning logbooks and renaming ####################################

log.data.F <- log.data.Fish %>% #create the join data and filter it by year to limit the observations
  filter(!is.na(Year))
names(log.data.F)

joined.fish <- log.data %>% # join the log book and the fish data by unique Trip ID
  mutate(Trip_ID=as.character(Trip_ID))%>% # need to convert to a character
  filter(Landings =="Fish")%>%
  left_join(log.data.F, by ="Trip_ID")

joined.fish.test <- log.data %>% # join the log book and the fish data by unique Trip ID
  mutate(Trip_ID=as.character(Trip_ID))%>% # need to convert to a character
  filter(Landings =="Fish")%>%
  left_join(log.data.F, by = c( "Trip_ID", "Day", "Month", "Year"))

anti.join.fish <-log.data %>% #use this to figure out which ones are not joining or do not have trip IDs
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Fish")%>%
  anti_join(log.data.F, by = "Trip_ID")

anti.join.fish.test <-log.data %>% #use this to figure out which ones are not joining or do not have trip IDs
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Fish")%>%
  anti_join(log.data.F,by = c("Trip_ID", "Day", "Month", "Year"))

log.data.L <-log.data.Lobster %>% # create the join data and filter it by year to limit the observations
  filter(!is.na(Year))
names(log.data.L)
  
joined.lobster <- log.data %>%  #join the log books and lobster data by unique trip id
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Spiny Lobster")%>%
  left_join(lob.trip.trans, by ="Trip_ID")

anti.join.lobster <-log.data %>% #use this to figure out which ones are and are not joining, seems like all :(
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Spiny Lobster")%>%
  anti_join(lob.trip.trans, by = "Trip_ID")

log.data.C <- log.data.Conch %>% #create the join data and filter it by year to limit the observations
  rename(Trip_ID = Rec_ID) %>% # rename this variable
  filter(!is.na(Year))
names(log.data.C)

joined.Conch <- log.data %>% # join the log book and conch data by unique trip id 
  filter(Landings =="Queen Conch")%>%
  left_join(log.data.C, by ="Trip_ID") 

anti.join.conch <- log.data %>% # use this join to see what values are not joining 
  filter(Landings =="Queen Conch")%>%
  anti_join(log.data.C, by ="Trip_ID") 

## when joining by trip id, there are a few observations in the specific sections that are missig Trip ID
## numbers and are omitted from this joined sheet. Additionally, observations from the logbook that have 
## conch, lobster, or fish observations in the original sheet are included in the join but have no
## additional information from the join, essentially empty cells since the specific sheets are a small
## subset of observations 

##################################### beginning to filter by species per year and month ###########################

unique(joined.fish$Species_latin_name)

distinct.species.year <- joined.fish %>% # looking at the number of distinct species per year
  group_by(Year.x) %>%
  select(Year.x, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

unique(joined.fish.test$Species_latin_name)

distinct.species.year.test <- joined.fish.test %>% # looking at the number of distinct species per year
  group_by(Year) %>%
  select(Year, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.year <- joined.fish %>% # looking at number of individuals per species per year 
  group_by(Year.x) %>%
  select(Year.x, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

species.year.test <- joined.fish.test %>% # looking at number of individuals per species per year 
  group_by(Year) %>%
  select(Year, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

distinct.species.month <- joined.fish %>% # looking at the number of distinct species per month
  group_by(Year.x, Month.x) %>%
  select(Year.x, Month.x, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.month <- joined.fish %>% # looking at number of individuals per species per month
  group_by(Year.x, Month.x) %>%
  select(Year.x, Month.x,Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

####################### filtering by species per year per gear and per month per gear ###############

distinct.species.year.gear <- joined.fish %>% # looking at the number of distinct species per year per gear
  group_by(Year.x, Gear.x) %>%
  select(Year.x,Gear.x, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.year.gear <- joined.fish %>% # looking at number of individuals per species per year per gear
  group_by(Year.x, Gear.x) %>%
  select(Year.x, Gear.x, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

distinct.species.month.gear <- joined.fish %>% # looking at the number of distinct species per month per gear
  group_by(Year.x, Month.x, Gear.x) %>%
  select(Year.x, Month.x, Gear.x, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.month.gear <- joined.fish %>% # looking at number of individuals per species per month per gear
  group_by(Year.x, Month.x, Gear.x) %>%
  select(Year.x, Month.x,Gear.x,Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

###################################### Zone Analysis For Fish ##########################################

zones.fish <- log.data %>% 
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Weight_(Lbs)`) %>% 
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
  filter(!is.na(zone_id)) %>% 
  mutate(weight.per.zone=`Weight_(Lbs)`/n.zones) %>% 
  filter(Landings=="Fish")%>%
  arrange(Trip_ID)
head(zones.fish)

#amount of fish per zone per year
zone.fish.year <- zones.fish %>% 
  group_by(Year,Landings,zone_id)%>% 
  filter(Landings=="Fish") %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.year)

#amount of fish per zone per month
zone.fish.month <- zones.fish %>% 
  group_by(Year,Month,Landings,zone_id)%>% 
  filter(Landings=="Fish") %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.month)

#amount of fish per zone per year per gear
zone.fish.gear.year<-zones.fish %>%
  group_by(Year, Landings, Gear, zone_id) %>%
  filter(Landings=="Fish") %>%
  select(Year, Landings, Gear, zone_id, weight.per.zone)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.gear.year)  

#amount of fish per zone per month per gear
zone.fish.gear.month<-zones.fish %>%
  group_by(Year, Month, Landings, Gear, zone_id) %>%
  filter(Landings=="Fish") %>%
  select(Year, Month, Landings, Gear, zone_id, weight.per.zone)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.gear.month)  

#number of trips for fish per zone per year
zone.fish.trips.year <- zones.fish %>% 
  group_by(Year,Landings,zone_id)%>% 
  filter(Landings=="Fish") %>%
  select(Year, Landings, Trip_ID,zone_id) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
View(zone.fish.trips.year)

#number of trips for fish per zone per month
zone.fish.trips.monthly <- zones.fish %>% 
  group_by(Year,Month,Landings,zone_id)%>% 
  filter(Landings=="Fish") %>%
  select(Year, Month, Landings, Trip_ID,zone_id) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
View(zone.fish.trips.monthly)

#number of trips for fish per zone per year per gear
zone.fish.trips.gear.year <- zones.fish %>% 
  group_by(Year,Landings,zone_id, Gear)%>% 
  filter(Landings=="Fish") %>%
  select(Year, Landings, Trip_ID,zone_id, Gear) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
View(zone.fish.trips.gear.year)

#number of trips for fish per zone per month per gear
zone.fish.trips.gear.monthly <- zones.fish %>% 
  group_by(Year,Month, Landings,zone_id, Gear)%>% 
  filter(Landings=="Fish") %>%
  select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
View(zone.fish.trips.gear.monthly)

#amount of fish caught per zone for each unique trip ID for each year
zone.fish.uni.trips.year <- zones.fish%>% 
  group_by(Year,Landings,zone_id, Trip_ID)%>% 
  filter(Landings=="Fish") %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.uni.trips.year)

#amount of fish caught per zone for each unique trip ID for each month
zone.fish.uni.trips.monthly <- zones.fish%>% 
  group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
  filter(Landings=="Fish") %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
View(zone.fish.uni.trips.monthly)

#joined tables showing amount of fish caught for total number of trips per zone per year
joined.zone.fish.trips.year <- zone.fish.year %>% # join the log book and the fish data by unique Trip ID
  filter(Landings =="Fish")%>%
  left_join(zone.fish.trips.year,by = c("Year", "Landings", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
View(joined.zone.fish.trips.year)

#joined tables showing amount of fish caught for trips per zone by gear type per year
joined.zone.fish.trips.gear.year <- zone.fish.gear.year %>% # join the log book and the fish data by unique Trip ID
  filter(Landings =="Fish")%>%
  left_join(zone.fish.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
View(joined.zone.fish.trips.gear.year)

#joined tables showing amount of fish caught for total number of trips per zone per month
joined.zone.fish.trips.monthly <- zone.fish.month %>% # join the log book and the fish data by unique Trip ID
  filter(Landings =="Fish")%>%
  left_join(zone.fish.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
View(joined.zone.fish.trips.monthly)

#joined tables showing amount of fish caught for trips per zone by gear type per month
joined.zone.fish.trips.gear.monthly <- zone.fish.gear.month %>% # join the log book and the fish data by unique Trip ID
  filter(Landings =="Fish")%>%
  left_join(zone.fish.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
View(joined.zone.fish.trips.gear.monthly)

################################## Zone Analysis for Lobsters ####################################
#lobsters are done as number of individuals since that is how they were recorded
#except for a few select records potencially accidentally done by weight 

names(log.data)
zones.lob <- log.data %>% 
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear, `Num_ind`) %>% 
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
  filter(!is.na(zone_id))%>%
  mutate(Num_ind=as.numeric(Num_ind))%>%
  mutate(ind.per.zone= Num_ind/n.zones) %>% 
  filter(Landings=="Spiny Lobster")%>%
  arrange(Trip_ID)
 head(zones.lob)

 #amount of lobsters per zone per year
 zone.lob.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id)%>% 
   filter(Landings=="Spiny Lobster") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.year)
 
 #amount of lobsters per zone per month
 zone.lob.month <- zones.lob %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   filter(Landings=="Spiny Lobster") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.month)
 
 #amount of lobsters per zone per year per gear
 zone.lob.gear.year<-zones.lob %>%
   group_by(Year, Landings, Gear, zone_id) %>%
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.gear.year)  
 
 #amount of lobsters per zone per month per gear
 zone.lob.gear.month<-zones.lob %>%
   group_by(Year, Month, Landings, Gear, zone_id) %>%
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Month, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.gear.month)  
 
 #number of trips for lobsters per zone per year
 zone.lob.trips.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id)%>% 
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.lob.trips.year)
 
 #number of trips for lobsters per zone per month
 zone.lob.trips.monthly <- zones.lob %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Month, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.lob.trips.monthly)
 
 #number of trips for lobsters per zone per year per gear
 zone.lob.trips.gear.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id, Gear)%>% 
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.lob.trips.gear.year)
 
 #number of trips for lobsters per zone per month per gear
 zone.lob.trips.gear.monthly <- zones.lob %>% 
   group_by(Year,Month, Landings,zone_id, Gear)%>% 
   filter(Landings=="Spiny Lobster") %>%
   select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.lob.trips.gear.monthly)
 
 #number of lobster caught per zone for each unique trip ID for each year
 zone.lob.uni.trips.year <- zones.lob%>% 
   group_by(Year,Landings,zone_id, Trip_ID)%>% 
   filter(Landings=="Spiny Lobster") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.uni.trips.year)
 
 #number of lobsters caught per zone for each unique trip ID for each month
 zone.lob.uni.trips.monthly <- zones.lob%>% 
   group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
   filter(Landings=="Spiny Lobster") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.lob.uni.trips.monthly)
 
 #joined tables showing amount of lobsters caught for total number of trips per zone per year
 joined.zone.lob.trips.year <- zone.lob.year %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Spiny Lobster")%>%
   left_join(zone.lob.trips.year,  by = c("Year", "Landings", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 View(joined.zone.lob.trips.year)
 
 #joined tables showing amount of lobsters caught for trips per zone by gear type per year
 joined.zone.lob.trips.gear.year <- zone.lob.gear.year %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Spiny Lobster")%>%
   left_join(zone.lob.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 View(joined.zone.lob.trips.gear.year)
 
 #joined tables showing amount of lobsters caught for total number of trips per zone per month
 joined.zone.lob.trips.monthly <- zone.lob.month %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Spiny Lobster")%>%
   left_join(zone.lob.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 View(joined.zone.lob.trips.monthly)
 
 #joined tables showing amount of lobsters caught for trips per zone by gear type per month
 joined.zone.lob.trips.gear.monthly <- zone.lob.gear.month %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Spiny Lobster")%>%
   left_join(zone.lob.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 View(joined.zone.lob.trips.gear.monthly)

##################################### Zone Analysis for Queen Conch #############################
 #Queen Conch are done as number of individuals since that is how they were recorded
 #except for a few select records potencially accidentally done by weight 
 
 names(log.data)
 zones.conch <- log.data %>% 
   select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Num_ind`) %>% 
   mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
   gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
   filter(!is.na(zone_id))%>%
   mutate(Num_ind=as.numeric(Num_ind))%>%
   mutate(ind.per.zone= Num_ind/n.zones) %>% 
   filter(Landings=="Queen Conch")%>%
   arrange(Trip_ID)
 head(zones.conch)
 
 #amount of Queen Conch per zone per year
 zone.conch.year <- zones.conch %>% 
   group_by(Year,Landings,zone_id)%>% 
   filter(Landings=="Queen Conch") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.year)
 
 #amount of Queen Conch per zone per month
 zone.conch.month <- zones.conch %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   filter(Landings=="Queen Conch") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.month)
 
 #amount of Queen Conch per zone per year per gear
 zone.conch.gear.year<-zones.conch %>%
   group_by(Year, Landings, Gear, zone_id) %>%
   filter(Landings=="Queen Conch") %>%
   select(Year, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.gear.year)  
 
 #amount of Queen Conch per zone per month per gear
 zone.conch.gear.month<-zones.conch %>%
   group_by(Year, Month, Landings, Gear, zone_id) %>%
   filter(Landings=="Queen Conch") %>%
   select(Year, Month, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.gear.month)  
 
 #number of trips for Queen Conch per zone per year
 zone.conch.trips.year <- zones.conch %>% 
   group_by(Year,Landings,zone_id)%>% 
   filter(Landings=="Queen Conch") %>%
   select(Year, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.conch.trips.year)
 
 #number of trips for Queen Conch per zone per month
 zone.conch.trips.monthly <- zones.conch %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   filter(Landings=="Queen Conch") %>%
   select(Year, Month, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.conch.trips.monthly)
 
 #number of trips for Queen Conch per zone per year per gear
 zone.conch.trips.gear.year <- zones.conch %>% 
   group_by(Year,Landings,zone_id, Gear)%>% 
   filter(Landings=="Queen Conch") %>%
   select(Year, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.conch.trips.gear.year)
 
 #number of trips for Queen Conch per zone per month per gear
 zone.conch.trips.gear.monthly <- zones.conch %>% 
   group_by(Year,Month, Landings,zone_id, Gear)%>% 
   filter(Landings=="Queen Conch") %>%
   select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 View(zone.conch.trips.gear.monthly)
 
 #number of Queen Conch caught per zone for each unique trip ID for each year
 zone.conch.uni.trips.year <- zones.conch%>% 
   group_by(Year,Landings,zone_id, Trip_ID)%>% 
   filter(Landings=="Queen Conch") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.uni.trips.year)
 
 #number of Queen Conch caught per zone for each unique trip ID for each month
 zone.conch.uni.trips.monthly <- zones.conch%>% 
   group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
   filter(Landings=="Queen Conch") %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 View(zone.conch.uni.trips.monthly)
 
 #joined tables showing amount of Queen Conch caught for total number of trips per zone per year
 joined.zone.conch.trips.year <- zone.conch.year %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Queen Conch")%>%
   left_join(zone.conch.trips.year, by = c("Year", "Landings", "zone_id") )%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 View(joined.zone.conch.trips.year)
 
 #joined tables showing amount of Queen Conch caught for trips per zone by gear type per year
 joined.zone.conch.trips.gear.year <- zone.conch.gear.year %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Queen Conch")%>%
   left_join(zone.conch.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id"))%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 View(joined.zone.conch.trips.gear.year)
 
 #joined tables showing amount of Queen Conch caught for total number of trips per zone per month
 joined.zone.conch.trips.monthly <- zone.conch.month %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Queen Conch")%>%
   left_join(zone.conch.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 View(joined.zone.conch.trips.monthly)
 
 #joined tables showing amount of Queen Conch caught for trips per zone by gear type per month
 joined.zone.conch.trips.gear.monthly <- zone.conch.gear.month %>% # join the log book and the fish data by unique Trip ID
   filter(Landings =="Queen Conch")%>%
   left_join(zone.conch.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 View(joined.zone.conch.trips.gear.monthly)
 
 ####################################### Zone Fish Data by Species #####################
 
 
#log.data <- ifelse(log.data$Year==2004,2014,log.data$Year)
# filter(!Landings %in% c("Fish","fish","FIsh"))
# ifelse(log.data$Landings %in% c("Fish","fish","FIsh"), "Fish",)
#summary(log.data)
#names(log.data) <- gsub(" ","_",names(log.data))
#gsub(".*_",/,n)