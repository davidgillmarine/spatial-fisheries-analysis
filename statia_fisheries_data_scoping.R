library(sf)  #importing the correct library packages
library(rio)
install_formats()
library(tidyverse)


########################### Import the Data ##############################3
input.dir <- '~/OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/' #set the import directory
log.data.total <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"), #import the correct file and the page of the fisheries 
                   which = 1, skip =1)                                                            #spreadsheet and tell it where to start from the top

########################### select only the columns we are interested in and remove spaces ############
log.data <- select(log.data.total,Rec_ID:"No catch") #between Rec_ID and "No Catch are our interesting columns #rename the original data for analysis
names(log.data)    #check the names 
names(log.data) <- gsub(" ","_",names(log.data)) #get rid of the names between spaces
names(log.data) <- gsub("No.lobster/fish_etc.","Num_ind",names(log.data)) #get rid of weird lobster name
########################## Clean and tidy up the data by replaceing rogue names ###################
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

#################### Breaking out the data by year, and month, and by gear. And by fish, lobster and conch #############
trips.year <- log.data %>%    # looking at the number of trips per year 
  group_by(Year) %>%           #group by the relevent groups
  select(Year, Trip_ID) %>%    # select the groups of interest
  summarize(n_distinct(Trip_ID))   #summerize by these groups by unique Trip_ID
  
trips.month <- log.data %>%       #looking at the number of trips per month for each year
  group_by(Year, Month) %>%         
  select (Year, Month, Trip_ID) %>%  
  summarize(n_distinct(Trip_ID))     

gear.trips.year <- log.data %>%    #looking at the types of gear used, and the number used per year
  group_by(Year, Gear) %>%         
  select(Trip_ID, Year, Gear) %>%  
  summarize(n_distinct(Trip_ID))   

gear.trips.month <- log.data %>%   #looking at the types of gear used, and the amount used per month
  group_by(Year, Month, Gear) %>%   
  select(Year, Month, Trip_ID, Gear) %>% 
  summarize(n_distinct(Trip_ID))    

####################### looking at how much fish was caught and by what type of gear ################
fish.weight.year <- log.data %>%   # looking at the amount of fish caught per year
  group_by(Year, Landings)%>%      #group by the relevent groups
  filter(Landings=="Fish") %>%     # sort the data just by fish
  select(Year,Landings,`Weight_(Lbs)`) %>%   #select the groups of interest
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))   #summerize by these groups by unique Trip_ID and remove NAs

fish.weight.month <- log.data %>%     # looking at the amount of fish caught per year
  group_by(Year, Month, Landings)%>%  
  filter(Landings=="Fish") %>%         
  select(Year, Month, Landings,`Weight_(Lbs)`) %>%   
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))  

fish.gear<- log.data %>%       # looking at the amount of fish caught per gear
  group_by(Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.year <- log.data %>%      # looking at the amount of fish caught per gear per year
  group_by(Year, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Year,Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.month <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  select(Year,Month,Landings, Gear, `Weight_(Lbs)`) %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

##################### looking at how much lobster was caught and by what type of gear ################

lobster.number.year <- log.data %>%    #looking at the amount of lobster caught per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year, Landings, Num_ind)%>% 
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.number.month <- log.data %>%    #looking at the amount of lobster caught per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year, Month, Landings, Num_ind)%>% 
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))
 
lobster.gear <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Landings, Gear,Num_ind)%>% 
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.year <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per year
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year,Landings, Gear,Num_ind)%>% 
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.month <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per month
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month,Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year,Month,Landings, Gear,Num_ind)%>% 
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

##################looking at how much queen conch was caught and by what type of gear ################

conch.number.year <- log.data %>%    #looking at the amount of queen conch caught per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings) %>%
  filter(Landings=="Queen Conch") %>%
  select(Year, Landings, Num_ind)%>% 
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.number.month <- log.data %>%    #looking at the amount of queen conch caught per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings) %>%
  filter(Landings=="Queen Conch") %>%
  select(Year, Month,Landings, Num_ind)%>% 
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear <- log.data %>%    #looking at the amount of queen conch caught per gear type in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  select(Landings,Gear, Num_ind)%>% 
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear.year <- log.data %>%    #looking at the amount of queen conch caught per gear type per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  select(Year, Landings,Gear, Num_ind)%>% 
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear.month <- log.data %>%    #looking at the amount of queen conch caught per gear type per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  select(Year, Month, Landings, Gear, Num_ind)%>% 
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))
  
  
####################### reading logbook entries for fish, lobster, and conch #############
# reading in the fish, lobster and conch sample pages of the log book and replacing unusual characters

log.data.Fish <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"), 
                         which = 2, skip =0, .name_repair="universal")   #import sheet for fish data
names(log.data.Fish)  #check the names 

log.data.Lobster <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"),
                         which = 3, skip =0, .name_repair="universal",col_types=c("numeric")) # import sheet for lobster data
names(log.data.Lobster) #check names

log.data.Conch <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"),
                         which = 4, skip =0,.name_repair="universal") #import sheet for conch data
names(log.data.Conch)   #check names

####################### cleaning logbooks and renaming ####################################

log.data.F <- log.data.Fish %>% #create the join data and filter it by year to limit the observations
  filter(!is.na(Year))
names(log.data.F)

log.data.L <-log.data.Lobster %>% # create the join data and filter it by year to limit the observations
  filter(!is.na(Year))
names(log.data.L)

log.data.C <- log.data.Conch %>% #create the join data and filter it by year to limit the observations
  rename(Trip_ID = Rec_ID) %>% # rename this variable
  filter(!is.na(Year))
names(log.data.C)


##################################### Beginning to filter by species per year and month ###########################
unique(log.data.F$Species_latin_name)    

distinct.species.year <- log.data.F %>% # looking at the number of distinct species per year
  group_by(Year) %>% 
  summarize(n_distinct(Species_latin_name)) #looking at how many distinct species were caught per year

species.year <- log.data.F %>% # looking at number of individuals per species per year 
  group_by(Year) %>%
  select(Year, Species_latin_name) %>%
  count(Species_latin_name, name = "Count") # counting the number of individuals per species per year

distinct.species.month <- log.data.F %>% # looking at the number of distinct species per month
  group_by(Year, Month) %>%
  select(Year, Month, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))  #looking at how many distinct species were caught per month 

species.month <- log.data.F %>% # looking at number of individuals per species per month
  group_by(Year, Month) %>%
  select(Year, Month,Species_latin_name) %>%
  count(Species_latin_name, name = "Count") # counting the number of individuals per species per month

####################### filtering by species per year per gear and per month per gear ###############
distinct.species.gear <- log.data.F %>% # looking at the number of distinct species per year
  group_by(Gear) %>%
  summarize(n_distinct(Species_latin_name))

distinct.species.year.gear <- log.data.F %>% # looking at the number of distinct species per year per gear
  group_by(Year, Gear) %>%
  select(Year,Gear, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.year.gear <- log.data.F %>% # looking at number of individuals per species per year per gear
  group_by( Gear, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

distinct.species.month.gear <- log.data.F %>% # looking at the number of distinct species per month per gear
  group_by(Year, Month, Gear) %>%
  select(Year, Month, Gear, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.month.gear <- log.data.F %>% # looking at number of individuals per species per month per gear
  group_by(Year, Month, Gear) %>%
  select(Year, Month,Gear,Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

###################################### Zone Analysis For Fish ##########################################

zones.fish <- log.data %>% #rename vacriable to zone.fish
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Weight_(Lbs)`) %>% #select relevent variables
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% #mutate and keep new variable as n.zones
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% #bring these values together, and sum and rename them
  filter(!is.na(zone_id)) %>%     #filter by zone ID
  mutate(weight.per.zone=`Weight_(Lbs)`/n.zones) %>%  #mutate and keep new variable as weight per zone
  filter(Landings=="Fish")%>%     # filter for only fish
  arrange(Trip_ID)                # arrange by unique trip ID for clarity
head(zones.fish)   #check to make sure it was ordered properly

#amount of fish per zone per year
zone.fish.year <- zones.fish %>% #rename variable to zone.fish.year for analysis
 group_by(Year,Landings,zone_id)%>%  #group by the year, landings, and the zone ID
 summarize(weight.total=sum(weight.per.zone,na.rm = T)) #summerize by the total amount fo fish caught in that zone for that year
head(zone.fish.year)

#amount of fish per zone per month
zone.fish.month <- zones.fish %>% 
  group_by(Year,Month,Landings,zone_id)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
head(zone.fish.month)

#amount of fish per zone per year per gear
zone.fish.gear.year<-zones.fish %>%
  group_by(Year, Landings, Gear, zone_id) %>%
  select(Year, Landings, Gear, zone_id, weight.per.zone)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
head(zone.fish.gear.year)  

#amount of fish per zone per month per gear
zone.fish.gear.month<-zones.fish %>%
  group_by(Year, Month, Landings, Gear, zone_id) %>%
  select(Year, Month, Landings, Gear, zone_id, weight.per.zone)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T))
head(zone.fish.gear.month)  

#number of trips for fish per zone per year
zone.fish.trips.year <- zones.fish %>% 
  group_by(Year,Landings,zone_id)%>% 
  select(Year, Landings, Trip_ID,zone_id) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
head(zone.fish.trips.year)

#number of trips for fish per zone per month
zone.fish.trips.monthly <- zones.fish %>% 
  group_by(Year,Month,Landings,zone_id)%>% 
  select(Year, Month, Landings, Trip_ID,zone_id) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
head(zone.fish.trips.monthly)

#number of trips for fish per zone per year per gear
zone.fish.trips.gear.year <- zones.fish %>% 
  group_by(Year,Landings,zone_id, Gear)%>% 
  select(Year, Landings, Trip_ID,zone_id, Gear) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
head(zone.fish.trips.gear.year)

#number of trips for fish per zone per month per gear
zone.fish.trips.gear.monthly <- zones.fish %>% 
  group_by(Year,Month, Landings,zone_id, Gear)%>% 
  select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
  summarize(Num.Trips=n_distinct(Trip_ID))
head(zone.fish.trips.gear.monthly)

#amount of fish caught per zone for each unique trip ID for each year
#zone.fish.uni.trips.year <- zones.fish%>% 
 #group_by(Year,Landings,zone_id, Trip_ID)%>% 
 #filter(Landings=="Fish") %>%
  #select(Year,Landings,zone_id, Trip_ID, weight.per.zone)%>%
 #summarize(weight.total=sum(weight.per.zone,na.rm = T))
#head(zone.fish.uni.trips.year)

#amount of fish caught per zone for each unique trip ID for each month
#zone.fish.uni.trips.monthly <- zones.fish%>% 
#  group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
#  summarize(weight.total=sum(weight.per.zone,na.rm = T))
#head(zone.fish.uni.trips.monthly)

#joined tables showing amount of fish caught for total number of trips per zone per year and the average amount of fish per trip
joined.zone.fish.trips.year <- zone.fish.year %>% # join the log book and the fish data by unique Trip ID
  left_join(zone.fish.trips.year,by = c("Year", "Landings", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
head(joined.zone.fish.trips.year)

#joined tables showing amount of fish caught for trips per zone by gear type per year and the average amount of fish per trip
joined.zone.fish.trips.gear.year <- zone.fish.gear.year %>% # join the log book and the fish data by unique Trip ID
  left_join(zone.fish.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
head(joined.zone.fish.trips.gear.year)

#joined tables showing amount of fish caught for total number of trips per zone per month and the average amount of fish per trip
joined.zone.fish.trips.monthly <- zone.fish.month %>% # join the log book and the fish data by unique Trip ID
  left_join(zone.fish.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
head(joined.zone.fish.trips.monthly)

#joined tables showing amount of fish caught for trips per zone by gear type per month
joined.zone.fish.trips.gear.monthly <- zone.fish.gear.month %>% # join the log book and the fish data by unique Trip ID
  left_join(zone.fish.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
  mutate(avg.fish.per.trip=weight.total/Num.Trips)
head(joined.zone.fish.trips.gear.monthly)

################################## Zone Analysis for Lobsters ####################################
#lobsters are done as number of individuals since that is how they were recorded
#except for a few select records potencially accidentally done by weight 

names(log.data) #check the names for the data as a whole
zones.lob <- log.data %>% #rename the lobster zones from the log data
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear, `Num_ind`) %>% #select the correct variables
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
  filter(!is.na(zone_id))%>%
  mutate(Num_ind=as.numeric(Num_ind))%>%
  mutate(ind.per.zone= Num_ind/n.zones) %>% 
  filter(Landings=="Spiny Lobster")%>%
  arrange(Trip_ID)
 head(zones.lob) #check your work

 #amount of lobsters per zone per year
 zone.lob.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.lob.year)
 
 #amount of lobsters per zone per month
 zone.lob.month <- zones.lob %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.lob.month)
 
 #amount of lobsters per zone per year per gear
 zone.lob.gear.year<-zones.lob %>%
   group_by(Year, Landings, Gear, zone_id) %>%
   select(Year, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.lob.gear.year)  
 
 #amount of lobsters per zone per month per gear
 zone.lob.gear.month<-zones.lob %>%
   group_by(Year, Month, Landings, Gear, zone_id) %>%
   select(Year, Month, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.lob.gear.month)  
 
 #number of trips for lobsters per zone per year
 zone.lob.trips.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id)%>% 
   select(Year, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.lob.trips.year)
 
 #number of trips for lobsters per zone per month
 zone.lob.trips.monthly <- zones.lob %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   select(Year, Month, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.lob.trips.monthly)
 
 #number of trips for lobsters per zone per year per gear
 zone.lob.trips.gear.year <- zones.lob %>% 
   group_by(Year,Landings,zone_id, Gear)%>% 
   select(Year, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.lob.trips.gear.year)
 
 #number of trips for lobsters per zone per month per gear
 zone.lob.trips.gear.monthly <- zones.lob %>% 
   group_by(Year,Month, Landings,zone_id, Gear)%>% 
   select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.lob.trips.gear.monthly)
 
 #number of lobster caught per zone for each unique trip ID for each year
 #zone.lob.uni.trips.year <- zones.lob%>% 
  # group_by(Year,Landings,zone_id, Trip_ID)%>% 
  # summarize(ind.total=sum(ind.per.zone,na.rm = T))
 #head(zone.lob.uni.trips.year)
 
 #number of lobsters caught per zone for each unique trip ID for each month
 #zone.lob.uni.trips.monthly <- zones.lob%>% 
  # group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
  # summarize(ind.total=sum(ind.per.zone,na.rm = T))
 #head(zone.lob.uni.trips.monthly)
 
 #joined tables showing amount of lobsters caught for total number of trips per zone per year and the average number of lobsters per trip
 joined.zone.lob.trips.year <- zone.lob.year %>% # join the log book and the lobsters data by unique Trip ID
   left_join(zone.lob.trips.year,  by = c("Year", "Landings", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 head(joined.zone.lob.trips.year)
 
 #joined tables showing amount of lobsters caught for trips per zone by gear type per year
 joined.zone.lob.trips.gear.year <- zone.lob.gear.year %>% # join the log book and the lobsters data by unique Trip ID
   left_join(zone.lob.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 head(joined.zone.lob.trips.gear.year)
 
 #joined tables showing amount of lobsters caught for total number of trips per zone per month
 joined.zone.lob.trips.monthly <- zone.lob.month %>% # join the log book and the fish data by unique Trip ID
   left_join(zone.lob.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 head(joined.zone.lob.trips.monthly)
 
 #joined tables showing amount of lobsters caught for trips per zone by gear type per month
 joined.zone.lob.trips.gear.monthly <- zone.lob.gear.month %>% # join the log book and the fish data by unique Trip ID
   left_join(zone.lob.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.lob.per.trip=ind.total/Num.Trips)
 head(joined.zone.lob.trips.gear.monthly)

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
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.conch.year)
 
 #amount of Queen Conch per zone per month
 zone.conch.month <- zones.conch %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.conch.month)
 
 #amount of Queen Conch per zone per year per gear
 zone.conch.gear.year<-zones.conch %>%
   group_by(Year, Landings, Gear, zone_id) %>%
   select(Year, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.conch.gear.year)  
 
 #amount of Queen Conch per zone per month per gear
 zone.conch.gear.month<-zones.conch %>%
   group_by(Year, Month, Landings, Gear, zone_id) %>%
   select(Year, Month, Landings, Gear, zone_id, ind.per.zone)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T))
 head(zone.conch.gear.month)  
 
 #number of trips for Queen Conch per zone per year
 zone.conch.trips.year <- zones.conch %>% 
   group_by(Year,Landings,zone_id)%>% 
   select(Year, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.conch.trips.year)
 
 #number of trips for Queen Conch per zone per month
 zone.conch.trips.monthly <- zones.conch %>% 
   group_by(Year,Month,Landings,zone_id)%>% 
   select(Year, Month, Landings, Trip_ID,zone_id) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.conch.trips.monthly)
 
 #number of trips for Queen Conch per zone per year per gear
 zone.conch.trips.gear.year <- zones.conch %>% 
   group_by(Year,Landings,zone_id, Gear)%>% 
   select(Year, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.conch.trips.gear.year)
 
 #number of trips for Queen Conch per zone per month per gear
 zone.conch.trips.gear.monthly <- zones.conch %>% 
   group_by(Year,Month, Landings,zone_id, Gear)%>% 
   select(Year, Month, Landings, Trip_ID,zone_id, Gear) %>%
   summarize(Num.Trips=n_distinct(Trip_ID))
 head(zone.conch.trips.gear.monthly)
 
 #number of Queen Conch caught per zone for each unique trip ID for each year
 #zone.conch.uni.trips.year <- zones.conch%>% 
#   group_by(Year,Landings,zone_id, Trip_ID)%>% 
 #  summarize(ind.total=sum(ind.per.zone,na.rm = T))
# head(zone.conch.uni.trips.year)
 
 #number of Queen Conch caught per zone for each unique trip ID for each month
# zone.conch.uni.trips.monthly <- zones.conch%>% 
 #  group_by(Year,Month, Landings,zone_id, Trip_ID)%>% 
 #  summarize(ind.total=sum(ind.per.zone,na.rm = T))
 #head(zone.conch.uni.trips.monthly)
 
 #joined tables showing amount of Queen Conch caught for total number of trips per zone per year
 joined.zone.conch.trips.year <- zone.conch.year %>% # join the log book and the conch data by unique Trip ID
   left_join(zone.conch.trips.year, by = c("Year", "Landings", "zone_id") )%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 head(joined.zone.conch.trips.year)
 
 #joined tables showing amount of Queen Conch caught for trips per zone by gear type per year
 joined.zone.conch.trips.gear.year <- zone.conch.gear.year %>% # join the log book and the conch data by unique Trip ID
   left_join(zone.conch.trips.gear.year, by = c("Year", "Landings", "Gear", "zone_id"))%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 head(joined.zone.conch.trips.gear.year)
 
 #joined tables showing amount of Queen Conch caught for total number of trips per zone per month
 joined.zone.conch.trips.monthly <- zone.conch.month %>% # join the log book and the fish data by unique Trip ID
   left_join(zone.conch.trips.monthly, by = c("Year", "Month", "Landings", "zone_id"))%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 head(joined.zone.conch.trips.monthly)
 
 #joined tables showing amount of Queen Conch caught for trips per zone by gear type per month
 joined.zone.conch.trips.gear.monthly <- zone.conch.gear.month %>% # join the log book and the fish data by unique Trip ID
   left_join(zone.conch.trips.gear.monthly, by = c("Year", "Month", "Landings", "Gear", "zone_id") )%>%
   mutate(avg.conch.per.trip=ind.total/Num.Trips)
 head(joined.zone.conch.trips.gear.monthly)
 
 ####################################### Zone Fish Data by Species #########################
 GCRM.data.Fish <- import(paste0(input.dir,"GCRMN FISH BIOMASS DATA EUX 2018.xlsx"),
                        which = 6, skip =0, .name_repair="universal")   #import sheet for fish LW values
 names(GCRM.data.Fish) #check the names
 
 #rename this variable for consistency across other sheets and variables
 GCRM.data.Fish <-   rename(GCRM.data.Fish, Species_latin_name = scientific) 
 
 #join both the log data sheets and the detailed sample fish sheet together
 log.data.F2.join <-log.data%>%   
   mutate(Trip_ID=as.character(Trip_ID))%>% #change from numeric to a character
   filter(Landings=="Fish")%>%  #filter by just fish
   right_join(log.data.Fish) #join from the right hand side to avoid duplicating the Trip_ID
  # mutate(Z1:Z6 = )
 head(log.data.F2.join) #check to make sure it joined correctly

#join the coral reef monitoring species data to the fish data in order to assess effort by family
 fish.GCRM.join <-log.data.F2.join %>% 
   left_join(GCRM.data.Fish)%>%
   filter(!is.na(Trip_ID))
head(fish.GCRM.join) #check the top to see if it joined correctly
tail(fish.GCRM.join) #check the bottom to see if it joined correctly
 
 names(fish.GCRM.join) #check the names to get the correct names for each variable 
 
 zones.fish.species.1 <-fish.GCRM.join  %>% #join the GCRM and Log data together to get the correct names
   mutate(Trip_ID=as.numeric(Trip_ID))%>% #change from numeric to a character
   left_join(log.data, by = c("Trip_ID", "Day", "Month", "Year"))
 head(zones.fish.species.1)
 
 names(zones.fish.species.1)
 zones.fish.species.2 <-zones.fish.species.1  %>% #
   select(Trip_ID,Year,Month,Day,Gear.x, Z1.y:Z6.y, Landings.y, Species_common_name,Species_latin_name,family,Length_.cm.,FL.TL, TL2FL, a, b) %>% 
   mutate(Trip_ID=as.numeric(Trip_ID))%>% #change from numeric to a character
   filter(Landings.y =="Fish")%>%
   mutate(n.zones=rowSums(!is.na(select(., Z1.y:Z6.y)))) %>% 
   #mutate(n.fish.zone = rowSums(.,n_distinct(Species_latin_name)))%>%
   gather(key="zone.total",value="zone_id",Z1.y:Z6.y) %>% 
   filter(!is.na(zone_id)) %>% 
   #filter(!is.na(Species_latin_name)) %>%
   #mutate(weight.per.zone=Weight_.lbs./n.zones)%>%
   mutate(ind.fish.weight = ((a*Length_.cm.)^b)*TL2FL)
  head(zones.fish.species.2)
 
 #types of fish per zone per year
 zone.species.year <- zones.fish.species %>% 
   group_by(Year,zone_id,Species_latin_name)%>%
   count(Species_latin_name, name = "Num.ind")
   #select(Year,zone_id,Species_latin_name, Num.ind)
   #mutate(fish.weight.total= Num.ind*ind.fish.weight)
   #summarize(n_distinct(Species_latin_name))
 head(zone.species.year)
 
 zone.species.monthly <- zones.fish.species %>% 
   group_by(Year,Month, zone_id,Species_latin_name)%>%
   count(Species_latin_name, name = "Num.ind")
 #summarize(weight.total=sum(weight.per.zone,na.rm = T))
 head(zone.species.monthly)
 
 zone.species.weight.year <- zones.fish.species %>% 
   group_by(Year,zone_id,Species_latin_name, ind.fish.weight)%>%
 #count(Species_latin_name, name = "Num.ind")%>%
 #select(Year,zone_id,Species_latin_name, Num.ind, ind.fish.weight)%>%
 #mutate(fish.weight.total= Num.ind*ind.fish.weight)
 summarize(fish.weight.total=sum(ind.fish.weight))
 #count(Species_latin_name, name = "Num.ind")
 head(zone.species.weight.year)
 
 ################################# adding zone areas and initial map making #####################
 input.dir <- "/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/"
 
 allfiles <- list.files(input.dir,recursive = T, full.names = T) 
 
 # Select kml files with 1) digit then 1 letter, 2) digit then 2 letters, 3) digit then .kml, 4) digit then buffer
 file.list <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
                grep("FAD_[0-9]_[a-z]*",allfiles,value = T))
 
 zone.ind <- st_read(file.list[1])
 zone.ind$zone_id <- as.character(gsub("Zone_","", zone.ind$Name)) # removes all non-digit characters
 #zone.ind$zone_id <- as.character(gsub(".kml","",zone.ind$Name)) # removes all non-digit characters
 
 for (i in (2:length(file.list))) {
   # retrieve kml 
   X <- st_read(file.list[i]) 
   # extract zone_id from file name 
   X$zone_id <- as.character(gsub("Zone_","", X$Name)) # removes all non-digit characters
   #X$zone_id <- as.character(gsub(".kml","",X$zone_id)) # removes all non-digit characters
   # combine X to the previous shp
   zone.ind <- rbind(zone.ind,X) 
 }
 zone.ind$zone_id <- gsub("/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/","",zone.ind$zone_id)
 
 # head map
 plot(st_geometry(zone.ind))
 
 # get area
 zone.ind$area <- st_area(zone.ind)
 zone.area <-zone.ind%>%
   rename(area_m2 = area)
 zone.area$area_m2<-gsub(" [m^2]", "", zone.area$area_m2)
 
 # Example join
 fishing.zones <-zone.fish.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind2 <- zone.area %>% 
   left_join(fishing.zones, by = "zone_id")%>%
   mutate(area_m2 = as.numeric(area_m2))%>%
   mutate(area_km=area_m2/1000000)%>%
   mutate (fishing_pressure=weight.total/area_km)%>%
   select(Name,zone_id,Year, Landings,area_m2,area_km,weight.total,fishing_pressure, geometry)%>%
   rename(weight_lb=weight.total)
 