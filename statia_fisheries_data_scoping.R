library(sf)  #importing the correct library packages
library(rio)
install_formats()
library(tidyverse)
library(ggplot2)
install.packages("cowplot")
library(cowplot)


###################################### Import the Data ##############################
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
  summarize(n_distinct(Trip_ID))   #summerize by these groups by unique Trip_ID
  
trips.month <- log.data %>%       #looking at the number of trips per month for each year
  group_by(Year, Month) %>%         
  summarize(n_distinct(Trip_ID))     

gear.trips.year <- log.data %>%    #looking at the types of gear used, and the number used per year
  group_by(Year, Gear) %>%         
  summarize(n_distinct(Trip_ID))   

gear.trips.month <- log.data %>%   #looking at the types of gear used, and the amount used per month
  group_by(Year, Month, Gear) %>%   
  summarize(n_distinct(Trip_ID))    

####################### looking at how much fish was caught and by what type of gear ################
fish.weight.year <- log.data %>%   # looking at the amount of fish caught per year
  group_by(Year, Landings)%>%      #group by the relevent groups
  filter(Landings=="Fish") %>%     # sort the data just by fish
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))   #summerize by these groups by unique Trip_ID and remove NAs

fish.weight.month <- log.data %>%     # looking at the amount of fish caught per year
  group_by(Year, Month, Landings)%>%  
  filter(Landings=="Fish") %>%         
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))  

fish.gear<- log.data %>%       # looking at the amount of fish caught per gear
  group_by(Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.year <- log.data %>%      # looking at the amount of fish caught per gear per year
  group_by(Year, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

fish.gear.month <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(`Weight_(Lbs)`, na.rm=TRUE))

##################### looking at how much lobster was caught and by what type of gear ################

lobster.number.year <- log.data %>%    #looking at the amount of lobster caught per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.number.month <- log.data %>%    #looking at the amount of lobster caught per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))
 
lobster.gear <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.year <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per year
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.month <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per month
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month,Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

##################looking at how much queen conch was caught and by what type of gear ################

conch.number.year <- log.data %>%    #looking at the amount of queen conch caught per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings) %>%
  filter(Landings=="Queen Conch") %>%
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.number.month <- log.data %>%    #looking at the amount of queen conch caught per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings) %>%
  filter(Landings=="Queen Conch") %>%
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear <- log.data %>%    #looking at the amount of queen conch caught per gear type in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear.year <- log.data %>%    #looking at the amount of queen conch caught per gear type per year in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))

conch.gear.month <- log.data %>%    #looking at the amount of queen conch caught per gear type per month in terms of individuals
  mutate(Num_ind=as.numeric(Num_ind))%>%  
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
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
  count(Species_latin_name, name = "Count") # counting the number of individuals per species per year

distinct.species.month <- log.data.F %>% # looking at the number of distinct species per month
  group_by(Year, Month) %>%
  summarize(n_distinct(Species_latin_name))  #looking at how many distinct species were caught per month 

species.month <- log.data.F %>% # looking at number of individuals per species per month
  group_by(Year, Month) %>%
  count(Species_latin_name, name = "Count") # counting the number of individuals per species per month

#join with GCRM table and look at this for the family level and do the same thing donw below with the gear types
GCRM.data.Fish <- import(paste0(input.dir,"GCRMN FISH BIOMASS DATA EUX 2018.xlsx"),
                         which = 6, skip =0, .name_repair="universal")   #import sheet for fish LW values
names(GCRM.data.Fish) #check the names

#rename this variable for consistency across other sheets and variables
GCRM.data.Fish <-   rename(GCRM.data.Fish, Species_latin_name = scientific)

family.year <-log.data.F%>%
  left_join(GCRM.data.Fish)%>%
  group_by(Year)%>%
  count(family, name = "Count")
head(family.year)
  

####################### filtering by species per year per gear and per month per gear ###############
distinct.species.gear <- log.data.F %>% # looking at the number of distinct species per year
  group_by(Gear) %>%
  summarize(n_distinct(Species_latin_name))

distinct.species.year.gear <- log.data.F %>% # looking at the number of distinct species per year per gear
  group_by(Year, Gear) %>%
  summarize(n_distinct(Species_latin_name))

species.year.gear <- log.data.F %>% # looking at number of individuals per species per year per gear
  group_by( Gear, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

distinct.species.month.gear <- log.data.F %>% # looking at the number of distinct species per month per gear
  group_by(Year, Month, Gear) %>%
  summarize(n_distinct(Species_latin_name))

species.month.gear <- log.data.F %>% # looking at number of individuals per species per month per gear
  group_by(Year, Month, Gear) %>%
  count(Species_latin_name, name = "Count")

family.year.gear <-log.data.F%>%
  left_join(GCRM.data.Fish)%>%
  group_by(Year, Gear)%>%
  count(family, name = "Count")
head(family.year.gear)

###################################### Zone Analysis For Fish ##########################################

zones.fish <- log.data %>% #rename variable to zone.fish
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Weight_(Lbs)`) %>% #select relevent variables
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% #mutate and keep new variable as n.zones
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% #bring these values together, and sum and rename them
  filter(!is.na(zone_id)) %>%     #filter by zone ID and remove NAs
  mutate(weight.per.zone=`Weight_(Lbs)`/n.zones) %>%  #mutate and keep new variable as weight per zone
  filter(Landings=="Fish")%>%     # filter for only fish
  arrange(Trip_ID)                # arrange by unique trip ID for clarity
head(zones.fish)   #check to make sure it was ordered properly

#amount of fish per zone per year, the number of trips, and the average number of fish per trip
zone.fish.year <- zones.fish %>% #rename variable to zone.fish.year for analysis
 group_by(Year,zone_id)%>%  #group by the year, landings, and the zone ID
 summarize(weight.total=sum(weight.per.zone,na.rm = T),
           Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)#summerize by the total amount fo fish caught in that zone for that year
head(zone.fish.year)

#amount of fish per zone per month, the number of trips, and the average number of fish per trip
zone.fish.month <- zones.fish %>% 
  group_by(Year,Month,zone_id)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)#summerize by the total amount fo fish caught in that zone for that month
head(zone.fish.month)

#amount of fish per zone per year per gear, the number of trips using that gear, and the average weight using that gear per trip
zone.fish.gear.year<-zones.fish %>%
  group_by(Year, Gear, zone_id) %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)
head(zone.fish.gear.year)  

#amount of fish per zone per month per gear, the number of trips using that gear, and the average weight using that gear per trip
zone.fish.gear.month<-zones.fish %>%
  group_by(Year, Month,Gear, zone_id) %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)
head(zone.fish.gear.month)  

################################## Zone Analysis for Lobsters ####################################
#lobsters are done as number of individuals since that is how they were recorded
#except for a few select records potentially, accidentally done by weight 

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

 #number of lobsters per zone per year, the number of trips per zone, and the average number of lobster caught per trip
 zone.lob.year <- zones.lob %>% 
   group_by(Year,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.lob.year)
 
 
 #number of lobsters per zone per month, the number of trips per zone, and the average number of lobsters caught per trip
 zone.lob.month <- zones.lob %>% 
   group_by(Year,Month,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.lob.month)
 
 #number of lobsters per zone per year per gear, the number of trips, and the average number of lobsters caught per trip
 zone.lob.gear.year<-zones.lob %>%
   group_by(Year, Gear, zone_id) %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.lob.gear.year)  
 
 #number of lobsters per zone per month per gear, the number of trips, and the average number of lobsters caught per trip
 zone.lob.gear.month<-zones.lob %>%
   group_by(Year, Month, Gear, zone_id) %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.lob.gear.month)  
 

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
 
 #number of Queen Conch per zone per year, the number of trips, and the average number of conch caught per trip
 zone.conch.year <- zones.conch %>% 
   group_by(Year,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.conch.year)
 
 #number of Queen Conch per zone per month, the number of trips, and the average number of conch caught per trip
 zone.conch.month <- zones.conch %>% 
   group_by(Year,Month,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.conch.month)
 
 #amount of Queen Conch per zone per year per gear, the number of trips, and the average number of conch caught per trip
 zone.conch.gear.year<-zones.conch %>%
   group_by(Year, Gear, zone_id) %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.conch.gear.year)  
 
 #amount of Queen Conch per zone per month per gear, the number of trips, and the average number of conch caught per trip
 zone.conch.gear.month<-zones.conch %>%
   group_by(Year, Month, Gear, zone_id) %>%
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.conch.gear.month)  
 
 ####################################### Zone Fish Data by Species #########################
 
 #join both the log data sheets and the detailed sample fish sheet together
 log.data.F2.join <-log.data%>%   
   mutate(Trip_ID=as.character(Trip_ID))%>% #change from numeric to a character
   filter(Landings=="Fish")%>%  #filter by just fish
   right_join(log.data.F, by = c("Trip_ID", "Day", "Month", "Year", "Gear")) #join from the right hand side to avoid duplicating the Trip_ID
  # mutate(Z1:Z6 = )
 head(log.data.F2.join) #check to make sure it joined correctly

#join the coral reef monitoring species data to the fish data in order to assess effort by family
 fish.GCRM.join <-log.data.F2.join %>% 
   left_join(GCRM.data.Fish, by = "Species_latin_name" )%>%
   filter(!is.na(Trip_ID))
head(fish.GCRM.join) #check the top to see if it joined correctly
tail(fish.GCRM.join) #check the bottom to see if it joined correctly
 
names(fish.GCRM.join) #check the names to get the correct names for each variable 
 
zones.fish.species.1 <- fish.GCRM.join  %>% #
   select(Trip_ID,Year,Month,Day,Gear, Z1:Z6, Species_common_name,Species_latin_name,family,Length_.cm.,FL.TL, TL2FL, a, b) %>% 
   mutate(Trip_ID=as.numeric(Trip_ID))%>% #change from numeric to a character
   mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
   gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
   filter(!is.na(zone_id)) %>%
   filter(!is.na(Species_latin_name))%>%
   mutate(ind.fish.weight = ((a*Length_.cm.)^b)*TL2FL)%>%
   mutate(weight.per.zone=ind.fish.weight/n.zones)%>%
  arrange(Trip_ID)
  head(zones.fish.species.1)
  
 #types of fish per zone per year, the weight of the fish, the number of fish, and the avg. fish weight
 zone.species.year <- zones.fish.species.1 %>% 
   group_by(Year,zone_id,Species_latin_name)%>%
   summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
             total.weight=sum(ind.fish.weight, na.rm=T),
             Species_latin_name_n=n() )%>%
   mutate(avg.fish.weight=total.weight/Species_latin_name_n)%>%
   rename(n.ind = Species_latin_name_n)
 head(zone.species.year)
 
 
 #types of fish per zone per month, the weight of the fish, the number of fish, and the avg. fish weight
 zone.species.month <- zones.fish.species.1 %>% 
   group_by(Year,Month,zone_id,Species_latin_name)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              Species_latin_name_n=n() )%>%
    mutate(avg.fish.weight=total.weight/Species_latin_name_n)%>%
    rename(n.ind = Species_latin_name_n)
 head(zone.species.month)
 
 #family breakdown per zone per year, the weight of the fish, the number of fish, and the avg. fish weight
 zone.family.year <- zones.fish.species.1 %>% 
   group_by(Year,zone_id,family)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
             family_n=n())%>%
   mutate(avg.fish.weight=total.weight/family_n)%>%
   rename(n.ind = family_n)
 head(zone.family.year)
 
 # family breakdown of the amount of fish caught for each family, the number of individuals, 
 # and the average fish weight per year 
 family.year <- zones.fish.species.1 %>% 
    group_by(Year,family)%>%
    summarize(total.weight=sum(ind.fish.weight, na.rm=T),
              family_n=n())%>%
    mutate(avg.fish.weight=total.weight/family_n)%>%
    rename(n.ind = family_n)
 head(family.year)
 
 #family breakdown per zone per month, the weight of the fish, the number of fish, and the avg. fish weight
 zone.family.month <- zones.fish.species.1 %>% 
   group_by(Year,Month, zone_id,family)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              family_n=n())%>%
    mutate(avg.fish.weight=total.weight/family_n)%>%
    rename(n.ind = family_n)
 head(zone.family.month)
 
 #types of fish per zone per year, the weight of the fish, the number of fish, and the avg. fish weight
 zone.species.year.gear <- zones.fish.species.1 %>% 
   group_by(Year,Gear,zone_id,Species_latin_name)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              Species_latin_name_n=n() )%>%
    mutate(avg.fish.weight=total.weight/Species_latin_name_n)%>%
    rename(n.ind = Species_latin_name_n)
 head(zone.species.year.gear)
 
 #types of fish per zone per month, the weight of the fish, the number of fish, and the avg. fish weight
 zone.species.month.gear <- zones.fish.species.1 %>% 
   group_by(Year,Month,Gear, zone_id,Species_latin_name)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              Species_latin_name_n=n() )%>%
    mutate(avg.fish.weight=total.weight/Species_latin_name_n)%>%
    rename(n.ind = Species_latin_name_n)
 head(zone.species.month.gear)
 
 #family breakdown per zone per year, the weight of the fish, the number of fish, and the avg. fish weight
 zone.family.year.gear <- zones.fish.species.1 %>% 
   group_by(Year,Gear,zone_id,family)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              family_n=n())%>%
    mutate(avg.fish.weight=total.weight/family_n)%>%
    rename(n.ind = family_n)
 head(zone.family.year.gear)
 
 #family breakdown per zone per month, the weight of the fish, the number of fish, and the avg. fish weight
 zone.family.month.gear <- zones.fish.species.1 %>% 
   group_by(Year,Month,Gear, zone_id,family)%>%
    summarize(zone.weight.total=sum(weight.per.zone,na.rm = T),
              total.weight=sum(ind.fish.weight, na.rm=T),
              family_n=n())%>%
    mutate(avg.fish.weight=total.weight/family_n)%>%
    rename(n.ind = family_n)
 head(zone.family.month.gear)
 
 ########################## adding zone areas and initial map making for fishing pressure #####################
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
 # head map
 plot(st_geometry(zone.ind))
 # get area
 zone.ind$area_m2 <- as.numeric(st_area(zone.ind))
 
 # Example join
 fishing.zones <-zone.fish.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind2 <- zone.ind %>% 
   left_join(fishing.zones, by = "zone_id")%>%
   #mutate(area_m2 = as.numeric(area_m2))%>%
   mutate(area_km2 = area_m2/1000000)%>%
   mutate (fishing_pressure=weight.total/area_km2)%>%
   select(Name,zone_id,Year,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
   rename(weight_lb=weight.total)
 
 zone.ind3 <- zone.ind2 %>% 
   group_by(zone_id) %>% 
   summarise(fishing_pressure=mean(fishing_pressure)) %>% 
   filter(!grepl("FAD",zone_id))
 
 # Plot fishing pressure maps
 fish.zone.2012 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2012), aes(fill = fishing_pressure)) +
   labs(title = paste0("2012"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2013), aes(fill = fishing_pressure)) +
   labs(title = paste0("2013"), x="Total landings per sqkm") +
   theme_bw() 
 fish.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2014), aes(fill = fishing_pressure)) +
   labs(title = paste0("2014"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2015), aes(fill = fishing_pressure)) +
   labs(title = paste0("2015"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2016), aes(fill = fishing_pressure)) +
   labs(title = paste0("2016"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2017), aes(fill = fishing_pressure)) +
   labs(title = paste0("2017"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2018), aes(fill = fishing_pressure)) +
   labs(title = paste0("2018"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2019), aes(fill = fishing_pressure)) +
   labs(title = paste0("2019"), x="Total landings per sqkm") +
   theme_bw()
 plot_grid(fish.zone.2012,fish.zone.2013,fish.zone.2014, fish.zone.2015, fish.zone.2016, fish.zone.2017, fish.zone.2018, fish.zone.2019)
 
 ######################### mapping lobster catch by individual count ###########################
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
 # head map
 plot(st_geometry(zone.ind))
 # get area
 zone.ind$area_m2 <- as.numeric(st_area(zone.ind))
  # Example join
 lobster.zones <-zone.lob.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind5 <- zone.ind %>% 
   left_join(lobster.zones, by = "zone_id")%>%
   #mutate(area_m2 = as.numeric(area_m2))%>%
   mutate(area_km2 = area_m2/1000000)%>%
   mutate (lobster_pressure=ind.total/area_km2)%>%
   select(Name,zone_id,Year,area_m2,area_km2,ind.total,lobster_pressure, geometry)
   #rename(weight_lb=weight.total)
 head(zone.ind5)
 
 zone.ind6 <- zone.ind5 %>% 
   group_by(zone_id) %>% 
   summarise(lobster_pressure=mean(lobster_pressure)) %>% 
   filter(!grepl("FAD",zone_id))
 
 # Plot lobster pressure maps
 lobster.zone.2012 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2012), aes(fill = lobster_pressure)) +
   labs(title = paste0("2012"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2013), aes(fill = lobster_pressure)) +
   labs(title = paste0("2013"), x="Total landings per sqkm") +
   theme_bw() 
 lobster.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2014), aes(fill = lobster_pressure)) +
   labs(title = paste0("2014"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2015), aes(fill = lobster_pressure)) +
   labs(title = paste0("2015"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2016), aes(fill = lobster_pressure)) +
   labs(title = paste0("2016"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2017), aes(fill = lobster_pressure)) +
   labs(title = paste0("2017"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2018), aes(fill = lobster_pressure)) +
   labs(title = paste0("2018"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2019), aes(fill = lobster_pressure)) +
   labs(title = paste0("2019"), x="Total landings per sqkm") +
   theme_bw()
 plot_grid(lobster.zone.2012,lobster.zone.2013,lobster.zone.2014,lobster.zone.2015,lobster.zone.2016,lobster.zone.2017,lobster.zone.2018,lobster.zone.2019)
 
 ############################### mapping conch catch by individual count ######################
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
 # head map
 plot(st_geometry(zone.ind))
 # get area
 zone.ind$area_m2 <- as.numeric(st_area(zone.ind))
 
 # Example join
 conch.zones <-zone.conch.year%>%
    mutate (zone_id = as.character(zone_id))
 
 zone.ind8 <- zone.ind %>% 
    left_join(conch.zones, by = "zone_id")%>%
    #mutate(area_m2 = as.numeric(area_m2))%>%
    mutate(area_km2 = area_m2/1000000)%>%
    mutate (conch_pressure=ind.total/area_km2)%>%
    select(Name,zone_id,Year,area_m2,area_km2,ind.total,conch_pressure, geometry)
 #rename(weight_lb=weight.total)
 head(zone.ind8)
 
 zone.ind9 <- zone.ind8 %>% 
    group_by(zone_id) %>% 
    summarise(conch_pressure=mean(conch_pressure)) %>% 
    filter(!grepl("FAD",zone_id))
 
 # Plot lobster pressure maps
 conch.zone.2012 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2012), aes(fill = conch_pressure)) +
    labs(title = paste0("2012"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2013 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2013), aes(fill = conch_pressure)) +
    labs(title = paste0("2013"), x="Total landings per sqkm") +
    theme_bw() 
 conch.zone.2014 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2014), aes(fill = conch_pressure)) +
    labs(title = paste0("2014"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2015 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2015), aes(fill = conch_pressure)) +
    labs(title = paste0("2015"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2016 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2016), aes(fill = conch_pressure)) +
    labs(title = paste0("2016"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2017 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2017), aes(fill = conch_pressure)) +
    labs(title = paste0("2017"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2018 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2018), aes(fill = conch_pressure)) +
    labs(title = paste0("2018"), x="Total landings per sqkm") +
    theme_bw()
 conch.zone.2019 <- ggplot() +
    geom_sf(data=filter(zone.ind8,Year==2019), aes(fill = conch_pressure)) +
    labs(title = paste0("2019"), x="Total landings per sqkm") +
    theme_bw()
 plot_grid(conch.zone.2012,conch.zone.2013,conch.zone.2014,conch.zone.2015,conch.zone.2016,conch.zone.2017,conch.zone.2018,conch.zone.2019)
 
 ##############################potential bar plots for family stats##############################

f.Acanthuridae<-subset(family.year, family=="Acanthuridae")

 barplot(n.ind~Year, data=f.Acanthuridae)
 
 counts <- tapply(family.year$n.ind, list(family.year$Year, family.year$family), sum)
 barplot(counts, main="Number of Individuals Caught per Family per Year",
         xlab="Scientific Family Name", ylab="Number of Individuals", ylim=c(0, 2500),
         col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
         legend = rownames(counts), beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)

 
family.year.group<-family.year[order(family.year$Year),] 
family.year.group$color[family.year.group$Year==2012] <- "red"
family.year.group$color[family.year.group$Year==2013] <- "orange"
family.year.group$color[family.year.group$Year==2014] <- "blue"
family.year.group$color[family.year.group$Year==2015] <- "darkblue"
family.year.group$color[family.year.group$Year==2016] <- "green"
family.year.group$color[family.year.group$Year==2017] <- "darkgreen"
family.year.group$color[family.year.group$Year==2018] <- "purple"
family.year.group$color[family.year.group$Year==2019] <- "deeppink"

dotchart(family.year.group$n.ind, labels=family.year.group$family,cex=.7,
         cex.axis=1.25, cex.lab =2,cex.main=2, 
         groups = family.year.group$Year, gcolor="black", color=family.year.group$color)
 
 
 
 
 
 
 
 
 
 
 
 