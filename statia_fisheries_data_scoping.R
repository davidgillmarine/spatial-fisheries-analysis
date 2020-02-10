library(sf)  #importing the correct library packages
library(rio)
install_formats()
library(ggplot2)
#install.packages("cowplot")
library(cowplot)
library(tidyverse)
#library(psych)
#library(plyr)

###################################### Import the Data ##############################
input.dir <- '~/OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/' #set the import directory
#input.dir <- 'R:/Gill/spatial-fisheries-analysis/tables/raw/' #set the import directory
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

log.data <- log.data %>% 
  mutate(Num_ind=as.numeric(Num_ind),
         Trip_ID=as.character(Trip_ID),
         `Weight_(kg)`=`Weight_(Lbs)`/2.2,
         in.park=ifelse(`max_(m)`<=30, 1,0)) 
  
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
  filter(Landings=="Fish") %>%     # sort the data just by fish
  group_by(Year)%>%      #group by the relevent groups
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))   #summerize by these groups by unique Trip_ID and remove NAs

fish.weight.month <- log.data %>%     # looking at the amount of fish caught per year
  filter(Landings=="Fish") %>%         
  group_by(Year, Month)%>%  
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))  

fish.gear<- log.data %>%       # looking at the amount of fish caught per gear
  filter(Landings =="Fish") %>%
  group_by(Gear) %>%
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))

fish.gear.year <- log.data %>%      # looking at the amount of fish caught per gear per year
  filter(Landings =="Fish") %>%
  group_by(Year, Gear) %>%
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))

fish.gear.month <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))

fish.gear.month.all <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(`Weight_(kg)`, na.rm=TRUE))

##################### looking at how much lobster was caught and by what type of gear ################

lobster.number.year <- log.data %>%    #looking at the amount of lobster caught per year in terms of individuals
  filter(Landings=="Spiny Lobster") %>%
  group_by(Year) %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.number.month <- log.data %>%    #looking at the amount of lobster caught per month in terms of individuals
  group_by(Year, Month, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))
 
lobster.gear <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type
  group_by(Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.year <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per year
  group_by(Year, Landings, Gear) %>%
  filter(Landings=="Spiny Lobster") %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.month <- log.data %>%    #looking at the amount of lobster caught in terms of individuals by gear type per month
  group_by(Year, Month,Landings, Gear) %>%
  summarize(Num_lob = sum(Num_ind, na.rm=TRUE))

lobster.gear.month.all <- log.data %>%      # looking at the amount of fish caught per gear per month
  filter(Landings=="Spiny Lobster") %>%
  group_by(Month, Gear) %>%
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
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings=="Queen Conch") %>%
  summarize(Num_conch = sum(Num_ind, na.rm=TRUE))
  
####################### reading logbook entries for fish, lobster, and conch #############
# reading in the fish, lobster and conch sample pages of the log book and replacing unusual characters

log.data.Fish <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"), 
                         which = 2, skip =0, .name_repair="universal")   #import sheet for fish data
names(log.data.Fish)  #check the names 

log.data.Lobster <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"),
                         which = 3, skip =0, .name_repair="universal") # import sheet for lobster data
names(log.data.Lobster) #check names

log.data.Conch <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"),
                         which = 4, skip =0,.name_repair="universal") #import sheet for conch data
names(log.data.Conch)   #check names

####################### cleaning logbooks and renaming ####################################

log.data.F <- log.data.Fish %>% #create the join data and filter it by year to limit the observations
  filter(!is.na(Year))%>%
  mutate(Trip_ID=as.character(Trip_ID))
names(log.data.F)

unique(log.data.F$Gear)
log.data.F$Gear <- ifelse(log.data.F$Gear %in% c("PT","Pt","pt"), "PT",log.data.F$Gear)
unique(log.data.F$Gear)

unique(log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Lactophrys polygonia"), "Acanthostracion polygonia",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Canthiderhines macrocerus"), "Cantherhines macrocerus",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Cephalophilis fulva","Epinephelus fulvus"), "Cephalopholis fulva",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Alectris ciliaris"), "Alectis ciliaris",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("caranx latus"), "Caranx latus",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Aluterus schoepfi"), "Aluterus schoepfii",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("caranx lugubris"), "Caranx lugubris",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Corypahaena hippurus"), "Coryphaena hippurus",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Epinephelus cruentatus"), "Cephalopholis cruentata",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Epinephelus stritatus"), "Epinephelus striatus",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Euthynnuss pelamis"), "Katsuwonus pelamis",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Haemulon plumierii"), "Haemulon plumieri",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Haemulon striatus"), "Haemulon striatum",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Holocanthus ciliaris"), "Holacanthus ciliaris",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Lactophrys quadricornis"), "Acanthostracion quadricornis",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Lutjanus Buccanella"), "Lutjanus buccanella",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Pteris volitans"), "Pterois volitans",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Scarus iserti"), "Scarus iseri",log.data.F$Species_latin_name)
log.data.F$Species_latin_name <- ifelse(log.data.F$Species_latin_name %in% c("Selar crumenphthalamus"), "Selar crumenophthalmus",log.data.F$Species_latin_name)

unique(log.data.F$Species_latin_name)

log.data.L <-log.data.Lobster %>% # create the join data and filter it by year to limit the observations
  filter(!is.na(Year))%>%
  mutate(Trip_ID=as.character(Trip_ID))
names(log.data.L)

log.data.C <- log.data.Conch %>% #create the join data and filter it by year to limit the observations
  mutate(Rec_ID=as.character(Rec_ID)) %>% # rename this variable
  filter(!is.na(Year))
names(log.data.C)

###################################### Zone Analysis For Fish ##########################################
zones.fish <- log.data %>% #rename variable to zone.fish
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Weight_(kg)`,`max_(m)`,in.park) %>% #select relevent variables
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% #mutate and keep new variable as n.zones
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% #bring these values together, and sum and rename them
  filter(!is.na(zone_id)) %>%     #filter by zone ID and remove NAs
  mutate(weight.per.zone=`Weight_(kg)`/n.zones) %>%  #mutate and keep new variable as weight per zone
  filter(Landings=="Fish")%>%     # filter for only fish
  arrange(Trip_ID)                # arrange by unique trip ID for clarity
head(zones.fish)   #check to make sure it was ordered properly

#amount of fish per zone per year, the number of trips, and the average number of fish per trip
zone.fish.year <- zones.fish %>% #rename variable to zone.fish.year for analysis
 group_by(Year,zone_id)%>%  #group by the year, landings, and the zone ID
 summarize(weight.total=sum(weight.per.zone,na.rm = T), #summerize by the total amount fo fish caught in that zone for that year
           Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips) #calculate the average catch per trip
head(zone.fish.year)

#amount of fish per zone per month, the number of trips, and the average number of fish per trip
zone.fish.month <- zones.fish %>% 
  group_by(Year,Month,zone_id)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)#summerize by the total amount fo fish caught in that zone for that month
head(zone.fish.month)

#amount of fish per zone per gear, the number of trips using that gear, and the average weight using that gear per trip
zone.fish.gear<-zones.fish %>%
  group_by(Gear, zone_id) %>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.weight.per.trip=weight.total/Num.Trips)
head(zone.fish.gear) 

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

#looking at the amount of fish caught within the park, aka from 0-30m in depth for each year and by zone
zone.fish.inpark.year <-zones.fish%>%
  group_by(Year, zone_id,in.park)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm=T),
            Num.Trips=n_distinct(Trip_ID))%>%
  filter(in.park==1)
head(zone.fish.inpark.year)

#looking at the amount of fish caught within the park, aka from 0-30m in depth, for each year, by gear and zone
zone.fish.inpark.gear.year <-zones.fish%>%
  group_by(Year, Gear,zone_id,in.park)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm=T),
            Num.Trips=n_distinct(Trip_ID))%>%
  filter(in.park==1)
head(zone.fish.inpark.gear.year)
################################## Zone Analysis for Lobsters ####################################
#lobsters are done as number of individuals since that is how they were recorded
#except for a few select records potentially, accidentally done by weight 

names(log.data) #check the names for the data as a whole
zones.lob <- log.data %>% #rename the lobster zones from the log data
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear, `Num_ind`,`max_(m)`,in.park) %>% #select the correct variables
  mutate(n.zones=rowSums(!is.na(select(., Z1:Z6)))) %>% 
  gather(key="zone.total",value="zone_id",Z1:Z6) %>% 
  filter(!is.na(zone_id))%>%
  mutate(ind.per.zone= Num_ind/n.zones) %>% 
  filter(Landings=="Spiny Lobster")%>%
  arrange(Rec_ID)
 head(zones.lob) #check your work

 #number of lobsters per zone per year, the number of trips per zone, and the average number of lobster caught per trip
 zone.lob.year <- zones.lob %>% 
   group_by(Year,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone, na.rm = T),
             Num.Trips=n_distinct(Trip_ID))%>%
   mutate(avg.ind.per.trip=ind.total/Num.Trips)
 head(zone.lob.year)
 
 #number of lobsters per zone per month, the number of trips per zone, and the average number of lobsters caught per trip
 zone.lob.month <- zones.lob %>% 
   group_by(Year,Month,zone_id)%>% 
   summarize(ind.total=sum(ind.per.zone,na.rm = T),
             Num.Trips=n_distinct(Trip_ID),
             mean.ind=mean(ind.total/Num.Trips, na.rm=T))%>%
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
 
 # number of lobsters caught within the marine park for each year and zone 
 zone.lob.inpark.year <-zones.lob%>%
   group_by(Year, zone_id,in.park)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm=T),
             Num.Trips=n_distinct(Trip_ID))%>%
   filter(in.park==1)
 head(zone.lob.inpark.year)
 
 # number of lobsters caught within the marine park for each year and gear type
 zone.lob.inpark.gear.year <-zones.lob%>%
   group_by(Year, Gear,zone_id,in.park)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm=T),
             Num.Trips=n_distinct(Trip_ID))%>%
   filter(in.park==1)
 head(zone.lob.inpark.gear.year)
 
##################################### Zone Analysis for Queen Conch #############################
 #Queen Conch are done as number of individuals since that is how they were recorded
 #except for a few select records potencially accidentally done by weight 
 
 names(log.data)
 zones.conch <- log.data %>% 
   select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,`Num_ind`, `max_(m)`,in.park) %>% 
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
 
 # number of Queen conch caught within the marine park for each year and zone 
 zone.conch.inpark.year <-zones.conch%>%
   group_by(Year, zone_id,in.park)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm=T),
             Num.Trips=n_distinct(Trip_ID))%>%
   filter(in.park==1)
 head(zone.conch.inpark.year)
 
 # number of Queen conch caught within the marine park for each year and gear type
 zone.conch.inpark.gear.year <-zones.conch%>%
   group_by(Year, Gear,zone_id,in.park)%>%
   summarize(ind.total=sum(ind.per.zone,na.rm=T),
             Num.Trips=n_distinct(Trip_ID))%>%
   filter(in.park==1)
 head(zone.conch.inpark.gear.year)
 
####################################### Fish Data by Species #########################
 #join with GCRM table and look at this for the family level and do the same thing donw below with the gear types
 GCRM.data.Fish <- import(paste0(input.dir,"GCRMN FISH BIOMASS DATA EUX 2018.xlsx"),
                          which = 6, skip =0, .name_repair="universal")   #import sheet for fish LW values
 
 names(GCRM.data.Fish) #check the names
 
 #rename this variable for consistency across other sheets and variables
 GCRM.data.Fish <-   GCRM.data.Fish %>% 
   rename(Species_latin_name = scientific) %>% 
   group_by(Species_latin_name,family,trophic,NOAAtrophic,Paddacktrophic) %>% 
   summarise(a=mean(a), b=mean(b), TL2FL=mean(TL2FL))
 
 # check to see if names line up
 log.data.F.antijoin <- log.data.F %>% 
   select(Species_common_name, Species_latin_name) %>% 
   anti_join(GCRM.data.Fish, by = "Species_latin_name") %>% 
   distinct(Species_latin_name)
 
#join the coral reef monitoring species data to the fish data in order to assess effort by family
 fish.GCRM.join <-log.data.F %>% 
   left_join(GCRM.data.Fish, by = "Species_latin_name" )%>%
   filter(!is.na(Species_latin_name))
head(fish.GCRM.join) #check the top to see if it joined correctly
tail(fish.GCRM.join) #check the bottom to see if it joined correctly
names(fish.GCRM.join) #check the names to get the correct names for each variable 
unique(fish.GCRM.join$Gear) #make sure that the gears are still correct with no mispellings

fish.species <- fish.GCRM.join  %>% #create a data set that has each individual fish weight calculated and organized by Rec_ID
  select(Sample_ID,Rec_ID,Year,Month,Day,Gear, Species_common_name,Species_latin_name,family,
         Length_.cm.,FL.TL, TL2FL, a, b, trophic) %>% 
  mutate(Rec_ID=as.numeric(Rec_ID))%>% #change from character to numeric 
  mutate(ind.fish.weight = ifelse(FL.TL=="TL",((a*((Length_.cm.*TL2FL)^b))/1000),(a*((Length_.cm.)^b))/1000))%>%
  group_by(Sample_ID)%>%
  mutate(trip.wt=sum(ind.fish.weight,na.rm = T),
         rec.num=n(),
         species.num=n_distinct(Species_latin_name),
         pct.wt = (ind.fish.weight/trip.wt)*100,
         prop.wt=ind.fish.weight/trip.wt)
head(fish.species)
  
# Proportion weight by family
prop.fam.weight <- fish.species %>% 
    group_by(Sample_ID,trophic,family) %>% 
    summarise(prop.wt=sum(prop.wt,na.rm = T)) %>% 
    group_by(trophic,family) %>% 
    summarise(prop.wt=mean(prop.wt,na.rm = T),num.samples=n()) %>% 
    filter(!is.na(family))
head(prop.fam.weight)

# Proportion weight by family, gear
prop.fam.gear.weight <- fish.species %>% 
    group_by(Sample_ID,Gear,trophic,family) %>% 
    summarise(prop.wt=sum(prop.wt,na.rm = T)) %>% 
    group_by(trophic,Gear,family) %>% 
    summarise(prop.wt=mean(prop.wt,na.rm = T), num.samples=n()) %>% 
    filter(!is.na(family) & !is.na(Gear))
head(prop.fam.gear.weight)  

# Proportion weight by trophic group
prop.troph.weight <- fish.species %>% 
    group_by(Sample_ID,trophic) %>% 
    summarise(prop.wt=sum(prop.wt,na.rm = T)) %>% 
    group_by(trophic) %>% 
    summarise(prop.wt=mean(prop.wt,na.rm = T), num.samples=n()) %>% 
    filter(!is.na(trophic))
head(prop.troph.weight) 

# Proportion weight by trophic group and gear
prop.troph.gear.weight <- fish.species %>% 
  group_by(Sample_ID,Gear,trophic) %>% 
  summarise(prop.wt=sum(prop.wt,na.rm = T)) %>% 
  group_by(trophic,Gear) %>% 
  summarise(prop.wt=mean(prop.wt,na.rm = T), num.samples=n()) %>% 
  filter(!is.na(trophic)& !is.na(Gear))
head(prop.troph.gear.weight) 
  
# Mean weight by Year by species
mean.fish.weight <- fish.species %>% 
  group_by(Sample_ID,Species_latin_name,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T)) %>% 
  group_by(Year,Species_latin_name) %>% 
  summarise(mean.fish.weight=mean(ind.fish.weight,na.rm = T),num.samples=n()) %>% 
  filter(!is.na(Species_latin_name))
head(mean.fish.weight)
 
# Mean weight by Year by family
mean.family.weight <- fish.species %>% 
  group_by(Sample_ID,family,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T)) %>% 
  group_by(Year,family) %>% 
  summarise(mean.fish.weight=mean(ind.fish.weight,na.rm = T),num.samples=n()) %>% 
  filter(!is.na(family))
head(mean.family.weight)

############################### Further Lobster Analysis, Gender and Berried ############################
#looking at the sample set of lobsters in terms of seeing how many are female, berried, or undersized per year
lob.gender.berried.O.U.year <- log.data.L%>%
  select(Month,Year,Lenght_.mm.,Sex_.M.F.,Berried_.Y.N.,Over.Under.size_.O.U.,Trip_ID)%>%
  mutate(female=ifelse(Sex_.M.F.=="F", 1,0),
         berried=ifelse(Berried_.Y.N. == "Y",1,0),
         undersized=ifelse(Over.Under.size_.O.U.=="U",1,0))%>%
  group_by(Year)%>%
  summarize(Num_F=sum(female,na.rm=T),
            Num_berried=sum(berried,na.rm=T),
            Num_undersized=sum(undersized,na.rm=T),
            mean.length=mean(Lenght_.mm.),
            samp.num.ind=n())%>%
  mutate(prop.berried=Num_berried/samp.num.ind)
head(lob.gender.berried.O.U.year)

#looking at the sample set of lobsters in terms of seeing how many are female, berried, or undersized per month
lob.gender.berried.O.U.month <- log.data.L%>%
  select(Month,Year,Lenght_.mm.,Sex_.M.F.,Berried_.Y.N.,Over.Under.size_.O.U.,Trip_ID)%>%
  mutate(female=ifelse(Sex_.M.F.=="F", 1,0),
         berried=ifelse(Berried_.Y.N. == "Y",1,0),
         undersized=ifelse(Over.Under.size_.O.U.=="U",1,0))%>%
  group_by(Year,Month)%>%
  summarize(Num_F=sum(female,na.rm=T),
            Num_berried=sum(berried,na.rm=T),
            Num_undersized=sum(undersized,na.rm=T),
            mean.length=mean(Lenght_.mm.),
            samp.num.ind=n())%>%
  mutate(prop.berried=Num_berried/samp.num.ind)
head(lob.gender.berried.O.U.month)

############################ Further conch analysis in terms of shell length and lip thickness ############
#looking at further summary statistics involving conch sex, shell length, and lip thickness 
#for each year 
conch.length.thickness.year<-log.data.C%>%
  select(Month,Year,Shell_length_.cm.,Lip_thickness_.mm.,Tot_weight_.g.,Sex,Sample_ID,Rec_ID)%>%
  mutate(female=ifelse(Sex=="F",1,0),
         Shell_length_.cm.=as.numeric(Shell_length_.cm.))%>%
  group_by(Year)%>%
  summarize(Num_F=sum(female,na.rm=T),
            mean_shell_length=mean(Shell_length_.cm.,na.rm=T),
            mean_lip_thickness=mean(Lip_thickness_.mm.,na.rm=T),
            samp.num.ind=n())
head(conch.length.thickness.year)
#need to do the standard deviations for the conch 
#looking at further summary statistics involving conch sex, shell length, and lip thickness 
#for each month
conch.length.thickness.month<-log.data.C%>%
  select(Month,Year,Shell_length_.cm.,Lip_thickness_.mm.,Tot_weight_.g.,Sex,Sample_ID,Rec_ID)%>%
  mutate(female=ifelse(Sex=="F",1,0),
         Shell_length_.cm.=as.numeric(Shell_length_.cm.))%>%
  group_by(Year,Month)%>%
  summarize(Num_F=sum(female,na.rm=T),
            mean_shell_length=mean(Shell_length_.cm.,na.rm=T),
            mean_lip_thickness=mean(Lip_thickness_.mm.,na.rm=T),
            samp.num.ind=n())
head(conch.length.thickness.month)
########################## adding zone areas and initial map making for fishing pressure #####################
gis.dir <- "/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/"
#gis.dir <-"R:/Gill/spatial-fisheries-analysis/tables/raw/Fisheries_Zones"

 allfiles <- list.files(gis.dir,recursive = T, full.names = T) 
 # Select kml files with 1) digit then 1 letter, 2) digit then 2 letters, 3) digit then .kml, 4) digit then buffer
 file.list <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
                grep("FAD_[0-9]_[a-z]*",allfiles,value = T))
 
 zone.ind <- st_read(file.list[1])
 zone.ind$zone_id <- as.character(gsub("Zone_","", zone.ind$Name)) # removes all non-digit characters
 
 for (i in (2:length(file.list))) {
   # retrieve kml 
   X <- st_read(file.list[i]) 
   # extract zone_id from file name 
   X$zone_id <- as.character(gsub("Zone_","", X$Name)) # removes all non-digit characters
   # combine X to the previous shp
   zone.ind <- rbind(zone.ind,X) 
 }
 # head map
 plot(st_geometry(zone.ind))
 # get area
 zone.ind$area_m2 <- as.numeric(st_area(zone.ind))
 
 # changing the character type and joining the fishing summaries with the spatial geometries
 fishing.zones <-zone.fish.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind2 <- zone.ind %>% 
   left_join(fishing.zones, by = "zone_id")%>%
   mutate(area_km2 = area_m2/1000000)%>%
   mutate (fishing_pressure=weight.total/area_km2)%>%
   select(Name,zone_id,Year,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
   rename(weight_kg=weight.total)
 
 zone.ind.joiner <-zone.ind2 %>%
   select(Name,zone_id,Year,area_m2,area_km2,geometry)
 head(zone.ind.joiner)
   
 zone.ind3 <- zone.ind2 %>% 
   group_by(zone_id) %>% 
   summarise(fishing_pressure=mean(fishing_pressure)) %>% 
   filter(!grepl("FAD",zone_id))

 #calculating the range for use in standardizing the scales of all the plots
 zone.ind.range.fish <- range(zone.ind2$fishing_pressure, na.rm = TRUE) 
 zone.ind.range.fish #check to make sure it worked 
 
 # Plot fishing pressure maps
 fish.zone.2012 <- ggplot() +    #enable the ggplot layer 
   geom_sf(data=filter(zone.ind2,Year==2012), aes(fill = fishing_pressure)) +  #use the geom_sf to plot spatially
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2012"), x="Total Landings per sqkm") +  #create the correct labels for the plot
   theme_bw()  #set the theme of the plot to blue and white 
 fish.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2013), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2013"), x="Total Landings per sqkm") +
   theme_bw() 
 fish.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2014), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2014"), x="Total Landings per sqkm") +
   theme_bw()
 fish.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2015), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2015"), x="Total Landings per sqkm") +
   theme_bw()
 fish.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2016), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2016"), x="Total Landings per sqkm") +
   theme_bw()
 fish.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2017), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2017"), x="Total Landings per SqKm") +
   theme_bw()
 fish.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2018), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2018"), x="Total Landings per sqkm") +
   theme_bw()
 fish.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2019), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.fish[2]))) +
   labs(title = paste0("Map of Fishing Effort for 2019"), x="Total Landings per sqkm") +
   theme_bw()
 plot_grid_fish<-plot_grid(fish.zone.2012,fish.zone.2013,fish.zone.2014, fish.zone.2015, fish.zone.2016, fish.zone.2017, fish.zone.2018, fish.zone.2019)
 plot(fish.zone.2012)
 plot(fish.zone.2013)
 plot(fish.zone.2014)
 plot(fish.zone.2015)
 plot(fish.zone.2016)
 plot(fish.zone.2017)
 plot(fish.zone.2018)
 plot(fish.zone.2019)

 getwd()
 # saving files
 ggsave("Fishing_Effort_2012-2019.png", plot = plot_grid_fish, device = "png", path="Final_Figures_Tables/",scale = 1.5,width = 12, height = 12, units="in")
 ggsave("Fishing_Effort_2012.png", plot = fish.zone.2012, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2013.png", plot = fish.zone.2013, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2014.png", plot = fish.zone.2014, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2015.png", plot = fish.zone.2015, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2016.png", plot = fish.zone.2016, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2017.png", plot = fish.zone.2017, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2018.png", plot = fish.zone.2018, device = "png", path="Final_Figures_Tables/")
 ggsave("Fishing_Effort_2019.png", plot = fish.zone.2019, device = "png", path="Final_Figures_Tables/")
 
######################### mapping lobster catch by individual count ###########################
 lobster.zones <-zone.lob.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind5 <- zone.ind.joiner %>% 
   left_join(lobster.zones, by = c("zone_id", "Year"))%>%
   mutate (lobster_pressure=ind.total/area_km2)%>%
   select(Name,zone_id,Year,area_m2,area_km2,ind.total,lobster_pressure, geometry)
 head(zone.ind5)
 
 zone.ind6 <- zone.ind5 %>% 
   group_by(zone_id) %>% 
   summarise(lobster_pressure=mean(lobster_pressure)) %>% 
   filter(!grepl("FAD",zone_id))

  #calculating the range for use in standardizing the scales of all the plots
 zone.ind.range.lob <- range(zone.ind5$lobster_pressure, na.rm = TRUE) 
 zone.ind.range.lob #check to make sure it worked 
 
 # Plot lobster fishing pressure maps
 lobster.zone.2012 <- ggplot() +  #initialize ggplot
   geom_sf(data=filter(zone.ind5,Year==2012), aes(fill = lobster_pressure)) + #use the geom_sf function in ggplot to use spatial geometries to make maps
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure", #standardize the legend with the range you calculated for lobsters
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2012"), x="Total Landings per sqkm") + #label the title and X axis of maps
   theme_bw()  #set the theme as blue and white 
 lobster.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2013), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2013"), x="Total landings per sqkm") +
   theme_bw() 
 lobster.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2014), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2014"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2015), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2015"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2016), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2016"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2017), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2017"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2018), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2018"), x="Total landings per sqkm") +
   theme_bw()
 lobster.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind5,Year==2019), aes(fill = lobster_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.lob[2]))) +
   labs(title = paste0("Map of Lobster Fishing Effort for 2019"), x="Total landings per sqkm") +
   theme_bw()
 plot_grid_lobster <-plot_grid(lobster.zone.2012,lobster.zone.2013,lobster.zone.2014,lobster.zone.2015,lobster.zone.2016,lobster.zone.2017,lobster.zone.2018,lobster.zone.2019)
 
 # saving files
 ggsave("Lobster_Effort_2012-2019.png", plot = plot_grid_lobster, device = "png", path="Final_Figures_Tables/",scale = 1.5,width = 12, height = 12, dpi=300, units="in")
 ggsave("Lobster_Effort_2012.png", plot = lobster.zone.2012, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2013.png", plot = lobster.zone.2013, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2014.png", plot = lobster.zone.2014, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2015.png", plot = lobster.zone.2015, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2016.png", plot = lobster.zone.2016, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2017.png", plot = lobster.zone.2017, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2018.png", plot = lobster.zone.2018, device = "png", path="Final_Figures_Tables/")
 ggsave("Lobster_Effort_2019.png", plot = lobster.zone.2019, device = "png", path="Final_Figures_Tables/")
 
 ############################### mapping conch catch by individual count ######################
 #join with the conch
 conch.zones <-zone.conch.year%>%
    mutate (zone_id = as.character(zone_id))
 
 zone.ind8 <- zone.ind.joiner %>% 
    left_join(conch.zones, by = c("zone_id", "Year"))%>%
    mutate (conch_pressure=ind.total/area_km2)%>%
    select(Name,zone_id,Year,area_m2,area_km2,ind.total,conch_pressure, geometry)
 head(zone.ind8)
 
 zone.ind9 <- zone.ind8 %>% 
    group_by(zone_id) %>% 
    summarise(conch_pressure=mean(conch_pressure)) %>% 
    filter(!grepl("FAD",zone_id))
 
 #calculating the range for use in standardizing the scales of all the plots
 zone.ind.range.conch <- range(zone.ind8$conch_pressure, na.rm = TRUE) 
 zone.ind.range.conch #check to make sure it worked 
 
 # Plot conch fishing pressure maps
 conch.zone.2012 <- ggplot() + #initialize ggplot
   geom_sf(data=filter(zone.ind8,Year==2012), aes(fill = conch_pressure)) + #use the geom_sf function in ggplot to make the map and fill it with the fishing pressure variable 
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure", #format the legend
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) + 
   labs(title = paste0("Map of Conch Fishing Effort for 2012"), x="Total landings per sqkm") + #format the labels for the plot
   theme_bw() #set the theme as blue and white
 conch.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2013), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2013"), x="Total landings per sqkm") +
   theme_bw() 
 conch.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2014), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2014"), x="Total landings per sqkm") +
   theme_bw()
 conch.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2015), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2015"), x="Total landings per sqkm") +
   theme_bw()
 conch.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2016), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2016"), x="Total landings per sqkm") +
   theme_bw()
 conch.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2017), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2017"), x="Total landings per sqkm") +
   theme_bw()
 conch.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2018), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2018"), x="Total landings per sqkm") +
   theme_bw()
 conch.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind8,Year==2019), aes(fill = conch_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind.range.conch[2]))) +
   labs(title = paste0("Map of Conch Fishing Effort for 2019"), x="Total landings per sqkm") +
   theme_bw()
 plot_grid_conch<-plot_grid(conch.zone.2012,conch.zone.2013,conch.zone.2014,conch.zone.2015,conch.zone.2016,conch.zone.2017,conch.zone.2018,conch.zone.2019)
 
 # saving files
 ggsave("Conch_Effort_2012-2019.png", plot = plot_grid_conch, device = "png", path="Final_Figures_Tables/",scale = 1.5,width = 12, height = 12,dpi=300, units="in")
 ggsave("Conch_Effort_2012.png", plot = conch.zone.2012, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2013.png", plot = conch.zone.2013, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2014.png", plot = conch.zone.2014, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2015.png", plot = conch.zone.2015, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2016.png", plot = conch.zone.2016, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2017.png", plot = conch.zone.2017, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2018.png", plot = conch.zone.2018, device = "png", path="Final_Figures_Tables/")
 ggsave("Conch_Effort_2019.png", plot = conch.zone.2019, device = "png", path="Final_Figures_Tables/")
 
 ############################## potential bar plots for family stats ##############################

unique(fish.family.year$family) 
barplot(Num.ind~Year, data=family.subset)
 
 counts <- tapply(fish.family.year$Num.ind, list(fish.family.year$Year, fish.family.year$family), sum)
 barplot(counts, main="Number of Individuals Caught per Family per Year",
         xlab="Scientific Family Name", ylab="Number of Individuals", ylim=c(0, 2500),
         col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
         legend = rownames(counts), beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)

 
fish.family.year.group<-fish.family.year[order(fish.family.year$Year),] 
fish.family.year.group$color[fish.family.year.group$Year==2012] <- "red"
fish.family.year.group$color[fish.family.year.group$Year==2013] <- "orange"
fish.family.year.group$color[fish.family.year.group$Year==2014] <- "blue"
fish.family.year.group$color[fish.family.year.group$Year==2015] <- "darkblue"
fish.family.year.group$color[fish.family.year.group$Year==2016] <- "green"
fish.family.year.group$color[fish.family.year.group$Year==2017] <- "darkgreen"
fish.family.year.group$color[fish.family.year.group$Year==2018] <- "purple"
fish.family.year.group$color[fish.family.year.group$Year==2019] <- "deeppink"

dotchart(fish.family.year.group$Num.ind, labels=fish.family.year.group$family,cex=.7,
         cex.axis=1.25, cex.lab =2,cex.main=2, 
         groups = fish.family.year.group$Year, gcolor="black", color=fish.family.year.group$color)

############################### families of interest plots ########################################
family.subset <- fish.family.year %>%
  group_by(Year)%>%
  filter(family %in% c("Acanthuridae","Lutjanidae","Scaridae","Serranidae"))
head(family.subset)

family.counts <- tapply(family.subset$Num.ind, list(family.subset$Year, family.subset$family), sum)
barplot(family.counts, main="Number of Individuals Caught per Family per Year",
        xlab="Scientific Family Name", ylab="Number of Individuals", ylim=c(0, 2500),
        col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
        legend = rownames(family.counts), beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)

fish.subset.group<-family.subset[order(family.subset$Year),] 
fish.subset.group$color[fish.subset.group$Year==2012] <- "red"
fish.subset.group$color[fish.subset.group$Year==2013] <- "orange"
fish.subset.group$color[fish.subset.group$Year==2014] <- "blue"
fish.subset.group$color[fish.subset.group$Year==2015] <- "darkblue"
fish.subset.group$color[fish.subset.group$Year==2016] <- "green"
fish.subset.group$color[fish.subset.group$Year==2017] <- "darkgreen"
fish.subset.group$color[fish.subset.group$Year==2018] <- "purple"
fish.subset.group$color[fish.subset.group$Year==2019] <- "deeppink"

dotchart(fish.subset.group$Num.ind, labels=fish.subset.group$family,cex=.7,
         cex.axis=1.25, cex.lab =2,cex.main=2, 
         groups = fish.subset.group$Year, gcolor="black", color=fish.subset.group$color)
##################### assessing annual fishing effort by gear type and prepping for maps #################  
# changing the character type and joining the fishing summaries with the spatial geometries
fishing.zones.gear <-zone.fish.gear%>%
  mutate (zone_id = as.character(zone_id))

zone.ind.joiner.2 <-zone.ind %>%
  select(Name,zone_id,area_m2,area_km2,geometry)
head(zone.ind.joiner.2)

zone.ind10 <- zone.ind %>% 
  left_join(fishing.zones.gear, by = "zone_id")%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (fishing_pressure=weight.total/area_km2)%>%
  select(Name,zone_id,Gear,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
  rename(weight_kg=weight.total)
head(zone.ind10)
unique(zone.ind10$Gear)
range(zone.ind10$fishing_pressure, na.rm=TRUE)

zone.gear.joiner <-zone.ind10 %>%
  filter(Gear=="HL")%>%
  select(Name,zone_id,area_m2,area_km2,geometry)
head(zone.gear.joiner)

# Plot fishing pressure maps by gear type 
fish.zone.HL <- ggplot() +
  geom_sf(data=filter(zone.ind10,Gear=="HL"), aes( fill = fishing_pressure)) +
  scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                       na.value="gray90",limits=c(0,250)) +
  labs(title = paste0("Map of Fishing Effort by Hand Line"), x="Total Landings per sqkm") +
  theme_bw()
plot(fish.zone.HL)

fish.zone.FD <- ggplot() +
  geom_sf(data=filter(zone.ind10,Gear=="FD"), aes( fill = fishing_pressure)) +
  scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                       na.value="gray90",limits=c(0,250)) +
  labs(title = paste0("Map of Fishing Effort by FD"), x="Total Landings per sqkm") +
  theme_bw()
plot(fish.zone.FD)

fish.zone.LL <- ggplot() +
  geom_sf(data=filter(zone.ind10,Gear=="LL"), aes(fill = fishing_pressure)) +
  scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                       na.value="gray90",limits=c(0,250)) +
  labs(title = paste0("Map of Fishing Effort by Long Line"), x="Total Landings per sqkm") +
  theme_bw()
plot(fish.zone.LL)

################## assessing annual lobster fishing effort by gear type and prepping for maps #################  
 
lobster.zones.gear <-zone.lob.gear.year%>%
  mutate (zone_id = as.character(zone_id))

zone.ind11 <- zone.ind.joiner %>% 
  left_join(lobster.zones.gear, by = c("zone_id", "Year"))%>%
  mutate (lobster_pressure=ind.total/area_km2)%>%
  select(Name,zone_id,Year,Gear,area_m2,area_km2,ind.total,lobster_pressure, geometry)
head(zone.ind11) 

################## assessing annual conch fishing effort by gear type and prepping for maps #################
conch.zones.gear <-zone.conch.gear.year%>%
  mutate (zone_id = as.character(zone_id))

zone.ind12 <- zone.ind.joiner %>% 
  left_join(conch.zones.gear, by = c("zone_id", "Year"))%>%
  mutate (conch_pressure=ind.total/area_km2)%>%
  select(Name,zone_id,Year,Gear,area_m2,area_km2,ind.total,conch_pressure, geometry)
head(zone.ind12) 

##################### visualization of sub-species means over the years ##################################
#sample subset for a sample species 
redhind.subset <- fish.species.year %>%
  group_by(Year)%>%
  filter(Species_latin_name %in% c("Epinephelus guttatus"))%>%
  filter(!is.na(pct.wt))
head(redhind.subset)

#boxplot of means using ggplotfor redhind fish
Year.f<-factor(redhind.subset$Year,levels=c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),) 
redhind.fish.wt.means<-ggplot(redhind.subset, aes(Year.f, avg.fish.wt))
redhind.fish.wt.means + geom_boxplot(aes(fill = Year.f))+
  theme(axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  theme(legend.position="none")+
  labs(x="Year", y="Average Fish Weight (lbs)",fill = "Year")+
  ggtitle("Box Plot of Average Fish Weight from 2012-2019 ")

# boxplot of means for redhind fish using a normal boxplot function
boxplot(avg.fish.wt~Year,data=redhind.subset, main="Comparison of Redhind Fish Average Weight (lbs) for 2012-2019",
        col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
        xlab="Years",
        ylab="Average Fish Weight (lbs)",
        cex=1.5, cex.axis=1.25, cex.lab =1.5,cex.main=2)

#sample means calulation for the redhind species
redhind.mean.year <-redhind.subset%>%
  group_by(Year)%>%
  summarise(mean.avg.fish.wt=mean(avg.fish.wt), 
            mean.pct.spec=mean(pct.spec),
            mean.pct.wt=mean(pct.wt))
head(redhind.mean.year)

################### seasonal plots for fish, lobsters, and conch, no zones########################
#looking at fish monthly season totals
fish.months.season <- zones.fish %>% 
  group_by(Month)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            n=as.numeric(sum(!is.na(`Weight_(kg)`))),
            se.avg.wt = (sd.avg.wt)/(sqrt(n)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)
head(fish.months.season)
plot(fish.months.season$weight.total~fish.months.season$Month, 
     col=fish.months.season$Month)
plot(fish.months.season$avg.wt.per.trip~fish.months.season$Month, 
     col=fish.months.season$Month)

fishing_seasons_sum<-ggplot(fish.months.season, mapping = aes(x=Month, y=weight.total))
fishing_seasons_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Total Weight (kg)")+
  ggtitle("Seasonality of Total Weight of Fish Caught from 2012-2019")
ggsave("Fish_Seasonality_Sum_2012-2019.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

fishing_seasons_avg<-ggplot(fish.months.season, mapping = aes(x=Month, y=avg.wt.per.trip))
fishing_seasons_avg+
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=1 )+
  geom_point(color="blue", size=6)+
  geom_hline(yintercept = 0) +
  ylim(0,70)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Weight per Trip (kg)")+
  ggtitle("Seasonality of Average Catch per Trip from 2012-2019")
ggsave("Fish_Seasonality_Avg_2012-2019.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at the total amount of fish caught each year and looking for total changes
fish.years <- zones.fish %>% 
  group_by(Year)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            n=as.numeric(sum(!is.na(`Weight_(kg)`))),
            se.avg.wt = (sd.avg.wt)/(sqrt(n)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)
head(fish.years)
plot(fish.years$weight.total~fish.years$Year, 
     col=fish.years$Year)
#using ggplot to create better looking plots of yearly summaries 
fishing_years_sum<-ggplot(fish.years, mapping = aes(x=Year, y=weight.total))
fishing_years_sum+geom_point(color="blue",size=6)+ylim(0,4000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2019 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Total Weight of Fish Caught Each Year from 2012-2019")
ggsave("Fish_Year_Totals_2012-2019.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create better looking plots of yearly summaries 
fishing_years_avg<-ggplot(fish.years, mapping = aes(x=Year, y=avg.wt.per.trip))
fishing_years_avg+
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=1 )+
  geom_point(color="blue", size=6)+
  geom_hline(yintercept = 0) +
  ylim(0,60)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2019 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Average Catch per Year from 2012-2019")
ggsave("Fish_Year_Avg_2012-2019.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at lobsters monthly to see if thers is seasonality
lob.months.season <- zones.lob %>% 
  group_by(Month)%>% 
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            n=as.numeric(sum(!is.na(Num_ind))),
            se.avg.ind = (sd.avg.ind)/(sqrt(n)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)
head(lob.months.season)
plot(lob.months.season$ind.total~lob.months.season$Month)

#using ggplot to create maps of lobster seasonality from 2012-2019
lobster_season_sum<-ggplot(lob.months.season, mapping = aes(x=Month, y=ind.total))
lobster_season_sum+geom_point(color="blue",size=6)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  scale_y_continuous(breaks = seq(0,5500, by = 1000))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Seasonality of Total Number of Lobsters Caught from 2012-2019")
ggsave("Lobster_Seasonality_Sum_2012-2019.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#average lobster catch per month for all of the years summed from 2012-2019
lobster_season_avg<-ggplot(lob.months.season, mapping = aes(x=Month, y=avg.ind.per.trip))
lobster_season_avg+
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=1 )+
  geom_point(color="blue", size=6)+
  geom_hline(yintercept = 0) +
  ylim(0,90)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Number of Individuals Per Trip")+
  ggtitle("Seasonality of Average Catch per Trip from 2012-2019")
ggsave("Fish_Seasonality_Avg_2012-2019.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at lobsters yearly totals to track any yield changes 
lob.years <- zones.lob %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(lob.years)
plot(lob.years$ind.total~lob.years$Year)

#using ggplot to create maps of lobster seasonality from 2012-2019
lobster_years_sum<-ggplot(lob.years, mapping = aes(x=Year, y=ind.total))
lobster_years_sum+geom_point(color="blue",size=4)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=25, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,7000, by = 1000))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Plot Depicting Lobster Yield Totals from 2012-2019")
ggsave("Lobster_Yield_2012-2019.png", path="Final_Figures_Tables/", scale=1.5)

#looking at conch
conch.months.season <- zones.conch %>% 
  group_by(Month)%>% 
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(conch.months.season)
plot(conch.months.season$ind.total~conch.months.season$Month)

#using ggplot to create maps of conch seasonality from 2012-2019
conch_season<-ggplot(conch.months.season, mapping = aes(x=Month, y=ind.total))
conch_season+geom_point(color="blue",size=4)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=25, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Plot Depicting Conch Seasonality from 2012-2019")
ggsave("Conch_Seasonality_2012-2019.png", path="Final_Figures_Tables/", scale=1.5)

#looking at conch
conch.years <- zones.conch %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(conch.years)
plot(conch.years$ind.total~conch.years$Year)

#using ggplot to create maps of conch seasonality from 2012-2019
conch_years_sum<-ggplot(conch.years, mapping = aes(x=Year, y=ind.total))
conch_years_sum+geom_point(color="blue",size=4)+ylim(0,3000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=25, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2019, by = 1))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Plot Depicting Conch Total Yield from 2012-2019")
ggsave("Conch_Year_Totals_2012-2019.png", path="Final_Figures_Tables/", scale=1.5)
