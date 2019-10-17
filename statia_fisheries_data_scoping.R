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

unique(log.data.F$Gear)
log.data.F$Gear <- ifelse(log.data.F$Gear %in% c("PT","Pt","pt"), "PT",log.data.F$Gear)
unique(log.data.F$Gear)

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
  filter(Landings=="Spiny Lobster")
  #arrange(Trip_ID)
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
 
 ####################################### Fish Data by Species #########################
 
#join the coral reef monitoring species data to the fish data in order to assess effort by family
 fish.GCRM.join <-log.data.F %>% 
   left_join(GCRM.data.Fish, by = "Species_latin_name" )%>%
   filter(!is.na(Species_latin_name))
head(fish.GCRM.join) #check the top to see if it joined correctly
tail(fish.GCRM.join) #check the bottom to see if it joined correctly
 
names(fish.GCRM.join) #check the names to get the correct names for each variable 

unique(fish.GCRM.join$Gear)
 
fish.species <- fish.GCRM.join  %>% #create a data set that has each individual fish weight calculated and organized by Rec_ID
   select(Rec_ID,Year,Month,Day,Gear, Species_common_name,Species_latin_name,family,Length_.cm.,FL.TL, TL2FL, a, b) %>% 
   mutate(Rec_ID=as.numeric(Rec_ID))%>% #change from character to numeric 
   mutate(ind.fish.weight = ((a*Length_.cm.)^b)*TL2FL)%>%
   arrange(Rec_ID)
  head(fish.species)
  
unique(fish.species$Gear)
  
 #types of fish per year, the weight of the fish, the number of fish, and the avg. fish weight
 fish.species.year <- fish.species %>% 
   group_by(Year,Species_latin_name)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))%>%
   mutate(avg.fish.weight=total.weight/Num.ind)
 head(fish.species.year)
 
 #types of fish per month, the weight of the fish, the number of fish, and the avg. fish weight
 fish.species.month <- fish.species %>% 
   group_by(Year,Month,Species_latin_name)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))%>%
   mutate(avg.fish.weight=total.weight/Num.ind)
 head(fish.species.month)
 
 #family breakdown per year, the weight of the fish, and the number of fish
 fish.family.year <- fish.species %>% 
   group_by(Year,family)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))
 head(fish.family.year)
 
 # family breakdown of the amount of fish caught per month for each family, the number of individuals,and the total weight per family 
 fish.family.month <- fish.species %>% 
   group_by(Year,Month,family)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))
 head(fish.family.month)
 
 #types of fish per year per gear, the weight of the fish, the number of fish, and the avg. fish weight
 fish.species.gear.year <- fish.species %>% 
   group_by(Year,Gear,Species_latin_name)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))%>%
   mutate(avg.fish.weight=total.weight/Num.ind)
 head(fish.species.gear.year)
 unique(fish.species.gear.year$Gear)
 
 #types of fish per month per gear, the weight of the fish, the number of fish, and the avg. fish weight
 fish.species.gear.month <- fish.species %>% 
   group_by(Year,Month,Gear,Species_latin_name)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))%>%
   mutate(avg.fish.weight=total.weight/Num.ind)
 head(fish.species.gear.month)
 unique(fish.species.gear.month$Gear)
 
 #family breakdown per year per gear, the weight of the fish, and the number of fish
 fish.family.gear.year <- fish.species %>% 
   group_by(Year,Gear,family)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))
 head(fish.family.gear.year)
 unique(fish.family.gear.year$Gear)
 
 # family breakdown of fish caught per month per gear for each family, the number of individuals,and the total weight per family 
 fish.family.gear.month <- fish.species %>% 
   group_by(Year,Month,Gear,family)%>%
   summarize(total.weight=sum(ind.fish.weight, na.rm=T),
             Num.ind=n_distinct(Rec_ID))
 head(fish.family.gear.month)
 unique(fish.family.gear.month$Gear)
 
 ########################## adding zone areas and initial map making for fishing pressure #####################
 gis.dir <- "/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/"
 
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
 
 # Example join
 fishing.zones <-zone.fish.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind2 <- zone.ind %>% 
   left_join(fishing.zones, by = "zone_id")%>%
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
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("Map of Fishing Effort for 2012"), x="Total Landings per SqKm") +
   theme_bw()
 fish.zone.2013 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2013), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("Map of Fishing Effort 2013"), x="Total Landings per SqKm") +
   theme_bw() 
 fish.zone.2014 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2014), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("2014"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2015 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2015), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("Map of Fishing Effort for 2015"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2016 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2016), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("2016"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2017 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2017), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("2017"), x="Total Landings per SqKm") +
   theme_bw()
 fish.zone.2018 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2018), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("2018"), x="Total landings per sqkm") +
   theme_bw()
 fish.zone.2019 <- ggplot() +
   geom_sf(data=filter(zone.ind2,Year==2019), aes(fill = fishing_pressure)) +
   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="Fishing Pressure",
                        na.value="gray90",limits=c(0,max(zone.ind2$fishing_pressure))) +
   labs(title = paste0("2019"), x="Total landings per sqkm") +
   theme_bw()
 plot_grid(fish.zone.2012,fish.zone.2013,fish.zone.2014, fish.zone.2015, fish.zone.2016, fish.zone.2017, fish.zone.2018, fish.zone.2019)
 plot(fish.zone.2012)
 plot(fish.zone.2013)
 plot(fish.zone.2014)
 plot(fish.zone.2015)
 plot(fish.zone.2016)
 plot(fish.zone.2017)
 plot(fish.zone.2018)
 plot(fish.zone.2019)
 ######################### mapping lobster catch by individual count ###########################
  # Example join
 lobster.zones <-zone.lob.year%>%
   mutate (zone_id = as.character(zone_id))
 
 zone.ind5 <- zone.ind %>% 
   inner_join(lobster.zones, by = "zone_id" )%>%
   mutate(area_km2 = area_m2/1000000)%>%
   mutate (lobster_pressure=ind.total/area_km2)%>%
   select(Name,zone_id,Year,area_m2,area_km2,ind.total,lobster_pressure, geometry)
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
 
 ############################## potential bar plots for family stats ##############################

unique(fish.family.year$family) 

family.subset <- fish.family.year %>%
  group_by(Year)%>%
  filter(family %in% c("Acanthuridae","Lutjanidae","Scaridae","Serranidae"))
head(family.subset)

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
######################## analysis of percentage composition of fish species ########################

#fish.GCRM.join.pct <-log.data.F %>% 
#  left_join(GCRM.data.Fish, by = "Species_latin_name" )%>%
#  filter(!is.na(Trip_ID)) %>% 
#  mutate(ind.fish.weight = ((a*Length_.cm.)^b)*TL2FL) 

unique(fish.species$Gear)

fish.GCRM.join.gear <-  fish.species %>% 
  group_by(Gear) %>% 
  summarise(gear.sum.wt=sum(ind.fish.weight, na.rm=T))
head(fish.GCRM.join.gear)

fish.GCRM.join.gear.family <- fish.species %>% 
  group_by(Gear,family) %>% 
  summarise(sum.weight=sum(ind.fish.weight, na.rm=T)) %>% 
  left_join(fish.GCRM.join.gear, by="Gear") %>% 
  mutate(pct.wt=sum.weight/gear.sum.wt*100)
head(fish.GCRM.join.gear.family)
 
family.percent.gear.comp <- family.year.gear %>%
  mutate(percent.comp = Count/sum(Count)*100)
head(family.percent.gear.comp)

family.percent.gear.weight <- fish.family.gear.year %>%
  mutate(percent.weight = total.weight/sum(total.weight)*100)
head(family.percent.gear.weight)
  
 
 
 
 