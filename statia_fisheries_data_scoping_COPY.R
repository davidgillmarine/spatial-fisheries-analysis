#######NOTE: This file is named as a Copy, but this is NOT a copy!!! This is the file that has all of the Data cleaning in it as well, super important.
# may want to delete the statia_fisheries_data_scoping.R file, as I don't think it holds anything different, but going to check now
# right now, just run this AFTER statia_fisheries_data_scoping.R

install.packages('pacman')
pacman::p_load(sf,rio,ggpubr,cowplot, gridExtra, tidyverse)


# library(sf)  #importing the correct library packages
# library(rio)
# install_formats()
# library(ggplot2)
# library(cowplot)
# library(tidyverse)
# library(gridExtra)
# library("RColorBrewer")
# library(forcats)
# library(ggpubr)
# library(gridExtra)


###################################### Import the Data ##############################
input.dir <- 'R:/Gill/research/spatial-fisheries-analysis/tables/raw/' #set the import directory
log.data.total <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"), #import the correct file and the page of the fisheries 
                         which = 1, skip =1)                                                            #spreadsheet and tell it where to start from the top

########################### select only the columns we are interested in and remove spaces ############
# perhaps consider all lowercase/CamelCase with . or _ between words, no special characters
log.data <- select(log.data.total,Rec_ID:"No catch") #between Rec_ID and "No Catch are our interesting columns #rename the original data for analysis
names(log.data)    #check the names 
names(log.data) <- gsub(" ","_",names(log.data)) #get rid of the names between spaces
names(log.data) <- gsub("No.lobster/fish_etc.","Num_ind",names(log.data)) #get rid of weird lobster name

########################## Clean and tidy up the data by replacing rogue names ###################
unique(log.data$Year) #check
log.data$Year <- ifelse(log.data$Year==2004,2014,log.data$Year) ## edit the years 
log.data$Year <- ifelse(log.data$Year==2011,2017,log.data$Year) ## edit the years
unique(log.data$Year) #check 

unique(log.data$Gear) #check
log.data$Gear <- ifelse(log.data$Gear=="pt","PT",log.data$Gear) # edit the gear types
unique(log.data$Gear) #check

unique(log.data$Landings) #check
# log.data <- log.data %>% 
#    mutate(Landings=tolower(Landings))  # option to simplify landings by changing all to lower case 
log.data$Landings <- ifelse(log.data$Landings %in% c("Fish","fish","FIsh"), "Fish",log.data$Landings) # edit for Fish
log.data$Landings <- ifelse(log.data$Landings %in% c("Queen Conch","Conch","conch"), "Queen Conch",log.data$Landings) # edit for Conch
log.data$Landings <- ifelse(log.data$Landings %in% c("Whelk","Whelks"), "Whelk",log.data$Landings) # edit for Whelk
unique(log.data$Landings) #check

#
log.data <- log.data %>% 
  mutate(Num_ind=as.numeric(Num_ind),
         Trip_ID=as.character(Trip_ID),
         weight.kg=`Weight_(Lbs)`/2.2,
         in.park=ifelse(`max_(m)`<=30, 1,0),
         n.zones=rowSums(!is.na(select(., Z1:Z6))), # get # of zones fished 
         weight.per.zone=weight.kg/n.zones,
         ind.per.zone=Num_ind/n.zones) 

fishing.area.sqkm <- 64.89144 # km2
park.area.sqkm <- 27.5 # km2

source("fisheries_summary_functions.R")



#################### Breaking out the data by year, and month, and by gear. And by fish, lobster and conch #############
log.data <- log.data %>% 
  group_by(Year) %>%           #group by the relevent groups
  mutate(trips.year=n_distinct(Trip_ID)) %>%    #summerize by these groups by unique Trip_ID
  group_by(Year, Month) %>%         
  mutate(trips.month=n_distinct(Trip_ID)) %>%      
  group_by(Year, Gear) %>%         
  mutate(gear.trips.year =n_distinct(Trip_ID)) %>% 
  group_by(Year, Month, Gear) %>%   
  mutate(gear.trips.month=n_distinct(Trip_ID)) %>% 
  ungroup()

####################### looking at how much fish was caught and by what type of gear ################
# Example

my_landings(log.data,"all","Fish",weight.kg,Year)



fish.weight.year <- log.data %>%   # looking at the amount of fish caught per year
  filter(Landings=="Fish") %>%     # sort the data just by fish
  group_by(Year)%>%      #group by the relevent groups
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE),
            n_trips=n_distinct(Trip_ID))%>%   #summarize by these groups by unique Trip_ID and remove NAs
  mutate(fish_intensity_sqkm=fish_weight/fishing.area.sqkm)
head(fish.weight.year)
write_excel_csv(fish.weight.year, "Final_Figures_Tables/yearly_fishing_effort.xlxs")

fish.weight.inpark.year <- log.data %>%   # looking at the amount of fish caught per year
  filter(Landings=="Fish") %>%     # sort the data just by fish
  group_by(Year)%>%      #group by the relevant groups
  filter(in.park==1)%>%
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE),
            n_trips=n_distinct(Trip_ID))%>%   #summerize by these groups by unique Trip_ID and remove NAs
  mutate(Fishing_Effort=fish_weight/park.area.sqkm)
head(fish.weight.inpark.year)
write_excel_csv(fish.weight.inpark.year, "Final_Figures_Tables/yearly_fishing_effort_inpark.xlxs")

fish.weight.month <- log.data %>%     # looking at the amount of fish caught per year
  filter(Landings=="Fish") %>%         
  group_by(Year, Month)%>%  
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE))  

fish.gear<- log.data %>%       # looking at the amount of fish caught per gear
  filter(Landings =="Fish") %>%
  group_by(Gear) %>%
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE))

fish.gear.year <- log.data %>%      # looking at the amount of fish caught per gear per year
  filter(Landings =="Fish") %>%
  group_by(Year, Gear) %>%
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE))

fish.gear.month <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Year, Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE))

fish.gear.month.all <- log.data %>%      # looking at the amount of fish caught per gear per month
  group_by(Month, Landings, Gear) %>%
  filter(Landings =="Fish") %>%
  summarize(fish_weight = sum(weight.kg, na.rm=TRUE))

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
  filter(!is.na(Rec_ID))%>%
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
  filter(!is.na(Rec_ID))
names(log.data.C)

###################################### Zone Analysis For Fish ##########################################

log.data.zone <- log.data %>% #rename variable to zone.fish
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,weight.kg,`max_(m)`,in.park,n.zones,weight.per.zone, ind.per.zone) %>% #select relevent variables
  gather(key="col.nam",value="zone_id",Z1:Z6) %>% #bring these values together, and sum and rename them # swap names
  filter(!is.na(zone_id)) %>%     #filter by zone ID and remove NAs
  arrange(Trip_ID)                # arrange by unique trip ID for clarity
head(log.data.zone)   #check to make sure it was ordered properly

my_landings(log.data.zone,"Spiny Lobster",ind.per.zone,Year,zone_id)


#amount of fish per zone per year, the number of trips, and the average number of fish per trip
zone.fish.year <- zones.fish %>% #rename variable to zone.fish.year for analysis
  filter(Landings=="Fish") %>% 
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

#number of lobsters per zone per gear, the number of trips, and the average number of lobsters caught per trip
zone.lob.gear<-zones.lob %>%
  group_by(Gear, zone_id) %>%
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(zone.lob.gear)  

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

#amount of Queen Conch per zone per gear, the number of trips, and the average number of conch caught per trip
zone.conch.gear<-zones.conch %>%
  group_by(Gear, zone_id) %>%
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(zone.conch.gear) 

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
#join with GCRM table and look at this for the family level and do the same thing down below with the gear types
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
         pct.wt = ((ind.fish.weight/trip.wt)*100),
         prop.wt=ind.fish.weight/trip.wt)
head(fish.species)

# Proportion weight by family
prop.fam.weight <- fish.species %>% 
  group_by(Sample_ID,trophic,family, Year) %>% 
  filter(Year<2019)%>%
  summarise(prop.wt=sum(prop.wt,na.rm = T),
            pct.wt=sum(pct.wt,na.rm = T)) %>% 
  group_by(family) %>% 
  summarise(mean.prop.wt=mean(prop.wt,na.rm = T),
            mean.pct.wt=mean(pct.wt,na.rm = T),
            num.samples=n()) %>%
  mutate(catch.prop=(num.samples/309), 
         pie.prop=(mean.prop.wt*catch.prop)*100,
         pie.filter=ifelse(pie.prop<=.1,NA,pie.prop))%>%
  filter(!is.na(family))%>%
  arrange(desc(pie.prop))

head(prop.fam.weight)
sum(prop.fam.weight$pie.filter, na.rm=T)

#bar plot showing the percent composition of fish per trip 
prop.fam.weight%>%
  mutate(Family = fct_reorder(family, pie.prop)) %>%
  filter(pie.prop>.1)%>%
  ggplot( aes(x=Family, y=pie.prop, label=sprintf("%0.2f", round(pie.prop, digits = 2)))) +
  geom_bar(stat="identity", fill="DARKGREEN") +
  geom_text(size = 6, hjust = -.125)+
  coord_flip() +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=30, face="bold"),
        axis.title.y = element_text(size=30, face="bold"),
        plot.title = element_text(size=35, face="bold"))+
  labs(title="Barplot Showing Relative % Catch Per Trip by Family",
       x="Family", y="Relative % Catch Per Trip")
ggsave("Relative_Pct_Catch_2012-2018.png", path="Final_Figures_Tables/",scale=1.25,width=14, height=9, units=c("in"))

#bar plot showing the mean percent catch composition of fish, however this does not seem as accurate as the 
#above plot because there are some cases where it is the majority of the catch but only part of the time
#so it does not get at the exploitation question like the other graph 
#bar plot showing the percent composition of fish per trip with a mutiplier 
prop.fam.weight%>%
  mutate(Family = fct_reorder(family, mean.pct.wt)) %>%
  filter(pie.prop>0)%>%
  ggplot( aes(x=Family, y=mean.pct.wt)) +
  geom_bar(stat="identity", fill="DARKGREEN") +
  coord_flip() +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=30, face="bold"),
        axis.title.y = element_text(size=30, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  labs(title="Barplot Showing Mean % Catch Per Trip by Family",
       x="Family", y="Mean % Catch Per Trip")
ggsave("Mean_Pct_Catch_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

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
  group_by(family) %>% 
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
gis.dir <-"R:/Gill/research/spatial-fisheries-analysis/spatial/raw/Fisheries_Zones"

allfiles <- list.files(gis.dir,recursive = T, full.names = T) 
allfiles
# Select kml files with 1) digit then 1 letter, 2) digit then 2 letters, 3) digit then .kml, 4) digit then buffer
file.list <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
               #grep("FAD_[0-9]_[a-z]*",allfiles,value = T),
               grep("St_Eustatius_*",allfiles,value = T))

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

#create areas for erasing and intersecting for the Marine park (A) and land area (B) and zones 
A<-zone.ind
B<-st_read(file.list[9]) # ??? name file (Habitat map with Statia outline)
# Erase land area (B) from marine park polygon (A)
zone.ind <-st_difference(A,B) # erase area in the remaining zones that overlap with A 
map.fill<-st_difference(B,zone.ind)
plot(st_geometry(zone.ind))
plot(st_geometry(map.fill))

# get area
zone.ind$area_m2 <- as.numeric(st_area(zone.ind))
sum(zone.ind$area_m2[1:8])/1000000
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
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2012), aes(fill = fishing_pressure)) +  #use the geom_sf to plot spatially
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2012)

fish.zone.2013 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2013), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2014 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2014), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2015 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2015), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2016 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2016), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2017 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2017), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2018 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2018), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
fish.zone.2019 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2019), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2019"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 

plot_grid_fish<-plot_grid(fish.zone.2012,fish.zone.2013,fish.zone.2014, 
                          fish.zone.2015, fish.zone.2016,
                          fish.zone.2017, fish.zone.2018)
plot(plot_grid_fish)

fish_eff_pres<-ggarrange(fish.zone.2012 + rremove("xlab"),fish.zone.2013+ rremove("xlab"),
                         fish.zone.2014+ rremove("xlab"), fish.zone.2015+ rremove("xlab"), 
                         fish.zone.2016+ rremove("xlab"),fish.zone.2017+ rremove("xlab"), 
                         fish.zone.2018+ rremove("xlab"),
                         labels = c("2012", "2013","2014","2015","2016","2017","2018"),
                         font.label = list(size = 30, face = "bold"),
                         common.legend = TRUE, legend = "top")
fish_eff_pres

plot(fish.zone.2012)
plot(fish.zone.2013)
plot(fish.zone.2014)
plot(fish.zone.2015)
plot(fish.zone.2016)
plot(fish.zone.2017)
plot(fish.zone.2018)
plot(fish.zone.2019)

# saving files
ggsave("Fishing_Effort_2012-2018.png", plot = plot_grid_fish, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
ggsave("Fishing_Effort_2012-2018_pres.png", plot = fish_eff_pres, device = "png", path="Final_Figures_Tables/",scale=1.2, width=18, height=10, units="in")
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
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure", #standardize the legend with the range you calculated for lobsters
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2012"), x="Total Landings per sqkm", y=NULL) + #label the title and X axis of maps
  theme(panel.background = element_rect(fill = "white", colour = "black"))  #set the theme as blue and white 
lobster.zone.2013 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2013), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2013"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) 
lobster.zone.2014 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2014), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2014"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
lobster.zone.2015 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2015), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2015"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
lobster.zone.2016 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2016), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2016"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
lobster.zone.2017 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2017), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2017"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
lobster.zone.2018 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2018), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2018"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
lobster.zone.2019 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2019), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity for 2019"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot_grid_lobster <-plot_grid(lobster.zone.2012,lobster.zone.2013,lobster.zone.2014,lobster.zone.2015,lobster.zone.2016,lobster.zone.2017,lobster.zone.2018)

# saving files
ggsave("Lobster_Effort_2012-2018.png", plot = plot_grid_lobster, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
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
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure", #format the legend
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) + 
  labs(title = paste0("Map of Conch Fishing Intensity for 2012"), x="Total landings per sqkm", y=NULL) + #format the labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black")) #set the theme as blue and white
conch.zone.2013 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2013), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2013"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) 
conch.zone.2014 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2014), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2014"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
conch.zone.2015 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2015), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2015"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
conch.zone.2016 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2016), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2016"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
conch.zone.2017 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2017), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2017"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
conch.zone.2018 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2018), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2018"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
conch.zone.2019 <- ggplot() +
  geom_sf(data=filter(zone.ind8,Year==2019), aes(fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",
                      na.value="gray",limits=c(0,max(zone.ind.range.conch[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity for 2019"), x="Total landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot_grid_conch<-plot_grid(conch.zone.2012,conch.zone.2013,conch.zone.2014,conch.zone.2015,conch.zone.2016,conch.zone.2017,conch.zone.2018)

# saving files
ggsave("Conch_Effort_2012-2018.png", plot = plot_grid_conch, device = "png", path="Final_Figures_Tables/",scale = 1.25,width = 12, height = 8,units="in")
ggsave("Conch_Effort_2012.png", plot = conch.zone.2012, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2013.png", plot = conch.zone.2013, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2014.png", plot = conch.zone.2014, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2015.png", plot = conch.zone.2015, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2016.png", plot = conch.zone.2016, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2017.png", plot = conch.zone.2017, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2018.png", plot = conch.zone.2018, device = "png", path="Final_Figures_Tables/")
ggsave("Conch_Effort_2019.png", plot = conch.zone.2019, device = "png", path="Final_Figures_Tables/")

##################### visualization of sub-species means over the years ##################################
family.subset <- fish.species %>%
  group_by(Year)%>%
  filter(family %in% c("Acanthuridae","Lutjanidae","Scaridae","Serranidae"))
head(family.subset)

parrotfish.weight.year <-mean.family.weight%>%
  group_by(Year)%>%
  filter(family=="Scaridae", Year<2019)
head(parrotfish.weight.year)

redhind.weight.year <-mean.fish.weight%>%
  group_by(Year)%>%
  filter(Species_latin_name=="Epinephelus guttatus", Year<2019)
head(redhind.weight.year)

family.counts <- tapply(mean.family.weight$num.samples, list(mean.family.weight$Year, mean.family.weight$family), sum)
barplot(family.counts, main="Number of Individuals Caught per Family per Year",
        xlab="Scientific Family Name", ylab="Number of Individuals", ylim=c(0, 60),
        col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
        legend = rownames(family.counts), beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)

parrotfish.counts<- tapply(parrotfish.weight.year$num.samples, list(parrotfish.weight.year$Year), sum)
barplot(parrotfish.counts, main="Number of Parrotfish Caught per Year",
        xlab="Year", ylab="Number of Individuals", ylim=c(0, 50),
        col=c("darkblue"),
        legend = NULL, beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)
parrotfish<-ggplot(parrotfish.weight.year,aes(Year))
parrotfish+geom_bar(aes(weight=num.samples))

#sample subset for redhind by year
redhind.subset <- fish.species %>%
  group_by(Species_latin_name,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(Species_latin_name),
         Species_latin_name %in% c("Epinephelus guttatus", na.rm=T),
         Year<2019)
head(redhind.subset)

redhind_years_sum<-ggplot(redhind.subset, mapping = aes(x=Year, y=ind.fish.weight))
redhind_years_sum+geom_point(color="blue",size=6)+ylim(0,200)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Redhind Caught per Year From 2012-2018")
ggsave("Redhind_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

redhind_years_avg<-ggplot(redhind.subset, mapping = aes(x=Year, y=mean.fish.weight))
redhind_years_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,5)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Redhind Catch per Trip per Year From 2012-2018")
ggsave("Redhind_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#sample subset for redhind by month
redhind.subset.month <- fish.species %>%
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Species_latin_name,Month) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(Species_latin_name),
         Species_latin_name %in% c("Epinephelus guttatus", na.rm=T))
head(redhind.subset.month)

redhind_month_sum<-ggplot(redhind.subset.month, mapping = aes(x=Month, y=ind.fish.weight))
redhind_month_sum+geom_point(color="blue",size=6)+ylim(0,150)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(1,12 , by = 1))+
  labs(x="Month", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Redhind Caught Seasonally From 2012-2018")
ggsave("Redhind_Month_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

redhind_month_avg<-ggplot(redhind.subset.month, mapping = aes(x=Month, y=mean.fish.weight))
redhind_month_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,5)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(1,12 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Redhind Catch per Trip per Month From 2012-2018")
ggsave("Redhind_Month_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#sample subset for parrotfish
parrotfish.subset <- fish.species %>%
  group_by(family,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(family),
         family %in% c("Scaridae", na.rm=T),
         Year<2019)
head(parrotfish.subset)

parrotfish_years_sum<-ggplot(parrotfish.subset, mapping = aes(x=Year, y=ind.fish.weight))
parrotfish_years_sum+geom_point(color="blue",size=6)+ylim(0,100)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Parrotfish Caught per Year From 2012-2018")
ggsave("Parrotfish_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

parrotfish_years_avg<-ggplot(parrotfish.subset, mapping = aes(x=Year, y=mean.fish.weight))
parrotfish_years_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,3)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Parrotfish Catch per Trip per Year From 2012-2018")
ggsave("Parrotfish_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

##################### assessing annual Fishing Intensity by gear type and prepping for maps #################  
# changing the character type and joining the fishing summaries with the spatial geometries
gis.dir <- "/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/Fisheries_Zones/"
#gis.dir <-"R:/Gill/spatial-fisheries-analysis/tables/raw/Fisheries_Zones"

allfiles <- list.files(gis.dir,recursive = T, full.names = T) 
# Select kml files with 1) digit then 1 letter, 2) digit then 2 letters, 3) digit then .kml, 4) digit then buffer
file.list.gear <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
                    grep("St_Eustatius_*",allfiles,value = T))

zone.ind.gear <- st_read(file.list.gear[1])
zone.ind.gear$zone_id <- as.character(gsub("Zone_","", zone.ind.gear$Name)) # removes all non-digit characters

for (i in (2:length(file.list.gear))) {
  # retrieve kml 
  X <- st_read(file.list.gear[i]) 
  # extract zone_id from file name 
  X$zone_id <- as.character(gsub("Zone_","", X$Name)) # removes all non-digit characters
  # combine X to the previous shp
  zone.ind.gear <- rbind(zone.ind.gear,X) 
}
# head map
plot(st_geometry(zone.ind.gear))

#create areas for erasing and intersecting for the Marine park (A) and land area (B) and zones 
A<-zone.ind.gear
B<-st_read(file.list.gear[9])
# Erase land area (B) from marine park polygon (A)
zone.ind.gear <-st_difference(A,B) # erase area in the remaining zones that overlap with A 
plot(st_geometry(zone.ind.gear))


# get area
zone.ind.gear$area_m2 <- as.numeric(st_area(zone.ind.gear))

fishing.zones.gear <-zone.fish.gear%>%
  mutate (zone_id = as.numeric(zone_id))

zone.ind2.gear <- zone.ind.gear %>% 
  mutate (zone_id = as.numeric(zone_id))%>%
  left_join(fishing.zones.gear, by = "zone_id")%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (fishing_pressure=weight.total/area_km2)%>%
  select(Name,zone_id,Gear,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
  rename(weight_kg=weight.total)
head(zone.ind2.gear)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.gear <- range(zone.ind2.gear$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.gear #check to make sure it worked 


# Plot fishing pressure maps by gear type 
fish.zone.DL.HL <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.gear,Gear=="HL"| Gear=="DL"), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear,Gear=="HL"| Gear=="DL"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.gear[2]))) +
  labs(title = paste0("Drop and Hand Line"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"),
        plot.title = element_text(size=25, face="bold"))
plot(fish.zone.DL.HL)

fish.zone.SD.FD <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.gear,Gear=="SD"| Gear=="FD"), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear,Gear=="SD"| Gear=="FD"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.gear[2]))) +
  labs(title = paste0("Scuba and Free Diving"), x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold") ,
        plot.title = element_text(size=25, face="bold"))
plot(fish.zone.SD.FD)

fish.zone.LL.TR <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear,Gear=="LL"| Gear =="TR"), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear,Gear=="LL"| Gear =="TR"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      limits=c(0,max(zone.ind2.range.gear[2]))) +
  labs(title = paste0("Trolling and Long Line"), x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"),
        plot.title = element_text(size=25, face="bold"))
plot(fish.zone.LL.TR)

fish.zone.PT <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.gear,Gear=="PT"), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear,Gear=="HL"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.gear[2]))) +
  labs(title = paste0("Pot Traps"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold") ,
        plot.title = element_text(size=25, face="bold"))
plot(fish.zone.PT)

fish.zone.NET <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear,Gear=="NET"), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear,Gear=="HL"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.gear[2]))) +
  labs(title = paste0("Beach Seine"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"),
        plot.title = element_text(size=25, face="bold"))
plot(fish.zone.NET)

plot_grid_fish_gear<-plot_grid(fish.zone.DL.HL,fish.zone.LL.TR,fish.zone.SD.FD, 
                               fish.zone.PT, fish.zone.NET)
plot(plot_grid_fish_gear)

fish_eff_gear_pres<-ggarrange(fish.zone.DL.HL+ rremove("xlab"),fish.zone.LL.TR+ rremove("xlab"),
                              fish.zone.SD.FD+ rremove("xlab"), fish.zone.PT+ rremove("xlab"), 
                              fish.zone.NET+ rremove("xlab"),
                              common.legend = TRUE, legend = "top")

ggsave("Fishing_Effort_by_Gear_Type.png", plot = plot_grid_fish_gear, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
ggsave("Fishing_Effort_by_Gear_Type_pres.png", plot = fish_eff_gear_pres, device = "png", path="Final_Figures_Tables/",scale=1.2, width=18, height=10, units="in")

#looking at individual gears by year through 2018 
fishing.zones.gear.year <-zone.fish.gear.year%>%
  mutate (zone_id = as.numeric(zone_id))

zone.ind2.gear.year<-zone.ind.gear%>%
  mutate (zone_id = as.numeric(zone_id))%>%
  left_join(fishing.zones.gear.year, by = "zone_id")%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (fishing_pressure=weight.total/area_km2)%>%
  select(Name,zone_id,Year,Gear,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
  rename(weight_kg=weight.total)
head(zone.ind2.gear.year)
#looking at pot trap numbers by zone through 2018
zone.ind2.gear.pottrap<-zone.ind2.gear.year%>%
  filter(Gear=="PT")
head(zone.ind2.gear.pottrap)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.pottrap <- range(zone.ind2.gear.pottrap$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.pottrap #check to make sure it worked 

fish.zone.pottrap.2012 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2012), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2012"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2012)

fish.zone.pottrap.2013 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2013), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2013), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2013"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2013)

fish.zone.pottrap.2014 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2014), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2014), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2014"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2014)

fish.zone.pottrap.2015 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2015), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2015), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2015"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2015)

fish.zone.pottrap.2016 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2016), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2016"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2016)

fish.zone.pottrap.2017 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2017), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2017), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2017"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2017)

fish.zone.pottrap.2018 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.pottrap,Year==2018), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2018), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.pottrap[2]))) +
  labs(title = paste0("Fishing Intensity by Pot Trap for 2018"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.pottrap.2018)

plot_grid_fish_pottrap<-plot_grid(fish.zone.pottrap.2012, fish.zone.pottrap.2013, fish.zone.pottrap.2014,
                                  fish.zone.pottrap.2015, fish.zone.pottrap.2016, fish.zone.pottrap.2017,
                                  fish.zone.pottrap.2018)

ggsave("Fishing_Effort_by_Pottrap_2012_2018.png", plot = plot_grid_fish_pottrap, device = "png", path="Final_Figures_Tables/",
       scale = 1.25, width=12, height=8, units="in")

#looking at diving numbers by zone through 2018
zone.ind2.gear.diving<-zone.ind2.gear.year%>%
  filter(Gear=="FD" | Gear=="SD")
head(zone.ind2.gear.diving)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.diving <- range(zone.ind2.gear.diving$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.diving #check to make sure it worked 

fish.zone.diving.2012 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2012), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2012"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2012)

fish.zone.diving.2013 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2013), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2013), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2013"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2013)

fish.zone.diving.2014 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2014), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2014), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2014"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2014)

fish.zone.diving.2015 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2015), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2015), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2015"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2015)

fish.zone.diving.2016 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2016), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2016"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2016)

fish.zone.diving.2017 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2017), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2017), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2017"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2017)

fish.zone.diving.2018 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.diving,Year==2018), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2018), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.diving[2]))) +
  labs(title = paste0("Fishing Intensity by Diving for 2018"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.diving.2018)

plot_grid_fish_diving<-plot_grid(fish.zone.diving.2012, fish.zone.diving.2013, fish.zone.diving.2014,
                                 fish.zone.diving.2015, fish.zone.diving.2016, fish.zone.diving.2017,
                                 fish.zone.diving.2018)

ggsave("Fishing_Effort_by_Diving_2012_2018.png", plot = plot_grid_fish_diving, device = "png", path="Final_Figures_Tables/",
       scale = 1.25, width=12, height=8, units="in")

#looking at trolling numbers by zone through 2018
zone.ind2.gear.trolling<-zone.ind2.gear.year%>%
  filter(Gear=="TR")
head(zone.ind2.gear.trolling)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.trolling<- range(zone.ind2.gear.trolling$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.trolling #check to make sure it worked 

fish.zone.trolling.2012 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2012), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2012"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2012)

fish.zone.trolling.2013 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2013), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2013), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2013"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2013)

fish.zone.trolling.2014 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2014), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2014), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2014"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2014)

fish.zone.trolling.2015 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2015), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2015), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2015"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2015)

fish.zone.trolling.2016 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2016), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2016"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2016)

fish.zone.trolling.2017 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2017), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2017), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2017"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2017)

fish.zone.trolling.2018 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.trolling,Year==2018), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2018), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.trolling[2]))) +
  labs(title = paste0("Fishing Intensity by Trolling for 2018"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.trolling.2018)

plot_grid_fish_trolling<-plot_grid(fish.zone.trolling.2012, fish.zone.trolling.2013, fish.zone.trolling.2014,
                                   fish.zone.trolling.2015, fish.zone.trolling.2016, fish.zone.trolling.2017,
                                   fish.zone.trolling.2018)

ggsave("Fishing_Effort_by_Trolling_2012_2018.png", plot = plot_grid_fish_trolling, device = "png", path="Final_Figures_Tables/",
       scale = 1.25, width=12, height=8, units="in")

#looking at hand line numbers by zone through 2018
zone.ind2.gear.handline<-zone.ind2.gear.year%>%
  filter(Gear=="HL")
head(zone.ind2.gear.handline)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.handline<- range(zone.ind2.gear.handline$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.handline #check to make sure it worked 

fish.zone.handline.2012 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2012), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2012"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2012)

fish.zone.handline.2013 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2013), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2013), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2013"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2013)

fish.zone.handline.2014 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2014), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2014), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2014"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2014)

fish.zone.handline.2015 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2015), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2015), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2015"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2015)

fish.zone.handline.2016 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2016), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2016"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2016)

fish.zone.handline.2017 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2017), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2017), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2017"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2017)

fish.zone.handline.2018 <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind2.gear.handline,Year==2018), aes( fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.gear.year,Year==2018), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind2.range.handline[2]))) +
  labs(title = paste0("Fishing Intensity by Handline for 2018"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(fish.zone.handline.2018)

plot_grid_fish_handline<-plot_grid(fish.zone.handline.2012, fish.zone.handline.2013, fish.zone.handline.2014,
                                   fish.zone.handline.2015, fish.zone.handline.2016, fish.zone.handline.2017,
                                   fish.zone.handline.2018)

ggsave("Fishing_Effort_by_Handline_2012_2018.png", plot = plot_grid_fish_handline, device = "png", path="Final_Figures_Tables/",
       scale = 1.25, width=12, height=8, units="in")


################## assessing annual lobster Fishing Intensity by gear type and prepping for maps #################  

lobster.zones.gear <-zone.lob.gear%>%
  mutate (zone_id = as.character(zone_id))

zone.ind3.gear <- zone.ind.gear %>% 
  left_join(lobster.zones.gear, by = c("zone_id"))%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (lobster_pressure=ind.total/area_km2)%>%
  select(Name,zone_id,Gear,area_m2,area_km2,ind.total,lobster_pressure, geometry)
head(zone.ind3.gear) 

#calculating the range for use in standardizing the scales of all the plots
zone.ind3.range.gear <- range(zone.ind3.gear$lobster_pressure, na.rm = TRUE) 
zone.ind3.range.gear #check to make sure it worked 


# Plot fishing pressure maps by gear type 
lob.zone.FD <- ggplot() +
  geom_sf(data=filter(zone.ind3.gear,Gear=="FD"), aes( fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="FD"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind3.range.gear[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity by Free Diving"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(lob.zone.FD)

lob.zone.SD <- ggplot() +
  geom_sf(data=filter(zone.ind3.gear,Gear=="SD"), aes( fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="SD"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind3.range.gear[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity by Scuba Diving"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(lob.zone.SD)

lob.zone.PT <- ggplot() +
  geom_sf(data=filter(zone.ind3.gear,Gear=="PT"), aes( fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="PT"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind3.range.gear[2]))) +
  labs(title = paste0("Map of Lobster Fishing Intensity by Pot Trap"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(lob.zone.PT)

plot_grid_lob_gear<-plot_grid(lob.zone.FD,lob.zone.PT,lob.zone.SD)

ggsave("Lobster_Fishing_Effort_by_Gear_Type.png", plot = plot_grid_lob_gear, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")

################## assessing annual conch Fishing Intensity by gear type and prepping for maps #################
conch.zones.gear <-zone.conch.gear%>%
  mutate (zone_id = as.character(zone_id))

zone.ind4.gear <- zone.ind.gear %>% 
  left_join(conch.zones.gear, by = c("zone_id"))%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (conch_pressure=ind.total/area_km2)%>%
  select(Name,zone_id,Gear,area_m2,area_km2,ind.total,conch_pressure, geometry)
head(zone.ind4.gear) 

#calculating the range for use in standardizing the scales of all the plots
zone.ind4.range.gear <- range(zone.ind4.gear$conch_pressure, na.rm = TRUE) 
zone.ind4.range.gear #check to make sure it worked 

# Plot fishing pressure maps by gear type 
conch.zone.FD <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind4.gear,Gear=="FD"), aes( fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="FD"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind4.range.gear[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity by Free Diving"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(conch.zone.FD)

conch.zone.SD <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind4.gear,Gear=="SD"), aes( fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="SD"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind4.range.gear[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity by Scuba Diving"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(conch.zone.SD)

conch.zone.PT <- ggplot() +
  geom_sf(data=zone.ind.gear, fill="grey")+
  geom_sf(data=filter(zone.ind4.gear,Gear=="PT"), aes( fill = conch_pressure)) +
  geom_sf_label(data=filter(zone.ind3.gear,Gear=="PT"), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Conch Fishing Pressure",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind4.range.gear[2]))) +
  labs(title = paste0("Map of Conch Fishing Intensity by Pot Trap"), x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"))
plot(conch.zone.PT)

plot_grid_conch_gear<-plot_grid(conch.zone.FD,conch.zone.PT,conch.zone.SD)
ggsave("Conch_Fishing_Effort_by_Gear_Type.png", plot = plot_grid_conch_gear, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
################### seasonal plots for fish, lobsters, and conch, no zones########################
#looking at fish monthly season totals
fish.months.season <- zones.fish %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
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
  ggtitle("Seasonality of Total Weight of Fish Caught From 2012-2018")
ggsave("Fish_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

fishing_seasons_avg<-ggplot(fish.months.season, mapping = aes(x=Month, y=avg.wt.per.trip))
fishing_seasons_avg+
  geom_errorbar(aes(ymin=avg.wt.per.trip-se.avg.wt, ymax=avg.wt.per.trip+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.wt.per.trip-1.96*se.avg.wt, ymax=avg.wt.per.trip+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Weight per Trip (kg)")+
  ggtitle("Seasonality of Average Catch of Fish per Trip From 2012-2018")
ggsave("Fish_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at the total amount of fish caught each year and looking for total changes
fish.years <- zones.fish %>% 
  group_by(Year)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)%>%
  filter(Year<2019)
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
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Total Weight of Fish Caught per Year From 2012-2018")
ggsave("Fish_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create better looking plots of yearly summaries 
fishing_years_avg<-ggplot(fish.years, mapping = aes(x=Year, y=avg.wt.per.trip))
fishing_years_avg+
  geom_errorbar(aes(ymin=avg.wt.per.trip-se.avg.wt, ymax=avg.wt.per.trip+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.wt.per.trip-1.96*se.avg.wt, ymax=avg.wt.per.trip+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,35)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Average Fish Catch per Trip per Year From 2012-2018")
ggsave("Fish_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at the total amount of fish caught each year per gear and looking for total changes
fish.years.gears <- zones.fish %>% 
  group_by(Year, Gear)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)%>%
  filter(Year<2019)
head(fish.years.gears)

fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="HL","DL.HL",fish.years.gears$Gear) ## edit the gears to group them 
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="DL","DL.HL",fish.years.gears$Gear) ## edit the gears to group them 
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="SD","SD.FD",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="FD","SD.FD",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="TR","TR.LL",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="LL","TR.LL",fish.years.gears$Gear) ## edit the years

fish.years.gears.sum<-fish.years.gears%>%
  group_by(Year, Gear)%>% 
  summarize(weight.total=sum(weight.total,na.rm = T))%>%
  filter(Year<2019)
head(fish.years.gears.sum)

plot(fish.years.gears$weight.total~fish.years.gears$Year, 
     col=fish.years.gears$Year)

#using ggplot to create better looking plots of yearly summaries for gear weight totals 
fishing_years_gears_sum_plot<-ggplot(fish.years.gears.sum, mapping = aes(x=Year, y=weight.total, fill=Gear, 
                                                                         label=sprintf("%0.2f", round(weight.total, digits = 2))))
fishing_years_gears_sum_plot+
  geom_bar(stat="identity")+ylim(0,4000)+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5))+
  #geom_text(size = 3, hjust = 0.5, vjust = 1.5, position =     "stack")+
  #geom_text(hjust = 0.5, vjust = 3, position =     "stack")+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=15),
        legend.key.size = unit(.5, "inches"))+
  scale_fill_manual(values=c("yellowgreen","DARKOLIVEGREEN","MEDIUMSEAGREEN","DARKGREEN","DARKSEAGREEN"),
                    labels=c("DL.HL", "BS", "PT", "SD.FD","TR.LL"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Total Weight of Fish Landings per Gear From 2012-2018")
ggsave("Fish_Year_Gear_Totals_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at lobsters monthly to see if there is seasonality
lob.months.season <- zones.lob %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)
head(lob.months.season)
plot(lob.months.season$ind.total~lob.months.season$Month)

#using ggplot to create maps of lobster seasonality from 2012-2019
lobster_season_sum<-ggplot(lob.months.season, mapping = aes(x=Month, y=ind.total))
lobster_season_sum+geom_point(color="blue",size=6)+ylim(0,5000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Seasonality of Total Number of Lobsters Caught From 2012-2018")
ggsave("Lobster_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#average lobster catch per month for all of the years summed from 2012-2019
lobster_season_avg<-ggplot(lob.months.season, mapping = aes(x=Month, y=avg.ind.per.trip))
lobster_season_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Number of Individuals Per Trip")+
  ggtitle("Seasonality of Average Catch of Lobsters per Trip From 2012-2018")
ggsave("Lobster_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at lobsters yearly totals to track any yield changes 
lob.years <- zones.lob %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)%>%
  filter(Year<2019)
head(lob.years)
plot(lob.years$ind.total~lob.years$Year)

#using ggplot to create plots of lobster yield from 2012-2019
lobster_years_sum<-ggplot(lob.years, mapping = aes(x=Year, y=ind.total))
lobster_years_sum+geom_point(color="blue",size=6)+ylim(0,8000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Total Number of Lobsters Caught per Year From 2012-2018")
ggsave("Lobster_Yield_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create plots of average catch per trip for each year
lobster_year_avg<-ggplot(lob.years, mapping = aes(x=Year, y=avg.ind.per.trip))
lobster_year_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Average Number of Individuals Per Trip")+
  ggtitle("Average Lobster Catch per Trip per Year From 2012-2018")
ggsave("Lobster_Yearly_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at conch
conch.months.season <- zones.conch %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)
head(conch.months.season)
plot(conch.months.season$ind.total~conch.months.season$Month)

#using ggplot to create plots of conch seasonality from 2012-2019
conch_season_sum<-ggplot(conch.months.season, mapping = aes(x=Month, y=ind.total))
conch_season_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Seasonality of Total Number of Conch Caught From 2012-2018")
ggsave("Conch_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#average conch catch per month for all of the years summed from 2012-2019
conch_season_avg<-ggplot(conch.months.season, mapping = aes(x=Month, y=avg.ind.per.trip))
conch_season_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,200)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Number of Individuals Per Trip")+
  ggtitle("Seasonality of Average Catch of Conch per Trip From 2012-2018")
ggsave("Conch_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at conch
conch.years <- zones.conch %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)%>%
  filter(Year<2019)
head(conch.years)
plot(conch.years$ind.total~conch.years$Year)

#using ggplot to create maps of conch seasonality from 2012-2019
conch_years_sum<-ggplot(conch.years, mapping = aes(x=Year, y=ind.total))
conch_years_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Total Number of Conch Caught per Year From 2012-2018")
ggsave("Conch_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create plots of average catch per trip for each year
conch_year_avg<-ggplot(lob.years, mapping = aes(x=Year, y=avg.ind.per.trip))
conch_year_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Average Number of Individuals Per Trip")+
  ggtitle("Average Conch Catch per Trip per Year From 2012-2018")
ggsave("Conch_Yearly_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

###################### plots looking at amount of fish caught in the marine park #####################

file.list.inpark <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
                      grep("Habitat_Map_outline_polygon.kml",allfiles,value = T),
                      grep("St_Eustatius_Land.kml",allfiles,value = T))

zone.ind.inpark <- st_read(file.list.inpark[1])
zone.ind.inpark$zone_id <- as.character(gsub("Zone_","", zone.ind.inpark$Name)) # removes all non-digit characters

for (i in (2:length(file.list.inpark))) {
  # retrieve kml 
  X <- st_read(file.list.inpark[i]) 
  # extract zone_id from file name 
  X$zone_id <- as.character(gsub("Zone_","", X$Name)) # removes all non-digit characters
  # combine X to the previous shp
  zone.ind.inpark <- rbind(zone.ind.inpark,X) 
}
# head map
plot(st_geometry(zone.ind.inpark))

#create areas for erasing and intersecting for the Marine park (A) and land area (B) and zones 
A<-st_read(file.list.inpark[9])
B<-st_read(file.list.inpark[10])
# Erase land area (B) from marine park polygon (A)
A.erase <-st_difference(A,B) # erase area in the remaining zones that overlap with A 
plot(st_geometry(A.erase))
# Intersecting area
zone.clip <- st_intersection(A.erase,zone.ind.inpark)
plot(st_geometry(zone.clip))

# get area
zone.clip$area_m2 <- as.numeric(st_area(zone.clip))

# changing the character type and joining the fishing summaries with the spatial geometries
fishing.zones.inpark <-zone.fish.inpark.year

zone.ind2.inpark <- zone.clip %>%
  mutate (zone_id = as.numeric(zone_id))%>%
  left_join(fishing.zones.inpark, by = "zone_id")%>%
  mutate(area_km2 = area_m2/1000000)%>%
  mutate (fishing_pressure=weight.total/area_km2)%>%
  select(Name,zone_id,Year,area_m2,area_km2,weight.total,fishing_pressure, geometry)%>%
  rename(weight_kg=weight.total)

#calculating the range for use in standardizing the scales of all the plots
zone.ind.inpark.range.fish <- range(zone.ind2.inpark$fishing_pressure, na.rm = TRUE) 
zone.ind.inpark.range.fish #check to make sure it worked 

# Plot fishing pressure maps
fish.inpark.2012 <- ggplot() +    #enable the ggplot layer 
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2012), aes(fill = fishing_pressure)) +  #use the geom_sf to plot spatially
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2013 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2013), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2014 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2014), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2015 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2015), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2016 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2016), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2017 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2017), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2018 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2018), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs( x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark.2019 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2019), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in the Marine Park for 2019"), x="Total Landings per sqkm", y=NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.key.size = unit(1, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))

fish.inpark<-plot_grid(fish.inpark.2012,fish.inpark.2013,fish.inpark.2014, fish.inpark.2015, 
                       fish.inpark.2016, fish.inpark.2017, fish.inpark.2018)
plot(fish.inpark)

fish_eff_inpark_pres<-ggarrange(fish.inpark.2012+ rremove("xlab"),fish.inpark.2013+ rremove("xlab"),
                                fish.inpark.2014+ rremove("xlab"), fish.inpark.2015+ rremove("xlab"), 
                                fish.inpark.2016+ rremove("xlab"), fish.inpark.2017+ rremove("xlab"), 
                                fish.inpark.2018+ rremove("xlab"), 
                                labels = c("2012", "2013","2014","2015","2016","2017","2018"),
                                font.label = list(size = 30, face = "bold"), common.legend = TRUE, legend = "top")
fish_eff_inpark_pres

plot(fish.inpark.2012)
plot(fish.inpark.2013)
plot(fish.inpark.2014)
plot(fish.inpark.2015)
plot(fish.inpark.2016)
plot(fish.inpark.2017)
plot(fish.inpark.2018)
plot(fish.inpark.2019)

# saving files
ggsave("Fishing_Effort_Inpark_2012-2018.png", plot = fish.inpark, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
ggsave("Fishing_Effort_Inpark_2012-2018_pres.png", plot = fish_eff_inpark_pres, device = "png", path="Final_Figures_Tables/",scale=1.2, width=18, height=10, units="in")
ggsave("Fishing_Effort_Inpark_2012.png", plot = fish.inpark.2012, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2013.png", plot = fish.inpark.2013, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2014.png", plot = fish.inpark.2014, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2015.png", plot = fish.inpark.2015, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2016.png", plot = fish.inpark.2016, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2017.png", plot = fish.inpark.2017, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark_2018.png", plot = fish.inpark.2018, device = "png", path="Final_Figures_Tables/")
ggsave("Fishing_Effort_Inpark2019.png", plot = fish.inpark.2019, device = "png", path="Final_Figures_Tables/")
