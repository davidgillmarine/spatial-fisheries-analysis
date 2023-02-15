---
  title: "OEGM script"
author: "David A. Gill & Dana I. Grieco"
date: "3/17/2021"
output: html_document
---
  # Note for Others:
  If you use this script, please acknowledge David A. Gill, Samantha H. Cheng, and Dana I. Grieco in your paper. Thank you!
  
  
  # Setup
  Loads libraries, reads in data, cleans datasets

#######NOTE: This file is named as a Copy, but this is NOT a copy!!! This is the file that has all of the Data cleaning in it as well, and might (?)
  # be a compilation of ALL of the other files combined!!!
  # CAN we clean this by getting rid of all of those files and only sticking with this file?! Is this a true master? I think this would make 
  # life much easier (and get rid of having to edit duplicates all the time...)
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
input.dir <- 'R:/Gill/research/spatial-fisheries-analysis/tables/raw/' #set the import directory - for a PC
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

##################### DIG - Parrotfish (Scaridae family) Prep for LIME ##################################

# parrotfish (Scaridae) subset
scaridae.subset <- fish.species %>%
  group_by(Year)%>%
  filter(family %in% c("Scaridae"))
head(scaridae.subset)

#trying to get the length bins needed for LIME - getting somewhere, but not quite there...need counts
scaridae.length <- fish.species %>%
  group_by(Year, Length_.cm.)%>%
  filter(family=="Scaridae", Year<2019) %>%
  summarize()

##################### DIG - Stoplight Parrotfish Prep for LIME ##################################

# STOPLIGHT parrotfish (Scaridae, Sparisoma viride) subset
stoplight.species.subset <- fish.species %>%
  group_by(Year)%>%
  filter(Species_latin_name %in% c("Sparisoma viride"))
head(stoplight.species.subset)
write.csv(stoplight.species.subset, "R:/Gill/research/spatial-fisheries-analysis/tables/", row.names=TRUE)


##################### visualization of sub-species means over the years ##################################

family.subset <- fish.species %>%
  group_by(Year)%>%
  filter(family %in% c("Acanthuridae","Lutjanidae","Scaridae","Serranidae"))
head(family.subset)



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

#looking at trolling numbers by zone through 2018
zone.ind2.gear.trolling<-zone.ind2.gear.year%>%
  filter(Gear=="TR")
head(zone.ind2.gear.trolling)

#calculating the range for use in standardizing the scales of all the plots
zone.ind2.range.trolling<- range(zone.ind2.gear.trolling$fishing_pressure, na.rm = TRUE) 
zone.ind2.range.trolling #check to make sure it worked 



