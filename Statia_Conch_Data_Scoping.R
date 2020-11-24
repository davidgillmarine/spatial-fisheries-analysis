install.packages('pacman')
pacman::p_load(sf,rio,ggpubr,tidyverse)


###################################### Import the Data ##############################
input.dir <- '~/OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/' #set the import directory
#input.dir <- 'R:/Gill/research/spatial-fisheries-analysis/tables/raw/' #set the import directory
log.data.total <- import(paste0(input.dir,"Statia logbook Raw data last update Feb 8 2019.xlsx"), #import the correct file and the page of the fisheries 
                         which = 1, skip =1)                                                            #spreadsheet and tell it where to start from the top

########################### select only the columns we are interested in and remove spaces ############
# perhaps consider all lowercase/CamelCase with . or _ between words, no special characters
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

##################looking at how much queen conch was caught and by what type of gear ################

my_landings(log.data,"all","Queen Conch",Num_ind,Year)

my_landings(log.data,"park","Queen Conch",Num_ind,in.park,Year)

my_landings(log.data,"all","Queen Conch",Num_ind,Year,Month)

my_landings(log.data,"all","Queen Conch",Num_ind,Month)

my_landings(log.data,"all","Queen Conch",Num_ind,Year,Gear)

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

#example for using the function "my_landings" 
my_landings(log.data.zone,"all","Queen Conch",ind.per.zone,Year,zone_id)

#number of Queen Conch per zone per year, the number of trips, and the average number of conch caught per trip
zone.conch.year <- zones.conch %>% 
  group_by(Year,zone_id)%>% 
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(zone.conch.year)

#amount of Queen Conch per zone per year per gear, the number of trips, and the average number of conch caught per trip
zone.conch.gear.year<-zones.conch %>%
  group_by(Year, Gear, zone_id) %>%
  summarize(ind.total=sum(ind.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID))%>%
  mutate(avg.ind.per.trip=ind.total/Num.Trips)
head(zone.conch.gear.year)  

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


