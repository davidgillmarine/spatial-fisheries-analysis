## This is a script that imports, cleans, and organizes excel pages for all of the data, and a subset of 
## fisheries, lobster, and conch data
##Use this script first in order to clean and get the data ready for analysis in future scripts and map making



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

#Preparing the whole data script 
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

# This is preparing the scirpt by zone for all species 
log.data.zone <- log.data %>% #rename variable to zone.fish
  select(Rec_ID,Trip_ID,Year,Month,Day,Z1:Z6,Landings,Gear,weight.kg,Num.ind,`max_(m)`,in.park,n.zones,weight.per.zone, ind.per.zone) %>% #select relevent variables
  gather(key="col.nam",value="zone_id",Z1:Z6) %>% #bring these values together, and sum and rename them # swap names
  filter(!is.na(zone_id)) %>%     #filter by zone ID and remove NAs
  arrange(Trip_ID)                # arrange by unique trip ID for clarity
head(log.data.zone)   #check to make sure it was ordered properly

#export all of the clean data sheets so they can be called in later scripts 
#use date as a timestamp in case there are changes 


