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
  group_by(Year, Landings) %>%
  filter(Landings=="Spiny Lobster") %>%
  select(Year, Landings, Num_ind)
  #merge (Lob_num<-as.numeric(Num_ind, na.rm=TRUE))
  #summarize(, lobster_num = (sum(Lob_num, na.rm=TRUE)))
  
####################### reading logbook entries for fish, lobster, and conch #############
log.data.Fish <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 2, skip =0)   #import sheet for fish data
names(log.data.Fish)
names(log.data.Fish) <- gsub(" ","_",names(log.data.Fish)) #get rid of the spaces between names
names(log.data.Fish) <- gsub("/","_",names(log.data.Fish)) #get rid of the / between names

log.data.Lobster <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 3, skip =0) # import sheet for lobster data
names(log.data.Lobster)
names(log.data.Lobster) <- gsub(" ","_",names(log.data.L)) #get rid of the spaces between names
names(log.data.Lobster) <- gsub("/","_",names(log.data.L)) #get rid of the / between names

log.data.Conch <- import(paste0(input.dir,"Statia logbook Raw data last update April 20, 2018.xlsx"),
                         which = 4, skip =0) #import sheet for conch data
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

anti.join.fish <-log.data %>% #use this to figure out which ones are not joining or do not have trip IDs
  mutate(Trip_ID=as.character(Trip_ID))%>%
  filter(Landings =="Fish")%>%
  anti_join(log.data.F, by = "Trip_ID")

log.data.L <-log.data.Lobster %>% # create the join data and filter it by year to limit the observations
  filter(!is.na(Year))
names(log.data.L)
#log.data.L$Trip_ID <-type.convert(log.data.L$Trip_ID, as.is =TRUE)

lob.trip.trans <- log.data.L %>% 
  mutate(Trip_ID = as.character(Trip_ID)) # change the Trip ID from logical to a character
  
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

species.year <- joined.fish %>% # looking at number of individuals per species per year 
  group_by(Year.x) %>%
  select(Year.x, Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

distinct.species.month <- joined.fish %>% # looking at the number of distinct species per month
  group_by(Year.x, Month.x) %>%
  select(Year.x, Month.x, Species_latin_name) %>%
  summarize(n_distinct(Species_latin_name))

species.month <- joined.fish %>% # looking at number of individuals per species per month
  group_by(Year.x, Month.x) %>%
  select(Year.x, Month.x,Species_latin_name) %>%
  count(Species_latin_name, name = "Count")

########################### filtering by species per year per gear and per month per gear ###############

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



  
#band_members %>% left_join(band_instruments)
  
#log.data <- ifelse(log.data$Year==2004,2014,log.data$Year)
# filter(!Landings %in% c("Fish","fish","FIsh"))
# ifelse(log.data$Landings %in% c("Fish","fish","FIsh"), "Fish",)
#summary(log.data)
#names(log.data) <- gsub(" ","_",names(log.data))
# gsub(".*_",/,n)


