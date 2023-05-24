library(rio)
library(janitor)
library(tidyverse)

#set working directory to the R Drive
workdir <- "R:/Gill/research/spatial-fisheries-analysis/"
input.dir <- paste0(workdir,"tables/raw/")
spatialdir <-  paste0(workdir,"spatial/raw/") 
plotdir <- paste0(workdir,"output/")
tabledir <- paste0(workdir,"output/")
speciesdir <- "R:/Gill/research/spatial-fisheries-analysis/tables/final/LIME_Outputs/"
  #^directory where all individual species catch will be housed, for LIME analysis


###################################### Import the Data ##############################
input.dir <- 'R:/Gill/research/spatial-fisheries-analysis/tables/raw/' #set the import directory


# import data
fish.dat <- import(paste0(input.dir,"Statia logbook Raw data last update October 2022.xlsx"), #import the correct file and the page of the fisheries 
                   which = "Fish")  %>%     #spreadsheet and tell it where to start from the top
  clean_names() %>% 
  filter(!is.na(species_latin_name))


#check names and fish
names(fish.dat)
unique(fish.dat$species_latin_name)

# create fake data to get seq 0:50 columns
fake.dat <- data.frame(species_latin_name="",length_cm=seq(0,50,1)) # create fake data
fake.dat <- fish.dat %>% 
  slice(0) %>%  # get column names from fish.dat
  bind_rows(fake.dat) # add fake data


fish.dat.gp <- fish.dat %>% 
  mutate(length_cm=as.integer(round(length_cm))) %>%  # round up
  bind_rows(fake.dat) %>% # add fake data
  group_by(species_latin_name,year,length_cm) %>% 
  summarise(num=n()) # count unique values
head(fish.dat.gp)

fish.dat.gp1 <- fish.dat.gp %>% 
  spread(length_cm,num,fill = 0) %>% # spread to columns
  filter(!is.na(species_latin_name) & !(species_latin_name=="")) # get rid of blank values


fish.dat.gp1 %>% view()
  #**NOTE: there is one species who does not have it's species latin name listed - need to try and find it via common name
write.csv(fish.dat.gp1,paste0(speciesdir,today.date,"__ALL_Species.csv"))


###################################### DIG Continuation of this work! ##############################

###  ()
#  ()
#  ()
#  ()


### Acanthuridae ()
# Acanthurus bahianus (ocean surgeonfish)
# Acanthurus chirurgus (doctorfish)
# Acanthurus coeruleus (blue tang)
#  ()
#  ()

### Aulostomidae ()
# Aulostomus maculatus (trumpetfish)
#  ()

### Balistidae ()
# Balistes vetula (queen triggerfish)
#  ()

### Cirrhitidae ()
# Amblycirrhitus pinos (redspotted hawkfish)
#  ()

### Haemulidae ()
# Anisotremus surinamensis (black margate)
#  ()

### Labridae ()
# Bodianus rufus (Spanish hogfish)
#  ()

### Monacanthidae ()
# Cantherhines macrocerus (whitespotted filefish)
# Cantherhines pullus (orangespotted filefish)
#  ()
#  ()

### Ostraciidae ()
# Acanthostracion polygonia (honeycomb cowfish)
#  ()
#  ()
#  ()

### Pomacentridae ()
# Abudefduf saxatilis (sergeant major) 
# Abudefduf taurus (night sergeant)
#  ()
#  ()



### SCARIDAE (PARROTFISH)
#  ()
# Sparisoma viride (Stoplight parrotfish)
Sparisoma.viride <- fish.dat.gp1 %>% 
  filter(species_latin_name == "Sparisoma viride") %>% 
  view ()
write.csv(Sparisoma.viride,paste0(speciesdir,today.date,"_Sparisoma.viride.csv"))
#  ()
#  ()

### Sparidae ()
# Calamus bajonado (jolthead porgy)
#  ()
#  ()


###  ()
#  ()
#  ()
#  ()




#... now switching over to LIME!!!!

