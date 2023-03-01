library(rio)
library(janitor)
library(tidyverse)


###################################### Import the Data ##############################
input.dir <- 'R:/Gill/research/spatial-fisheries-analysis/tables/raw/' #set the import directory


# import data
fish.dat <- import(paste0(input.dir,"Statia logbook Raw data last update October 2022.xlsx"), #import the correct file and the page of the fisheries 
                   which = "Fish")  %>%     #spreadsheet and tell it where to start from the top
  clean_names() %>% 
  filter(!is.na(species_common_name))


#check names and fish
names(fish.dat)
unique(fish.dat$species_common_name)

# create fake data to get seq 0:50 columns
fake.dat <- data.frame(species_common_name="",length_cm=seq(0,50,1)) # create fake data
fake.dat <- fish.dat %>% 
  slice(0) %>%  # get column names from fish.dat
  bind_rows(fake.dat) # add fake data



fish.dat.gp <- fish.dat %>% 
  mutate(length_cm=as.integer(round(length_cm))) %>%  # round up
  bind_rows(fake.dat) %>% # add fake data
  group_by(species_common_name,year,length_cm) %>% 
  summarise(num=n()) # count unique values
head(fish.dat.gp)

fish.dat.gp1 <- fish.dat.gp %>% 
  spread(length_cm,num,fill = 0) %>% # spread to columns
  filter(!is.na(species_common_name) & !(species_common_name=="")) # get rid of blank values


fish.dat.gp1 %>% view()
