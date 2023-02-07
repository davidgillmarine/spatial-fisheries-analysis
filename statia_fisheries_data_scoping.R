install.packages('pacman')
pacman::p_load(sf,rio,ggpubr,cowplot, gridExtra,tidyverse) # tidyverse last so there is no overwrite in package loading and function names


source("fisheries_summary_functions.R")


####################### looking at how much fish was caught and by what type of gear ################
# Example for how to use summarizing script fisheries_summary_functions for Fish landings, this can also 
# be used for Spiny Lobster and Queen Conch 

fish.weight.year<-my_landings(log.data,"all","Fish",weight.kg,Year) #amount of fish caught per year for the whole fishing area
head(fish.weight.year) #this is used to check and make sure that the data was analyzed correctly
export(fish.weight.year,"Final_Figures_Tables-yearly_fishing_effort.xlsx") # This is to export the number of landings and intensity of fishing 
  # for each year for table in paper 

fish.weight.inpark.year<-my_landings(log.data,"park","Fish",weight.kg,Year) #amount of fish caught per year inside the park ##double check with archive 
head(fish.weight.inpark.year) #check your work
export(fish.weight.inpark.year, "Final_Figures_Tables-yearly_fishing_effort_inpark.xlsx") 

#looking at the amount of fish caught per month, looking at seasonality 
fish.weight.month<-my_landings(log.data,"all","Fish",weight.kg,Year,Month)
#looking at the amount of fish caught by gear type
fish.gear<-my_landings(log.data,"all","Fish",weight.kg,Gear)
#looking at the amount of fish caught by year and gear type
fish.gear.year<-my_landings(log.data,"all","Fish",weight.kg,Year,Gear)
#looking at the amount of fish caught by month, and gear type looking for seasonal gear usage
fish.gear.month<-my_landings(log.data,"all","Fish",weight.kg,Month,Gear)


###################################### Zone Analysis For Fish ##########################################
#example for using the function "my_landings"  for zone analysis for fish, which can then be used for map making 
#looking at the aggregation of fish landings per zone in order to calculate fishing intensity 

#amount of fish per zone per year, the number of trips, and the average number of fish per trip
zone.fish.year<-my_landings(log.data.zone,"all","Fish",weight.per.zone,Year,zone_id)
head(zone.fish.year)

#amount of fish per zone per year per gear, the number of trips using that gear, and the average weight using that gear per trip
zone.fish.gear.year<-my_landings(log.data.zone,"all","Fish",weight.per.zone,Year,Gear,zone_id)
head(zone.fish.gear.year)

#looking at the amount of fish caught within the park, aka from 0-30m in depth for each year and by zone
zone.fish.inpark.year<-my_landings(log.data.zone,"park","Fish",weight.per.zone,Year,zone_id)
head(zone.fish.inpark.year)

#looking at the amount of fish caught within the park, aka from 0-30m in depth, for each year, by gear and zone
zone.fish.inpark.gear.year<-my_landings(log.data.zone,"park","Fish",weight.per.zone,Year,Gear,zone_id)
head(zone.fish.inpark.gear.year)


