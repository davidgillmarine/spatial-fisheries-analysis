

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