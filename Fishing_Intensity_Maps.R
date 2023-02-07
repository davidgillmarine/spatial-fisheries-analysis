

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
  labs(title = paste0("Fishing Intensity for 2012"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
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
  labs(title = paste0("Fishing Intensity for 2013"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2013)

fish.zone.2014 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2014), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2014"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2014)

fish.zone.2015 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2015), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2015"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2015)

fish.zone.2016 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2016), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2016"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2016)

fish.zone.2017 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2017), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2017"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2017)

fish.zone.2018 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2018), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2018"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2018)

fish.zone.2019 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+ 
  geom_sf(data=filter(zone.ind2,Year==2019), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",  #use this to format the scale, set the limits using the range you calculated
                      na.value="gray",limits=c(0,max(zone.ind.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity for 2019"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(fish.zone.2019)

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
  labs(title = paste0("Fishing Intensity in Marine Park for 2012"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2012)

fish.inpark.2013 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2013), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2013"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2013)

fish.inpark.2014 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2014), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2014"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2014)

fish.inpark.2015 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2015), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2015"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2015)

fish.inpark.2016 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2016), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2016"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2016)

fish.inpark.2017 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2017), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2017"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2017)

fish.inpark.2018 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=zone.clip, fill="grey")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2018), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2018"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2018)

fish.inpark.2019 <- ggplot() +
  geom_sf(data=map.fill, fill="beige")+
  geom_sf(data=filter(zone.ind2.inpark,Year==2019), aes(fill = fishing_pressure)) +
  geom_sf_label(data=filter(zone.ind2.inpark,Year==2016), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.inpark.range.fish[2]))) +
  labs(title = paste0("Fishing Intensity in Marine Park for 2019"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))+  #set the theme of the plot to blue and white 
  scale_y_continuous(breaks = seq(17.46, 17.52, by = .02))
plot(fish.inpark.2019)

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

