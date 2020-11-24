
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