

######################### mapping lobster catch by individual count ###########################
lobster.zones <-zone.lob.year%>%
  mutate (zone_id = as.character(zone_id))

zone.ind5 <- zone.ind.joiner %>% 
  left_join(lobster.zones, by = c("zone_id", "Year"))%>%
  mutate (lobster_pressure=ind.total/area_km2)%>%
  select(Name,zone_id,Year,area_m2,area_km2,ind.total,lobster_pressure, geometry)
head(zone.ind5)

zone.ind6 <- zone.ind5 %>% 
  group_by(zone_id) %>% 
  summarise(lobster_pressure=mean(lobster_pressure)) %>% 
  filter(!grepl("FAD",zone_id))

#calculating the range for use in standardizing the scales of all the plots
zone.ind.range.lob <- range(zone.ind5$lobster_pressure, na.rm = TRUE) 
zone.ind.range.lob #check to make sure it worked 

# Plot lobster fishing pressure maps
lobster.zone.2012 <- ggplot() +  #initialize ggplot
  geom_sf(data=filter(zone.ind5,Year==2012), aes(fill = lobster_pressure)) + #use the geom_sf function in ggplot to use spatial geometries to make maps
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)", #standardize the legend with the range you calculated for lobsters
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2012"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2012)

lobster.zone.2013 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2013), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2013"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2013)

lobster.zone.2014 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2014), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2014"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2014)

lobster.zone.2015 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2015), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2015"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2015)


lobster.zone.2016 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2016), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2016"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2016)

lobster.zone.2017 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2017), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2017"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2017)

lobster.zone.2018 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2018), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2018"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2018)

lobster.zone.2019 <- ggplot() +
  geom_sf(data=filter(zone.ind5,Year==2019), aes(fill = lobster_pressure)) +
  geom_sf_label(data=filter(zone.ind5,Year==2012), aes(label = zone_id),
                label.padding = unit(0.25, "lines"),
                label.r = unit(0, "lines"), label.size = 0.4)+
  scale_fill_gradient(low="#f7fbff",high="#2171b5",name="Lobster Fishing Intensity (kg/km^2/yr)",
                      na.value="gray",limits=c(0,max(zone.ind.range.lob[2]))) +
  labs(title = paste0("Lobster Fishing Intensity for 2019"),x="Total Landings per sqkm", y=NULL) +  #create the correct labels for the plot
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size=30, face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=20, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(.5, "inches"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15, face="bold"))  #set the theme of the plot to blue and white 
plot(lobster.zone.2019)

plot_grid_lobster <-plot_grid(lobster.zone.2012,lobster.zone.2013,lobster.zone.2014,lobster.zone.2015,lobster.zone.2016,lobster.zone.2017,lobster.zone.2018)

# saving files
ggsave("Lobster_Effort_2012-2018.png", plot = plot_grid_lobster, device = "png", path="Final_Figures_Tables/",scale = 1.25, width=12, height=8, units="in")
ggsave("Lobster_Effort_2012.png", plot = lobster.zone.2012, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2013.png", plot = lobster.zone.2013, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2014.png", plot = lobster.zone.2014, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2015.png", plot = lobster.zone.2015, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2016.png", plot = lobster.zone.2016, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2017.png", plot = lobster.zone.2017, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2018.png", plot = lobster.zone.2018, device = "png", path="Final_Figures_Tables/")
ggsave("Lobster_Effort_2019.png", plot = lobster.zone.2019, device = "png", path="Final_Figures_Tables/")
