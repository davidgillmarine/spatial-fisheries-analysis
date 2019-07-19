


input.dir <- "/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/"

allfiles <- list.files(input.dir,recursive = T, full.names = T) 

# Select kml files with 1) digit then 1 letter, 2) digit then 2 letters, 3) digit then .kml, 4) digit then buffer
file.list <- c(grep("Zone_[0-9]{1}.kml",allfiles,value = T),
               grep("FAD_[0-9]_[a-z]*",allfiles,value = T))

zone.ind <- st_read(file.list[1])
zone.ind$zone_id <- as.character(gsub("Zone_","", zone.ind$Name)) # removes all non-digit characters
#zone.ind$zone_id <- as.character(gsub(".kml","",zone.ind$Name)) # removes all non-digit characters

for (i in (2:length(file.list))) {
  # retrieve kml 
  X <- st_read(file.list[i]) 
  # extract zone_id from file name 
  X$zone_id <- as.character(gsub("Zone_","", X$Name)) # removes all non-digit characters
  #X$zone_id <- as.character(gsub(".kml","",X$zone_id)) # removes all non-digit characters
  # combine X to the previous shp
  zone.ind <- rbind(zone.ind,X) 
}
zone.ind$zone_id <- gsub("/Users/gcullinan//OneDrive - Duke University/MP Project/spatial-fisheries-analysis/Data/","",zone.ind$zone_id)

# view map
plot(st_geometry(zone.ind))

# get area
zone.ind$area <- st_area(zone.ind)
zone.area <-zone.ind%>%
  rename(area_m2 = area)
zone.area$area_m2<-gsub(" [m^2]", "", zone.area$area_m2)

# Example join
fishing.zones <-zone.fish.year%>%
  mutate (zone_id = as.character(zone_id))

zone.ind2 <- zone.area %>% 
  left_join(fishing.zones, by = "zone_id")%>%
  mutate(area_m2 = as.numeric(area_m2))%>%
  mutate(area_km=area_m2/1000000)




