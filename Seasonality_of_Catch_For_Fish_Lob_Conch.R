
################### seasonal plots for fish, lobsters, and conch, no zones########################
#looking at fish monthly season totals
fish.months.season <- zones.fish %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)
head(fish.months.season)
plot(fish.months.season$weight.total~fish.months.season$Month, 
     col=fish.months.season$Month)
plot(fish.months.season$avg.wt.per.trip~fish.months.season$Month, 
     col=fish.months.season$Month)

fishing_seasons_sum<-ggplot(fish.months.season, mapping = aes(x=Month, y=weight.total))
fishing_seasons_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Total Weight (kg)")+
  ggtitle("Seasonality of Total Weight of Fish Caught From 2012-2018")
ggsave("Fish_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

fishing_seasons_avg<-ggplot(fish.months.season, mapping = aes(x=Month, y=avg.wt.per.trip))
fishing_seasons_avg+
  geom_errorbar(aes(ymin=avg.wt.per.trip-se.avg.wt, ymax=avg.wt.per.trip+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.wt.per.trip-1.96*se.avg.wt, ymax=avg.wt.per.trip+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Weight per Trip (kg)")+
  ggtitle("Seasonality of Average Catch of Fish per Trip From 2012-2018")
ggsave("Fish_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at the total amount of fish caught each year and looking for total changes
fish.years <- zones.fish %>% 
  group_by(Year)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)%>%
  filter(Year<2019)
head(fish.years)
plot(fish.years$weight.total~fish.years$Year, 
     col=fish.years$Year)

#using ggplot to create better looking plots of yearly summaries 
fishing_years_sum<-ggplot(fish.years, mapping = aes(x=Year, y=weight.total))
fishing_years_sum+geom_point(color="blue",size=6)+ylim(0,4000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Total Weight of Fish Caught per Year From 2012-2018")
ggsave("Fish_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create better looking plots of yearly summaries 
fishing_years_avg<-ggplot(fish.years, mapping = aes(x=Year, y=avg.wt.per.trip))
fishing_years_avg+
  geom_errorbar(aes(ymin=avg.wt.per.trip-se.avg.wt, ymax=avg.wt.per.trip+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.wt.per.trip-1.96*se.avg.wt, ymax=avg.wt.per.trip+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,35)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Average Fish Catch per Trip per Year From 2012-2018")
ggsave("Fish_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at the total amount of fish caught each year per gear and looking for total changes
fish.years.gears <- zones.fish %>% 
  group_by(Year, Gear)%>% 
  summarize(weight.total=sum(weight.per.zone,na.rm = T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T), 
            avg.wt.per.trip = mean((weight.total/Num.Trips), na.rm=T),
            sd.avg.wt=sd(c(avg.wt.per.trip, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.wt.per.trip+sd.avg.wt,
         ci.lower=avg.wt.per.trip-sd.avg.wt)%>%
  filter(Year<2019)
head(fish.years.gears)

fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="HL","DL.HL",fish.years.gears$Gear) ## edit the gears to group them 
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="DL","DL.HL",fish.years.gears$Gear) ## edit the gears to group them 
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="SD","SD.FD",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="FD","SD.FD",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="TR","TR.LL",fish.years.gears$Gear) ## edit the years
fish.years.gears$Gear <- ifelse(fish.years.gears$Gear=="LL","TR.LL",fish.years.gears$Gear) ## edit the years

fish.years.gears.sum<-fish.years.gears%>%
  group_by(Year, Gear)%>% 
  summarize(weight.total=sum(weight.total,na.rm = T))%>%
  filter(Year<2019)
head(fish.years.gears.sum)

plot(fish.years.gears$weight.total~fish.years.gears$Year, 
     col=fish.years.gears$Year)

#using ggplot to create better looking plots of yearly summaries for gear weight totals 
fishing_years_gears_sum_plot<-ggplot(fish.years.gears.sum, mapping = aes(x=Year, y=weight.total, fill=Gear, 
                                                                         label=sprintf("%0.2f", round(weight.total, digits = 2))))
fishing_years_gears_sum_plot+
  geom_bar(stat="identity")+ylim(0,4000)+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5))+
  #geom_text(size = 3, hjust = 0.5, vjust = 1.5, position =     "stack")+
  #geom_text(hjust = 0.5, vjust = 3, position =     "stack")+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=15),
        legend.key.size = unit(.5, "inches"))+
  scale_fill_manual(values=c("yellowgreen","DARKOLIVEGREEN","MEDIUMSEAGREEN","DARKGREEN","DARKSEAGREEN"),
                    labels=c("DL.HL", "BS", "PT", "SD.FD","TR.LL"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Total Weight of Fish Landings per Gear From 2012-2018")
ggsave("Fish_Year_Gear_Totals_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at lobsters monthly to see if there is seasonality
lob.months.season <- zones.lob %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)
head(lob.months.season)
plot(lob.months.season$ind.total~lob.months.season$Month)

#using ggplot to create maps of lobster seasonality from 2012-2019
lobster_season_sum<-ggplot(lob.months.season, mapping = aes(x=Month, y=ind.total))
lobster_season_sum+geom_point(color="blue",size=6)+ylim(0,5000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Seasonality of Total Number of Lobsters Caught From 2012-2018")
ggsave("Lobster_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#average lobster catch per month for all of the years summed from 2012-2019
lobster_season_avg<-ggplot(lob.months.season, mapping = aes(x=Month, y=avg.ind.per.trip))
lobster_season_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Number of Individuals Per Trip")+
  ggtitle("Seasonality of Average Catch of Lobsters per Trip From 2012-2018")
ggsave("Lobster_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at lobsters yearly totals to track any yield changes 
lob.years <- zones.lob %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)%>%
  filter(Year<2019)
head(lob.years)
plot(lob.years$ind.total~lob.years$Year)

#using ggplot to create plots of lobster yield from 2012-2019
lobster_years_sum<-ggplot(lob.years, mapping = aes(x=Year, y=ind.total))
lobster_years_sum+geom_point(color="blue",size=6)+ylim(0,8000)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Total Number of Lobsters Caught per Year From 2012-2018")
ggsave("Lobster_Yield_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create plots of average catch per trip for each year
lobster_year_avg<-ggplot(lob.years, mapping = aes(x=Year, y=avg.ind.per.trip))
lobster_year_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Average Number of Individuals Per Trip")+
  ggtitle("Average Lobster Catch per Trip per Year From 2012-2018")
ggsave("Lobster_Yearly_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))

#looking at conch
conch.months.season <- zones.conch %>% 
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Month)%>%
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)
head(conch.months.season)
plot(conch.months.season$ind.total~conch.months.season$Month)

#using ggplot to create plots of conch seasonality from 2012-2019
conch_season_sum<-ggplot(conch.months.season, mapping = aes(x=Month, y=ind.total))
conch_season_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(x="Month", y="Total Number of Individuals")+
  ggtitle("Seasonality of Total Number of Conch Caught From 2012-2018")
ggsave("Conch_Seasonality_Sum_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#average conch catch per month for all of the years summed from 2012-2019
conch_season_avg<-ggplot(conch.months.season, mapping = aes(x=Month, y=avg.ind.per.trip))
conch_season_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,200)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  labs(x="Month", y="Average Number of Individuals Per Trip")+
  ggtitle("Seasonality of Average Catch of Conch per Trip From 2012-2018")
ggsave("Conch_Seasonality_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#looking at conch
conch.years <- zones.conch %>% 
  group_by(Year)%>% 
  summarize(ind.total=sum(ind.per.zone, na.rm=T),
            Num.Trips=n_distinct(Trip_ID, na.rm=T),
            avg.ind.per.trip = mean((ind.total/Num.Trips), na.rm=T),
            sd.avg.ind=sd(c(avg.ind.per.trip, na.rm=T)),
            se.avg.ind = (sd.avg.ind)/(sqrt(Num.Trips)))%>%
  mutate(ci.upper=avg.ind.per.trip+sd.avg.ind,
         ci.lower=avg.ind.per.trip-sd.avg.ind)%>%
  filter(Year<2019)
head(conch.years)
plot(conch.years$ind.total~conch.years$Year)

#using ggplot to create maps of conch seasonality from 2012-2019
conch_years_sum<-ggplot(conch.years, mapping = aes(x=Year, y=ind.total))
conch_years_sum+geom_point(color="blue",size=6)+ylim(0,2500)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Total Number of Individuals")+
  ggtitle("Total Number of Conch Caught per Year From 2012-2018")
ggsave("Conch_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#using ggplot to create plots of average catch per trip for each year
conch_year_avg<-ggplot(lob.years, mapping = aes(x=Year, y=avg.ind.per.trip))
conch_year_avg+
  geom_errorbar(aes(ymin=avg.ind.per.trip-se.avg.ind, ymax=avg.ind.per.trip+se.avg.ind), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=avg.ind.per.trip-1.96*se.avg.ind, ymax=avg.ind.per.trip+1.96*se.avg.ind), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,50)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018, by = 1))+
  labs(x="Year", y="Average Number of Individuals Per Trip")+
  ggtitle("Average Conch Catch per Trip per Year From 2012-2018")
ggsave("Conch_Yearly_Avg_2012-2018.png", path="Final_Figures_Tables/",width=14, height=9, units=c("in"))



