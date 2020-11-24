##################### visualization of sub-species means over the years ##################################
family.subset <- fish.species %>%
  group_by(Year)%>%
  filter(family %in% c("Acanthuridae","Lutjanidae","Scaridae","Serranidae"))
head(family.subset)

parrotfish.weight.year <-mean.family.weight%>%
  group_by(Year)%>%
  filter(family=="Scaridae", Year<2019)
head(parrotfish.weight.year)

redhind.weight.year <-mean.fish.weight%>%
  group_by(Year)%>%
  filter(Species_latin_name=="Epinephelus guttatus", Year<2019)
head(redhind.weight.year)

family.counts <- tapply(mean.family.weight$num.samples, list(mean.family.weight$Year, mean.family.weight$family), sum)
barplot(family.counts, main="Number of Individuals Caught per Family per Year",
        xlab="Scientific Family Name", ylab="Number of Individuals", ylim=c(0, 60),
        col=c("red", "orange","blue","darkblue","green", "darkgreen", "purple", "deeppink"),
        legend = rownames(family.counts), beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)

parrotfish.counts<- tapply(parrotfish.weight.year$num.samples, list(parrotfish.weight.year$Year), sum)
barplot(parrotfish.counts, main="Number of Parrotfish Caught per Year",
        xlab="Year", ylab="Number of Individuals", ylim=c(0, 50),
        col=c("darkblue"),
        legend = NULL, beside=TRUE,cex=.7, cex.axis=1, cex.lab =1.5,cex.main=2)
parrotfish<-ggplot(parrotfish.weight.year,aes(Year))
parrotfish+geom_bar(aes(weight=num.samples))

#sample subset for redhind by year
redhind.subset <- fish.species %>%
  group_by(Species_latin_name,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(Species_latin_name),
         Species_latin_name %in% c("Epinephelus guttatus", na.rm=T),
         Year<2019)
head(redhind.subset)

redhind_years_sum<-ggplot(redhind.subset, mapping = aes(x=Year, y=ind.fish.weight))
redhind_years_sum+geom_point(color="blue",size=6)+ylim(0,200)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Redhind Caught per Year From 2012-2018")
ggsave("Redhind_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

redhind_years_avg<-ggplot(redhind.subset, mapping = aes(x=Year, y=mean.fish.weight))
redhind_years_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,5)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Redhind Catch per Trip per Year From 2012-2018")
ggsave("Redhind_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#sample subset for redhind by month
redhind.subset.month <- fish.species %>%
  group_by(Year,Month)%>%
  filter(Year<2019)%>%
  group_by(Species_latin_name,Month) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(Species_latin_name),
         Species_latin_name %in% c("Epinephelus guttatus", na.rm=T))
head(redhind.subset.month)

redhind_month_sum<-ggplot(redhind.subset.month, mapping = aes(x=Month, y=ind.fish.weight))
redhind_month_sum+geom_point(color="blue",size=6)+ylim(0,150)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(1,12 , by = 1))+
  labs(x="Month", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Redhind Caught Seasonally From 2012-2018")
ggsave("Redhind_Month_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

redhind_month_avg<-ggplot(redhind.subset.month, mapping = aes(x=Month, y=mean.fish.weight))
redhind_month_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,5)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(1,12 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Redhind Catch per Trip per Month From 2012-2018")
ggsave("Redhind_Month_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

#sample subset for parrotfish
parrotfish.subset <- fish.species %>%
  group_by(family,Year) %>% 
  summarise(ind.fish.weight=sum(ind.fish.weight,na.rm = T),
            num.samples=n_distinct(Sample_ID),
            mean.fish.weight=mean(ind.fish.weight/num.samples,na.rm = T),
            sd.avg.wt=sd(c(mean.fish.weight, na.rm=T)),
            se.avg.wt = (sd.avg.wt)/(sqrt(num.samples)))%>%
  mutate(ci.upper=mean.fish.weight+sd.avg.wt,
         ci.lower=mean.fish.weight-sd.avg.wt,
         se.lower=mean.fish.weight+se.avg.wt,
         se.upeer=mean.fish.weight-se.avg.wt)%>% 
  filter(!is.na(family),
         family %in% c("Scaridae", na.rm=T),
         Year<2019)
head(parrotfish.subset)

parrotfish_years_sum<-ggplot(parrotfish.subset, mapping = aes(x=Year, y=ind.fish.weight))
parrotfish_years_sum+geom_point(color="blue",size=6)+ylim(0,100)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Total Weight (kg)")+
  ggtitle("Sample Weight of Parrotfish Caught per Year From 2012-2018")
ggsave("Parrotfish_Year_Totals_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))

parrotfish_years_avg<-ggplot(parrotfish.subset, mapping = aes(x=Year, y=mean.fish.weight))
parrotfish_years_avg+
  geom_errorbar(aes(ymin=mean.fish.weight-se.avg.wt, ymax=mean.fish.weight+se.avg.wt), 
                width=0.0, size=2, color="black") +
  geom_errorbar(aes(ymin=mean.fish.weight-1.96*se.avg.wt, ymax=mean.fish.weight+1.96*se.avg.wt), 
                width=0.0, size=0.5, color="black")+
  geom_point(color="blue", size=2)+
  geom_hline(yintercept = 0) +
  ylim(0,3)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25, face="bold"),
        axis.title.y = element_text(size=25, face="bold"),
        plot.title = element_text(size=30, face="bold"))+
  scale_x_continuous(breaks = seq(2012,2018 , by = 1))+
  labs(x="Year", y="Average Weight per Trip (kg)")+
  ggtitle("Sample Average Parrotfish Catch per Trip per Year From 2012-2018")
ggsave("Parrotfish_Year_Avg_2012-2018.png", path="Final_Figures_Tables/", width=14, height=9, units=c("in"))