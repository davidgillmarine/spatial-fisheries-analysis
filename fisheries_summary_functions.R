# Functions
sum2=function(x){sum(x,na.rm=TRUE)}
mean2=function(x){mean(x,na.rm=TRUE)}
median2=function(x){median(x,na.rm=TRUE)}
sd2 <- function(x){sd(x,na.rm=TRUE)}
min2 <- function(x){min(x,na.rm=TRUE)}
max2 <- function(x){max(x,na.rm=TRUE)}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


my_summarize <-  function (.data,x,...) { 
  # needed to use variable names in function
  group_var <- quos(...)
  enquo_x <- enquo(x)  # measurement variable
  mean_name <- paste0(quo_name(enquo_x)) # renaming variables
  # group and summarize
  .data %>%
    # filter(!is.na(UQ(enquo_x))) %>% 
    group_by(!!!group_var) %>% 
    summarise(m= mean2(UQ(enquo_x)), med=median2(UQ(enquo_x)),mode=Mode(UQ(enquo_x)),
              sd= sd2(UQ(enquo_x)),n=n(),n_NAs=sum(is.na(UQ(enquo_x))), se=sd/sqrt(sum(!is.na(UQ(enquo_x)))),
              min=min2(UQ(enquo_x)),max=max2(UQ(enquo_x)), qu_1st=quantile(UQ(enquo_x),0.25,na.rm=T), qu_3rd=quantile(UQ(enquo_x),0.75,na.rm=T),
              ci=1.96*se,se_lower=m-se,se_upper=m+se,ci_lower=m-ci,ci_upper=m+ci)  %>% 
    rename(!!mean_name:=m)
}

my_landings <-  function (.data,area="all",sp.gp,val,...) { 
  # needed to use variable names in function
  group_var <- quos(...)
  enquo_sp.gp <- enquo(sp.gp)  # measurement variable
  enquo_val <- enquo(val)  # measurement variable
  # group and summarize
  if(area=="park"){
      area.var <- park.area.sqkm
  }
  else{
    area.var <- fishing.area.sqkm
    }
  .data %>%
    filter(Landings==UQ(enquo_sp.gp)) %>% 
    group_by(!!!group_var) %>% 
    summarise(sum_land= sum2(UQ(enquo_val)),n_trips= n_distinct(Trip_ID),intensity_sqkm=sum_land/area.var)  
}

# Examples
 my_landings(log.data,"park","Spiny Lobster",Num_ind,in.park,Year)
 my_summarize(filter(log.data,Landings=="Fish"),weight.kg,Year,Gear)
