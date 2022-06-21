library(tidyverse)
library(readr)

x<-read.csv('processed_HK_data.csv')
x$date<-as.Date(x$date,format='%d-%m-%Y')
x$age.code<-as.numeric(x$age.code)
## hk switch to icd-10 code since the first day of January 2001
## (Guidelines for Completion of Medical Certificate of the Cause of Death)
## 2001-01-07 is the first Sunday.

x<-filter(x,date>as.Date('2001-01-06') & date<as.Date('2020-12-27'))
x$year<-format(x$date, format="%Y")
x$week<-format(x$date, format="%U")
x$year[x$week=='00']<-as.character(as.numeric(x$year[x$week=='00'])-1)
y_53<-unique(x$year[x$week=='53'])

## some years have 53 weeks (2006,2012,2017), while most have 52 weeks

x$week[x$week=='00' & x$year %in% y_53]<-'53'
x$week[x$week=='00']<-'52'



a<-x %>%group_by(week,year)%>%summarise(n_day=length(unique(date)))
unique(a$n_day) == 7
(as.numeric(max(x$date)-min(x$date))+1)/7 == dim(a)[1]

## age-specific mortality may sum up slightly less than the all-age death due to
## some patients has no age information (age.unit=X)

all_cause<-x%>%group_by(year,week)%>%filter(is.na(age.code)==FALSE)%>%summarise(weekly_death=n(),
            weekly_death_20=sum(age.unit=='D'|age.unit=='M')+
                    sum(age.unit=='Y' & (age.code<20)),
            weekly_death_40=sum(age.unit=='Y' & age.code>=20 & age.code<40),
            weekly_death_65=sum(age.unit=='Y' & age.code>=40 & age.code<65),
            weekly_death_85=sum(age.unit=='Y' & age.code>=65 & age.code<85),
            weekly_death_max=sum(age.unit=='Y' & age.code>=85))
sum(is.na(all_cause))
card_resp<-x%>%filter(substr(x$essential.cause,1,1) %in% c('I','J'))%>%
              group_by(year,week)%>%summarise(weekly_death=n(),
              weekly_death_20=sum(age.unit=='D'|age.unit=='M'|
              (age.unit=='Y' & age.code<20)),
              weekly_death_40=sum(age.unit=='Y' & age.code>=20 & age.code<40),
              weekly_death_65=sum(age.unit=='Y' & age.code>=40 & age.code<65),
              weekly_death_85=sum(age.unit=='Y' & age.code>=65 & age.code<85),
              weekly_death_max=sum(age.unit=='Y' & age.code>=85))
sum(is.na(card_resp))
p_I<-x%>%filter(substr(x$essential.cause,1,3) %in% 
                  c(paste("J",c('09',10:18),sep = "")))%>%
        group_by(year,week)%>%summarise(weekly_death=n(),
        weekly_death_20=sum(age.unit=='D'|age.unit=='M'|
        (age.unit=='Y' & age.code<20)),
        weekly_death_40=sum(age.unit=='Y' & age.code>=20 & age.code<40),
        weekly_death_65=sum(age.unit=='Y' & age.code>=40 & age.code<65),
        weekly_death_85=sum(age.unit=='Y' & age.code>=65 & age.code<85),
        weekly_death_max=sum(age.unit=='Y' & age.code>=85))
sum(is.na(p_I))
covid<-read_csv("data/enhanced_sur_covid_19_eng.csv")
covid_death<-covid%>%mutate(date=as.Date(`Report date`,'%d/%m/%Y'))%>%
        filter(date>as.Date('2001-01-06') & date<as.Date('2020-12-27'))%>%
        mutate(week=format(date, format="%U"),year=format(date, format="%Y"))%>%
        filter(`Hospitalised/Discharged/Deceased`=='Deceased')%>%
        filter(`HK/Non-HK resident`=='HK resident')%>%group_by(year,week)%>%
        summarise(weekly_death=n(),
                  weekly_death_20=sum(Age<20),
                  weekly_death_40=sum(Age>=20 & Age<40),
                  weekly_death_65=sum(Age>=40 & Age<65),
                  weekly_death_85=sum(Age>=65 & Age<85),
                  weekly_death_max=sum(Age>=85))
temp_df<-all_cause[992:1042,1:2] 
covid_death<-left_join(temp_df,covid_death)
covid_death[is.na(covid_death)]<-0
all_cause[992:1042,3:8]<- all_cause[992:1042,3:8]-covid_death[,3:8]



write.csv(all_cause,'weekly_all_cause_death_with_age_groups.csv',quote=F,row.names = FALSE)
write.csv(card_resp,'weekly_CRD_death_with_age_groups.csv',quote=F,row.names = FALSE)
write.csv(p_I,'weekly_P&I_death_with_age_groups.csv',quote=F,row.names = FALSE)

#####
resp<-x%>%filter(substr(x$essential.cause,1,1) %in% c('J'))%>%
        group_by(year,week)%>%summarise(weekly_death=n(),
                                        weekly_death_20=sum(age.unit=='D'|age.unit=='M'|
                                                                    (age.unit=='Y' & age.code<20)),
                                        weekly_death_40=sum(age.unit=='Y' & age.code>=20 & age.code<40),
                                        weekly_death_65=sum(age.unit=='Y' & age.code>=40 & age.code<65),
                                        weekly_death_85=sum(age.unit=='Y' & age.code>=65 & age.code<85),
                                        weekly_death_max=sum(age.unit=='Y' & age.code>=85))
sum(is.na(resp))
write.csv(resp,'weekly_resp_death_with_age_groups.csv',quote=F,row.names = FALSE)




plot(all_cause$weekly_death-all_cause$weekly_death_max,col='red',type = 'l',
     ylab = 'count',xlab = 'week number')
par(new=TRUE)
plot(p_I$weekly_death-p_I$weekly_death_max,col='blue',type = 'l',axe=F,ylab='',xlab='')
axis(side = 4, at = pretty(p_I$weekly_death-p_I$weekly_death_max))
legend('topleft',c("all cause","p_I"),fill = c('red','blue'))
title('weekly death count for 85- in Hong Kong since 2001')

plot(all_cause$weekly_death[782:1042],col='red',type = 'l',ylab = 'count',xlab = 'week number')
par(new=TRUE)
plot(card_resp$weekly_death[782:1042],col='blue',type = 'l',axe=F,ylab='',xlab='')
axis(side = 4, at =pretty(card_resp$weekly_death))
legend('topleft',c("all cause","card_resp"),fill = c('red','blue'))
title('weekly death count in Hong Kong in recent 5 years')



