library(tidyverse)
library(readr)
library(lubridate)
library(readr)
weekly_CRD_death <- read_csv("weekly_CRD_death_with_age_groups.csv")
weather_and_air <- read_csv("weather_and_air_related_covariates.csv")
hk_population <- read_csv("data/hk_population.csv",skip=6)
hk_pop<-hk_population[1:18,-1]
colnames(hk_pop)<-c('age_group',c(1998:2020))
weekly_CRD_death$idx<-seq.int(nrow(weekly_CRD_death))


all_pop<-cbind(c(1998:2020),apply(hk_pop[2:24],2, sum))
colnames(all_pop)<-c('year','pop')

pop_20<-cbind(c(1998:2020),apply(hk_pop[1:4,2:24],2, sum))
colnames(pop_20)<-c('year','pop')

pop_40<-cbind(c(1998:2020),apply(hk_pop[5:8,2:24],2, sum))
colnames(pop_40)<-c('year','pop')

pop_65<-cbind(c(1998:2020),apply(hk_pop[9:13,2:24],2, sum))
colnames(pop_65)<-c('year','pop')

pop_85<-cbind(c(1998:2020),apply(hk_pop[14:17,2:24],2, sum))
colnames(pop_85)<-c('year','pop')

pop_max<-cbind(c(1998:2020),apply(hk_pop[18,2:24],2, sum))
colnames(pop_max)<-c('year','pop')

df_all<-weekly_CRD_death%>%select(weekly_death,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(all_pop,by=c('year'='year'))

df_20<-weekly_CRD_death%>%select(weekly_death_20,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(pop_20,by=c('year'='year'))

df_40<-weekly_CRD_death%>%select(weekly_death_40,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(pop_40,by=c('year'='year'))

df_65<-weekly_CRD_death%>%select(weekly_death_65,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(pop_65,by=c('year'='year'))

df_85<-weekly_CRD_death%>%select(weekly_death_85,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(pop_85,by=c('year'='year'))

df_max<-weekly_CRD_death%>%select(weekly_death_max,idx)%>%left_join(
  weather_and_air,by=c('idx'='idx'))%>%merge(pop_max,by=c('year'='year'))





save(df_all,file = 'crd_data/all_age_weekly_mortality_CRD.rd')
save(df_20,file = 'crd_data/0-20_weekly_mortality_CRD.rd')
save(df_40,file = 'crd_data/20-40_weekly_mortality_CRD.rd')
save(df_65,file = 'crd_data/40-65_weekly_mortality_CRD.rd')
save(df_85,file = 'crd_data/65-85_weekly_mortality_CRD.rd')
save(df_max,file = 'crd_data/85+_weekly_mortality_CRD.rd')


