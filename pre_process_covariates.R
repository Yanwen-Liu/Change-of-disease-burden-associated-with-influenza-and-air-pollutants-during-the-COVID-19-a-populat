library(data.table)
library(dplyr)
library(lubridate)
library(readxl)


CompileData <- TRUE

## Data Preprocessing

if (CompileData) {
  d2001 <- read_excel("data/2001_EN.xlsx",skip = 10)
  d2002 <- read_excel("data/2002_EN.xlsx",skip = 10)
  d2003 <- read_excel("data/2003_EN.xlsx",skip = 10)
  d2004 <- read_excel("data/2004_EN.xlsx",skip = 10)
  d2005 <- read_excel("data/2005_EN.xlsx",skip = 10)
  d2006 <- read_excel("data/2006_EN.xlsx",skip = 10)
  d2007 <- read_excel("data/2007_EN.xlsx",skip = 10)
  d2008 <- read_excel("data/2008_EN.xlsx",skip = 10)
  d2009 <- read_excel("data/2009_EN.xlsx",skip = 10)
  d2010 <- read_excel("data/2010_EN.xlsx",skip = 10)
  d2011 <- read_excel("data/2011_EN.xlsx",skip = 10)
  d2012 <- read_excel("data/2012_EN.xlsx",skip = 10)
  d2013 <- read_excel("data/2013_EN.xlsx",skip = 10)
  d2014 <- read_excel("data/2014_EN.xlsx",skip = 10)
  d2015 <- read_excel("data/2015_EN.xlsx",skip = 10)
  d2016 <- read_excel("data/2016_EN.xlsx",skip = 10)
  d2017 <- read_excel("data/2017_EN.xlsx",skip = 10)
  d2018 <- read_excel("data/2018_EN.xlsx",skip = 10)
  d2019 <- read_excel("data/2019_EN.xlsx",skip = 10)
  d2020 <- read_excel("data/2020_EN.xlsx",skip = 10)
  
  
  # USe DATA from 2001 to 2020

  d2015$DATE<-as.Date(d2015$DATE)
  
  
  
  data.pre <- rbind(d2001, d2002, d2003, d2004,
                    d2005, d2006, d2007, d2008,
                    d2009, d2010, d2011, d2012,
                    d2013, d2014, d2015, d2016,d2017,d2018,d2019,d2020)
  
  
  colnames(data.pre) <- c('date', 'station', 'hour', 'co', 'fsp', 'no2', 'nox', 'o3', 'rsp', 'so2')
  colnames(data.pre)
  
  #save(data.pre, file = 'predata.rd')
  
  
} else {
  # Read in the saved predata.rd to data.pre
}


data.set <- data.pre %>% select(date,
                                station,
                                hour,
                                fsp,
                                rsp,
                                no2,
                                o3
)


data.set$fsp <- as.numeric(data.set$fsp)
data.set$rsp <- as.numeric(data.set$rsp)
data.set$no2 <- as.numeric(data.set$no2)
data.set$o3 <- as.numeric(data.set$o3)


data.set$station <- toupper(data.set$station)
data.set$date<-as.Date(data.set$date)



# Group the data by date and station

data.grp <- data.set %>% group_by(date) %>% summarise(pm2.5 = mean(fsp,na.rm = T),
                                                      pm10 = mean(rsp,na.rm = T),
                                                      o3 = mean(o3,na.rm = T),
                                                      no2 = mean(no2,na.rm = T))

# Add weather data

data.w <-read.csv('data/HK_weather_1997_2020.csv', as.is=T)
colnames(data.w)<-c('No.','X','MP','MaxAT','MeanAT','MinAT','MDPT','MRH','MAC','TR','NRV','TBS','DGSR','TE','PWD','MWS') 
# Add date column to data starting before Jan 1 1997.
data.w$date<-as.Date(1:nrow(data.w),origin='1996-12-31')
# Confirm that the last row is 31/12/20
data.p <- as.data.table(data.grp)
data.p$date <- as.Date(data.p$date)
data.p[,2:5]<-round(data.p[,2:5],1)

data.w.only <- data.w %>% select(date, MeanAT, MRH,MaxAT,MinAT)
data.grp.w <- merge(data.p, data.w.only, by='date')

data.grp.w <-filter(data.grp.w ,date>as.Date('2001-01-06') & 
                      date<as.Date('2020-12-27'))
data.grp.w $year<-format(data.grp.w $date, format="%Y")
data.grp.w $week<-format(data.grp.w $date, format="%U")
data.grp.w $year[data.grp.w $week=='00']<-as.character(
  as.numeric(data.grp.w $year[data.grp.w $week=='00'])-1)
y_53<-unique(data.grp.w $year[data.grp.w $week=='53'])

## some years have 53 weeks (2006,2012,2017), while most have 52 weeks

data.grp.w $week[data.grp.w $week=='00' & data.grp.w $year %in% y_53]<-'53'
data.grp.w $week[data.grp.w $week=='00']<-'52'

a<-data.grp.w %>%group_by(week,year)%>%summarise(n_day=length(unique(date)))
unique(a$n_day) == 7
(as.numeric(max(data.grp.w$date)-min(data.grp.w$date))+1)/7 == dim(a)[1]

temp_air<-data.grp.w%>%group_by(year,week)%>%summarise(mean_temp=mean(MeanAT),
                       pm2.5=mean(pm2.5),pm10=mean(pm10),no2=mean(no2),
                       o3=mean(o3),humidity=mean(MRH),max_temp=max(MaxAT),
                       min_temp=min(MinAT))
temp_air[,3:10]<-round(temp_air[,3:10],1)
temp_air$idx<-seq.int(nrow(temp_air))


flu_new <- read_excel("data/flux_data.xlsx",skip=2)

flu_new_se <- flu_new[,c(1,2,4,11:13)]

colnames(flu_new_se)<-c('year','week','end_date',
                         'H1','H3','flu_b')
flu_new_se<-flu_new_se%>%mutate(flu_a=H1+H3)%>%select(-H1,-H3)
flu_new_se$end_date <-as.Date(flu_new_se$end_date)
flu_old <- read_excel("data/lab data 98-16.xls", 
                         sheet = "Laboratory detections")
flu_old[is.na(flu_old)]<-0
flu_old_se<-flu_old[,c(1:6,8)]
colnames(flu_old_se)<-c('week','end_date','total_speci','H1N1','H3','b','H1')
flu_old_se<-flu_old_se%>%mutate(a=H1+H3+H1N1)%>%select(-H1,-H3,-H1N1)
flu_old_se$end_date<-as.Date(as.numeric(flu_old_se$end_date), origin = "1899-12-30")
flu_old_se$year<-format(flu_old_se$end_date, format="%Y")
flu_old_final<-flu_old_se%>%filter(year<2014)%>%mutate(flu_a=a/total_speci,flu_b=b/total_speci)%>%select(
 year,week,end_date,flu_a,flu_b)
flu_all<-rbind(flu_old_final,flu_new_se)
flu_all$start_date<-flu_all$end_date-6
flu_all<-filter(flu_all,start_date>as.Date('2001-01-06') & 
                  start_date<as.Date('2020-12-27'))
flu_all$year<-format(flu_all$start_date, format="%Y")
flu_all$week<-format(flu_all$start_date, format="%U")
flu_all$year[flu_all$week=='00']<-as.character(
  as.numeric(flu_all$year[flu_all$week=='00'])-1)
y_53<-unique(flu_all$year[flu_all$week=='53'])


flu_all$idx<-seq.int(nrow(flu_all))
flu_df<-select(flu_all,idx,flu_a,flu_b,start_date)
df_all<-left_join(temp_air,flu_df)




write.csv(df_all,'weather_and_air_related_covariates.csv',quote=F,row.names = F)
