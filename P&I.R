library(xgboost)
library(ggplot2)
set.seed(2022)
library(gridExtra)

param0 <- list(
  "objective"  = "count:poisson"
  , "eval_metric" = "poisson-nloglik"
  , "eta" = .05
  , "subsample" = 0.5
  , "colsample_bytree" = 1
  , "min_child_weight" = 1
  , "max_depth" = 4
  , 'monotone_constraints'=c(1,0,0,1,1,1,0,0,0,1,1)
  # "year" "week"  "mean_temp" "pm10"  "no2"      
  #  "o3"   "humidity"  "max_temp"  "min_temp"  "flu_a" "flu_b"    
)
plot_df<-data.frame(week_number = numeric(),
                    actual_death = numeric(),
                    predicted_death = numeric(),
                    age_group = character())
variable_attr<-data.frame(week=numeric(),
                          year=numeric(),
                          pop=numeric(),
                          variable=character(),
                          excess_death=numeric(),
                          age_group=character())
plot_attr<-list()

plot_imp<-list()

load('PI_data/85+_weekly_mortality_PI.rd')
load('PI_data/0-20_weekly_mortality_PI.rd')
load('PI_data/20-40_weekly_mortality_PI.rd')
load('PI_data/40-65_weekly_mortality_PI.rd')
load('PI_data/65-85_weekly_mortality_PI.rd')
load('PI_data/all_age_weekly_mortality_PI.rd')
variable_list<-colnames(df_all[,-c(1,2,3,4,5,6,10,11,12,15,16)])
df_list<-c('20','40','65','85','max','all')
df_name<-c('0-20','20-40','40-65','65-85','85+','all')

h2o.init(nthreads = -1) 

for (i in df_list){
  df<-get(paste('df',i,sep='_'))
  colnames(df)[2]<-'weekly_death'
  df<-df[,-15]
  df$week<-as.numeric(df$week)
  df$pop<-lowess(df$idx, df$pop)$y
  list_variable<-colnames(df)[c(6:9,14)]
  # who<-c(15,45,25,60,0)
  df<-subset(df,year>2009)
  df_train<-subset(df,year<2020)
  xgtrain<-xgb.DMatrix(as.matrix(df_train[,-c(2,3,6,15)]),
                       label = df_train$weekly_death)
  setinfo(xgtrain, "base_margin", log(df_train$pop))
  xg<-xgb.DMatrix(as.matrix(df[,-c(2,3,6,15)]),
                  label = df$weekly_death)
  setinfo(xg, "base_margin", log(df$pop))
  
  xgb<-xgb.cv(data = xgtrain,
              nrounds = 500, params = param0,nfold = 5)
  nround<-xgb$evaluation_log$iter[which.min(xgb$evaluation_log$test_poisson_nloglik_mean)]
  final<-xgboost(data = xgtrain, params = param0, 
                 nrounds = nround,verbose=0)
  pred_xg<-predict(final,xg)
  
  
  plot_imp[[i]]<-xgb.importance(feature_names = colnames(xgtrain),model=final)
  
  
  
  
  df_h2o<-as.h2o(df)
  df_h2o$death_rate<-df_h2o$weekly_death/df_h2o$pop
  df_train<-df_h2o[df_h2o$year<2020,]
  df_test<-df_h2o[df_h2o$year==2020,]
  fit<-h2o.gam(x=c('year','week','mean_temp','pm10','no2','o3','humidity',
                   'max_temp','min_temp','flu_a','flu_b'),y='death_rate',
               training_frame = df_train,nfolds = 5,seed=2022,alpha = 0,
               family = 'gaussian',gam_columns = c('week','humidity','mean_temp'),
               bs=c(0,0,0))
  pred <- h2o.predict(object = fit, newdata = df_h2o)
  pred_gam<-as.data.frame(pred)
  plot_temp<-cbind(df[,c('idx','weekly_death')],pred_gam*df$pop,pred_xg)
  colnames(plot_temp)<-c('week_number','actual_death','predicted_gam','predicted_xg')
  plot_temp$age_group<-df_name[which(df_list==i)]
  plot_df<-rbind(plot_df,plot_temp)
  
  for (j in variable_list){
    df_new<-df
    df_new[,j]<-0
    xg<-xgb.DMatrix(as.matrix(df_new[,-c(2,3,6,15)]) 
                    # label = df$weekly_death
    )
    setinfo(xg, "base_margin", log(df_new$pop))
    pred_var<-predict(final,xg)
    
    df_pred<-cbind(df[,c('week','year','pop')],(pred_xg-pred_var))
    colnames(df_pred)<-c('week','year','pop','death_attr')
    df_pred$age_group<-df_name[which(df_list==i)]
    df_pred$variable<-j
    variable_attr<-rbind(variable_attr,df_pred)
  }
  library(tidyverse)
  library(wesanderson)
  plot_attr[[which(df_list==i)]]<-variable_attr%>%group_by(year,age_group,variable)%>%summarise(yearly_attri=sum(death_attr))%>%
    ggplot(data=)+geom_col(aes(x=as.factor(year),y=yearly_attri,fill=variable),position = "dodge")+
    labs(x='year',y='death counts',title=paste(df_name[which(df_list==i)],'age group'))+
    theme(text=element_text(size=8))+scale_fill_manual(values=wes_palette( name="Darjeeling1"))
  
}



ppl <- list(p1 = arrangeGrob(grobs=plot_attr[1:2]),
            p2 = arrangeGrob(grobs=plot_attr[3:4]),
            p3 = arrangeGrob(grobs=plot_attr[5:6])
)

class(ppl) <- c("arrangelist", class(plot_attr))

ggsave("result/xgboost_varaible_attr_PI.pdf", ppl)




pdf('xgboost_imp_PI.pdf')
par(mfrow = c(3, 2))
for (i in df_list){
  xgb.plot.importance(plot_imp[[i]])
  title(paste(df_name[which(df_list==i)],'age group'))
}
dev.off()

ggplot(data=plot_df,aes(x=week_number))+geom_line(aes(y=actual_death,color='Observed'))+
  geom_line(data=subset(plot_df,week_number<=990),linetype=1,aes(y=predicted_xg,color='XGBoost'))+
  geom_line(data=subset(plot_df,week_number>=990),linetype=6 ,aes(y=predicted_xg,color='XGBoost'))+
  geom_line(data=subset(plot_df,week_number<=990),linetype=1,aes(y=predicted_gam,color='GAM'))+
  geom_line(data=subset(plot_df,week_number>=990),linetype=6 ,aes(y=predicted_gam,color='GAM'))+
  geom_vline(xintercept=991,linetype = 2)+
  scale_x_continuous(breaks = round(seq(from=470,to=1044,by=52.12)),
                     labels = as.character(seq(from=2010,to=2021)))+
  labs(#title='Using 2010 to 2019 data to predict weekly PI death counts of different age groups after 2020',
    x='year',y='death counts')+
  facet_grid(age_group ~ .,scales = "free")+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_blank(),
        strip.background = element_rect(colour="white", fill="white"))+
  scale_color_manual(name = "Method", 
                     labels = c('GAM', 'Observed','XGBoost'),
                     values = c('blue2','grey20','red2'))



variable_attr$variable[variable_attr$variable=='pm10']<-'PM 10'
variable_attr$variable[variable_attr$variable=='o3']<-'O3'
variable_attr$variable[variable_attr$variable=='no2']<-'NO2'
variable_attr$variable[variable_attr$variable=='flu_a']<-'Flu A'
variable_attr$variable[variable_attr$variable=='flu_b']<-'Flu B'


variable_attr%>%group_by(year,age_group,variable)%>%summarise(yearly_attri=sum(death_attr))%>%
  ggplot(data=)+geom_col(aes(x=as.factor(year),y=yearly_attri,fill=variable),position = "dodge")+
  labs(x='year',y='death counts')+
  theme(text=element_text(size=8))+scale_fill_manual(values=wes_palette( name="Darjeeling1"))+
  facet_wrap(age_group ~ .,ncol = 2,scales = "free")+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_blank(),
        strip.background = element_rect(colour="white", fill="white"))

annual_excess<-data.frame(excess_death = numeric(),
                          upr = numeric(),
                          lwr = numeric(),
                          age_group = character(),
                          method=character())
age_list<-unique(plot_df$age_group)  

for (i in age_list){
  excess_temp<-data.frame(excess_death = rep(NA,2),
                          upr = rep(NA,2),
                          lwr = rep(NA,2),
                          age_group = rep(i,2),
                          method=c('gam','xg'))
  df<-plot_df%>%filter(week_number>991)%>%filter(age_group==i)%>%
    mutate(excess=actual_death-predicted_gam)
  excess_gam<-df$excess
  ann<-rep(NA,10000)
  for (j in 1:10000){
    ann[j]<-sum(sample(excess_gam,length(excess_gam),replace=TRUE))
  }
  excess_temp$excess_death[1]<-ann[order(ann)][5000]
  excess_temp$lwr[1]<-ann[order(ann)][250]
  excess_temp$upr[1]<-ann[order(ann)][9750]
  
  
  df<-plot_df%>%filter(week_number>991)%>%filter(age_group==i)%>%
    mutate(excess=actual_death-predicted_xg)
  excess_xg<-df$excess
  ann<-rep(NA,10000)
  for (j in 1:10000){
    ann[j]<-sum(sample(excess_xg,length(excess_xg),replace=TRUE))
  }
  excess_temp$excess_death[2]<-ann[order(ann)][5000]
  excess_temp$lwr[2]<-ann[order(ann)][250]
  excess_temp$upr[2]<-ann[order(ann)][9750]
  annual_excess<-rbind(annual_excess,excess_temp)
}



write.csv(annual_excess,'result/annual_excess_PI.csv',quote=F,row.names = FALSE)




### annual excess rate plot

bar_plot_df<-data.frame(category = character(),
                        excess_rate = numeric(),
                        upr = numeric(),
                        lwr = numeric(),
                        age_group = character(),
                        variable=character())
variable_list<-unique(variable_attr$variable)
age_list<-unique(variable_attr$age_group)
for (i in variable_list){
  for (j in age_list){
    bar_plot_temp<-data.frame(category = c('Pre Pandemic','Pandemic'),
                              excess_rate = rep(NA,2),
                              upr = rep(NA,2),
                              lwr = rep(NA,2),
                              age_group = rep(j,2),
                              variable=rep(i,2))
    temp_df_pre<-variable_attr%>%filter(variable==i)%>%filter(age_group==j)%>%
      filter(year<2020)
    size<-dim(temp_df_pre)[1]
    excess_rate<-rep(NA,10000)
    for (n in 1:10000){
      temp_df<-temp_df_pre[sample(1:size,size,replace=TRUE),]
      excess_rate[n]<-sum(100*temp_df$death_attr/temp_df$pop)/10
    }
    
    bar_plot_temp$excess_rate[1]<-excess_rate[order(excess_rate)][5000]
    bar_plot_temp$lwr[1]<-excess_rate[order(excess_rate)][250]
    bar_plot_temp$upr[1]<-excess_rate[order(excess_rate)][9750]
    
    
    temp_df_pan<-variable_attr%>%filter(variable==i)%>%filter(age_group==j)%>%
      filter(year==2020)
    size<-dim(temp_df_pan)[1]
    excess_rate<-rep(NA,10000)
    for (n in 1:10000){
      temp_df<-temp_df_pan[sample(1:size,size,replace=TRUE),]
      excess_rate[n]<-sum(100*temp_df$death_attr/temp_df$pop)
    }
    
    bar_plot_temp$excess_rate[2]<-excess_rate[order(excess_rate)][5000]
    bar_plot_temp$lwr[2]<-excess_rate[order(excess_rate)][250]
    bar_plot_temp$upr[2]<-excess_rate[order(excess_rate)][9750]
    
    bar_plot_df<-rbind(bar_plot_df,bar_plot_temp)
  }
}
bar_plot_df$category<-factor(bar_plot_df$category,levels = c("Pre Pandemic","Pandemic"))

ggplot(bar_plot_df,aes(x=variable,y=excess_rate,fill=category,group=category))+
  geom_bar(stat="identity",position = "dodge")+
  geom_errorbar(aes(ymin = lwr,ymax = upr),position=position_dodge())+
  facet_wrap(age_group~.,scales='free')+
  labs(x='Air pollutants and influenza proxies',y='Annual excess mortality rate (per 100,000 population)')+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_blank(),
        strip.background = element_rect(colour="white", fill="white"))





#####
#####
##### sensitivity analysis

param0 <- list(
  "objective"  = "count:poisson"
  , "eval_metric" = "poisson-nloglik"
  , "eta" = .05
  , "subsample" = 0.5
  , "colsample_bytree" = 1
  , "min_child_weight" = 1
  , "max_depth" = 4
  , 'monotone_constraints'=c(0,0,0,1,1,1,0,0,0,1,1)
)


test_year<-c(2016:2019)
sensitivity_plot<-list()
eval_matrix<-data.frame(test_year = character(),
                        age_group = character(),
                        train = character(),
                        val = character())
for (j in test_year){
  plot_df<-data.frame(week_number = numeric(),
                      actual_death = numeric(),
                      predicted_death = numeric(),
                      age_group = character())
  for (i in df_list){
    df<-get(paste('df',i,sep='_'))
    colnames(df)[2]<-'weekly_death'
    df<-df[,-15]
    df$week<-as.numeric(df$week)
    df$pop<-lowess(df$idx, df$pop)$y
    list_variable<-colnames(df)[c(6:9,14)]
    # who<-c(15,45,25,60,0)
    df<-subset(df,year>(j-11)&year<(j+1))
    df_train<-subset(df,year<j)
    df_val<-subset(df,year==j)
    xgtrain<-xgb.DMatrix(as.matrix(df_train[,-c(2,3,6,15)]),
                         label = df_train$weekly_death)
    setinfo(xgtrain, "base_margin", log(df_train$pop))
    xgval<-xgb.DMatrix(as.matrix(df_val[,-c(2,3,6,15)]),
                       label = df_val$weekly_death)
    setinfo(xgval, "base_margin", log(df_val$pop))
    xg<-xgb.DMatrix(as.matrix(df[,-c(2,3,6,15)]),
                    label = df$weekly_death)
    setinfo(xg, "base_margin", log(df$pop))
    
    xgb<-xgb.cv(data = xgtrain,
                nrounds = 500, params = param0,nfold = 5)
    nround<-xgb$evaluation_log$iter[which.min(xgb$evaluation_log$test_poisson_nloglik_mean)]
    final<-xgb.train(data = xgtrain, params = param0,watchlist = list(val=xgval,train=xgtrain),
                     nrounds = nround,verbose=0)
    eval_temp<-c(j,i,final$evaluation_log$train_poisson_nloglik[nround],final$evaluation_log$val_poisson_nloglik[nround])
    eval_matrix<-rbind(eval_matrix,eval_temp)
    final$evaluation_log[dim(final$evaluation_log)[1]]
    pred<-predict(final,xg)
    
    plot_temp<-cbind(df[,c('idx','weekly_death')],pred)
    colnames(plot_temp)<-c('week_number','actual_death','predicted_death')
    plot_temp$age_group<-df_name[which(df_list==i)]
    plot_df<-rbind(plot_df,plot_temp)
  }
  sensitivity_plot[[j]]<-ggplot(data=plot_df,aes(x=week_number))+geom_line(aes(y=actual_death,color='real'))+
    geom_line(aes(y=predicted_death,color='predicted'))+
    geom_vline(xintercept=df$idx[which(df$year==j&df$week==1)],linetype = 2)+
    scale_x_continuous(breaks = round(seq(from=min(df$idx),to=max(df$idx)+1,by=52)),
                       labels = as.character(seq(from=j-10,to=j+1)))+
    labs(#title=paste( 'Using',as.character(j-10), 'to',as.character(j-1),
      #'data to predict weekly P&I death counts \n of different age groups in',as.character(j)),
      x='year',y='death counts')+
    facet_grid(age_group ~ .,scales = "free")#+theme(text=element_text(size=6))
  
  
}

ppl <- list(p1 = arrangeGrob(grobs=sensitivity_plot[2016]),
            p2 = arrangeGrob(grobs=sensitivity_plot[2017]),
            p1 = arrangeGrob(grobs=sensitivity_plot[2018]),
            p2 = arrangeGrob(grobs=sensitivity_plot[2019])
)

class(ppl) <- c("arrangelist", class(sensitivity_plot))

ggsave("result/xgboost_sensitivity_PI.pdf", ppl)
sensitivity_plot[2019]

write.csv(eval_matrix,'result/model_valdiation_evaluation_P&I.csv',quote=F,row.names = FALSE)



