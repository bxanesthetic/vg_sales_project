library(lubridate)
library(tidyverse)
library(scales)
library(gridExtra)

e<-exp(1)
meta_data<-read_csv("E:\\DOWNLOADS\\meta_full_third.csv")
vg_data<-read_csv("E:\\DOWNLOADS\\vgsales_full_third.csv")
igdb_data<-read_csv("E:\\DOWNLOADS\\igdb_full_third.csv")
colnames(igdb_data)<-c('index','game_url_string','series')


#rewording meta_platform to use in later join
meta_data$platform<-gsub('^https://www.metacritic.com/game/','',meta_data$meta_full_url)
meta_data$platform<-gsub('/.*$','',meta_data$platform)


#extracting game url name to use in later join
meta_data$game_url_string<-gsub('^https://www.metacritic.com/game/.*/','',meta_data$meta_full_url)


#list of platforms available in metacritic and equivalent in vgchartz
vg_plat<-c('PS3','X360','PC','WiiU','3DS','PSV','iOS','Wii','DS','PSP','PS2','PS','XB','GC','GBA','DC','PS4','XOne','NS')
meta_plat<-c('playstation-3','xbox-360','pc','wii-u','3ds','playstation-vita','ios','wii','ds','psp', 'playstation-2','playstation','xbox','gamecube','game-boy-advance','dreamcast','playstation-4','xbox-one','switch')


#rewording platforms from vgchartz format to metacritic format 
for (i in 1:length(vg_plat)){
  for (j in 1:length(vg_data$platform))
    if (vg_data$platform[j]==vg_plat[i])
      vg_data$platform[j]<-meta_plat[i]
}


#'K-A' is the precurser to the 'E' rating by the esrb, so it makes sense to change all 'K-A' ratings to 'E'
meta_data$meta_esrb<-gsub("K-A","E",meta_data$meta_esrb)


#Remoing '/r' & '/n' from user score strings 
meta_data$meta_user_score<-gsub('\r','',meta_data$meta_user_score)
meta_data$meta_user_score<-gsub('\n','',meta_data$meta_user_score)


#turning sales data into numerics
vg_data$global_sales<-gsub('m','',vg_data$global_sales)
vg_data$na_sales<-gsub('m','',vg_data$na_sales)
vg_data$eu_sales<-gsub('m','',vg_data$eu_sales)
vg_data$jp_sales<-gsub('m','',vg_data$jp_sales)
vg_data$other_sales<-gsub('m','',vg_data$other_sales)

vg_data$global_sales<-1000000*as.numeric(vg_data$global_sales)
vg_data$na_sales<-1000000*as.numeric(vg_data$na_sales)
vg_data$eu_sales<-1000000*as.numeric(vg_data$eu_sales)
vg_data$jp_sales<-1000000*as.numeric(vg_data$jp_sales)
vg_data$other_sales<-1000000*as.numeric(vg_data$other_sales)


#user_score is not numeric for some reason
meta_data$meta_user_score<-as.numeric(meta_data$meta_user_score)


#parsing_dates
vg_data$release_date<-dmy(vg_data$release_date)


#Sales by platform 
by_platform<-vg_data%>%group_by(platform)%>%summarise(sum_by_platform=sum(global_sales,na.rm=TRUE))
by_platform<-by_platform%>%arrange(desc(sum_by_platform))
by_platform<-by_platform[1:15,]


#Bar_graph sales per platform top 15
ggplot(by_platform,aes(x=reorder(platform, -sum_by_platform),y=sum_by_platform))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab('Top 15 platforms')+ylab('Total Sales')+scale_y_continuous(labels=comma)



#Current gen
cr_platform<-vg_data%>%filter(platform%in%c('playstation-4','xbox-one','switch') )%>%group_by(platform)%>%summarise(sum_by_platform=sum(global_sales,na.rm=TRUE))
cr_platform<-cr_platform%>%mutate(total_sum=sum(sum_by_platform),value=(sum_by_platform/total_sum))


#Graphing current gen percent of sales
ggplot(cr_platform,aes(x='',y=value,fill=platform))+geom_bar(width = 1,stat='identity')+
  coord_polar("y", start=0)+theme(axis.text.x=element_blank()) +
  geom_text(aes(label = percent((value))), position = position_stack(vjust = 0.5))


#by genre graph 
by_genre<-vg_data%>%group_by(game_genre)%>%summarise(genre_sales=sum(global_sales,na.rm=TRUE))

ggplot(by_genre,aes(x=game_genre,y=genre_sales,fill=game_genre))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = comma,expand = expand_scale(mult = c(0, .1)))


#joining both data sets
full_data<-left_join(meta_data,vg_data,by=c('game_url_string','platform'))


full_data<-full_data[,c('name','platform','developer','meta_release_date','publisher','na_sales','eu_sales','jp_sales',
                        'other_sales','global_sales','game_genre','meta_critic_score','meta_user_score','meta_esrb','meta_multiplayer','game_url_string')]

colnames(full_data)<-c('game','platform','developer','release_date','publisher','na_sales','eu_sales','jp_sales',
                       'other_sales','global_sales','genre','critic_score','user_score','esrb_rating','multiplayer','game_url_string')


#parsing release dates into time format 
full_data$release_date<-mdy(full_data$release_date)


#Deleting rows with no sales data, if we have reviews and no sales data its logical that there must be an error 
full_data<-full_data[which(full_data$global_sales!=0),]
full_data<-full_data[!is.na(full_data$global_sales),]


#Merging games across different platforms 
full_data_merged_plats<-full_data%>%group_by(game)%>%mutate(total_sales=sum(global_sales),avg_user_score=mean(user_score),avg_critic_score=mean(critic_score))
full_data_merged_plats$multiplat<-1
full_data_merged_plats<-full_data_merged_plats%>%group_by(game)%>%mutate(number_platforms=sum(multiplat))
full_data_merged_plats$multiplat<-NULL
full_data_merged_plats$platform<-NULL


#Deleting duplicate rows
full_data_merged_plats<-full_data_merged_plats[!duplicated(full_data_merged_plats$game),]

#Joining series data
full_data_merged_plats<-left_join(full_data_merged_plats,igdb_data,by=c('game_url_string'))
full_data_merged_plats$index<-NULL

#Deleting duplicate rows
full_data_merged_plats<-full_data_merged_plats[!duplicated(full_data_merged_plats$game),]

#Turning into factors 
full_data_merged_plats$developer<-as.factor(full_data_merged_plats$developer)
full_data_merged_plats$publisher<-as.factor(full_data_merged_plats$publisher)
full_data_merged_plats$genre<-as.factor(full_data_merged_plats$genre)
full_data_merged_plats$esrb_rating<-as.factor(full_data_merged_plats$esrb_rating)
full_data_merged_plats$multiplayer<-as.factor(full_data_merged_plats$multiplayer)
full_data_merged_plats$series<-as.factor(full_data_merged_plats$series)

  
#Deleting Number_platform outlier (17 when no other more than 10)
full_data_merged_plats<-full_data_merged_plats[-(which(full_data_merged_plats$number_platforms>10)),]

#Distribution of Data
ggplot(full_data_merged_plats,aes(x=total_sales))+geom_histogram(aes(y=..density..),bins = 20)+scale_x_continuous(labels=comma)


#Taking log of sales 
full_data_merged_plats$log_total_sales<-log(full_data_merged_plats$total_sales)


#Distribution of log(total_sales)
ggplot(full_data_merged_plats,aes(x=log_total_sales))+geom_histogram(aes(y=..density..),bins =20)


# SINGLE LINEAR REGRESSION GRAPH ON LOG DATA
critic_plot<-ggplot(full_data_merged_plats,aes(x=avg_critic_score,y=log_total_sales))+geom_point()+ylab('Total Sales Log(Millions)')+xlab('Average Critic Score')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm',se=FALSE)

user_plot<-ggplot(full_data_merged_plats,aes(x=avg_user_score,y=log_total_sales))+geom_point()+ylab('Total Sales Log(Millions)')+
  xlab('Average User Score')+
  theme(plot.title = element_text(hjust = 0.5))+  geom_smooth(method='lm',se=FALSE)

grid.arrange(critic_plot,user_plot,nrow=1,top='Relationship Between Review Scores and Videogame Sales')


#SINGLE LINEAR REGRESSION ON NUMBER PLATFORMS
ggplot(full_data_merged_plats,aes(x=number_platforms,y=log_total_sales))+geom_point()+ylab('Total Sales Log(Millions)')+xlab('Number of Platforms Game Available')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm',se=FALSE)


#MULTIPLE LINEAR REGRESSION
sales_model<-lm(log_total_sales ~ avg_critic_score+release_date+number_platforms+factor(developer)+factor(esrb_rating)+factor(publisher)+factor(multiplayer)+factor(genre)+factor(series),full_data_merged_plats)

summary(sales_model)


#TESTING 
sales_model_test<-lm(log_total_sales ~ avg_critic_score+release_date+number_platforms+factor(esrb_rating)+factor(multiplayer)+factor(genre)+factor(series),full_data_merged_plats)

summary(sales_model_test)


#SINGLE LINEAR REGRESSION ON Average critic/user score by genre
ggplot(full_data_merged_plats,aes(x=avg_user_score,y=log_total_sales))+geom_point()+ylab('Total Sales Log(Millions)')+xlab('Average critic score')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm',se=FALSE)+facet_wrap(~genre)

ggplot(full_data_merged_plats,aes(x=avg_critic_score,y=log_total_sales))+geom_point()+ylab('Total Sales Log(Millions)')+xlab('Average user score')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method='lm',se=FALSE)+facet_wrap(~genre)

#Box plot sales by genre 
average_line<-mean(full_data_merged_plats$log_total_sales)

ggplot(full_data_merged_plats,aes(x=as.factor(genre),y=log_total_sales))+geom_boxplot()+ylab('Total Sales Log(Millions)')+xlab('Genre')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank())+
  geom_smooth(method='lm',se=FALSE)+facet_wrap(~genre)+
  geom_hline(yintercept=average_line, linetype="dashed", color = "red")

ggplot(full_data_merged_plats,aes(x=multiplayer,y=log_total_sales))+geom_boxplot()+ylab('Total Sales Log(Millions)')+xlab('Genre')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank())+
  geom_smooth(method='lm',se=FALSE)
