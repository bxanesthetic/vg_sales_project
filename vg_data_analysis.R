library(lubridate)
library(tidyverse)
library(scales)
library(gridExtra)
library(knitr)

meta_data<-read_csv("/Users/jorge/DATA_FILES/meta_data.csv")
vg_data<-read_csv("/Users/jorge/DATA_FILES/vgsales_data.csv")
igdb_data<-read_csv("/Users/jorge/DATA_FILES/igdb_data.csv")
colnames(igdb_data)<-c('index','game_url_string','series')

#rewording meta_platform to use in later join
meta_data$platform<-gsub('^https://www.metacritic.com/game/','',meta_data$meta_full_url)
meta_data$platform<-gsub('/.*$','',meta_data$platform)


#extracting game url string to use in later join
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


#Removing '/r' & '/n' from user score strings 
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


#joining both data sets
full_data<-left_join(meta_data,vg_data,by=c('game_url_string','platform'))


full_data<-full_data[,c('name','platform','developer','meta_release_date','publisher','na_sales','eu_sales','jp_sales',
                        'other_sales','global_sales','game_genre','meta_critic_score','meta_user_score','meta_esrb','meta_multiplayer','game_url_string')]

colnames(full_data)<-c('game','platform','developer','release_date','publisher','na_sales','eu_sales','jp_sales',
                       'other_sales','global_sales','genre','critic_score','user_score','esrb_rating','multiplayer','game_url_string')


#parsing release dates into proper format 
full_data$release_date<-mdy(full_data$release_date)


#Deleting rows with no sales data, if we have reviews and no sales data it must be an error
full_data<-full_data[which(full_data$global_sales!=0),]
full_data<-full_data[!is.na(full_data$global_sales),]


#Merging game data from different platforms 
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

#Turning categorical variables into factors 
full_data_merged_plats$developer<-as.factor(full_data_merged_plats$developer)
full_data_merged_plats$publisher<-as.factor(full_data_merged_plats$publisher)
full_data_merged_plats$genre<-as.factor(full_data_merged_plats$genre)
full_data_merged_plats$esrb_rating<-as.factor(full_data_merged_plats$esrb_rating)
full_data_merged_plats$multiplayer<-as.factor(full_data_merged_plats$multiplayer)
full_data_merged_plats$series<-as.factor(full_data_merged_plats$series)

  
#Deleting Number_platform outlier (17 when no other more than 10)
full_data_merged_plats<-full_data_merged_plats[-(which(full_data_merged_plats$number_platforms>10)),]

full_data_clean<-full_data_merged_plats[,c('game','total_sales','release_date','genre','avg_critic_score','avg_user_score','esrb_rating','multiplayer','number_platforms','series')]
