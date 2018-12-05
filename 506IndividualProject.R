library(data.table)
library(tidyr)
library(stringr)
library(ggmap)
library(mapdata)
library(tidyverse)
library(ggplot2)
library(rgeos)
library(maptools)
library(dplyr)
library(tmap)
library(sp)
library(gridExtra)
library(grid)

##load data
crime=read.csv("D:/STATS506/Project/Individual/Crime_Data_from_2010_to_Present.csv")
census=read.csv("D:/STATS506/Project/Individual/Census_Data_by_Council_District.csv")
arrest=read.csv("D:/STATS506/Project/Individual/Arrest_Data_from_2010_to_Present.csv")
district_map=rgdal::readOGR(dsn="C:/Users/zubad/Desktop/council_district.shp",layer="council_district")
##separate some columns
newcrime=crime%>%separate(Date.Reported,into=c("month","date","year"),sep="/")%>%extract(Location,into=c("long","lat"),"([0-9]*\\.*[0-9]*),(.*[0-9]*\\.*[0-9]*).")
newarrest=arrest%>%separate(Arrest.Date,into=c("month","date","year"),sep="/")%>%extract(Location,into=c("long","lat"),"([0-9]*\\.*[0-9]*),(.*[0-9]*\\.*[0-9]*).")
newcensus=census%>%separate(Council.District,into=c("District.ID","NAME"),sep=" - ")
newcensus=as.data.table(newcensus)[order(as.numeric(District.ID))]

#1
##find out the top ten crimes
crime_freq_topten=as.data.table(newcrime)[year==2010,.SD,.SDcols=c(2,10,11)][,.(freq=.N),by=.(Crime.Code,Crime.Code.Description)][order(-freq)][c(1:10)]
table=tableGrob(crime_freq_topten)
grid.arrange(table,top=textGrob("Top Ten Crimes in LA in 2010",vjust=9,hjust=1.1,gp=gpar(fontsize=18)),nrow=1)
##find out the month with the most frequencies
crime_freq_624=as.data.table(newcrime)[year==2010&Crime.Code==624,.SD,.SDcols=c(2,10,11)][,.(freq=.N),by=.(month)][order(-freq)][c(1:10)]
ggplot(data=crime_freq_624,aes(x=month,y=freq,group="")) +
  geom_line()+
  labs(title='Simple Assault Crime in LA in 2010',y="frequency")+
  theme(plot.title = element_text(size=22))
##draw map
`2010crime`=as.data.table(newcrime)[year==2010,.SD,.SDcols=c(2,10,11,28,29)]
May_624crime=`2010crime`[Crime.Code==624&month=='05',]
numberr=nrow(district_map@data)
district_map@data$District.ID=1:numberr
total_map=merge(district_map,newcensus)
fort_map=fortify(district_map,region="District.ID")
new=merge(fort_map,newcensus,by.x='id', by.y='District.ID', all.x=TRUE)
pop_map=ggplot(new,aes(long,lat,group=group))+
  geom_polygon(color='black',fill='white')+
  geom_polygon(aes(x=long,y=lat, group=group, fill=Pop2010), data=new, color='black')+
  scale_fill_gradient(low='lightblue',high='darkblue',name="population in 2010")
pop_map+
  geom_point(aes(x=as.numeric(lat),y=as.numeric(long),color='Crime.Code'),data=May_624crime,inherit.aes=F)+
  scale_color_manual(values =c('Crime.Code'='#90FFFF'),name='simple assault crime')+
  labs(title="The Distribution of Crime")+
  theme(plot.title = element_text(size=22))


#2
##compute average times
avergt.crime=newcrime%>%select(Crime.Code.Description,Time.Occurred)%>%group_by(Crime.Code.Description)%>%summarise(averg=mean(Time.Occurred))
avergt.arrest=newarrest%>%select(Charge.Group.Code,Charge.Group.Description,Time)%>%group_by(Charge.Group.Description)%>%summarise(averg=mean(Time))%>%filter(averg!='NA')
##calculate the distance and do the visualization 
as.matrix(avergt.crime)
dist_crime=dist(avergt.crime,diag=T,upper=T)
cmd_crime=cmdscale(dist_crime)
total_crime=cbind(as.tibble(cmd_crime),avergt.crime$Crime.Code.Description)
ggplot(total_crime,aes(x=V1,y=V2,color=avergt.crime$Crime.Code.Description))+
  geom_point()+
  theme(legend.text=element_text(size=5,),legend.title=element_text(size=1))+
  labs(title='The Distance Between Average Time of Each Crime Type')+
  theme(plot.title = element_text(size=19.5))
as.matrix(avergt.arrest)
dist_arrest=dist(avergt.arrest,diag=T,upper=T)
cmd_arrest=cmdscale(dist_arrest)
total_arrest=cbind(as.tibble(cmd_arrest),avergt.arrest$Charge.Group.Description)
ggplot(total_arrest,aes(x=V1,y=V2,color=avergt.arrest$Charge.Group.Description))+
  geom_point()+
  labs(title='The Distance Between Average Time of Each Arrest Type')+
  guides(fill=guide_legend(title="crime type"))+
  theme(plot.title = element_text(size=19),legend.position="none")
  

