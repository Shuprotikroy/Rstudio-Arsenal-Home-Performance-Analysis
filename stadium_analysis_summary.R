# sorting stadium wise data
emirates_stadium_ds <- filteredpldataset %>% filter(HomeTeam=="Arsenal")
#changed date format and stored it to another variable in order to plot it in main graph
sample <-format(as.Date(emirates_stadium_ds$Date),"%d/%b/%y")
#plotting graph, ggrepel was used to equidistant labels
ggplot(data = emirates_stadium_ds,aes(x=Date,y=FTHG,group=1))+geom_line(color="black", size = 2,linetype = "dotted")+geom_text_repel(aes(label = sample,fontface="bold",family="mono",color=as.factor(FTHG)))+geom_point(size = 4,aes(color=as.factor(FTHG)))+theme_classic()+ theme(panel.background = element_rect(fill="white",colour = "black",size = 0.5,linetype = "solid"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black",size=1.5),text = element_text(family = "mono", face = "bold"))+labs(x="Months",y="Full Total Home Goals Scored",family="mono",title = "Home Goals Scored By Arsenal")

HGS <- ggplot(data = emirates_stadium_ds,aes(x=Date,y=FTHG,group=1))+geom_line(color="black", size = 2,linetype = "dotted")+geom_point(size = 4,aes(color=as.factor(FTHG)))+theme_classic()+ theme(panel.background = element_rect(fill="white",colour = "black",size = 0.5,linetype = "solid"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black",size=1.5),text = element_text(family = "mono", face = "bold"))+labs(x="Months",y="Full Total Home Goals Scored",family="mono",title = "Home Goals Scored By Arsenal",color="Full Time Home Goals")
#Pie Chart
#counting occurences of FTR and storing it in a new df
emirate_FTR_pie <-count(emirates_stadium_ds$FTR)

#calculating percentages
library(formattable)
emirate_FTR_pie <- emirate_FTR_pie %>% mutate(percent_A = n/sum(n))
emirate_FTR_pie <- emirate_FTR_pie %>% mutate(percent = percent(percent_A))

emirate_FTR_pie$FTRsummed[3]="Arsenal Win%"
emirate_FTR_pie$FTRsummed[2]="Draw%"
emirate_FTR_pie$FTRsummed[1]="Away Team win%"
ggplot(emirate_FTR_pie,aes(x="",y = n, fill=FTRsummed))+geom_col(size=2,color="white")+geom_text( color="black",size = 4,aes(label= percent, fontface="bold"),position = position_stack(vjust = 0.5))+scale_color_brewer(palette = "YlOrRd")+scale_fill_brewer(palette = 18,direction = -1)+coord_polar(theta="y")+labs(title = "% Of Games Won By Arsenal Vs Away Teams(Emirates Stadium)", fill="Full TIme Results")+theme_void()+theme(text = element_text(family = "mono",face="bold"))

#calculating distibution of total goals to Goals, missed shots, shots on target
Shots_Taken <- c("Home Shots","Home Target Shots","Goals")
 Shots_Taken <- c("Shots On Target","Missed Shots","Goals")
 emirates_HSHST_summarized <- data.frame(Shots_Taken)
 emirates_HSHST_summarized<- emirates_HSHST_summarized %>% mutate(n=0)
 emirate_sum_home_totalshots <- sum(emirates_stadium_ds$HS)
 emirates_HSHST_summarized$n[1] = sum(emirates_stadium_ds$HST)
 emirates_HSHST_summarized$n[3] = sum(emirates_stadium_ds$FTHG)
 emirates_HSHST_summarized$n[2] = (emirates_HSHST_summarized$n[1]+emirates_HSHST_summarized$n[3])

   emirate_sum_home_totalshots -(emirates_HSHST_summarized$n[1]+emirates_HSHST_summarized$n[3])
   ggplot(emirates_HSHST_summarized,aes(x="",y = n, fill=Shots_Taken))+geom_col(size=2,color="white")+geom_text( color="black",size = 4,aes(label= n, fontface="bold"),position = position_stack(vjust = 0.5))+labs(y="Total Shots Taken(347)")+theme(text = element_text(face = "bold",family = "mono"))+labs(title="Distribution Of Total No Of Shots(Arsenal)",fill="Distribution Out Of Total No Of Shots i.e.347")
   ggsave('emirates_stadium_totalgoalshare.jpg')
   
   
   
   
   #away teams in emirates
   
   #FTHG performance by away teams
   ggplot(data = emirates_stadium_ds, aes(x = FTAG, y = reorder(AwayTeam, +FTAG),fill=as.factor(FTAG))) + geom_bar(stat="identity",width=0.70) + geom_col(size = 1.5)+geom_text(aes(label = FTAG,fontface = "bold", family = "mono",hjust=2))+theme_classic()+theme( text = element_text(family = "mono", face = "bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title  = element_text(size = 10),axis.text = element_text(size=8))+scale_fill_brewer(palette = 10)+labs(x="Away Goals Scored",y="Away Teams",fill="No Of Goals Scored By Teams",title="Full Time Goals Scored By Away Teams")
   
   #Yellow Card Summary
   emirates_stadium_ds <- emirates_stadium_ds %>%  mutate(Total_Yellow=(HY+AY))
    Sum_Yellow_Cards <- c("Home_Team_Sum","Away_Team_Sum")
    emirates_Yellow_Cards_Summarized <- data.frame(Sum_Yellow_Cards)
    emirates_Yellow_Cards_Summarized$n[1] = sum(emirates_stadium_ds$HY)
    emirates_Yellow_Cards_Summarized$n[2] = sum(emirates_stadium_ds$AY)
    emirates_Yellow_Cards_Summarized$Sum_Yellow_Cards[1]="Home Team Yellow Cards i.e."
    emirates_Yellow_Cards_Summarized$Sum_Yellow_Cards[2]="Away Team Yellow Cards i.e."
   parts = c(emirates_Yellow_Cards_Summarized$n)
    names(parts) = paste0(emirates_Yellow_Cards_Summarized$Sum_Yellow_Cards)
     waffle(parts, rows = 2,colors=c('#FDD017','#C68E17'),title="Distribution Of Total Yellow Cards(66)")
   
             
  #Distribution of corners
  emirates_stadium_ds <- emirates_stadium_ds %>%  mutate(Total_Corners=HC+AC)
  
  
   group <-c("Home Corners","Away Corners")
   value<-c(sum(emirates_stadium_ds$HC),sum(emirates_stadium_ds$AC))
  emirates_corners_summarized<-data.frame(group,value)
  treemap(emirates_corners_summarized,index="value",vSize="value",vColor = "group",type="categorical",palette = "RdYlGn",title = "Distribution Of Total Corners Taken(205)",title.legend = "Corners Taken",fontfamily.legend = "sans",fontfamily.title = "sans",fontface.labels = "bold")

  
  
  #Distribution of fouls
   Fouls <-c("Home Fouls","Away Fouls")
   foul_values<-c(sum(emirates_stadium_ds$HF),sum(emirates_stadium_ds$AF))
   emirates_Fouls_Summarized<-data.frame(Fouls,foul_values)
   View(emirates_Fouls_Summarized)
   sum<-emirates_Fouls_Summarized$foul_values[1]+emirates_Fouls_Summarized$foul_values[2]
   emirates_Fouls_Summarized <- emirates_Fouls_Summarized %>% mutate(percent=foul_values/sum)
  ggplot(emirates_Fouls_Summarized,aes(x=hsize,y=foul_values,fill=Fouls))+geom_col(color="black",size=1)+coord_polar(theta="y")+xlim(c(0.1,hsize+0.5))+geom_text( color="black",size = 4,aes(label= percent, fontface="bold"),position = position_stack(vjust = 0.5))+scale_fill_brewer(palette = 14)+theme_void()
  