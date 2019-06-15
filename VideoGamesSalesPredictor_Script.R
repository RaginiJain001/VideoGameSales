#Loading Libraries
suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))
suppressMessages(library(readr)) 
suppressMessages(library(dplyr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tidyr))
suppressMessages(library(gridExtra))
suppressMessages(library(corrplot))
suppressMessages(library(scales))
suppressMessages(library(ggrepel))
suppressMessages(library(fmsb))
suppressMessages(library(corrgram))
suppressMessages(library(GGally))
suppressMessages(library(caTools))
suppressMessages(library(psych))
suppressMessages(library(caret))
options(warn =-1)

#Data Sourcing
#https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/downloads/video-game-sales-with-ratings.zip/2
temp <- tempfile()
wd <- tempdir()
download.file("https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/downloads/video-game-sales-with-ratings.zip/2",temp, mode="wb")
wd <- getwd()
unzip(temp, wd)
games <- read.csv(paste(wd,"Video_Games_Sales_as_at_22_Dec_2016.csv", sep="/"),,stringsAsFactors = FALSE)
unlink(c(temp, wd))
str(games)

#Data Preparation

#change factor into numeric
games$User_Count<-as.numeric(as.character(games$User_Count))
games$User_Score<-as.numeric(as.character(games$User_Score))
games$Critic_Count<-as.numeric(as.character(games$Critic_Count))
games$Critic_Score<-as.numeric(as.character(games$Critic_Score))
games<-games%>%filter(Year_of_Release<=2017)
#create a new dataframe cleanup of all NA's ...
games2 <- na.omit(games)
#there are still few rows for which the Rating is an empty string
games2<-filter(games2,Rating!='')


#create new columns to regroup the Platform by manufacturers
sony<-c('PS','PS2','PS3','PS4' ,'PSP','PSV')
microsoft<-c('PC','X360','XB','XOne')
nintendo<-c('3DS','DS','GBA','GC','N64','Wii','WiiU')
sega<-c('DC')
newPlatform<-function(x){
  if (x %in% sony == TRUE) {return('SONY')}
  else if(x %in% microsoft == TRUE) {return('MICROSOFT')}
  else if(x %in% nintendo == TRUE) {return('NINTENDO')}
  else if(x %in% sega == TRUE) {return('SEGA')}
  else{return('OTHER')}
}
games2$newPlatform<-sapply(games2$Platform, newPlatform)

#Data Visualization


#Distribution of Global Sales across the genres
options(repr.plot.width=8, repr.plot.height=4)
theme_g<-theme(axis.text.x = element_text(angle=90),plot.title  =element_text(size=8),panel.background = element_rect(fill="black"),
               panel.grid.major = element_blank(),
               panel.grid.minor=element_blank())
g1<-ggplot(games2,aes(x=log(Global_Sales),fill=Genre))+geom_density(alpha=0.3)+labs(x="Global Sales")+theme_g
g2<-games2 %>% select(Rating)%>%count(Rating)%>%ggplot(aes(x=Rating,y=n,fill=Rating))+geom_bar(stat="identity")+theme_g
grid.arrange(g1,g2,nrow=1,ncol=2)

#Distribution of Global Sales across Genres and Rating
games2 %>% filter(!Rating=='' & !Genre=='')%>%ggplot(aes(x=Genre,y=log(Global_Sales),col=Rating))+geom_boxplot(varwidth=TRUE)+facet_wrap(~Rating)+theme(axis.text.x=element_text(angle=90),panel.background = element_rect(), panel.grid.major = element_blank(), panel.grid.minor=element_blank())


#Relationship between Critic Score, Critic Count and User Score, User Count, since there exists blank space in User count and user score, normalizing it for plotting purposes
options(repr.plot.width=8, repr.plot.height=4)
g1<-ggplot(games2,aes(x=Critic_Count,y=Critic_Score)) + stat_binhex() + scale_fill_gradientn(colours=c("black","red"),name = "Count",na.value=NA)+theme(panel.background = element_rect(fill="black"),
                                                                                                                                                       panel.grid.major = element_blank(),
                                                                                                                                                       panel.grid.minor=element_blank())+labs(x="Critic Count",y="Critic Score")
g2<-ggplot(games2,aes(x=User_Count,y=User_Score)) + stat_binhex() + scale_fill_gradientn(colours=c("black","red"),name = "Count",na.value=NA)+theme(panel.background = element_rect(fill="black"),
                                                                                                                                                   panel.grid.major = element_blank(),
                                                                                                                                                   panel.grid.minor=element_blank())+labs(x="User Count",y="User Score")
grid.arrange(g1,g2,nrow=1,ncol=2)

#Correlation between User Score and critic Score
options(repr.plot.width=5, repr.plot.height=4)
ggplot(games2,aes(x=Critic_Score,y=User_Score)) + stat_binhex() + scale_fill_gradientn(colours=c("black","yellow"),name = "Frequency",na.value=NA)+theme(panel.background = element_rect(fill="black"),
                                                                                                                                                        panel.grid.major = element_blank(),
                                                                                                                                                        panel.grid.minor=element_blank())+geom_smooth(method="lm",col="yellow4")

#Global sales comparison with regional sales
options(repr.plot.width=5, repr.plot.height=4)
games2 %>% select(Global_Sales,NA_Sales,Genre)%>%filter(!Genre=='')%>%ggplot(aes(x=Global_Sales,y=NA_Sales,col=Genre))+geom_point()+geom_smooth(method="lm")+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),                         panel.grid.minor=element_blank())+labs(title="Global Sales Vs NA Sales across Genres")
games2 %>% select(Global_Sales,EU_Sales,Genre)%>%filter(!Genre=='')%>%ggplot(aes(x=Global_Sales,y=EU_Sales,col=Genre))+geom_point()+geom_smooth(method="lm")+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),                         panel.grid.minor=element_blank())+labs(title="Global Sales Vs EU Sales across Genres")

#Data Analysis

#Number of Video games Released in each year
options(repr.plot.width=8, repr.plot.height=4)
games2 %>% select(Name,Genre,Year_of_Release)%>% filter(!Genre=='')%>% group_by(Year_of_Release,Genre)%>% summarise(no_of_games=n())%>%ggplot(aes(x=Year_of_Release,y=no_of_games,group=Genre,col=Genre))+geom_point(size=0.5)+geom_line()+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),                         panel.grid.minor=element_blank())+labs(title="Games Released in a year")

#User Score for each genre games
s<-games2%>%select(Genre,User_Score)%>%filter(! Genre=='')%>%ggplot(aes(x=Genre,y=User_Score,col=Genre))+geom_jitter(size=0.3)+theme(legend.position = "none",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),                         panel.grid.minor=element_blank())+labs(title="User Score for Games/Genre")
s+stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),col="white")

#Top Video Game Publisher 
options(repr.plot.width=10, repr.plot.height=4)
games2 %>% select(Publisher,Global_Sales)%>%group_by(Publisher)%>%
  summarise(Total_sales=sum(Global_Sales))%>%arrange(desc(Total_sales))%>% head(10)%>%
  ggplot(aes(x=factor(Publisher,level=Publisher),y=Total_sales,fill=Publisher))+geom_bar(stat="identity")+
  theme(legend.position="none",axis.text.x=element_text(angle=90),panel.background = element_rect(fill="black"),
        panel.grid.major = element_blank(), panel.grid.minor=element_blank())+labs(x="Publisher",y="Total Sales",title="Top 10 Publishers")+
  scale_fill_brewer(palette="Spectral")

#Video games Sales- Yearly
games2 %>% select(Year_of_Release,Global_Sales,Publisher)%>%group_by(Year_of_Release,Publisher)%>%
  summarise(Total_sales=sum(Global_Sales))%>%ggplot(aes(x=Year_of_Release,y=Total_sales,group=Publisher,fill=Publisher))+
  geom_area()+theme(legend.position="none",axis.text.x=element_text(angle=90),panel.background = element_rect(fill="black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor=element_blank())+labs(title="Year wise Total sales")

#Top Video games and its Sales
options(repr.plot.width=8, repr.plot.height=5)
games2 %>%select(Name,Global_Sales)%>%group_by(Name)%>%summarise(global_sales=sum(Global_Sales))%>%arrange(desc(global_sales))%>%head(40)%>%ggplot(aes(x=Name,y=global_sales,group=1))+geom_line(col="green4",size=2)+geom_point(aes(col=global_sales),size=3)+scale_color_gradientn(colours = heat.colors(20))+theme_g+labs(title="Top Video games by Sales")

#Regionwise Sales
games2 %>% select(Year_of_Release,NA_Sales,EU_Sales,JP_Sales,Other_Sales)%>%group_by(Year_of_Release)%>%
  summarise(North_America=sum(NA_Sales),EU=sum(EU_Sales),JP=sum(JP_Sales),Other=sum(Other_Sales))%>%ggplot(aes(x=Year_of_Release,y=North_America,group=1,colour="NA"))+geom_line()+geom_line(aes(y=EU,colour="EU"))+geom_line(aes(y=JP,colour="JP"))+geom_line(aes(y=Other,colour="Other"))+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"),
                                                                                                                                                                                                                                                                                                  panel.grid.major = element_blank(),
                                                                                                                                                                                                                                                                                                  panel.grid.minor=element_blank())+labs(y="Total Sales",title="Region wise Sales")+scale_colour_manual("", 
                                                                                                                                                                                                                                                                                                                                                                                                        breaks = c("NA", "EU", "JP","Other"),
                                                                                                                                                                                                                                                                                                                                                                                                        values = c("red", "green", "blue","purple"))

#Video games Genres and its top publishers
options(repr.plot.width=10, repr.plot.height=6)
games %>%select(Publisher,Genre)%>%filter(Publisher %in% c("Nintendo","Electronic Arts","Sony","Activision","Ubisoft"))%>%group_by(Publisher,Genre)%>%summarise(count=n())%>%arrange(desc(count))%>%ggplot(aes(x=factor(''),y=count,fill=Publisher))+geom_bar(stat="identity",position="fill",width=1)+scale_fill_brewer(palette = "PRGn")+facet_wrap(~Genre)+coord_polar(theta="y")+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"),
                                                                                                                                                                                                                                                                                                                                                                                           panel.grid.major = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                           panel.grid.minor=element_blank())+labs(x="")
#Popular Game Platform
cons<-games %>% select(Platform,Global_Sales)%>% group_by(Platform)%>%summarise(Sales=sum(Global_Sales))
cons$percent<-round(cons$Sales/sum(cons$Sales)*100)
cons$percent<-sort(cons$percent)
ggplot(cons,aes(x=Platform,y=percent),fill=Platform)+geom_bar(stat="identity",fill="yellow4")+
  coord_polar()+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),,panel.background = element_rect(fill="black"), panel.grid.major = element_blank(), 
                      panel.grid.minor=element_blank())

#Game genre and its Platform
games %>% select(Platform,Genre,Global_Sales)%>%group_by(Platform,Genre)%>%summarise(sales=sum(Global_Sales))%>%ggplot(aes(x=Genre,y=Platform,fill=sales))+geom_tile()+coord_flip()+scale_fill_gradientn(colors=brewer.pal(9,"Oranges"))+theme(legend.position="bottom",axis.text.x = element_text(angle=90),plot.title = element_text(size=5))+geom_label(aes(label=sales),size=1.5,hjust=0.4)+labs(title="Which Genre and Platform got High Sales?")

#Rating and Sales
games %>% select(Rating,Year_of_Release,Global_Sales)%>%filter(!Rating == '')%>% group_by(Rating,Year_of_Release)%>%summarise(rsales =sum(Global_Sales))%>%arrange(desc(rsales))%>%ggplot(aes(x=Year_of_Release,y=rsales,group=Rating,fill=Rating))+geom_bar(stat="identity",position = "fill",alpha=0.7)+scale_y_continuous(labels = percent_format())+theme(legend.position = "bottom",axis.text.x = element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank(),  
                                                                                                                                                                                                                                                                                                                                                              panel.grid.minor=element_blank())


#Modelling Scores / sales

#select years for which Critic was on
games3<-filter(games2,Year_of_Release>=1999)
#rescale the User_Score
games3$User_Score_num = as.numeric(as.character(games3$User_Score)) *10
#quick plot
ggplot(data=games3,aes(x=User_Score_num,y=Critic_Score)) + geom_point(aes(color=factor(newPlatform), shape=factor(Rating)),size=2,alpha=.5) + geom_smooth(method = "lm", size=.5,color="black", formula = y ~ x)+labs(x="User Score",y="Critic Score")
#breakdown per Rating
ggplot(data=games3,aes(x=User_Score_num,y=Critic_Score)) + geom_point(aes(color=factor(newPlatform)),size=2,alpha=.5) + geom_smooth(method = "lm", size=.5,color="black", formula = y ~ x) + facet_wrap(~Rating)+labs(x="User Score",y="Critic Score")

#Predicting sales as a function of Critic score
#Understanding correlation of sales and critic score
portable<-c('3DS','DS','GBA','PSP','PSV')
type<-function(x){
  if (x %in% portable == TRUE) {return('PORTABLE')}
  else{return('HOME')}
}
games3$Type<-sapply(games3$Platform, type)
sonyUs<-ggplot(data=filter(games3,newPlatform=='SONY'),aes(x=Critic_Score,y=NA_Sales)) + geom_point(aes(color=Genre,shape=Type),size=3,alpha=.5) + ylim(0,10) + geom_smooth()+labs(title="Sony")
micUs<-ggplot(data=filter(games3,newPlatform=='MICROSOFT'),aes(x=Critic_Score,y=NA_Sales)) + geom_point(aes(color=Genre,shape=Type),size=3,alpha=.5) + ylim(0,10)+ geom_smooth()+labs(title="Microsoft")
ninUs<-ggplot(data=filter(games3,newPlatform=='NINTENDO'),aes(x=Critic_Score,y=NA_Sales)) + geom_point(aes(color=Genre,shape=Type),size=3,alpha=.5) + ylim(0,10)+ geom_smooth()+labs(title="Nintendo")
grid.arrange(sonyUs,ncol=1)
grid.arrange(micUs,ncol=1)
grid.arrange(ninUs,ncol=1)

# Machine Learning Linear Regression
test_index <- createDataPartition(y = games3$NA_Sales, times = 1,
                                  p = 0.7, list = FALSE)
train_set <- games3[test_index,]
test_set <- games3[-test_index,]

RMSE <- function(true_sales, predicted_sales){
  
  sqrt(mean((true_sales - predicted_sales)^2, na.rm=T))
}
mu <- mean(train_set$NA_Sales) 

#Calculating critic averages
Critic_avgs <- train_set %>% 
  group_by(Critic_Score) %>% 
  summarize(b_i = mean(NA_Sales - mu))

#Calculating user averages
user_avgs <- train_set %>% 
  left_join(Critic_avgs, by='Critic_Score') %>%
  group_by(User_Score) %>%
  summarize(b_u = mean(NA_Sales - mu - b_i))

#Predicting ratings on test set
predicted_sales <- test_set %>% 
  left_join(Critic_avgs, by='Critic_Score') %>%
  left_join(user_avgs, by='User_Score') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_rmse <- RMSE(test_set$NA_Sales, predicted_sales)
rmse_results <- data_frame(method="Critic + User Effects Model on test set",  
                           RMSE = model_rmse )
rmse_results %>% knitr::kable()

#Polynomial regression (1st test)
#define a function to plot the summary of a model(MSE, RMSE, R^2, plots)
plotRes<-function(mod){
  print(mod)
  summary(mod)
  #create DF with prediction and real values
  mod.predictions <- predict(mod,test)
  mod.res<- cbind(mod.predictions,test$NA_Sales)
  colnames(mod.res) <- c('pred','real')
  mod.res <- as.data.frame(mod.res)
  #make plots of residuals,etc...
  g1<-ggplot(data=mod.res,aes(x=pred,y=real)) + geom_point()
  g2<-ggplot(data=mod.res,aes(x=real-pred)) + geom_histogram(bins=50)
  g3<-ggplot(data=mod.res,aes(x=pred,y=real-pred)) + geom_point()
  grid.arrange(g1,g2,g3,nrow=2, ncol=2)
  #calculate metrics
  mse <- mean((mod.res$real-mod.res$pred)^2)
  rmse<-mse^0.5
  SSE = sum((mod.res$pred - mod.res$real)^2)
  SST = sum( (mean(test$NA_Sales) - mod.res$real)^2)
  R2 = 1 - SSE/SST
  sprintf("MSE: %f RMSE : %f R2 :%f", mse,rmse,R2)
}
#Data Partition
set.seed(101)
split<-sample.split(games3$NA_Sales,SplitRatio=.7)
train<-subset(games3,split==T)
test<-subset(games3,split==F)
#Polynomial degree 3 model
model<-lm(NA_Sales ~ Critic_Score + I(Critic_Score^2) + I(Critic_Score^3),train)
plotRes(model)
#Test sample
testFunc <- function(x) {model$coefficients[1] + x*model$coefficients[2] + x*x*model$coefficients[3] + x*x*x*model$coefficients[4]}
ggplot(data=test,aes(x=Critic_Score,y=NA_Sales)) + geom_point() + stat_function(fun=testFunc,color='red',size=1) + xlab("Critic Score") + ylab('NA Sales')
model_test<-lm(NA_Sales ~ Critic_Score + I(Critic_Score^2) + I(Critic_Score^3),test)
plotRes(model_test)



