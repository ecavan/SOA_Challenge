#install.packages("stringr")         

#Libraries
library(readxl)
library("stringr")   
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(naniar)


############################Reading Data##############################


#League Data
League_Shooting<-as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Shooting"))
League_Passing <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Passing"))
League_Defense <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Defense"))
League_Goalkeeping <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Goalkeeping"))

#Tournament Data 
Tournament_Results<- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "Tournament Results"))
Tournament_Shooting<- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", 
                                               sheet = "Tournament Shooting", col_types = c("text", 
                                                                                            "text", "text", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "text", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric")))
Tournament_Passing<- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "Tournament Passing"))
Tournament_Defense<- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "Tournament Defense"))
Tournament_Goalkeeping<- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "Tournament Goalkeeping"))


#Salary Data
Salaries_2020 <- read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "2020 Salaries")
Salaries_2021 <- read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "2021 Salaries")


##########################Quick Summary of the Data######################
summary(League_Shooting)
summary(League_Passing)
summary(League_Defense)
summary(League_Goalkeeping)

summary(Tournament_Shooting)
summary(Tournament_Passing)
summary(Tournament_Defense)
summary(Tournament_Goalkeeping)

summary(Salaries_2020)
summary(Salaries_2021)



str(League_Shooting)
str(League_Passing)
str(League_Defense)
str(League_Goalkeeping)

str(Tournament_Shooting)
str(Tournament_Passing)
str(Tournament_Defense)
str(Tournament_Goalkeeping)

str(Salaries_2020)
str(Salaries_2021)


sapply(League_Shooting, function(x) sum(is.na(x)))
sapply(League_Passing, function(x) sum(is.na(x)))
sapply(League_Defense, function(x) sum(is.na(x)))
sapply(League_Goalkeeping, function(x) sum(is.na(x)))

sapply(Tournament_Shooting, function(x) sum(is.na(x)))
sapply(Tournament_Passing, function(x) sum(is.na(x)))
sapply(Tournament_Defense, function(x) sum(is.na(x)))
sapply(Tournament_Goalkeeping, function(x) sum(is.na(x)))

sapply(Salaries_2020, function(x) sum(is.na(x)))
sapply(Salaries_2021, function(x) sum(is.na(x)))


n_var_miss(League_Shooting)
n_var_miss(League_Passing)
n_var_miss(League_Defense)
n_var_miss(League_Goalkeeping)

n_var_miss(Tournament_Shooting)
n_var_miss(Tournament_Passing)
n_var_miss(Tournament_Defense)
n_var_miss(Tournament_Goalkeeping)

n_var_miss(Salaries_2020)
n_var_miss(Salaries_2021)


vis_miss(League_Shooting)
vis_miss(League_Passing)
vis_miss(League_Defense)
vis_miss(League_Goalkeeping)

vis_miss(Tournament_Shooting)
vis_miss(Tournament_Passing)
vis_miss(Tournament_Defense)
vis_miss(Tournament_Goalkeeping)

vis_miss(Salaries_2020)
vis_miss(Salaries_2021)

gg_miss_upset(League_Shooting,order.by = "freq", nsets = 10)
gg_miss_upset(League_Passing,order.by = "freq", nsets = 10)
gg_miss_upset(League_Defense,order.by = "freq", nsets = 10)
gg_miss_upset(League_Goalkeeping,order.by = "freq", nsets = 10)

gg_miss_upset(Tournament_Shooting,order.by = "freq", nsets = 10)
gg_miss_upset(Tournament_Passing,order.by = "freq", nsets = 10)
gg_miss_upset(Tournament_Defense,order.by = "freq", nsets = 10)
gg_miss_upset(Tournament_Goalkeeping,order.by = "freq", nsets = 10)

gg_miss_upset(Salaries_2020,order.by = "freq", nsets = 10)
gg_miss_upset(Salaries_2021,order.by = "freq", nsets = 10)


League_Shooting$Year <- as.factor(League_Shooting$Year )
Tournament_Shooting$Year <- as.factor(Tournament_Shooting$Year)
gg_miss_fct(x = League_Shooting, fct = Year)
gg_miss_fct(x = Tournament_Shooting, fct = Year)


League_Passing$Year <- as.factor(League_Passing$Year )
Tournament_Passing$Year <- as.factor(Tournament_Passing$Year)
gg_miss_fct(x = League_Passing, fct = Year)
gg_miss_fct(x = Tournament_Passing, fct = Year)


League_Goalkeeping$Year <-as.factor(League_Goalkeeping$Year)
Tournament_Goalkeeping$Year <-as.factor(Tournament_Goalkeeping$Year)

gg_miss_fct(x = League_Goalkeeping, fct = Year)
gg_miss_fct(x = Tournament_Goalkeeping, fct = Year)


###########################Combining Data ##############################

#merging dataframes for league 
League_df = merge(x=League_Shooting,y=League_Passing,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == '90s.x'] <- '90s Shooting'
names(League_df)[names(League_df) == '90s.y'] <- '90s Passing'
League_df = merge(x=League_df,y=League_Defense,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == '90s'] <- '90s Defense'
League_df = merge(x=League_df,y=League_Goalkeeping,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == 'Playing Time 90s'] <- '90s Goalkeeping'


#merging dataframes for Tournament 
Tournament_df = merge(x=Tournament_Shooting,y=Tournament_Passing,by=c("Player","Nation","Pos","Age","Born","League","Year"),all=TRUE)
names(Tournament_df)[names(Tournament_df) == '90s.x'] <- '90s Shooting'
names(Tournament_df)[names(Tournament_df) == '90s.y'] <- '90s Passing'
Tournament_df = merge(x=Tournament_df,y=Tournament_Defense,by=c("Player","Nation","Pos","Age","Born","League","Year"),all=TRUE)
names(Tournament_df)[names(Tournament_df) == '90s'] <- '90s Defense'
Tournament_df = merge(x=Tournament_df,y=Tournament_Goalkeeping,by=c("Player","Nation","Pos","Age","Born","League","Year"),all=TRUE)
names(Tournament_df)[names(Tournament_df) == 'Playing Time 90s'] <- '90s Goalkeeping'




########################Data Cleaning ##################################

#Remove All Non-Alphanumeric Characters
League_df$Player<-str_replace_all(League_df$Player, "[^[:alnum:]]", "")   
Tournament_df$Player<-str_replace_all(Tournament_df$Player, "[^[:alnum:]]", "")   

#Remove All Punctuation Characters
League_df$Player<-str_replace_all(League_df$Player, "[[:punct:]]", "")    
Tournament_df$Player<-str_replace_all(Tournament_df$Player, "[[:punct:]]", "")    


#Changing variable types
League_df$Pos <-as.factor(League_df$Pos)
League_df$Nation <-as.factor(League_df$Nation)
League_df$Squad <-as.factor(League_df$Squad)
League_df$League <-as.factor(League_df$League)

Tournament_df$Nation <-as.factor(Tournament_df$Nation)
Tournament_df$Pos <-as.factor(Tournament_df$Pos)
Tournament_df$League <-as.factor(Tournament_df$League)





#######################Summary Visualizations ########################

#Getting rid of squad in league data so we can compare visualizations due to number of column constraint
Tournament_df$type <- 'Tournament'
League_new_df<-League_df[ , !(names(League_df) %in% drops)]
League_new_df$type <- 'League'
all_df<- rbind(Tournament_df, League_new_df)

#Age Distribution of Players in Tournament and League 
ggplot(all_df, aes(x=Age, color= type,fill = type)) + 
  geom_histogram(position = 'identity')+
  scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  scale_fill_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  labs(title="Player Age histogram plot",x="Age of Players", y = "Count")+
  theme_classic()



#Position Count in Tournament and League 
ggplot(all_df, aes(x=reorder(Pos, Pos, function(x)-length(x)),color= type,fill = type)) +
  geom_bar() + scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  scale_fill_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+ labs(x='Position')+ ggtitle("Count by Position")


#Plotting histogram distributions for variable "W", "D" and "L" from goalkeeping data
ggplot(all_df, aes(x=W,color=type,fill=type)) + 
  geom_histogram(binwidth=0.05,position='identity') +scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  scale_fill_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+labs(title="Plotting Distribution of W",x="W", y = "Count")+
  theme_classic()

ggplot(all_df, aes(x=D,color=type,fill=type)) + 
  geom_histogram(binwidth=0.05,position='identity') +scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  scale_fill_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+labs(title="Plotting Distribution of D",x="D", y = "Count")+
  theme_classic()

ggplot(all_df, aes(x=L,color=type,fill=type)) + 
  geom_histogram(binwidth=0.05,position='identity') +scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+
  scale_fill_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))+labs(title="Plotting Distribution of L",x="L", y = "Count")+
  theme_classic()



#Boxplots

ggplot(all_df, aes(x=Pos, y=Age, color=type)) +
  geom_boxplot()+scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

ggplot(all_df, aes(x=Nation, y=Age, color=type)) +
  geom_boxplot()+scale_color_manual(values=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))




#Correlation plot of numerical values 
numeric_var <- sapply(League_Shooting, is.numeric)
corr_matrix <- cor(League_Shooting[, numeric_var])
corrplot(corr_matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "shade")
ggcorrplot(corr_matrix, type = "lower",
           outline.color = "white",
           ggtheme = ggplot2::theme_gray, colors = c("red", "white", "green"))



numeric_var <- sapply(Tournament_Shooting, is.numeric)
corr_matrix <- cor(Tournament_Shooting[, numeric_var])
corrplot(corr_matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "shade")
ggcorrplot(corr_matrix, type = "lower",
           outline.color = "white",
           ggtheme = ggplot2::theme_gray, colors = c("red", "white", "green"))





#Finding distinct count of players 
League_Shooting %>%
  group_by(Player) %>%
  summarise(n_distinct(Player))




