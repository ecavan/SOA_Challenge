#Libraries 
library(readxl)
library(stringr)  
library(dplyr)
library(ggcorrplot)

library(caret)
library(rpart)



#League Data
League_Shooting<-as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Shooting"))
League_Passing <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Passing"))
League_Defense <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Defense"))
League_Goalkeeping <- as.data.frame(read_excel("Desktop/SOA Competition/2022-student-research-case-study-player-data.xlsx", sheet = "League Goalkeeping"))


#merging dataframes for league 
League_df = merge(x=League_Shooting,y=League_Passing,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == '90s.x'] <- '90s Shooting'
names(League_df)[names(League_df) == '90s.y'] <- '90s Passing'
League_df = merge(x=League_df,y=League_Defense,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == '90s'] <- '90s Defense'
League_df = merge(x=League_df,y=League_Goalkeeping,by=c("Player","Nation","Pos","Squad","Age","Born","League","Year"),all=TRUE)
names(League_df)[names(League_df) == 'Playing Time 90s'] <- '90s Goalkeeping'

#Remove All Non-Alphanumeric Characters
League_df$Player<-str_replace_all(League_df$Player, "[^[:alnum:]]", "")   

#Remove All Punctuation Characters
League_df$Player<-str_replace_all(League_df$Player, "[[:punct:]]", "")    
League_df


#Labeling Churn Data 
Player_YearCount<-League_df %>% select(Player,Year) %>% distinct() %>% group_by(Player) %>% summarize(n=n()) %>% filter(n==1) %>% left_join(League_df) %>% select(Player, Year) %>% distinct() 
Player_YearCount <- as.data.frame(Player_YearCount)
Player_YearCount <- Player_YearCount[Player_YearCount$Year==2020,]
Player_YearCount$churn <- 1



churn_df <- Player_YearCount %>% select(Player,churn) %>% right_join(League_df)

sum(is.na(churn_df$churn))
churn_df$churn[is.na(churn_df$churn) ] = 0
sum(is.na(churn_df$churn))


numeric_var <- sapply(churn_df, is.numeric)
corr_matrix <- cor(churn_df[, numeric_var])
ggcorrplot(corr_matrix)


str(churn_df)

churn_df$churn <- as.factor(churn_df$churn)
churn_df$Nation <- as.factor(churn_df$Nation)
churn_df$Pos <- as.factor(churn_df$Pos)
churn_df$Squad <- as.factor(churn_df$Squad)
churn_df$League <- as.factor(churn_df$League)

ggplot(churn_df, aes(x=League)) + ggtitle("League Percentage of Players") + xlab("League") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) +
  ylab("Percentage") + coord_flip() 

ggplot(churn_df, aes(x=Pos)) + ggtitle("Pos Percentage of Players") + xlab("League") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) +
  ylab("Percentage") + coord_flip() 






################### Filling NA's and Removing unnecessary fields ############

churn_df[is.na(churn_df)] <- 0

new_churn_df<- churn_df[c(2,4,6:8)]

new_churn_df<-new_churn_df %>%
  mutate(Position = case_when(
    Pos=="FW" |Pos=="FWMF" |Pos=="FWDF"  ~ "F",
    Pos=="MF" |Pos=="MFFW" |Pos=="MFGK" | Pos=="MFDF"~ "M",
    Pos== "DF" |Pos== "DFFW"|Pos== "DFMF"~ "D",
    Pos=="GKMF"| Pos=="GK"~ "GK"
  ))

new_churn_df$Position<-as.factor(new_churn_df$Position)
new_churn_df<-new_churn_df[c(1,3:6)]

#################### Splitting Test and Train ############################
intrain <- createDataPartition(churn_df$churn, p = 0.7, list = FALSE)
set.seed(2018)
training <- churn_df[intrain, ]
testing <- churn_df[- intrain, ]

intrain <- createDataPartition(new_churn_df$churn, p = 0.7, list = FALSE)
set.seed(2018)
training <- new_churn_df[intrain, ]
testing <- new_churn_df[- intrain, ]


dim(training); dim(testing)


################# Logistic Regression ######################################



lr_fit <- glm(churn ~., data = training,
              family=binomial(link='logit'))
summary(lr_fit)

lr_prob1 <- predict(lr_fit, testing, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"1","0")
table(Predicted = lr_pred1, Actual = testing$churn)

lr_prob2 <- predict(lr_fit, training, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"1","0")
lr_tab1 <- table(Predicted = lr_pred2, Actual = training$churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = testing$churn)

# Train
confusionMatrix(
  as.factor(lr_pred2),
  as.factor(training$churn)
)

# Test
confusionMatrix(
  as.factor(lr_pred1),
  as.factor(testing$churn)
)

lr_acc <- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc


####################### Random Forest #################################
#Set control parameters for random forest model selection
ctrl <- trainControl(method = "cv", number=5, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)


#Exploratory random forest model selection
 #rf_fit1 <- train(make.names(churn) ~., data = training,
           #       method = "rf",
            #      ntree = 75,
             #     tuneLength = 5,
              #    metric = "ROC",
               #   trControl = ctrl)
 
 
#Run optimal model
rf_fit2 <- randomForest(churn ~., data = testing, 
                        ntree = 75, mtry = 2, 
                        importance = TRUE, proximity = TRUE)


#Display variable importance from random tree
varImpPlot(rf_fit2, sort=T, n.var = 4, 
           main = 'Top 4 important variables')

rf_pred1 <- predict(rf_fit2, testing)
table(Predicted = rf_pred1, Actual = testing$churn)
plot(rf_fit2)

rf_pred2 <- predict(rf_fit2, training)
rf_tab1 <- table(Predicted = rf_pred2, Actual = training$churn)
rf_tab2 <- table(Predicted = rf_pred1, Actual = testing$churn)

# Train
confusionMatrix(
  as.factor(rf_pred2),
  as.factor(training$churn)
)

# Test
confusionMatrix(
  as.factor(rf_pred1),
  as.factor(testing$churn)
)

rf_acc <- sum(diag(rf_tab2))/sum(rf_tab2)
rf_acc
