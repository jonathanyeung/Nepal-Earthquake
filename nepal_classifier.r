library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('tidyr')
library('plyr')
library('dplyr') # data manipulation
library('caret')

#Notes:
#https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/ 
#https://www.r-bloggers.com/predicting-titanic-deaths-on-kaggle-ii-gbm/

train <- read.csv('./Data/train.csv', stringsAsFactors = T)
train_lab <- read.csv('./Data/train_labels.csv', stringsAsFactors = T)
test  <- read.csv('./Data/test.csv', stringsAsFactors = T)

train <- bind_cols(train, train_lab[c("damage_grade")])

full <- bind_rows(train, test)

# Looks like there is no missing data:
sapply(full, function(x) sum(is.na(x)) > 0)

#### Exploratory Data Anlaysis ####

# Understanding Geography:

# 31 values for geo_level_1
length(unique(full$geo_level_1_id))

# 1240 values for geo_level_2
length(unique(full$geo_level_2_id))

# 7084 values for geo_level_3
length(unique(full$geo_level_3_id))

ggplot(full[1:10000,], aes(x = geo_level_1_id, fill = factor(damage_grade))) +
  geom_bar(stat='count', position='dodge')

# Analyze how bad each geo_level_1 region is.  3 & 19 are particularly bad; 2 & 6 are relatively good.
ggplot(full[1:10000,], aes(x = geo_level_1_id, fill = factor(damage_grade))) +
  geom_bar(stat='count', position='fill') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 0:30)

# Further breakdown into geo region 1:
geo_1 <- full[1:10000,] %>% filter(geo_level_1_id == 1)
ggplot(geo_1, aes(x = geo_level_2_id, fill = factor(damage_grade))) +
  geom_bar(stat='count', position='fill') +
  scale_y_continuous(labels = percent_format())


# Manipulation of Data
full[,c('geo_level_1_id','geo_level_2_id','geo_level_3_id')] <- lapply(full[,c('geo_level_1_id','geo_level_2_id','geo_level_3_id')], factor)

full <- full[,c('geo_level_1_id', 'count_floors_pre_eq', 'age', 'area', 'height', 'land_surface_condition', 'foundation_type', 'roof_type', "ground_floor_type", "other_floor_type", "position", "plan_configuration", "damage_grade")]

train <- full[1:10000,]

## 75% of the sample size
index <- createDataPartition(train$damage_grade, p=0.75, list=FALSE)
train <- train[ index,]
test <- train[-index,]

control <- full[10001:20000,]

predictorNames = c('geo_level_1_id', 'count_floors_pre_eq', 'age', 'area', 'height', 'land_surface_condition', 'foundation_type', 'roof_type', "ground_floor_type", "other_floor_type", "position", "plan_configuration")


objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
grid <- expand.grid(n.trees = seq(500,10000,500), interaction.depth = 3, shrinkage = .001, n.minobsinnode = 20)
objModel <- train(train[,predictorNames],
                  train$damage_grade, 
                  method='rf', 
                  importance=T)
                  #trControl=objControl,
                  #tuneGrid = grid,
                  #metric = "Accuracy",
                  #preProc = c("center", "scale"))

predictions<-predict.train(object=objModel,test,type="raw")

density(predictions) %>% plot

predictions_final<-predict.train(object=objModel,control,type="raw")

confusionMatrix(predictions,test[,"damage_grade"])

prediction_dac <- predict(objModel, control[,predictorNames])

