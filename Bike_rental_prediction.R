# clear all the saved data
rm(list = ls())

# set working directory
setwd("D:/edwiser/Project")
getwd()


#Load Libraries
install.packages(c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees'))
install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","e1071"))
install.packages("GGally")
install.packages("corrplot")
library("plyr")
library("dplyr")
library("ggplot2")
library("data.table")
library("GGally")
library(e1071)
library(corrplot)
# input csv file
df_day = read.csv("day.csv",header = TRUE)
View(df_day)
# In the data set there are 16 variables 1 is the target variable and rest are preictors
summary(df_day)
# structure of the dataset
str(df_day)
                   ########Exploratory Data Analysis##################
# in the dataset the catagorical variavles are in integer format so we will convert it into factor numeric
df_day$season=as.factor(df_day$season)
df_day$mnth=as.factor(df_day$mnth)
df_day$yr=as.factor(df_day$yr)
df_day$holiday=as.factor(df_day$holiday)
df_day$weekday=as.factor(df_day$weekday)
df_day$workingday=as.factor(df_day$workingday)
df_day$weathersit=as.factor(df_day$weathersit)

# exreacting date from dteday
dte=unique(df_day$dteday)
df=data.frame(dte)
df_day$dteday=as.Date(df$dte,format="%Y-%m-%d")
df$dte=as.Date(df$dte,format="%Y-%m-%d")
df_day$dteday=format(as.Date(df$dte,format="%Y-%m-%d"), "%d")
df_day$dteday=as.factor(df_day$dteday)



# In the data set we can see that the target variable 'cnt' is the sum of the variables 'casual' and 'registered'
# since we have to predict the total count which is 'cnt', we can remove the 'casual' and 'registered' from the data set 'day'
# we can remove 'instant' which is nothing but a index/serial number
# In the attributes 'holiday' and 'workingday', we can say that holiday is a subset of workingday because if it is  a holiday,
# then it is not workingday and vice-versa. so we can remove holiday from the dataset.

# Removing variables ['casual','registered','instant','holiday'] from the data set day.
df_day[,c("instant","holiday","registered","casual")]<- list(NULL)
colnames(df_day)
#now we have 11 dependent variable and one target variable(cnt)

############## Distribution of numerical variables ############
# distribution of the variables (temp,atemp,hum,windspeed,cnt)
# creating a custom function univ_plots
univ_plots = function( x ) {
  
   ggplot(df_day)+geom_histogram(aes(x= x, y = ..density..),fill= "lightblue")+
   geom_density(aes(x= x, y = ..density..))
  
}

# distribution of the target variable cnt
univ_plots(df_day$cnt)
skewness(df_day$cnt)
# the variabe cnt is left skewed

# distribution of the numetical variable temp
univ_plots(df_day$temp)
skewness(df_day$temp)
#  the variabe temp is left skewed

# distribution of the numetical variable atemp
univ_plots(df_day$atemp)
skewness(df_day$atemp)

# distribution of the numetical variable hum
univ_plots(df_day$hum)
skewness(df_day$hum)

# distribution of the numetical variable windspeed
univ_plots(df_day$windspeed)
skewness(df_day$windspeed)

#except windspeed all the numerical variables are left skewed and windspeed is right skewed

###################### BIVARIATE ANALYSIS ###################
# Relation between numerical variables and the target variable cnt
#relation between cnt and the numerical variable temp
attach(df_day)
plot(temp,cnt)

#relation between cnt and the numerical variable atemp
plot(atemp,cnt)

#relation between cnt and the numerical variable hum
plot(hum,cnt)

#relation between cnt and the numerical variable windspeed
plot(windspeed,cnt)

############### Relation between catagorical variableS and the target variable cnt#######


# Relation between season and cnt
boxplot(df_day$cnt~df_day$season , xlab= "season", ylab = "cnt")

# Relation between weathersit and cnt
boxplot(df_day$cnt~df_day$weathersit, xlab = "weathersit", ylab = "cnt")

#Relation between workingday and cnt
boxplot(df_day$cnt~df_day$workingday, xlab= "workingday", ylab = "cnt")

# count based on weekdays
boxplot(df_day$cnt~df_day$weekday, xlab = "weekday", ylab = "cnt")

#monthly counts of bike rental
boxplot(df_day$cnt~df_day$mnth, xlab = "months", ylab = "counts")

# counts of rents based on year
boxplot(df_day$cnt~df_day$yr, xlab = "yr", ylab = "cnt")

                ##### Missing value analysis ####
missing_val = data.frame(apply(df_day,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df_day)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

# there is no missing values in the data set

#### Outlier Analysis ####
# outlier checking for the numerical features using boxplot

# boxplot for the numerical  feature cnt
boxplot(df_day$cnt, xlab = "counts")

# boxplot for the numerical  feature temp
boxplot(df_day$temp, xlab = "temp")

# boxplot for the numerical  feature atemp
boxplot(df_day$atemp, xlab = "atemp")

# boxplot for the numerical  feature hum
boxplot(df_day$hum, xlab = "humidity")

# boxplot for the numerical  feature windspeed
boxplot(df_day$windspeed, xlab = "windspeed")

#There are Some negative outliers presents in hum and some positive outliers in windspeed
#deection and removal of outliers
df = df_day
#selecting numerical variables
numeric_index = sapply(df_day,is.numeric) #selecting only numeric

numeric_data = df_day[,numeric_index]

cnames = colnames(numeric_data)

# loop for removing outliers from all the numerical variables
for (i in cnames){
  print(i)
  val = df_day[,i][df_day[,i] %in% boxplot.stats(df_day[,i])$out]
  print(length(val))
  df_day = df_day[which(!df_day[,i] %in% val),]
}
nrow(df_day)
# after removing outliers we have 717 observations

#boxplots after removing the outliers
boxplot(df_day$windspeed, xlab = "windspeed")
boxplot(df_day$hum, xlab = "humidity")

         ############################# Feature Selection ###########################

# Already we hahve removed some features which are not providing much information.
# Now we will select/remove the rest feature based on their importance.
# selection of numerical features based on correlation plot

# heatmap or correlation plot
library(corrgram)
corrgram(df_day[,numeric_index], order = F,upper.panel=panel.cor, text.panel=panel.txt, main = "Correlation Plot")
# there is high correlation between temp and atemp , so we can remove one of them 
# removing the numerical feature atemp
df_day = subset(df_day, select = -(atemp))
colnames(df_day)

# now we have 11 selected features one of them is the target feature and the rest are the independent features
# all the numerical independent variables are already in normalised form so we need to scale the features

############################## Sampling ########################
# Dividing the data into train and test or selection of train data using simple random sampling
train_index = sample(1:nrow(df_day), 0.75 * nrow(df_day))
train = df_day[train_index,]
test = df_day[-train_index,]
nrow(train)
nrow(test)
################### Model Developement ################

#Decision tree regression
library(rpart)
fit_DT = rpart(cnt ~ .,data = df_day, method = "anova")
#Ploting decision tree
par(cex = 0.8)
plot(fit_DT)
text(fit_DT)

#apply model on the test data 
predictions_DT = predict(fit_DT, test[,-11])
predictions_DT

#calculation of Errom matrics(mae,mape,RMSE) for Decision Tree
library(DMwR) # this library helps to calculate all the regression error matrics
regr.eval(test[,11], predictions_DT, stats = c("mae","rmse","mape"))

#mae = 597.8765570       
#rmse = 777.7050753      
#mape = 0.1815519 
    
  ######################### Random forrest ##########################
library(randomForest)
fit_RF = randomForest(cnt ~ . , data = train)

# apply model on test data
Predictions_RF = predict(fit_RF, test[,-11])
Predictions_RF

#calculation of Errom matrics(mae,mape,RMSE) for Random Forest
regr.eval(test[,11], Predictions_RF, stats = c("mae","rmse","mape"))

#mae = 588.6605235       
#rmse= 764.6186091     
#mape= 0.1877632

#################### Linear Regression ###########################

fit_LR = lm(cnt ~ . , data = train)
# summary of the model
#summary(fit_LR)

#apply model on the test data
predictions_LR = predict(fit_LR, test[,-11])
predictions_LR

#calculation of Errom matrics(mae,mape,RMSE) for Linear Regression
regr.eval(test[,11], predictions_LR, stats = c("mae","rmse","mape"))

#mae = 630.3705014       
#rmse = 845.8534901       
#mape = 0.1930862 

####### Conclusion ########

#Random Forest is the best model for this dataset

