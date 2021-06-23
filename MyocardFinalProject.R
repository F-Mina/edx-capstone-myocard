# Final Project Myocard - Choosen your own


# 3.1 Explore data set 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(caret)
library(broom)
library(RColorBrewer)

read_uci <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv")


heart_disease <- read_uci

# 3.1.2 Analysing the Missing value 

missing_value <- table(is.na(heart_disease))

# 3.1.3 Exploratory Data Analysis (EDA)

class(heart_disease)
str(heart_disease)
dim(heart_disease)
head(heart_disease)

mean(heart_disease$sex == 0)
mean(heart_disease$sex == 1)

# 3.2 EDA and Visualisation 

myocard <- heart_disease %>% mutate(gender = ifelse(sex > 0, "male", "female"))

str(myocard)

dim(myocard)

mean(myocard$gender == "female")

mean(myocard$gender == "male")

# 3.2.1 Distribution of Age 

myocard %>% ggplot(aes(age, fill = gender)) + geom_histogram(bins = 30, binwidth = 5, color = "black") + xlab("Age") + ylab("Frequency of Age") + ggtitle("Distribution of Age by gender")

# 3.2.2 Distribution of Diabetes 

myocard %>% ggplot(aes(diabetes)) + geom_bar(aes(fill = gender)) + scale_fill_brewer(palette="Blues") + 
  xlab("Diabetes") + ylab("Frequency of Diabetes") + ggtitle("Distribution of Diabetes by gender")

myocard %>% group_by(diabetes) %>% summarise(n = n()) %>% head()

# 3.2.3 Distribuition of Diabetes by gender 

myocard %>% group_by(gender) %>% summarise(diabetes = mean(diabetes == 0)) %>% 
  filter(gender == "female") 

myocard %>% group_by(gender) %>% summarise(diabetes = mean(diabetes == 1)) %>% 
  filter(gender == "female") 

myocard %>% group_by(gender) %>% summarise(diabetes = mean(diabetes == 0)) %>% 
  filter(gender == "male") 

myocard %>% group_by(gender) %>% summarise(diabetes = mean(diabetes == 1)) %>% 
  filter(gender == "male") 


# 3.2.4 Levels of Creatinine phosphokinase 
myocard %>% ggplot(aes(gender, creatinine_phosphokinase)) + geom_point(aes(color = gender)) + 
  ylab("Levels of Creatinine phosphokinase mg/kg") + xlab("Gender") + 
  ggtitle("Creatinine phosphokinase")

myocard %>% ggplot(aes(age, creatinine_phosphokinase)) + geom_point(aes(color = gender)) + 
  ylab("Levels of Creatinine phosphokinase mg/kg") + xlab("Age") + 
  ggtitle("Creatinine phosphokinase by Age and Gender")

myocard %>% group_by(gender) %>% summarise(creatinine_phosphokinase = mean(creatinine_phosphokinase))

myocard %>% group_by(gender) %>% summarise(creatinine_phosphokinase = sd(creatinine_phosphokinase))

# 3.2.5 Serum Creatinine 

myocard %>% ggplot(aes(gender, serum_creatinine)) + geom_point(aes(color = gender)) +
  ylab("Levels of Serum Creatinine mg/dL") + xlab("Gender") + 
  ggtitle("Serum Creatinine")

myocard %>% ggplot(aes(age, serum_creatinine)) + geom_point(aes(color = gender, shape = gender)) + 
  ylab("Levels of Serum Creatinine mg/dL") + xlab("Age") + 
  ggtitle("Serum Creatinine by Age and Gender")


myocard %>% group_by(gender) %>% summarise(mean(serum_creatinine), sd(serum_creatinine))

# 3.2.6 Serum Sodium Levels 

myocard %>% ggplot(aes(gender, serum_sodium)) + 
  geom_boxplot(aes(color = gender), outlier.colour = "blue", width = 0.3) + 
  ylab("Levels of Serum Sodium mEq/L") + xlab("Gender") + 
  ggtitle("Serum sodium levels")

myocard %>% group_by(gender) %>% summarise(mean(serum_sodium), sd(serum_sodium))

# 3.2.7 High Blood Pressure 

myocard %>% ggplot(aes(high_blood_pressure)) + geom_bar(aes(fill = gender)) + 
  scale_fill_brewer(palette = "Accent") + ylab("Frequency of High Blood Pressure") + 
  xlab("High Blood Pressure") + ggtitle("Distribution of High Blood Pressure by Gender")

myocard %>% group_by(gender) %>% summarise(high_blood_pressure = mean(high_blood_pressure == 0)) %>% filter(gender == "male") 
myocard %>% group_by(gender) %>% summarise(high_blood_pressure = mean(high_blood_pressure == 1)) %>% filter(gender == "male") 

myocard %>% group_by(gender) %>% summarise(high_blood_pressure = mean(high_blood_pressure == 0)) %>% filter(gender == "female") 
myocard %>% group_by(gender) %>% summarise(high_blood_pressure = mean(high_blood_pressure == 1)) %>% filter(gender == "female") 

# 3.2.8 Distribution of Smoking 

myocard %>% ggplot(aes(smoking)) + geom_bar(aes(fill = gender)) + 
  scale_fill_brewer(palette = "Paired") + ylab("Frequency of Smoking by Gender") + 
  xlab("Smoking") + ggtitle("Distribution of smoking")

myocard %>% group_by(gender) %>% summarise(smoking = mean(smoking == 0)) %>% filter(gender == "male") 
myocard %>% group_by(gender) %>% summarise(smoking = mean(smoking == 1)) %>% filter(gender == "male") 


myocard %>% group_by(gender) %>% summarise(smoking = mean(smoking == 0)) %>% filter(gender == "female") 
myocard %>% group_by(gender) %>% summarise(smoking = mean(smoking == 1)) %>% filter(gender == "female") 

# 3.2.9 Distribution of Time (Hospitalisation)


myocard %>% ggplot(aes(time, fill = gender)) + 
  geom_histogram(binwidth = 10, bins = 5, color = "black", alpha = 0.4) +
  xlim(c(0,300)) +
  xlab("Time") + ggtitle("Distribution of Hospitalisation")

myocard %>% ggplot(aes(time, age)) + geom_point(aes(color = gender)) + ggtitle("Time hospitalisation by Age and gender")

# 4.0 Linear Regression 

fit <- lm(time ~ age + sex + high_blood_pressure + smoking + creatinine_phosphokinase + serum_creatinine + serum_sodium, data = myocard)
fit
summary(fit)
tidy(fit)


# What the intercept of the model? 
fit$coef[1]


# 4.1.2 Predict age by time 

time_vs_age_hbp <- lm(time ~ age + high_blood_pressure, data = myocard)
time_vs_age_hbp
tidy(time_vs_age_hbp)
summary(time_vs_age_hbp)

myocard %>% ggplot(aes(time, age)) + geom_point(aes(color = gender, shape = factor(high_blood_pressure))) + geom_smooth(method = "lm") + xlab("Time") + ylab("Age") + ggtitle("Linear regression")


# 5.0 Machine Learning
# Linear Discriminant Analysis - LDA 

myocard <- subset(myocard, select = -14)
mean(myocard$time)

myocard <- myocard %>% mutate(time = ifelse(time > 130, "high", "low"))

myocard$time <- factor(myocard$time, levels = c("high", "low"))

set.seed(1)
test_index <- createDataPartition(myocard$time, times = 1, p = 0.1, list = FALSE)
train_set <- myocard[-test_index, ]
test_set <- myocard[test_index, ]

lda_fit <- train(time ~., method = "lda", data = train_set)
lda_predict <- predict(lda_fit, test_set)
confusionMatrix(lda_predict, test_set$time)


# 5.2 Quadratic Discriminant Analysis - QDA  

qda_fit <- train(time ~., method = "qda", data = train_set)
qda_predict <- predict(qda_fit, test_set)
confusionMatrix(qda_predict, test_set$time)













