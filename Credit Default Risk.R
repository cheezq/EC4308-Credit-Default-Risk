install.packages("PreProcess")
library(foreign)
library(ggplot2)
library(ggpubr)
library(GGally)
library(dplyr)
library(corrplot)
library(gridExtra)
library(caret)
library(e1071)
library(gridBase) # Draw Viz
library(gtable) # Viz
library(vcd) # mosaic
library(Hmisc) # rcorr
library(Amelia) # missmap
library(ROCR) 
library(rpart)
library(randomForest)
library(InformationValue)
library(PreProcess)
library(glmnet)
library(caret)

mydata = read.csv("UCI_Credit_Card.csv")

########################################## EDA #######################################
######################################################################################

data <- mydata[,-1]
#View the data
View(data)
head(data)

#Checking for missing data
sum(is.na.data.frame(data))
sum(is.null(data))
#Since the sum is equal to zero, there are no NA values

#Data Overview
dim(data)
sapply(data,class)
str(data)
summary(data)
#Column 24 is the dependent variable. Columns 2,3,4,6,7,8,9,10,11 are categorical variables
#whilst columns 5,12,13,14,15,16,17,18,19,20,21,23 are continuous variables

#Checking for Categorical Data
library(ggplot2)
library(ggpubr)
library(gridExtra)
graph <- list()
cnames <- colnames(data)
for (i in 1:ncol(data)){
  if ((i>=12)&(i<=23)){
    graph[[i]]<-ggplot(data,aes_string(x=colnames(data)[i]))+geom_histogram(fill="black",col="grey",aes(y=..density..),bins = 100)+geom_density(col="red")
  }else{  
    graph[[i]]<-ggplot(data,aes_string(x=colnames(data)[i]))+geom_histogram(fill="black",col="grey",aes(y=..density..))+geom_density(col="red")
  }
}
ggarrange(graph[[1]],graph[[2]],graph[[3]],graph[[4]],graph[[5]],graph[[6]],graph[[7]],graph[[8]],graph[[9]],graph[[10]],graph[[11]],graph[[12]],graph[[13]],graph[[14]],graph[[15]],graph[[16]],graph[[17]],graph[[18]],graph[[19]],graph[[20]],graph[[21]],graph[[22]],graph[[23]],graph[[24]],nrow=6,ncol=4)
ggarrange(graph[[1]],graph[[2]],graph[[3]],graph[[4]],nrow=2,ncol=2)
ggarrange(graph[[5]],graph[[6]],graph[[7]],graph[[8]],nrow=2,ncol=2)
ggarrange(graph[[9]],graph[[10]],graph[[11]],graph[[12]],nrow=2,ncol=2)
ggarrange(graph[[13]],graph[[14]],graph[[15]],graph[[16]],nrow=2,ncol=2)
ggarrange(graph[[17]],graph[[18]],graph[[19]],graph[[20]],nrow=2,ncol=2)
ggarrange(graph[[21]],graph[[22]],graph[[23]],graph[[24]],nrow=2,ncol=2)

# Inconsistent data
data.cat <- data[,c(3,4,6:11)]
for (i in 1:ncol(data.cat)){
  x <- factor(data.cat[,i])
  data.cat[,i] <- x
}
summary(data.cat,maxsum = 200)

categorical <- c(2,3,4,6,7,8,9,10,11)
categ <- list()
for (i in 1:length(categorical)){
  x <- unique(data[,categorical[i]])
  categ[[i]] <- x
}
categ[[1]]
#There are 2 unique values, 1 and 2, for the SEX variable. 1 represents male and 2
#represents female
categ[[2]]
#There are 7 unique values, 0,1,2,3,4,5 and 6 for the EDUCATION variable. 1 represents
#graduate school, 2 represents university, 3 represents high school, 4 represents others
#while 5, 6 and 0 are unaccounted for and represent unknowns
categ[[3]]
#There are 4 unique values, 0,1,2 and 3, for the MARRIAGE variable. 1 represents married
#and 2 represents single while 0 and 3 are unaccounted for and represent unknowns
categ[[4]]
#There are 11 unique values, -2,-1,0,1,2,3,4,5,6,7 and 8, for the repayment status in September variable PAY_0
#A positive number of n indicates payment delay for n months e.g. n=2 represents payment
#delay of 2 months. -1 indicates that payment have been made duly. 0 and -2 are unaccounted
#for and represents unknowns
categ[[5]]
categ[[6]]
categ[[7]]
(categ[[4]][order(categ[[4]])]==categ[[5]][order(categ[[5]])])==(categ[[6]][order(categ[[6]])]==categ[[7]][order(categ[[7]])])
#Columns PAY_2, PAY_3 and PAY_4, repayment status in August, July and June 2005 respectively,
#all have the same unique values as PAY_0.
categ[[8]]
categ[[9]]
(categ[[8]][order(categ[[8]])]==categ[[9]][order(categ[[9]])])
#Columns PAY_5 and PAY_6, representing the repayment status in May and April 2005 respectively,
#have the same unique values. There are 10 unique values, -2,-1,0,2,3,4,5,6,7 and 8. 
#A positive number of n indicates payment delay for n months e.g. n=2 represents payment
#delay of 2 months. -1 indicates that payment have been made duly. 0 and -2 are unaccounted
#for and represents unknowns. Different from the previous 4 columns, these 2 columns
#does not contain the value 1 i.e. there are no individuals who have a payment delay for
#1 month in May and April 2005

#Distribution of Continuous variables
bx1 <- ggplot(data,aes_string(y=colnames(data)[1]))+geom_boxplot()
bx2 <- ggplot(data,aes_string(y=colnames(data)[5]))+geom_boxplot()
bx3 <-ggplot(data,aes_string(y=colnames(data)[12]))+geom_boxplot()
bx4 <-ggplot(data,aes_string(y=colnames(data)[13]))+geom_boxplot()
bx5 <-ggplot(data,aes_string(y=colnames(data)[14]))+geom_boxplot()
bx6 <-ggplot(data,aes_string(y=colnames(data)[15]))+geom_boxplot()
bx7 <-ggplot(data,aes_string(y=colnames(data)[16]))+geom_boxplot()
bx8 <- ggplot(data,aes_string(y=colnames(data)[17]))+geom_boxplot()
bx9 <-ggplot(data,aes_string(y=colnames(data)[18]))+geom_boxplot()
bx10 <-ggplot(data,aes_string(y=colnames(data)[19]))+geom_boxplot()
bx11 <-ggplot(data,aes_string(y=colnames(data)[20]))+geom_boxplot()
bx12 <-ggplot(data,aes_string(y=colnames(data)[21]))+geom_boxplot()
bx13 <-ggplot(data,aes_string(y=colnames(data)[22]))+geom_boxplot()
bx14 <-ggplot(data,aes_string(y=colnames(data)[23]))+geom_boxplot()
grid.arrange(bx1,bx2,bx3,bx4,bx5,bx6,bx7,ncol=7)
grid.arrange(bx8,bx9,bx10,bx11,bx12,bx13,bx14,ncol=7)

#Correlation
library(GGally)
ggpairs(data=data,columns=c(1,5,12:23))
ggpairs(data=data,columns=c(12:17))

#Creating factor variables for plots
Sex.f <- sub(data$SEX, pattern=1,replacement="Male")
Sex.f <- sub(Sex.f, pattern=2,replacement="Female")
Sex.f <- factor(Sex.f)
Education.f <- sub(data$EDUCATION, pattern=1,replacement="Graduate School")
Education.f <- sub(Education.f, pattern=2,replacement="University")
Education.f <- sub(Education.f, pattern=3,replacement="High School")
Education.f <- sub(Education.f, pattern=4,replacement="Unknown")
Education.f <- sub(Education.f, pattern=5,replacement="Unknown")
Education.f <- sub(Education.f, pattern=6,replacement="Unknown")
Education.f <- sub(Education.f, pattern=0,replacement="Unknown")
Education.f <- factor(Education.f)
Marriage.f <- sub(data$MARRIAGE, pattern=1,replacement="Married")
Marriage.f <- sub(Marriage.f, pattern=2,replacement="Single")
Marriage.f <- sub(Marriage.f, pattern=0,replacement="Unknown")
Marriage.f <- sub(Marriage.f, pattern=3,replacement="Unknown")
Marriage.f <- factor(Marriage.f)
Age_Group <- c()
for (i in 1:nrow(data)){
  if (data$AGE[i]<=29){
    age.hold <- "20-29 Years Old"
    Age_Group <- c(Age_Group,age.hold)
  } else if (data$AGE[i]>=30 & data$AGE[i]<=39){
    age.hold <- "30-39 Years Old"
    Age_Group <- c(Age_Group,age.hold)
  } else if (data$AGE[i]>=40 & data$AGE[i]<=49){
    age.hold <- "40-49 Years Old"
    Age_Group <- c(Age_Group,age.hold)  
  } else if (data$AGE[i]>=50 & data$AGE[i]<=59){
    age.hold <- "50-59 Years Old"
    Age_Group <- c(Age_Group,age.hold) 
  } else if (data$AGE[i]>=60 & data$AGE[i]<=69){
    age.hold <- "60-69 Years Old"
    Age_Group <- c(Age_Group,age.hold)
  } else if (data$AGE[i]>=70 & data$AGE[i]<=79){
    age.hold <- "70-79 Years Old"
    Age_Group <- c(Age_Group,age.hold) 
  }
}
Age_Group <- factor(Age_Group)

Default.f <- sub(data$default.payment.next.month, pattern=1,replacement="Default")
Default.f <- sub(Default.f, pattern=0,replacement="No default")
Default.f <- factor(Default.f)

data.factor <- cbind(data,Sex.f,Education.f,Marriage.f,Age_Group,Default.f)
summary(data.factor)

#Limit balance against marriage

lim.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=LIMIT_BAL))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

#Limit balance against education

lim.education <- ggplot(data=data.factor,aes(x=Education.f,y=LIMIT_BAL),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

#Limit balance against sex

lim.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=LIMIT_BAL),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(lim.sex,lim.education,lim.marriage,ncol=3)

# Bill Amount 1 against demographics
b1.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT1))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b1.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT1),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b1.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT1),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b1.sex,b1.education,b1.marriage,ncol=3)

# Bill Amount 2 against demographics
b2.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT2))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b2.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT2),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b2.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT2),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b2.sex,b2.education,b2.marriage,ncol=3)

# Bill Amount 3 against demographics
b3.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT3))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b3.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT3),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b3.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT3),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b3.sex,b3.education,b3.marriage,ncol=3)

# Bill Amount 4 against demographics
b4.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT4))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b4.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT4),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b4.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT4),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b4.sex,b4.education,b4.marriage,ncol=3)

# Bill Amount 5 against demographics
b5.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT5))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b5.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT5),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b5.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT5),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b5.sex,b5.education,b5.marriage,ncol=3)

# Bill Amount 6 against demographics
b6.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=BILL_AMT6))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b6.education <- ggplot(data=data.factor,aes(x=Education.f,y=BILL_AMT6),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
b6.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=BILL_AMT6),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(b6.sex,b6.education,b6.marriage,ncol=3)

# Pay Amount 1 against demographics
p1.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT1))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p1.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT1),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p1.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT1),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p1.sex,p1.education,p1.marriage,ncol=3)

# Pay Amount 2 against demographics
p2.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT2))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p2.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT2),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p2.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT2),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p2.sex,p2.education,p2.marriage,ncol=3)

# Pay Amount 3 against demographics
p3.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT3))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p3.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT3),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p3.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT3),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p3.sex,p3.education,p3.marriage,ncol=3)

# Pay Amount 4 against demographics
p4.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT4))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p4.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT4),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p4.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT4),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p4.sex,p4.education,p4.marriage,ncol=3)

# Pay Amount 5 against demographics
p5.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT5))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p5.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT5),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p5.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT5),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p5.sex,p5.education,p5.marriage,ncol=3)

# Pay Amount 6 against demographics
p6.marriage <- ggplot(data=data.factor,aes(x=Marriage.f,y=PAY_AMT6))+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p6.education <- ggplot(data=data.factor,aes(x=Education.f,y=PAY_AMT6),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")
p6.sex <- ggplot(data=data.factor,aes(x=Sex.f,y=PAY_AMT6),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

grid.arrange(p6.sex,p6.education,p6.marriage,ncol=3)

#Sex against default payment
male.plot <- ggplot(data=data.factor[Sex.f=="Male",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Male")+ylim(0,1)+theme(legend.position = "none")+xlab("Male")
female.plot <- ggplot(data=data.factor[Sex.f=="Female",],aes(x=Default.f,fill=Default.f))+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Female")+ylim(0,1)+theme(legend.position = "none")+xlab("Female")
grid.arrange(male.plot,female.plot,ncol=2)

#Education against default payment
highschool.plot <- ggplot(data=data.factor[Education.f=="High School",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, High School")+ylim(0,1)+theme(legend.position = "none")+xlab("High School")
university.plot <- ggplot(data=data.factor[Education.f=="University",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, University")+ylim(0,1)+theme(legend.position = "none")+xlab("University")
graduateschool.plot <- ggplot(data=data.factor[Education.f=="Graduate School",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Graduate School")+ylim(0,1)+theme(legend.position = "none")+xlab("Graduate School")
eduuk.plot <- ggplot(data=data.factor[Education.f=="Unknown",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Unknown")+ylim(0,1)+theme(legend.position = "none")+xlab("Unknown")
grid.arrange(highschool.plot,university.plot,graduateschool.plot,eduuk.plot,ncol=4)

#Marriage against default payment
married.plot <- ggplot(data=data.factor[Marriage.f=="Married",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Married")+ylim(0,1)+theme(legend.position = "none")+xlab("Married")
single.plot <- ggplot(data=data.factor[Marriage.f=="Single",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Single")+ylim(0,1)+theme(legend.position = "none")+xlab("Single")
marriageuk.plot <- ggplot(data=data.factor[Marriage.f=="Unknown",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, Unknown")+ylim(0,1)+theme(legend.position = "none")+xlab("Unknown")
grid.arrange(married.plot,single.plot,marriageuk.plot,ncol=3)
grid.arrange(male.plot,female.plot,married.plot,single.plot,marriageuk.plot,ncol=5)

#Age Group against default payment
twenty.plot <- ggplot(data=data.factor[Age_Group=="20-29 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 20-29 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("20-29 Years Old")
thirty.plot <- ggplot(data=data.factor[Age_Group=="30-39 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 30-39 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("30-39 Years Old")
fourty.plot <- ggplot(data=data.factor[Age_Group=="40-49 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 40-49 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("40-49 Years Old")
fifty.plot <- ggplot(data=data.factor[Age_Group=="50-59 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 50-59 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("50-59 Years Old")
sixty.plot <- ggplot(data=data.factor[Age_Group=="60-69 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 60-69 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("60-69 Years Old")
seventy.plot <- ggplot(data=data.factor[Age_Group=="70-79 Years Old",],aes(x=Default.f,fill=Default.f),)+
  geom_bar(aes(y=..count../sum(..count..)))+ylab("Proportion, 70-79 Years Old")+ylim(0,1)+theme(legend.position = "none")+xlab("70-79 Years Old")
grid.arrange(twenty.plot,thirty.plot,fourty.plot,fifty.plot,sixty.plot,seventy.plot,ncol=6)

#Limit balance against default
lim.default <- ggplot(data=data.factor,aes(x=Default.f,y=LIMIT_BAL,fill=Default.f),)+
  geom_boxplot()+xlab("")+theme(legend.position = "none")

#Imbalance
table(data[[24]])
ggplot(data=data.factor,aes(x=Default.f,fill=Default.f))+geom_histogram(stat="count")+theme(legend.position = "none")
#Dependent variable is imbalanced

################################# Data Preprocessing #################################
######################################################################################

## Converting PAY_0 to PAY_1
colnames(mydata)[colnames(mydata) == "PAY_0"] = "PAY_1"

## Removing ID 
mydata = mydata[,-1]


## scaling(data normalisation)
scale_center_cc <- mydata
preProcess_cc_model <- preProcess(x = scale_center_cc[,c(-24)], method = c("center", "scale"))
scale_center_cc[,c(-24)] <-
  predict(object = preProcess_cc_model, newdata = scale_center_cc[,c(-24)])

## Splitting dataset into train and test data
set.seed(123)
smp_size <- floor(0.8 * nrow(mydata))
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
traindata <- scale_center_cc[train_ind,]
nrow(traindata)

xTrain <-traindata[,c(1:23)]
yTrain <-traindata[,c(24)]

## Test set
valid<- scale_center_cc[-train_ind,]
nrow(valid)

xTest <-valid[,c(1:23)]
yTest <-valid[,c(24)]

traindata$default.payment.next.month = as.numeric(traindata$default.payment.next.month )
yTest = as.numeric(yTest)
yTrain = as.numeric(yTrain)

MSE <- function(pred, truth){
  return(mean((truth - pred)^2))
}

################################# Feature Selection and Models #################################
################################################################################################

##### Logistic Regression #####
###############################
set.seed(123)

glm.m <- glm(default.payment.next.month ~ ., data=traindata ,family=binomial)
summary(glm.m)

pred =predict(glm.m,xTest ,type="response")

# Test MSE 
MSE(pred,yTest) #0.143781

#Accuracy 
optcut <- optimalCutoff(yTest, pred,
                        optimiseFor="misclasserror")
pred_1 <-ifelse(pred < optcut,0,1)
table(yTest, pred_1)

mean(yTest == pred_1) #0.8231667

#ROC
plotROC(actuals=yTest, predictedScores=pred) #0.7127


##### LASSO ######
##################

set.seed(123)

# Building the LASSO model

feat_mod_select <- cv.glmnet(as.matrix(xTrain) ,yTrain, standardize = TRUE, alpha =1)

# Checking coefficients with the minimum cross-validation error
as.matrix(round(coef(feat_mod_select,feat_mod_select$lambda.min),5))
# Results
plot(feat_mod_select) # LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_1,PAY_2,PAY_3,
#PAY_4,PAY_5,PAY_6,BILL_AMT1,PAY_AMT1,PAY_AMT2,PAY_AMT4,PAY_AMT5,PAY_AMT6


## Plot model with features selected by LASSO 
set.seed(123)

glm.lasso <- glm(default.payment.next.month ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_1
             +PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+PAY_AMT1+PAY_AMT2+PAY_AMT4
             +PAY_AMT5+PAY_AMT6, data=traindata ,family=binomial)
summary(glm.lasso)

pred_lasso =predict(glm.lasso,xTest ,type="response")

# Test MSE 
MSE(pred_lasso,yTest) #0.1439094

#Accuracy 
optcut <- optimalCutoff(yTest, pred_lasso,
                        optimiseFor="misclasserror")
pred_lasso_1 <-ifelse(pred_lasso < optcut,0,1)
table(yTest, pred_lasso_1)

mean(yTest == pred_lasso_1) #0.8223333

#ROC
plotROC(actuals=yTest, predictedScores=pred_lasso) #0.7115

######RPART########
###################


set.seed(123)
# Building the rPart model
rPartMod <- train(factor(default.payment.next.month)~.,data=traindata, method="rpart")
# Showing the variable importance in the rPart model
rpartImp <- varImp(rPartMod)
print(rpartImp)

## Import features PAY_1,PAY_2,PAY_3,PAY_4_PAY_5 


## Plot model with features selected by rpart
set.seed(123)

glm.rpart<- glm(default.payment.next.month ~PAY_1+PAY_2+PAY_3+PAY_4+PAY_5, data=traindata ,family=binomial)
summary(glm.rpart)

pred_rpart =predict(glm.rpart,xTest ,type="response")

# Test MSE 
MSE(pred_rpart,yTest) #0.1448095

#Accuracy 
optcut <- optimalCutoff(yTest, pred_rpart,
                        optimiseFor="misclasserror")
pred_rpart_1 <-ifelse(pred_rpart < optcut,0,1)
table(yTest, pred_rpart_1)

mean(yTest == pred_rpart_1) #0.8255

#ROC
plotROC(actuals=yTest, predictedScores=pred_rpart) #0.6988



##### Principal Component Analysis (PCR) 1SE Selection #####
############################################################
library(pls)

set.seed(123)

pcr.fit=pcr(default.payment.next.month~.,data=traindata, scale=TRUE, validation="CV")
summary(pcr.fit)

plot(pcr.fit, "loadings", comps = 1:2, legendpos = "topleft")
abline(h = 0) 

validationplot(pcr.fit, val.type="MSEP", main="10-fold CV",legendpos = "topright")

set.seed(123)
ncomp.onesigma = selectNcomp(pcr.fit, method = "onesigma", plot = TRUE, main="1 SE rule selection")

pcr.pred=predict(pcr.fit, newdata=xTest, ncomp=ncomp.onesigma)

## Checking TEST MSE  
MSE(pcr.pred,yTest) ##0.1516635

## Check Accuracy

optcut <- optimalCutoff(yTest, pcr.pred,
                        optimiseFor="misclasserror")
pcr.pred_1 <-ifelse(pcr.pred < optcut,0,1)
table(yTest, pcr.pred_1)

mean(yTest == pcr.pred_1) ## 0.8116667

## Check ROC

plotROC(actuals=yTest, predictedScores=pcr.pred) ## 0.696


##### PCR w/ CV Selection #####
###############################
set.seed(123)

ncomp.random = selectNcomp(pcr.fit, method = "randomization", plot = TRUE, main="Randomized selection")
pcr.pred1=predict(pcr.fit, newdata=xTest, ncomp=ncomp.random)

## Test MSE
MSE(pcr.pred1,yTest) ## 0.1501165

## Accuracy 
optcut <- optimalCutoff(yTest, pcr.pred1,
                        optimiseFor="misclasserror")
pcr.pred_2<-ifelse(pcr.pred1 < optcut,0,1)
table(yTest, pcr.pred_2)
mean(yTest == pcr.pred_2) ##0.8188333

##ROC
plotROC(actuals=yTest, predictedScores=pcr.pred1) #0.705


##### PCR w/ LASSO #####
########################
library(glmnet)

set.seed(123)
comps.mat=pcr.fit$scores
cv.out10d = cv.glmnet(comps.mat, yTrain, alpha = 1) #determine lambda.min and lambda.1SE
lasso.mod <- glmnet(comps.mat, yTrain, alpha = 1, lambda = cv.out10d$lambda.1se)
lasso.mod$beta

prin_comp = prcomp(xTrain,scale. = T)
test_data = predict(prin_comp, newdata = xTest)
test_data = as.data.frame(test_data)
test_data = as.matrix(test_data)
lasso.pred2 = predict(lasso.mod, s=cv.out10d$lambda.1se, newx = test_data)

#Test MSE
MSE(lasso.pred2,yTest) #0.1513066


#Accuracy 
optcut <- optimalCutoff(yTest, lasso.pred2,
                        optimiseFor="misclasserror")
lasso.pred_2<-ifelse(lasso.pred2 < optcut,0,1)
table(yTest, lasso.pred_2)
mean(yTest == lasso.pred_2) #0.8173333

#ROC
plotROC(actuals=yTest, predictedScores=lasso.pred2) #0.7003


##### Partial Least Squares (PLS) 1SE Selection #####
####################################################
set.seed(123)

pls.fit=plsr(default.payment.next.month~.,data=traindata, scale=TRUE, validation="CV")
summary(pls.fit)

#Plot 10-fold CV MSE's:
validationplot(pls.fit, val.type="MSEP", main="PLS 10-fold CV",legendpos = "topright")

set.seed(123)
#See where the 1SE rule of thumb lands you:
ncomp.onesigma = selectNcomp(pls.fit, method = "onesigma", plot = TRUE, main="PLS 1SE selection")

#Predict using the desired component number and display MSE:
plsr.pred=predict(pls.fit, newdata=xTest, ncomp=ncomp.onesigma)

#Test MSE
MSE(plsr.pred,yTest) #0.150366

## Accuracy
optcut <- optimalCutoff(yTest, plsr.pred,
                        optimiseFor="misclasserror")
plsr.pred_1<-ifelse(plsr.pred < optcut,0,1)
table(yTest, plsr.pred_1)
mean(yTest == plsr.pred_1) #0.8151667

#ROC
plotROC(actuals=yTest, predictedScores=plsr.pred) #0.7051

##### PLS CV Selection #####
############################
set.seed(123)

ncomp.onesigma_1 = selectNcomp(pls.fit, method = "randomization", plot = TRUE, main="Randomized selection")
pls.pred2=predict(pls.fit, newdata=xTest, ncomp=ncomp.onesigma_1)

#Check MSE of the 10-component model
MSE(pls.pred2,yTest) #0.150366

## ACCURACY
optcut <- optimalCutoff(yTest, pls.pred2,
                        optimiseFor="misclasserror")
pls.pred_2<-ifelse(pls.pred2 < optcut,0,1)
table(yTest, pls.pred_2)
mean(yTest == pls.pred_2) #0.8151667

##ROC CURVE
plotROC(actuals=yTest, predictedScores=pls.pred2) #0.707

##### RPART #####
#################

library(rpart)
set.seed(123)
temp = rpart(default.payment.next.month~.,method="anova",traindata, minsplit=5,cp=.0005)

bestcpAnova=temp$cptable[which.min(temp$cptable[,"xerror"]),"CP"]
bestAnova = prune(temp,cp=bestcpAnova)

#Plot the best MSE-based tree
#windows()
par(mfrow=c(1,1))
plot(bestAnova,uniform=TRUE)
text(bestAnova,digits=4,use.n=TRUE,fancy=FALSE)
max_row = length(unique(bestAnova$where)) #15
max_node =nrow(bestAnova$frame) #29

## MSE
predictions = predict(bestAnova,newdata = xTest)
MSE(predictions,yTest) #0.135064

## ACCURACY
optcut <- optimalCutoff(yTest, predictions,
                        optimiseFor="misclasserror")
predictions_1<-ifelse(predictions< optcut,0,1)
table(yTest, predictions_1)
mean(yTest == predictions_1) #0.8256667

# ROC
plotROC(actuals=yTest, predictedScores=predictions) #0.748

summary(bestAnova)

##### Random Forest mtry = 23 #####
###################################
library(randomForest)
set.seed(123)

#Recall bagging is achieved with mtry=P, since here we have 23 predictors, set mtry=23:
rffit3 = randomForest(default.payment.next.month~.,data=traindata,ntree=5000,maxnodes=max_node,mtry=23)
#windows()
plot(rffit3)


predictions2 = predict(rffit3,newdata = xTest)

#Test MSE
MSE(predictions2,yTest) #0.1331061

## ACCURACY
optcut <- optimalCutoff(xTest, predictions2,
                        optimiseFor="misclasserror")
predictions_2<-ifelse(predictions2 < optcut,0,1)
table(yTest, predictions_2)
mean(yTest == predictions_2) #0.335

##ROC CURVE
plotROC(actuals=yTest, predictedScores=predictions2) #0.762

varimp1 = importance(rffit3,type=1)
varimp1

##### Random Forest mtry = 7 (P/3) #####
########################################
set.seed(123)

rffit = randomForest(default.payment.next.month~.,
                     data=traindata ,ntree=5000,maxnodes=max_node,mtry=7)
predictions3 = predict(rffit,newdata = xTest)
MSE(predictions3,yTest)#0.1333163


## ACCURACY
optcut <- optimalCutoff(yTest, predictions3,
                        optimiseFor="misclasserror")
predictions_3<-ifelse(predictions3 < optcut,0,1)
table(yTest, predictions_3)
mean(yTest == predictions_3) #0.8261667

##ROC CURVE
plotROC(actuals=yTest, predictedScores=predictions3) #0.7685

varImp2 = importance(rffit,type=1)
varImp2

##### Boosting w/ Random Forest #####
#####################################

library(gbm) 
set.seed(123)

boostfit = gbm(default.payment.next.month~.,data=traindata,distribution='bernoulli',bag.fraction = .5,
               interaction.depth=5,n.trees=10000,shrinkage=.01,cv.folds=10)

#We can then compute the estimated optimal M (number of iterations) using the gbm.perf() function:
## Using cv
best = gbm.perf(boostfit, method="cv")
predictions4 = predict(boostfit,newdata = xTest,n.trees = best, type = "response")

#Test MSE
MSE(predictions4,yTest) #0.1323877

## ACCURACY
optcut <- optimalCutoff(yTest, predictions4,
                        optimiseFor="misclasserror")
predictions_4<-ifelse(predictions4 < optcut,0,1)
table(yTest, predictions_4)
mean(yTest == predictions_4) #0.8263333


##ROC CURVE
plotROC(actuals=yTest, predictedScores=predictions4) #0.7757

# Get variable importance information
vimp = summary(boostfit, n.trees = best, plotit = TRUE)  
row.names(vimp)
vimp

par(mar=c(5,7,4,1)+.1)
vimp_1 = summary(boostfit, n.trees = best, plotit = TRUE,las=1)  # Get variable importance information
print(vimp_1)

##### GBM with OOB #####
########################

set.seed(123)

best1=gbm.perf(boostfit, method="OOB")
predictions5 = predict(boostfit,newdata = xTest,n.trees = best1, type = "response")

#TEST MSE 
MSE(predictions5,yTest) #0.133236

## ACCURACY
optcut <- optimalCutoff(yTest, predictions5,
                        optimiseFor="misclasserror")
predictions_5<-ifelse(predictions5 < optcut,0,1)
table(yTest, predictions_5)
mean(yTest == predictions_5)#0.8265

##ROC CURVE
plotROC(actuals=yTest, predictedScores=predictions5) #0.77


vimp2 = summary(boostfit, n.trees = best1, plotit = TRUE)  # Get variable importance information
row.names(vimp2)
vimp2

par(mar=c(5,7,4,1)+.1)
vimp_2 = summary(boostfit, n.trees = best1, plotit = TRUE,las=1)  # Get variable importance information
print(vimp_2)

##### Neural Network #####
##########################

set.seed(123)

traindata$default.payment.next.month = as.factor(traindata$default.payment.next.month )
valid$default.payment.next.month = as.factor(valid$default.payment.next.month)

trainControl <- trainControl(method = "repeatedcv",
                             number = 10, 
                             repeats = 5) 
nnetTunegrid <- expand.grid(size = c(15.44),
                            decay = 0.007)
modelTune = train(default.payment.next.month~. ,data=traindata,method="nnet",
                  trControl = trainControl,maxit=50,rang=0.8,trace =TRUE, tuneGrid=
                    nnetTunegrid)

class_pred <- predict(modelTune, newdata=valid[,-c(24)])

## Accuracy
v <- as.data.frame(valid)

optcut <- optimalCutoff(v[,24], as.numeric(class_pred), optimiseFor="misclasserror")
class_pred1<-ifelse(as.numeric(class_pred) < optcut,0,1)
table(v[,24], class_pred1)
print("Test Accuracy is :")
print((mean(v[,24] == class_pred1))) # 0.822


prob_pred <- predict(modelTune,valid[,-24],type="prob")
head(prob_pred)

## ROC
library(pROC)
df_roc<-roc(valid$default.payment.next.month,
            prob_pred[,2],
            direction =  "auto")
plot(df_roc,main=paste("AUC",df_roc$auc)) #0.762077470004444


# Test MSE
MSE(as.numeric(class_pred),as.numeric(valid$default.payment.next.month))#0.211

set.seed(123)
library(NeuralNetTools)
imp<-olden(modelTune, bar_plot=F)
imp$fields<-row.names(imp)
imp %>% arrange(importance) 

impd<-filter(imp, importance>=0) %>% arrange(importance) 
print("-- Number of important columns --")
nrow(impd)
cNames<-impd
print("-- Names of Important columns --")
cNames$fields
print("-- Getting Data only for Important columns")
td<-traindata[,cNames$fields]
td$default.payment.next.month <- traindata[,'default.payment.next.month']
print("-- Dimensions of New TrainData created using Important columns")
dim(td)
head(td)

valid_td<-valid[,cNames$fields]
valid_td$default.payment.next.month <- valid[,'default.payment.next.month']


nnetTunegrid <- expand.grid(size = c(13),
                            decay = c(0.006))
## Neural Network with important variables only
modelTune1 <- train(
  default.payment.next.month ~ .,
  data = td,
  method = 'nnet',               #Kind of model - nnet
  trControl = trainControl,
  maxit = 50,                   #Run for Iterations
  rang = 0.7,                    #Not tunable - Initial Weights
  trace = TRUE,
  tuneGrid= nnetTunegrid         # Alternative tuneLength
)

class_pred_1 <- predict(modelTune1, newdata=valid_td[,-c(ncol(td))])


## Accuracy
v <- as.data.frame(valid_td)

optcut <- optimalCutoff(valid[,24], as.numeric(class_pred_1), optimiseFor="misclasserror")
class_pred_2<-ifelse(as.numeric(class_pred_1) < optcut,0,1)
table(valid[,24], class_pred_2)
print("Test Accuracy is :")
print((mean(valid[,24] == class_pred_2))) # 0.798


# ROC
prob_pred1 <- predict(modelTune1,
                     valid_td[,-c(ncol(td))],
                     type="prob")

df_roc1<-roc(valid_td$default.payment.next.month,
            prob_pred1[,2],
            direction =  "auto")

plot(df_roc1,
     main=paste("AUC", df_roc1$auc)) #0.761485


## MSE
MSE(as.numeric(class_pred_1),as.numeric(valid$default.payment.next.month)) #0.1768333


######################## ubSMOTE #################################################################
##############################################################################################
library(unbalanced)
b_data <- ubSMOTE(X = xTrain, Y = as.factor(yTrain),   # Also y be a factor & vector not a dataframe
                  perc.over=100,   #  100/100 = 2 instances generated for every rare instance
                  perc.under=300,  #  300/100 = 5 majority class instances randomly selected
                  k=3,             #              for each smoted observation.         
                  verbose=TRUE) 
traindata <- cbind(b_data$X, default.payment.next.month = b_data$Y)
table(traindata$default.payment.next.month)/nrow(traindata) #Ratio of classes in dependent variable
xTrain <-traindata[,c(1:23)]
yTrain <-as.numeric(traindata[,c(24)])
traindata[,24] <- sapply(traindata[,24], as.numeric)


##### Principal Component Analysis (PCR) 1SE Selection #####
############################################################
library(pls)

set.seed(123)

pcr.fit=pcr(default.payment.next.month~.,data=traindata, scale=TRUE, validation="CV")
summary(pcr.fit)

plot(pcr.fit, "loadings", comps = 1:2, legendpos = "topleft")
abline(h = 0) 

validationplot(pcr.fit, val.type="MSEP", main="10-fold CV",legendpos = "topright")

set.seed(123)
ncomp.onesigma = selectNcomp(pcr.fit, method = "onesigma", plot = TRUE, main="1 SE rule selection")

pcr.pred=predict(pcr.fit, newdata=xTest, ncomp=ncomp.onesigma)

## Checking TEST MSE  
MSE(pcr.pred,yTest) ##0.1516635

## Check Accuracy

optcut <- optimalCutoff(yTest, pcr.pred,
                        optimiseFor="misclasserror")
pcr.pred_1 <-ifelse(pcr.pred < optcut,1,2)
table(yTest, pcr.pred)

mean(yTest == pcr.pred) ## 0.8116667

## Check ROC

plotROC(actuals=yTest, predictedScores=pcr.pred) ## 0.696


##### PCR w/ CV Selection #####
###############################
set.seed(123)

ncomp.random = selectNcomp(pcr.fit, method = "randomization", plot = TRUE, main="Randomized selection")
pcr.pred1=predict(pcr.fit, newdata=xTest, ncomp=ncomp.random)

## Test MSE
MSE(pcr.pred1,yTest) ## 0.1501165

## Accuracy 
optcut <- optimalCutoff(yTest, pcr.pred1,
                        optimiseFor="misclasserror")
pcr.pred_2<-ifelse(pcr.pred1 < optcut,0,1)
table(yTest, pcr.pred_2)
mean(yTest == pcr.pred_2) ##0.8188333

##ROC
plotROC(actuals=yTest, predictedScores=pcr.pred1) #0.705


##### PCR w/ LASSO #####
########################
library(glmnet)

set.seed(123)
comps.mat=pcr.fit$scores
cv.out10d = cv.glmnet(comps.mat, yTrain, alpha = 1) #determine lambda.min and lambda.1SE
lasso.mod <- glmnet(comps.mat, yTrain, alpha = 1, lambda = cv.out10d$lambda.1se)
lasso.mod$beta

prin_comp = prcomp(xTrain,scale. = T)
test_data = predict(prin_comp, newdata = xTest)
test_data = as.data.frame(test_data)
test_data = as.matrix(test_data)
lasso.pred2 = predict(lasso.mod, s=cv.out10d$lambda.1se, newx = test_data)

#Test MSE
MSE(lasso.pred2,yTest) #0.1513066


#Accuracy 
optcut <- optimalCutoff(yTest, lasso.pred2,
                        optimiseFor="misclasserror")
lasso.pred_2<-ifelse(lasso.pred2 < optcut,0,1)
table(yTest, lasso.pred_2)
mean(yTest == lasso.pred_2) #0.8173333

#ROC
plotROC(actuals=yTest, predictedScores=lasso.pred2) #0.7003