
#-----------------------------------BIG MART SALES PREDICTION-------------------------------------------





#  ---------------------------------------Hypothesis---------------------------------------------








#required packages

library(dplyr)
library(caret)
library(ggplot2)
library(plyr)
library(randomForest)

#loading dataset

train=read.csv("E:/life R/projects/bigmartsalesprediction/Train_UWu5bXk.csv")
test=read.csv("E:/life R/projects/bigmartsalesprediction/Test_u94Q5KV.csv")

#observing the two data sets
View(train)
View(test)

#summarize the dataset
dim(train)
sapply(train,class)
glimpse(train) #better than str()
summary(train)

# checking the na values in the dataset
table(is.na(train))
colSums(is.na(train))

#------------------------inferences or drawings-------------------------------------- 






 
# removing  na values in the  column Item_Weight
train$Item_Weight[is.na(train$Item_Weight)]=median(train$Item_Weight[!is.na(train$Item_Weight)])

table(is.na(train$Item_Weight))



# unproper factor variables in Item_fat_Content column

table(train$Item_Fat_Content)

train$Item_Fat_Content <- revalue(train$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
train$Item_Fat_Content<-revalue(train$Item_Fat_Content,c("low fat"="Low Fat"))

prop.table(table(train$Item_Fat_Content))


------------------------
  
#Item_Visibility column is having 0 which is practically not possible
# replace it with median value 

train$Item_Visibility <- ifelse(train$Item_Visibility == 0,median(train$Item_Visibility), train$Item_Visibility)

train %>% filter(Item_Visibility == 0)


#--------------------------------
  
#blank spaces left in Outlet_Size

levels(train$Outlet_Size)[1]<-"Others"

#summarize dataset

dim(train)
sapply(train,class)
glimpse(train) #better than str
summary(train)
table(train$Item_Type)



#---------------Visualization---------------------------

"two types of analysis are to be done 

1.univariate analysis

2.bivariate analysis"



#Univariate Analaysis 

#Item_Oulet_Sales

ggplot(train, aes(Item_Outlet_Sales))+geom_density(fill = "red", alpha = "0.7")

max(train$Item_Outlet_Sales)


#Item_Fat_Content
ggplot(train, aes(Item_Fat_Content))+ geom_bar(fill= "darkblue")


#Item_Type

ggplot(train, aes(Item_Type))+ geom_bar(fill= "darkblue")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train$Item_Type))

x<-c(648,251,110,649 ,682,856,1232,214,910,425,169,64,1200,445,148)

pie(x,levels(train$Item_Type),main="item type",col=rainbow(length(x)))

#Item_MRP

boxplot(train$Item_MRP)


#Outlet_Identifier

ggplot(train, aes(Outlet_Identifier))+ geom_bar(fill= "darkblue")+theme(axis.text.x = element_text(angle = 45, hjust = 1))


table(train$Outlet_Identifier)


#Outlet_Type

ggplot(train, aes(Outlet_Type))+ geom_bar(fill= "darkblue")+theme(axis.text.x = element_text(angle = 45, hjust = 1))



#-----------BIVARIATE ANALYSIS---------------------------------------

#Outlets vs Total sales

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat ="identity", color = "purple") +
theme(axis.text.x = element_text(angle = 45, vjust =0.5, color = "black")) + 
ggtitle("Outlets vs Total Sales")

#Inference 'Out027' has more sales 

#-----------------------------------

# Fat Content vs Outlet Sales 

ggplot(train, aes(Item_Fat_Content, Item_Outlet_Sales)) + geom_bar(stat ="identity", color = "green") +theme(axis.text.x = element_text(angle = 45, vjust =0.5, color = "black")) + 
ggtitle("Fat content vs Total Sales")

# Inference 'Low Fat' Item as more sales

#------------------------------------

#Item Visibility vs Outlet Sales

ggplot(train, aes(Item_Visibility, Item_Outlet_Sales)) + geom_point(stat ="identity", color = "purple") +
theme(axis.text.x = element_text(angle = 45, vjust =0.5, color = "black")) + 
ggtitle("Item Visibility vs Outlet Sales")

# Inference less than 0.2 has more outlet sales

#------------------------------------------------


#Item Type vs Outlet Sales

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar(stat ="identity", color = "green") +theme(axis.text.x = element_text(angle = 90, vjust =0.5, color = "black")) + 
ggtitle("Fat content vs Total Sales")

# Inference
# 1. Vegitable and Snack has more sales
# 2. House hold & Frozen Foods is second
# 3. Breakfast, Seafood, Hard Drinks, Starchy Foods are less

#-------------------------------------------------------

ggplot(train, aes(Outlet_Size, Item_Outlet_Sales)) + geom_bar(stat ="identity", color = "green") +theme(axis.text.x = element_text(angle = 45, vjust =0.5, color = "black")) + 
  ggtitle("Fat content vs Total Sales")

# Inference : Medium size outlet has more sales

# Outlet Location Type vs Sales



#applying random forest for predicting the importance order of variables and rmse factor

library(randomForest)

set.seed(123)

#converting all into factors


train$Item_Type <- as.factor(train$Item_Type)
train$Outlet_Identifier <- as.factor(train$Outlet_Identifier)
train$Outlet_Size <- as.factor(train$Outlet_Size)
train$Outlet_Location_Type <- as.factor(train$Outlet_Location_Type)
train$Outlet_Type <- as.factor(train$Outlet_Type)
train$Item_Identifier <- as.factor(train$Item_Identifier)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)


rf<-randomForest(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type +Outlet_Type,data=train)

importance(rf)

summary(rf)

varImpPlot(rf)

  


































  



648,251,110,649 ,682,856,1232,214,910,425,169,64,1200,445,148