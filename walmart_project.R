dataset <- read.csv("Walmart_Store_sales.csv")
View (dataset)

numberOfNA = dim(dataset[is.na(dataset),])[1]
numberOfNA


#find the store with max sales
group1 <- aggregate (dataset$Weekly_Sales, by= list (dataset$Store), FUN= sum)
names(group1)[1] <- "storeID"
names(group1)[2] <- "TotalSales"
class(group1) 
View(group1)
max_sales_store <- group1[which.max(group1$`TotalSales`), 1]
print (paste("The store with max sales is", max_sales_store,  "and the max sales is" , group1[which.max(group1$`TotalSales`), 2]))

#find the store with most S.D

group2 = aggregate (dataset$Weekly_Sales, by= list (dataset$Store), FUN= sd)
names(group2)[1] <- "storeID"
names(group2)[2] <- "standard_deviation"
class(group2) 
View(group2)
max_sd_store <- group2[which.max(group2$`standard_deviation`), 1]
print (paste("The store with max standard deviation is", max_sd_store,  "and the max sd is" , group2[which.max(group2$`standard_deviation`), 2]))

#make a column for mean

group3 = aggregate (dataset$Weekly_Sales, by= list (dataset$Store), FUN= mean)
names(group3)[1] <- "storeID"
names(group3)[2] <- "mean"
class(group3) 
View(group3)

#merge data frames
library(dplyr)
library (tidyverse)
group12 = inner_join (group1,group2, by = "storeID")
class(group12)
View(group12)

group123 = inner_join (group12,group3, by = "storeID")
View (group123)

#create and add column of sd/mean ####################

my_func <- function(data,col1, col2){
  ratio= col1/col2
  return (ratio)
}
group4 <- mutate(group123, sd_mean_ratio = my_func(group123, group123$standard_deviation, group123$mean))
View(group4)  

#extract month and quarter out of date and make a new column  
#library(zoo)
#dataset$quarter =as.yearqtr(as.Date(dataset$Date, "%d/%m/%Y" ))
#View(dataset)


#different method to extract quarter

date_col <- dataset$Date
date_col
quarter_col <- substr(quarters(as.Date(date_col)), 2, 2) 
quarter_col
dataset <- cbind(dataset,quarter_col)
View(dataset)

#extract year

date_col <- as.POSIXct(dataset$Date, format= "%d-%m-%Y")
date_col
year_col <- format(as.Date(date_col), format="%Y")
dataset <- cbind(dataset,year_col )
View(dataset)
str(dataset) 

#for each store get Q2 2012 sales total
Sales_2012Q2 <-setNames(dataset %>% group_by(Store) %>% filter(year_col== "2012" & quarter_col == "2" ) %>%  summarise(sum(Weekly_Sales)),
                        c("Store","Sum_2012Q2_sales"))
Sales_2012Q2
View(Sales_2012Q2)


#for each store get Q3 2012 sales total

Sales_2012Q3 <-setNames(dataset %>% group_by(Store) %>% filter(year_col== "2012" & quarter_col == "3" ) %>%  summarise(sum(Weekly_Sales)),
                        c("Store","Sum_2012Q3_sales"))
Sales_2012Q3
View(Sales_2012Q3)

#calculate GR, add column and find store with max Growth Rate
walmart_GR <- merge(Sales_2012Q2, Sales_2012Q3, by = "Store")
View(walmart_GR)
walmart_GR <- mutate (walmart_GR, GR_col = ((Sum_2012Q2_sales - Sum_2012Q3_sales)/Sum_2012Q2_sales))  
View(walmart_GR)
max_GR <- walmart_GR [which.max(walmart_GR$GR_col), 4]
max_GR
max_store <- walmart_GR [which.max(walmart_GR$GR_col), 1]
max_store
print(paste("Store with max growth rate is ", max_store, "and growth rate is", max_GR))

#Find holidays with positive impact on sales

dataset$holiday_name <- ifelse( dataset$Date  %in% c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013") ,"Superbowl"
                                , ifelse( dataset$Date  %in% c("10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013") ,"Labor_Day"
                                          ,ifelse( dataset$Date  %in%  c("26-11-2010", "25-11-2011", "23-11-2012", "29-11-2013") ,"Thanksgiving",  
                                                   ifelse( dataset$Date  %in% c("31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")  ,"Christmas", "NA"))))


holiday_mean_df <- aggregate(dataset$Weekly_Sales, by= list(dataset$holiday_name), FUN= mean)
View(holiday_mean_df)

total_sales_df <- aggregate(dataset$Weekly_Sales, by= list(dataset$Holiday_Flag), FUN= mean)


View(total_sales_df)

holiday_mean_df[order(-holiday_mean_df$x),]

#Insights:  Thanksgiving, Super Bowl and Labor Day have higher than non-holiday mean sales
#Christmas has lower sales than non-holiday mean sales

#Give semester and monthly view of sales
library(lubridate)
dataset$sem_col <- semester(dataset$Date, with_year = F)
month_col <- month(as.Date(dataset$Date, format="%d-%m-%Y"))
month_col
dataset$month_col <- month.abb[month_col]
View(dataset)

#Generate year_month, year_sem columns for graphs

dataset$year_month <- paste(dataset$year_col,dataset$month_col,sep="_")
dataset$year_sem <-  paste(dataset$year_col,dataset$sem_col,sep="S")

#generate monthly and semester view plots
library(ggplot2)

ggplot(dataset, aes(as.Date(Date,format= "%d-%m-%Y"), Weekly_Sales, fill =month(Date))) +  
  geom_bar(size = 0.8, stat = "summary",
           fun = "sum")+ 
  theme_classic()+
  scale_x_date(date_labels = "%m/%Y",date_breaks = "1 month")+
  theme(axis.text.x = element_text(
    colour = 'black', angle = 50, size = 10,
    hjust = 0.5, vjust = 0.5, face = "bold"),
    axis.title.x=element_blank(),axis.text.y = element_text(
      colour = 'black', angle = 50, size = 10,
      hjust = 0.5, vjust = 0.5, face = "bold")) +
  theme(axis.title.x = element_text(colour = "blue", face = "bold"),
        axis.title.y = element_text(colour = "blue", face = "bold"))+xlab("Month")




p<-ggplot(data=dataset, aes(x=year_sem, y=Weekly_Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal()
p

#Create a week column and convert dates into days

dataset$Week<- format(as.Date(dataset$Date), "%W")
##Change dates into days by creating new variable.
dataset$Day <- day(as.Date(dataset$Date, format = '%d-%m-%Y'))
View(dataset)

#Building linear regression models for Store 1


library(MLmetrics)
library(corrplot)

Store_1 <- filter (dataset, Store == 1)
View(Store_1)

#Checking for na

number_NA = dim(Store_1[is.na(Store_1),])[1]
number_NA

#checking for outliers with boxplots

par(mfrow=c(2,3))
boxplot(Store_1$Weekly_Sales,col="Blue",main = "Weekly Sales") 
boxplot(Store_1$Unemployment,col="Blue",main = "Unemployment")
boxplot(Store_1$Temperature,col="Blue",main = "Temperature")
boxplot(Store_1$Fuel_Price,col = "Blue",main = "Fuel Price")
boxplot(Store_1$CPI,col = "Blue",main = "CPI")

#removing outliers from Weekly Sales column and Unemployment column

qws <- quantile(Store_1$Weekly_Sales, probs=c(.25, .75), na.rm = FALSE)
iqr_ws <- IQR(Store_1$Weekly_Sales)
Store_1<- subset(Store_1, Store_1$Weekly_Sales > (qws[1] - 1.5*iqr_ws) & Store_1$Weekly_Sales < (qws[2]+1.5*iqr_ws))
View(Store_1)

qw_u <- quantile(Store_1$Unemployment, probs=c(.25, .75), na.rm = FALSE)
iqr_u <- IQR(Store_1$Unemployment)
Store_1<- subset(Store_1, Store_1$Unemployment > (qw_u[1] - 1.5*iqr_u) & Store_1$Unemployment < (qw_u[2]+1.5*iqr_u))

View(Store_1)

#drop unnecessary columns

Store_1 <- subset(Store_1, select= -c(year_col, sem_col, month_col, quarter_col, year_month, year_sem))
Store_1 <- subset(Store_1, select = -c(Date, Store))

#Correlation plot

corr<-cor(Store_1 [,c(1,3:6)])
str(corr)

corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))


#creating dummy variables for holiday columns

holiday_flag <- as.factor(Store_1$Holiday_Flag)
dummy_holiday_flag <- data.frame(model.matrix(~holiday_flag))[,-1]
View(dummy_holiday_flag)

holiday_name <- as.factor(Store_1$holiday_name)
dummy_holiday_name <- data.frame(model.matrix(~holiday_name))[,-1]
View(dummy_holiday_name)

Store_1 <- cbind(Store_1,dummy_holiday_flag,dummy_holiday_name)

#dropping original columns

Store_1 = subset(Store_1, select = -c(Holiday_Flag,holiday_name) )
names(Store_1)
Store_1 = subset(Store_1, select = -c(Week, Day) )
View(Store_1)


#Split into train and test
library(caTools)
set.seed(123) 
sample = sample.split(Store_1, SplitRatio = 0.7) 

trainingSet = subset(Store_1, sample == T)

testSet = subset(Store_1, sample == F)

# Create model with all 9 columns: 
model1 = lm(formula = Weekly_Sales ~ . , data = trainingSet)

summary(model1)

#Creating revised dataset with only relevant columns

Store_1_rev = subset(Store_1, select = c(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment) )
View(Store_1_rev)
set.seed(123)
sample = sample.split(Store_1_rev, SplitRatio = 0.7) 

trainingSet = subset(Store_1_rev, sample == T)

testSet = subset(Store_1_rev, sample == F)

model2 = lm(formula = Weekly_Sales ~ . , data = trainingSet)
summary(model2)

#Model after dropping Fuel Price

model3 = lm(formula= Weekly_Sales~ Temperature + CPI + Unemployment, data = trainingSet)
summary(model3)

#model usimg only CPI and unemployment

model4 = lm(formula= Weekly_Sales~ CPI + Unemployment, data = trainingSet)
summary(model4)

y_pred_test = predict(model4, newdata = testSet)

y_pred_test

ggplot() + 
  geom_point(aes(x=testSet$Weekly_Sales,y=y_pred_test)) +
  xlab('actual_sales') +
  ylab('predicted_sales')+
  ggtitle('comparison of test data')


### Parameters to validate the accuracy of the model and improvise.

MAPE(y_pred_test,testSet$Weekly_Sales)

RMSE(y_pred_test,testSet$Weekly_Sales)

