store <-read.csv(file="store.csv", na.strings=c("", "NA"))
test <-read.csv(file="test.csv", na.strings=c("", "NA"))
train <-read.csv(file="train.csv", na.strings=c("", "NA"))

#Finding out missing values for variables
sum(is.na(train))
sum(is.na(test))
sum(is.na(store))

#to see the missing values locations in test dataset
test[!complete.cases(test),]
train[!complete.cases(train),]
store[!complete.cases(store),]

library(plyr)

summary(train)
summary(test)
summary(store)

#making sure that closed shops have sales = 0
summary(train$Sales[!train$Open])

#since prediction of sales for closed stores will be zero, we can opt out the closed stores data
train <- train[train$Open == 1,]

length(unique(train$Store))
length(unique(test$Store))

#All the stores in the train set are not present in the test set, so training for those stores wouldn't make sense
#Removing those stores from the train set
train <- train[train$Store %in% unique(test$Store),]

#handling the missing values in test
test[!complete.cases(test),]

#the store is opened all days except sunday, the day of week for the rows that have NA values does not have sundays
#Replacing NA values with 1 for these rows

test[is.na(test$Open),]$Open <- 1
sum(is.na(test$Open))

#Converting the Open, Promo, SchoolHoliday to logical values
train$Open <- as.logical(train$Open)
train$Promo <- as.logical(train$Promo)
train$SchoolHoliday <- as.logical(train$SchoolHoliday)

test$Open <- as.logical(test$Open)
test$Promo <- as.logical(test$Promo)
test$SchoolHoliday <- as.logical(test$SchoolHoliday)

#Exploring sales statistics
summary(train$Sales)
sd(train$Sales)

hist(train$Sales,xlab="Sales")

boxplot(train$Sales)

#There are so many outliers above 20000. Investigating sales above 20000
summary(train[train$Sales > 20000,])
outliers <- subset(train[train$Sales > 20000, ])

hist(aggregate(train$Sales, 
               by = list(train$Store), mean)$x, 75,
     main = "Mean sales of each store")

hist(train$Customers, 40)

hist(aggregate(train$Customers, 
               by = list(train$Store), mean)$x, 100,
     main = "Mean customers/store")

# Predicting that sales and customers are positively related, promo and sales are positively related
# as promo would attract more customers and customers is positively related to sales

tapply(train$Sales,train$DayOfWeek,mean)
#It is observed that sales are high on Mondays and Sundays and there is even distribution on all other days

#Performing ttest for with Promo and without promo
t.test(train[train$Promo,]$Sales,train[!train$Promo,]$Sales)

#Performing ttest for with Promo and without promo w.r.t. Customers
t.test(train[train$Promo,]$Customers,train[!train$Promo,]$Customers)

#we can observe that Promo has significant effect on sales
boxplot(train[train$Promo,]$Sales, train[!train$Promo,]$Sales, names= c("Sales with Promo", "Sales without Promo"))

boxplot(train[train$Promo,]$Customers, train[!train$Promo,]$Customers, names= c("Promo with Customer", "Promo without Customer"))

#Testing the same for SchoolHoliday and StateHoliday
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$SchoolHoliday,]$Sales,train[!train$SchoolHoliday,]$Sales)
#State Holidays have very significant effect, there is increase in sales for school holidays too, but the rate is very small

#merging train and store data
train_store <- merge(train, store, by = "Store")

librarylibrary(ggplot2)

ggplot(train_store, aes(x = factor(PromoInterval), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)


ggplot(train_store, 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)


ggplot(train_store, aes(StoreType, fill= StoreType)) +geom_bar()+
  ylab("Store count of total store") +
  ggtitle("Distribution of avilable StoreTypes")

ggplot(train_store, aes(Assortment, fill= Assortment)) +
  geom_bar()+xlab("AssortmentType")+ggtitle("Distribution of available AssortmentTypes")

ggplot(train_store, aes(x = Assortment , y = Sales, fill= Assortment)) + 
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,5000))+
  ggtitle("Boxplot showing the effect of Assortment Type on Sales")


ggplot(train_store, aes(x = Promo2 , y = Sales, color = factor (Promo2))) + 
  
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,10000))+
  scale_x_continuous(breaks = seq(0,1,1))+xlab("Promotion on/off")+
  ylab("Sales of Stores")+
  ggtitle("Boxplot of the effect of the promotion on sales")


qplot(factor(CompetitionOpenSinceYear), Sales, data = train_store, 
      fill = factor(CompetitionOpenSinceYear),geom = "boxplot")+
  scale_y_continuous(breaks = seq(0,100000,5000))+xlab("Competition Year")


ggplot(train_store[train$Store == 256,])+geom_line(aes(x= Date, y = Sales))+
  scale_y_continuous(breaks = seq(0,100000,4000))+xlab("Timeline")+
  ggtitle("Sales trend of a chosen store only for open days")


ggplot(train_store, aes(x = StateHoliday, y = Sales)) + 
  
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,5000))

#We can observe that type B store never closes even on Sunday\\

typeof(train$Sales)
str(train)

#sales for december 
tapply(train$Sales,train$DayOfWeek,mean)
plot(tapply(train$Sales,train$DayOfWeek,mean),xlab="Day",ylab="Sales mean")

plot(tapply(train$Sales,train$DateDay,mean),xlab="Month",ylab="Sales mean")

tapply(train$Sales,train$DateMonth,mean)
train[train$Sales==0,]

train_sales <- as.numeric(train$Sales == 0) && train$Open == 1
train_sales

train[train$Open == 1 && train$Sales == 0]

table(ifelse(train$Open == 1, "Opened"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))

install.packages("ggplot2", dependencies = TRUE)

storenum <- train[train$Store == 23, ]
mean(train$Sales)

storenum$pred <- mean(storenum$Sales)
storenum$pred
storenum$Error <- storenum$pred - storenum$Sales
storenum$Error

#root-mean square error
sqrt(mean((train$Sales-train$Pred)^2))
#1604.499

sqrt(mean(train$Sales- mean(train$Sales))^2)

train$meanpred <- mean(train$Sales)

train$Error <- train$meanpred - train$Sales

sqrt(mean((train$Sales-train$meanpred)^2))
#2953.502

#This result indicates that the median is a marginally worse predictor at least for the first store, so I will continue working with the mean.

#date extraction 

train$DYear <- as.numeric(strftime(train$Date, format="%y"))
# the strftime function extracts a string from the date, so this must be transformed to a numeric value
train$DMonth <- as.numeric(strftime(train$Date, format="%m"))
train$DDay <- as.numeric(strftime(train$Date, format="%d"))
train$DWeek <- as.numeric(strftime(train$Date, format="%W"))
      

test$DYear <- as.numeric(strftime(test$Date, format="%y"))
# the strftime function extracts a string from the date, so this must be transformed to a numeric value
test$DMonth <- as.numeric(strftime(test$Date, format="%m"))
test$DDay <- as.numeric(strftime(test$Date, format="%d"))
test$DWeek <- as.numeric(strftime(test$Date, format="%W"))




train <- train[c(1:2,4,6:9,12:15)]

                        
library(randomForest)

data(train)
x<- sample(1:nrow(train) , 2300)
Rf <- randomForest(Sales ~ ., train , ntree= 50, mtry= 2, sampsize=   )

Lm <- lm(Sales ~ ., train )

Pred <- predict(Rf)

lmpred <- predict(Lm)

sqrt(mean((train$Sales - lmpred)^2))

sqrt(mean((train$Sales - Pred)^2))

str(train)

str(Lm)

test <- test[c(1:3,5:12)]


levels(test$StateHoliday) <- levels(train$StateHoliday)

test$rfp <- predict(Rf , test) 

train1 <- train
test1<- test

train <- train[c(1)]
str(test)
str(train)

str(Pred)
 str(x)
str(train$Sales)
y <- train[x,3]
y
sqrt(mean((y - Pred)^2))

str(storenum)



sqrt(mean((store$Sales-store$Prediction)^2))

train4 <- train[1:41088,]

#neural network

library(neuralnet)


train3 <- train

str(train4)


train$Open <- as.numeric(train$Open)
train$Promo <- as.numeric(train$Promo)
train$SchoolHoliday <- as.numeric(train$SchoolHoliday)

xy<- train$StateHoliday
xy.factor <- factor(xy)
xy <- as.numeric(xy.factor)
train$StateHoliday <- xy

xytest<- test$StateHoliday
xytest.factor <- factor(xytest)
xytest <- as.numeric(xytest.factor)
test$StateHoliday <- xytest


str(test)
max = suppressWarnings(as.numeric(apply(train4, 2 ,max)))
min= suppressWarnings(as.numeric(apply(train4, 2, min)))

sampletrain <- train4[sample(nrow(train4)),]
sampletrain$prediction <- 0
ma1 <- mean(sampletrain$Sales)
sampletrain$Sales <- sampletrain$Sales - ma1
ma2 <- max(abs(sampletrain$Sales))
sampletrain$Sales <- sampletrain$Sales/ma2

length(min)
length(train)

z<-nrow(train)
  ind <- 1:z
  sampleIndex <- ind[floor(1+0.1*(i-1)*z):(0.1*i*z)]


n <- names(train)
f <- as.formula(paste("Sales ~", paste(n[n !=  "Sales"], collapse = " + ")))


#scaled <- as.data.frame(scale(train, center = min, scale = max-min))
library(neuralnet)
library(dplyr)
nn <- neuralnet(f , train4 , hidden = c(1,5) , threshold = c(0.5,0.3,0.1,0.03) )
plot(nn)
 

test1 <- test

test$Open <- as.numeric(test$Open)
test$Promo <- as.numeric(test$Promo)
test$SchoolHoliday <- as.numeric(test$SchoolHoliday)



c <- compute(nn, test[,c(2:11)])
c$net.result <- c$net.result * ma2 + ma1;
sampletrain$Sales <- sampletrain$Sales *ma2 +ma1
sampletrain$prediction <- c$net.result

sqrt(mean((sampletrain$Sales - c$net.result)^2))
testid<-test$Id
testid
View(testid)
testPrediction<-test$rfp
View(testPrediction)


write.table(test, "C:/Users/manup/mydata.txt", sep="\t")


library(xlsx)

install.packages(c("readxl","writexl")) 
library(writexl)
write_xlsx(test, "C:/Users/manup/mydata.xlsx")


