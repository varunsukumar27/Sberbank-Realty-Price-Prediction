library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
train1 <- read.csv("train.csv", header = TRUE)
test2 <- read.csv("test.csv", header = TRUE)
macros <- read.csv("macro.csv", header = TRUE, stringsAsFactors = FALSE)

#Binding the train and test datasets for data cleaning.

test$price_doc <- 0
data.combined <- rbind(train,test)
head(data.combined$price_doc)

#Data Exploration
#Exploring the data structure to identify the variable types.
str(train)
#Creating a density plot to check the spread of the price.
ggplot(train, aes(price_doc))+geom_density()
#Reducing the skew using a logarithmic transformation.
ggplot(train, aes(log10(price_doc)))+geom_density()
#Changing the variable type of timestamp to date.
train$timestamp <- date(train$timestamp)
test$timestamp <- date(test$timestamp)
data.combined$timestamp <- date(data.combined$timestamp)

#Aggregating the median of prices by year and month.
data.combined$month <- month(data.combined$timestamp)
data.combined$year <- year(data.combined$timestamp)
data.combined$mo_yr <- paste(data.combined$month,data.combined$year,sep = "")
head(data.combined$year)
head(data.combined$mo_yr)
time.agg <- data.frame(Date = unique(data.combined$mo_yr))
time.agg <- data.frame(Median = aggregate(price_doc~mo_yr,data.combined[1:30471,], FUN = median))
#time.agg <- time.agg[names = c("Dates","Median")]
 p <- ggplot(data=time.agg, aes(x = Median.mo_yr, y = Median.price_doc)) 
p+geom_bar(stat = "identity")+theme(text = element_text(size=10),
                                   axis.text.x = element_text(angle=90, hjust=1)) 

#Counting the types of variables available in the dataset.
table(sapply(train1,class))

#Visualizing the proportion of missing values.
library(VIM)
aggr(train[1:20], prop = T, numbers = F)
aggr(train[21:50], prop = T, numbers = F)
aggr(train[51:100], prop = T, numbers = F)
aggr(train[101:150], prop = T, numbers = F)
aggr(train[201:292], prop = T, numbers = F)

#Naive XGBoosting for first submission and to obtain the important variables.
sample_sub <- read.csv("sample_submission.csv")
train1 <- read.csv("train.csv")
test2<-read.csv("test.csv")
macros1 <- read.csv("macro.csv")
library(xgboost)
train1 <- merge(x=train1,y=macros1, by = "timestamp", all.x = TRUE)
test2 <- merge(x=test2,y=macros1, by ="timestamp", all.x = TRUE)
id <- test2$id
y_train <- train$price_doc
x_train <- subset(train1, select = -c(id, timestamp, price_doc))
x_test <- subset(test2, select = -c(id, timestamp,price_doc))

len_train <- nrow(x_train)
len_test <- nrow(x_test)

train_test <- rbind(x_train, x_test)

features <- colnames(train_test)
for (f in features) {
  if (class(train_test[[f]])=="factor") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(factor(train_test[[f]], levels=levels))
  }
}

# for (f in features) {
#   if (class(train_test[[f]])=="character") {
#     #cat("VARIABLE : ",f,"\n")
#     train_test[[f]] <- NULL
#     #train_test[[f]] <- as.numeric(factor(train_test[[f]], levels=levels))
#   }
# }

x_train = train_test[1:len_train,]
x_test = train_test[(len_train+1):(len_train+len_test),]

dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

xgb_params = list(
  seed = 9,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.05,
  objective = 'reg:linear',
  max_depth = 5,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

res = xgb.cv(xgb_params,
             dtrain,
             nrounds=2000,
             nfold=10,
             early_stopping_rounds=15,
             print_every_n = 10,
             verbose= 1,
             maximize=F)

best_nrounds = res$best_iteration; best_nrounds
gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
library(Metrics)
rmsle(y_train, predict(gbdt,dtrain))
xgb_predict <- predict(gbdt,dtest)
sample_sub$price_doc <- xgb_predict
write.csv(sample_sub, "naive_output3.csv", row.names = F)

imp <- xgb.importance(colnames(dtrain), model = gbdt)

xgb.plot.importance(imp, rel_to_first = TRUE, top_n = 10)
(gg <- xgb.ggplot.importance(imp, measure = "Frequency", rel_to_first = TRUE, top_n = 10))
gg + ggplot2::ylab("Frequency")

#Changing the data type of product_type into factor.
data.combined$product_type <- factor(data.combined$product_type)
data.combined$mo_yr <- format(as.Date(data.combined$timestamp), "%Y-%m")
df1 <- data.combined[c("price_doc","mo_yr")]
df1 <- df1[1:30471,]
time.agg<- aggregate(df1$price_doc, by = list(df1$mo_yr), FUN = mean)
time

#Data Exploration and Visualizing the imporatant variables.
#Plotting the average realty prices against time.
ggplot(data = time.agg, aes(x = Group.1, y = x, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Year-Month")+ylab("Average Realty Price")

#Preliminary analysis on the variables.
summary(train$full_sq)
sum(is.na(train$full_sq))
na_count <-sapply(train, function(y) sum(length(which(is.na(y))))/292)
na_count <- data.frame(na_count)
library(plyr)
colnames(na_count) <- "percent"
variance <- sapply(train,var, na.rm=TRUE)
variance <- data.frame(variance)
colnames(variance) <- "variance"
class(na_count[na_count$percent>0.0,])
variance$variable <-rownames(variance)

#1. Exploration of the variable timestamp.
data.combined$day <- day(data.combined$timestamp)
data.combined$month <- month(data.combined$timestamp)
data.combined$year <- year(data.combined$timestamp)
data.combined$week <- week(data.combined$timestamp)
data.combined$day_of_week <- wday(data.combined$timestamp)

#Visualizing the trend in a month.
df1 <- data.combined[c("price_doc","day")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$day), FUN = mean)
colnames(day.agg) <- c("day", "avg_price")
ggplot(data = day.agg, aes(x = day, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Day")+ylab("Average Realty Price")

#Visualizing the trend of the price by months.
df1 <- data.combined[c("price_doc","month")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$month), FUN = mean)
colnames(day.agg) <- c("month", "avg_price")
ggplot(data = day.agg, aes(x = month, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Month")+ylab("Average Realty Price")

#Visualizing the trend of the price by year.
df1 <- data.combined[c("price_doc","year")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$year), FUN = mean)
colnames(day.agg) <- c("year", "avg_price")
ggplot(data = day.agg, aes(x = year, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Year")+ylab("Average Realty Price")

#Visualizing the trend of the price by timestamp
df1 <- data.combined[c("price_doc","timestamp")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$timestamp), FUN = mean)
colnames(day.agg) <- c("timestamp", "avg_price")
ggplot(data = day.agg, aes(x = timestamp, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("timestamp")+ylab("Average Realty Price")


#Visualizing the trend of the price by day of the week.
df1 <- data.combined[c("price_doc","day_of_week")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$day_of_week), FUN = mean)
colnames(day.agg) <- c("day_of_week", "avg_price")
ggplot(data = day.agg, aes(x = day_of_week, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("day_of_week")+ylab("Average Realty Price")
library(corrplot)
df1 <- data.combined[c(292:294,296:298)]
df1 <- df1[1:30471,]
cor <- cor(df1)
corrplot(cor, method = "number")

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(df1,0.95)
corrplot(cor, p.mat = res1[[1]], sig.level=0.01, insig = "p-value")

#2. Exploring the first important variable full_sq.
df1 <- data.combined[c("price_doc","full_sq")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$full_sq), FUN = mean)
colnames(day.agg) <- c("avg_price","full_sq")
ggplot(data = day.agg, aes(x = full_sq, y = avg_price, group = 1))+geom_line()+geom_point()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
  xlab("timestamp")+ylab("Average Realty Price")

home_var <- c(3:11,292)
home_corr <- cor(data.combined[1:30471,][home_var], use = "complete.obs")
corrplot(home_corr, method = "number")

data.combined[1:30471,] %>% 
  filter(full_sq<2000) %>%
  ggplot(aes(x = full_sq, y = price_doc))+
  geom_point(color = 'red', alpha = 0.5) +
  labs(x = "Total Area in the house", y="price of the Realty", title = "Price by area")
  
#From the correlation plot we can see that the number of living rooms and 
#full_sq are the variables that have the highest correlation with each other.
summary(data.combined[1:30471,]$full_sq)
summary(data.combined[1:30471,]$num_room)
table(data.combined[1:30471,]$num_room)

#live_sq should always be smaller than the full_sq.
sum(data.combined[1:30471,]$life_sq > data.combined[1:30471,]$full_sq, na.rm = TRUE)
greater.life_sq <- data.combined[1:30471,] %>% filter(life_sq > full_sq)

#2 Examining the variable number of rooms
ggplot(data.combined[1:30471,], aes(num_room)) + 
  geom_histogram(fill = 'red', bins = 20)+
  ggtitle("Distribution of rooms")

#3. Examining the variable cafe_count_3000_price_2500.
summary(data.combined[1:30471,]$cafe_count_3000_price_2500)
table(data.combined[1:30471,]$cafe_count_3000_price_2500)
sum(is.na(data.combined[1:30471,]$cafe_count_3000_price_2500))
ggplot(data.combined[1:30471,], aes(cafe_count_3000_price_2500)) + 
  geom_histogram(fill = 'red', bins = 10)+
  ggtitle("Distribution of Cafe")
df1 <- data.combined[c("price_doc","cafe_count_3000_price_2500")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$cafe_count_3000_price_2500), FUN = mean,na.rm = TRUE)
colnames(day.agg) <- c("cafe_count_3000_price_2500","avg_price")
ggplot(data = day.agg, aes(x = cafe_count_3000_price_2500, y = avg_price, group = 1))+geom_line()+geom_point()


#4. Plotting the number of rooms against the average price.
df1 <- data.combined[c("price_doc","num_room")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$num_room), FUN = mean,na.rm = TRUE)
colnames(day.agg) <- c("num_room","avg_price")
ggplot(data = day.agg, aes(x = num_room, y = avg_price, group = 1))+geom_line()+geom_point()

#5. The relative measure of rooms to the size of the house.
data.combined$relative_room_size <- data.combined$life_sq/data.combined$num_room
df1 <- data.combined[c("price_doc","relative_room_size")]
df1 <- df1[1:30471,]
day.agg <- aggregate(df1$price_doc, by = list(df1$relative_room_size), FUN = mean)
colnames(day.agg) <- c("relative_room_size","avg_price")
ggplot(data = day.agg, aes(x = relative_room_size, y = avg_price, group = 1)) + geom_line()

#6. Examining the build year.
table(data.combined$build_year)
#There are a lot of values that do not make sense. We have to clean those values.
data.combined$build_year[which(data.combined$build_year == 20052009)] <- 2007
data.combined$build_year[which(data.combined$build_year < 1961)] <- NA
data.combined$build_year[which(data.combined$build_year == 4965)] <- NA
data.combined$build_year[which(data.combined$build_year > 2016)] <- NA

#Imputing the values of build_year using mice().
library(mice)
data.combined$years_after_survey <- 2017 - data.combined$year
df2 <- data.combined[c("build_year","price_doc","month","years_after_survey")]
df2 <- df2[1:30471,]
m <- cor(df2)
corrplot(m,method = "circle")
#df2$timestamp <- as.numeric(df2$timestamp)

temp_build_year <- mice(df2, m=10, method = "pmm", seed = 500)
completed_data <- complete(temp_build_year,1)
data.combined[1:30471,]$build_year <- completed_data$build_year

df2 <- data.combined[c("build_year","price_doc","month","years_after_survey")]
df2 <- df2
m <- cor(df2)
corrplot(m,method = "circle")
#df2$timestamp <- as.numeric(df2$timestamp)

temp_build_year <- mice(df2, m=10, method = "pmm", seed = 500)
completed_data <- complete(temp_build_year,1)
data.combined$build_year <- completed_data$build_year

#Creating a variable called age of the property.
data.combined$age_property <- 2017 - data.combined$build_year
table(data.combined$age_property)

#Finding the medain and average price for the different build years.
df1 <- data.combined[1:30471,][c("price_doc","age_property")]
age.agg <- aggregate(df1$price_doc, by = list(df1$age_property), FUN = mean)
colnames(age.agg) <- c("age","avg_price")
ggplot(age.agg, aes(x = age, y=avg_price, group = 1))+geom_line()+geom_point(aes(color = "red"))


#Properties within the range of 10-20 years cost the most
#As the age increases the average price drops.
