# install packages
install.packages('dplyr')
# load the package into r environment
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)
#read conversion data set into R
cd= read.csv(file='C:/Users/38933/Desktop/datasc/conversion_data.csv', head=TRUE,sep=',')
head(cd)
#check the structure of the data
str(cd)
summary(cd)
#the site is probably a US site, although it does have a large Chinese user base as well
#user base is pretty young conversion rate at around 3% is industry standard. It makes sense. 
#everything seems to make sense here except for max age 123 yrs
sort(unique(cd$age),decreasing=TRUE)
#Those 123 and 111 values seem unrealistic. How many users are we talking about
subset(cd,age>79)
#It is just 2 users! In this case, we can remove them, nothing will change. 
data=subset(cd,age<80)

#EDA of country vs conversion_rate
data_country = data %>%   
  group_by(country) %>%   
  summarise(conversion_rate = mean(converted))  
ggplot(data=data_country, aes(x=country, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill = country))
#Here it clearly looks like Chinese convert at a much lower rate than other countries


#EDA of total_page_visited vs conversion_rate
data_page=data %>%
  group_by(total_pages_visited) %>%
  summarise(conversion_rate = mean(converted))
ggplot(data=data_page, aes(x=total_pages_visited, y=conversion_rate))+
  geom_bar(stat = "identity",aes(fill=total_pages_visited))

qplot(total_pages_visited, conversion_rate, data=data_page, geom="line")
#Definitely spending more time on the site implies higher probability of conversion

#EDA of source vs conversion_rate
data_source=data %>% 
  group_by(source) %>%
  summarise(conversion_rate=mean(converted))
ggplot(data = data_source,aes(x=source,y=conversion_rate))+
  geom_bar(stat='identity',aes(fill=source))
#there are not signifficant difference between them

#EDA of new_user vs conversion_rate 
data_new_user=data %>%
  group_by(new_user) %>%
  summarise(conversion_rate=mean(converted))
ggplot(data = data_new_user,aes(x=new_user,y=conversion_rate))+
  geom_bar(stat='identity',aes(fill=new_user))
#old users have significant higher conversion rate than new_user

#machine learning
#build a model to predict conversion_rate
# let's make the class a factor 
cd$converted=as.factor(cd$converted)
cd$new_user=as.factor(cd$new_user)
# Shorter name, easier to plot
levels(cd$country)[levels(cd$country)=='Germany']='DE'
#Create test/training set with a standard 66% split
train_sample=sample(nrow(cd),size = nrow(cd)*0.66)
train_data=cd[train_sample,]
test_data=cd[-train_sample,]
rf=randomForest(y=train_data$converted,x=train_data[,-ncol(train_data)],
                ytest = test_data$converted,xtest = test_data[,-ncol(test_data)],
                ntree = 100,mtry=3,keep.forest = TRUE)
rf
varImpPlot(rf,type = 2)
rf=randomForest(y=train_data$converted,x=train_data[,-c(5,ncol(train_data))],
                ytest = test_data$converted,xtest=test_data[,-c(5,ncol(train_data))],ntree = 100,mtry=3,keep.forest = TRUE,classwt = c(0.7,0.3))
rf
varImpPlot(rf,type=2)

#partial dependency
op <- par(mfrow=c(2, 2))
partialPlot(rf,train_data,country,1)
partialPlot(rf,train_data,source,1)
partialPlot(rf,train_data,age,1)
partialPlot(rf,train_data,new_user,1)

#build a simple decision trr and check the 2 or 3 most important segments
tree = rpart(data$converted ~ ., data[, -c(5,ncol(data))], 
             control = rpart.control(maxdepth = 3), 
             parms = list(prior = c(0.7, 0.3))  ) 
tree
 
#Some conclusions and suggestions: 
#1. The site is working very well for young users. Definitely let's tell marketing to
#advertise and use marketing channel which are more likely to reach young people. 
#2. The site is working very well for Germany in terms of conversion. But the summary 
#showed that there are few Germans coming to the site: way less than UK, despite a larger population. 
#Again, marketing should get more Germans. Big opportunity. 
#3. Users with old accounts do much better. Targeted emails with offers to bring them back to the
#site could be a good idea to try.
#4. Something is wrong with the Chinese version of the site. 
#It is either poorly translated, doesn't fit the local culture, some payment issue or maybe it is 
#just in English! Given how many users are based in China, fixing this should be a top priority.
#Huge opportunity. 




