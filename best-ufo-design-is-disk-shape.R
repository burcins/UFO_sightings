#Hi all! This is my second Kaggle script, I just aim to practicing. Your comments are appreciated. 
library(tidyverse)
library(lubridate)
library(rpart)
library(caret)
scrubbed <- read_csv('../input/ufo-sightings/scrubbed.csv')
glimpse(scrubbed)
scrubbed <- setNames(scrubbed, c("datetime", "city","state","country","shape","duration_seconds", "duration_hours_min","comments", "date_posted", "latitude", "longitude"))
colnames(scrubbed)
scrubbed$datetime <- mdy_hm(scrubbed$datetime)
scrubbed$date_posted <- mdy(scrubbed$date_posted)
scrubbed$comments <- as.character(tolower(scrubbed$comments))
scrubbed$shape <- as.factor(scrubbed$shape)
glimpse(scrubbed)
scrubbed$state <- as.factor(scrubbed$state)
scrubbed$city <- as.factor(scrubbed$city)
scrubbed$country <- as.factor(scrubbed$country)
table(is.na(scrubbed$country))
table(is.na(scrubbed$state))
scrubbed1 <- scrubbed%>% separate(datetime, c("date", "time"), sep = " ")
scrubbed1 <- scrubbed1 %>% separate(date, c("year", "month", "day"), sep = "-")
min(scrubbed1$year)
max(scrubbed1$year)
table(scrubbed1$year)
ggplot(scrubbed1, aes(year)) + geom_bar(position= "dodge") + ggtitle("UFO sightings by year") + scale_x_discrete(breaks=seq(1906,2014,9))
# Let’s check by country base 
ggplot(scrubbed1, aes(year)) + geom_bar() + ggtitle("UFO sightings by year") + scale_x_discrete(breaks=seq(1906,2014,9)) + facet_grid(country~.)
# It seems that NA countries is a big part of data, so I try to handle it for better analysis
na.country <- which(is.na(scrubbed1$country))
scrubbed2 <- scrubbed1[-na.country,]
scrubbed.na <- scrubbed1[na.country,]
# Firstly I separate them, let’s see if it works
sum(is.na(scrubbed2$country))
#It is time to create a classification tree model for predicting, but I check in my own computer first, if I split first 75% of data as a train data and the remaining part as a train data directly, its predictions and model accuracy is same as just filling all NA countries with US. I thought it’s because of data order type, so I split train and test set randomly. 
set.seed(2)
sample_rows <- sample(nrow(scrubbed2), (nrow(scrubbed2)*0.75))
scrubbed2_train <- scrubbed2[sample_rows, ]
scrubbed2_test <- scrubbed2[-sample_rows, ]
model <- rpart(country ~ latitude + longitude, scrubbed2_train, method = "class")
scrubbed2_test$pred <- predict(model, scrubbed2_test, type = "class")
confusionMatrix(scrubbed2_test$pred, scrubbed2_test$country)
#Great, our accuracy of more than %99 is fine enough, it is time to implement this model to NA data,
scrubbed.na2 <- scrubbed.na[, - 7]
glimpse(scrubbed.na2)
scrubbed.na2$country <- predict(model, scrubbed.na2, type="class")
glimpse(scrubbed.na2)
# Before binding data we should change the column order, because recreated “country” column lost its position
scrubbed.na2 <- scrubbed.na2[, c(1,2,3,4,5,6,14,7,8,9,10,11,12,13)]
# Now it is time to unite our data, check UFO sights by country again
scrubbed_last <- rbind(scrubbed2, scrubbed.na2)
glimpse(scrubbed_last)
summary(scrubbed_last)
ggplot(scrubbed_last, aes(year)) + geom_bar() + ggtitle("UFO sightings by year among countries") + scale_x_discrete(breaks=seq(1906,2014,9)) + facet_grid(country~.)
# It seems that North America is the best place to observe UFOs, but it would be interesting to review by shape. I wonder two things about shapes, if shapes change by countries and/or year.
ggplot(scrubbed_last, aes(shape, fill=shape)) + geom_bar() + ggtitle("UFO sightings by shape among countries") + facet_grid(country~.)
table(scrubbed_last$shape, scrubbed_last$country)
ggplot(scrubbed_last, aes(year,shape, color=shape)) + geom_jitter() + ggtitle("UFO sightings by year&shape") + scale_x_discrete(breaks=seq(1906,2014,9))
# It seems that country has not big effect on shape and most popular one is light. On the other hand by the aliens point of view, disk shaped UFO’s are the oldest models, which are observed from 1910 but still popular today. 
#Thank you.
