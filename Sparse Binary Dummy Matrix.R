##Q1
data <- read.csv("columbia_data_set.csv.csv.csv", stringsAsFactors=TRUE)
View(data)
summary(data)
str(data)
data$hour <- as.factor(data$hour)
data$browser.id <- as.factor(data$browser.id)
a <- sort(table(data$site.id), decreasing=TRUE)/nrow(data)
a <- subset(a, cumsum(a)<0.99)
data$site.id[!(is.element(data$site.id,names(a)))] <- "Other"
data$site.id <- as.factor(data$site.id)
str(data$site.id)
require(Matrix)
length(names(a))
x <- model.matrix(~ -1 + impression.id + user.id + day.of.week + 
                    hour + site.id + ad.size + browser.id + state, data, 
                  contrasts.arg=list(day.of.week=contrasts(data$day.of.week, contrasts=F), 
                                     hour=contrasts(data$hour, contrasts=F),
                                     site.id=contrasts(data$site.id, contrasts=F),
                                     ad.size=contrasts(data$ad.size, contrasts=F),
                                     browser.id=contrasts(data$browser.id, contrasts=F),
                                     state=contrasts(data$state, contrasts=F)))
                                                                                                                                                                                    
x <- as.data.frame(x)
names(x)
View(x)
str(x)
dim(x)


