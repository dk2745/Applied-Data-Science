#Problem 1

#Data Munging on Trans.csv
  #Breaking date from 1 column to 7 columns based on width
Trans <- read.fwf("Trans.csv", widths=c(11,10,56,17,3,5,7), skip=1, col.names=c("LastName", "Date", "Email", "CreditCardNo", "Age", "Zip", "Amount"))
  #Converting Amount to a numeric
Trans$Amount <- substr(Trans$Amount, 2, 100000L)
Trans$Amount <- as.numeric(Trans$Amount)
  #Converting to Date Format for Time-Series analysis
Trans$Date <- format(as.Date(Trans$Date,"%d/%m/%Y"), "%d/%m/%y")
Trans$Date <-as.Date(Trans$Date, format="%d/%m/%y")
  #Creating a new variable AgeGroup
Trans$AgeGroup <- cut(Trans$Age, breaks=c(0,17,24,34,44,54,64,100), labels=c("<18","18-24","25-34","35-44","45-54","55-64",">65"))
  #Creating a new variable to segment users on spending behavior
Trans$SpendCat <- cut(Trans$Amount, breaks=c(0,100,200,300,400,500), labels=c("0-100","100-200","200-300","300-400","400-500"))

#Summary Statistics
summary(Trans$Age)
summary(Trans$AgeGroup)
summary(Trans$Amount)
summary(Trans$SpendCat)
summary(Trans$Date)
cor(Trans$Age, Trans$Amount)
p <- ggplot(Trans, aes(x=Date,y=Amount,group=2)) + geom_dotplot()
p + facet_grid(AgeGroup ~ SpendCat,scales="free_x") + xlab("Monthly Data (from Jan 2013 to Jan 2015)") + ylab("Relative Number of Transactions ") + labs(title = "Spending Pattern across Age Groups & Years") + theme_economist() + theme(axis.ticks=element_blank())

#Multivariate Analysis
Rreg <- lm(Trans$Amount~Trans$Age)
summary(Rreg)
var(Trans$Amount,Trans$Age)
cov(Trans$Amount,Trans$Age)
table(Trans$AgeGroup)

#Exploratory Visual Analysis
  #Choosing one Zip and analysing it's statistics
Zip=subset(Trans, Trans$Zip==sample(Trans$Zip,1))
ggplot(Zip,aes(x=AgeGroup)) + geom_histogram() + theme_economist()
pairs(~Age+Amount+SpendCat+AgeGroup,data=Zip)
  #Distribution of Amounts for 6 User AgeGroup
ggplot(Zip, aes(x=AgeGroup, y=Amount))+ geom_violin() + geom_jitter() + xlab("Age Groups") + ylab("Amount Spent ") + labs(title = "Spending Pattern for a Sample Zip across Age Groups") + theme_economist()
  #Segmentation scatter based on Spending Category and Age Groups
ggplot(Trans, aes(x=AgeGroup, y=SpendCat))+ geom_violin() + geom_jitter() + theme_economist()
  #Plotting Zip vs Amount for Trans
setkey(Trans.data,Zip)
Zip.Amount = Trans.data[, lapply(.SD[,list(Amount)], sum), by = Zip]
Zip.Amount <- Zip.Amount[order(Amount),]
Zip.Amount <- head(Zip.Amount,10)
Zip.Amount
summary(Zip.Amount$Amount)
  #Plotting AgeGroup vs Amount for Trans
AgeGroup.Amount = Trans.data[, lapply(.SD[,list(Amount)], sum), by = AgeGroup]
AgeGroup.Amount <- AgeGroup.Amount[order(Amount),]
AgeGroup.Amount
  #Plotting Time-Series for Seasonal Spending Patterns
Trans$Month <- format(Trans$Date, "%m")
Trans.data <- data.table(Trans)
Month.Amount <- Trans.data[, lapply(.SD[,list(Amount)], sum), by = Month]
Month.Amount <- Month.Amount[order(Month),]
Month.Amount$Month <- as.factor(Month.Amount$Month)
ggplot(Month.Amount, aes(x=Month,y=Amount, group=1)) + geom_line() + geom_point() + xlab("Months (Data from Jan 2013 to Jan 2015") + ylab("Amount Spent ") + labs(title = "Trend of Spending over the 12 Months") + theme_economist()


#Problem 2

#Data Munging on Traffic.csv
  #Removing Rows with NA elements
sum(is.na(Traffic)) #Returned 399
Traffic <- na.omit(Traffic)
Traffic$AADT.Year <- as.numeric(as.character(Traffic$AADT.Year))
Traffic$AADT..Vehicles <- as.numeric(Traffic$AADT..Vehicles)
  #Times Series Segmentation
Traffic$Years <- cut(Traffic$AADT.Year, breaks=c(0,1984,1991,1998,2005,2012), labels=c("<1984","1984-91","1991-98","1998-05","2005-12"))
Traffic$Years <- as.factor(Traffic$Years)
Traffic$GIS.Code <- as.factor(Traffic$GIS.Code)
Traffic$Begin.Milepoint <- as.factor(Traffic$Begin.Milepoint)
Traffic$End.Milepoint <- as.factor(Traffic$End.Milepoint)

#Exploratory Visual Analysis
  #Time Series Analysis of Road Types
ggplot(subset(Traffic,Traffic$AADT..Vehicles<300000), aes(x=Years, y=AADT..Vehicles)) + geom_jitter() + xlab("Months (Data from 1977 to 2011") + ylab("Number of Vehicles") + labs(title = "Time Series of Vehicles in AADT Data") + theme_economist()
q <- ggplot(subset(Traffic,Traffic$AADT..Vehicles<300000), aes(x=Years, y=AADT..Vehicles))+ geom_point() + scale_x_discrete("Years (from 1997 to 2011)") + scale_y_continuous() + xlab("Years (from 1997 to 2011)") + ylab("Number of Vehicles") + labs(title = "Time Series for Road Types") + theme_economist()
q + facet_grid(~Roadway.Type)
  #Choosing one GIS.Code and analyzing its Statistics
Traffic$GIS.Code <- as.numeric(as.character(Traffic$GIS.Code))
Code=subset(Traffic, Traffic$GIS.Code==sample(Traffic$GIS.Code,1))
ggplot(Code,aes(x=Years)) + geom_histogram() + theme_economist()
ggplot(Code, aes(x=Years, y=AADT..Vehicles))+ geom_boxplot() + geom_jitter() + xlab("Years (from 1997 to 2011)") + ylab("Number of Vehicles") + labs(title = "Average Annual Daily Traffic for a Sample Roadway") + theme_economist()

