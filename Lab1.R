# Note: replace default data frame name – cannot start with numbers! Munging has begun! (ugh)
EPI_data <- read.csv("epi2024results06022024.csv")
View(EPI_data)

attach(EPI_data) # sets the ‘default’ object 

EPI.new # prints out values EPI_data$EPI.new
NAs <- is.na(EPI.new) # records True values if the value is NA 
NAs
EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 

#Excercise 1
summary(EPI.new) # stats 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new) 
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.)) 
rug(EPI.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ")) 
rug(EPI.new)

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

#Excercise 2 
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#PAR
plot(ecdf(PAR.new), do.points=FALSE, verticals=TRUE) 
qqnorm(PAR.new); qqline(PAR.new) 
qqplot(rnorm(250), PAR.new, xlab = "Q-Q plot for norm dsn") 
qqline(PAR.new)
qqplot(rt(250, df = 5), PAR.new, xlab = "Q-Q plot for t dsn") 
qqline(PAR.new)

#TCG
plot(ecdf(TCG.new), do.points=FALSE, verticals=TRUE) 
qqnorm(TCG.new); qqline(TCG.new) 
qqplot(rnorm(250), TCG.new, xlab = "Q-Q plot for norm dsn") 
qqline(TCG.new)
qqplot(rt(250, df = 5), TCG.new, xlab = "Q-Q plot for t dsn") 
qqline(TCG.new)
