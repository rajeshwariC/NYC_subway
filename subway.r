subway<-read.csv(file.choose(),header=T)
head(subway)
summary(subway)

variable.names(subway)
str(subway)
mean(table(subway$rain))
table(subway$conds)
table(subway$ENTRIESn_hourly,subway$rain)

#section 1
#Statisticalo test
by(subway$ENTRIESn_hourly,subway$rain,summary)
will<-wilcox.test(formula=ENTRIESn_hourly~rain,data=subway)
par(mfrow=c(1,2))
rainy_days<-subset(subway,rain==1)
head(rainy_days)
non_rainy_days<-subset(subway,rain==0)
head(non_rainy_days)

h1<-hist(subway$ENTRIESn_hourly[subway$rain==0],col="red")
h2<-hist(subway$ENTRIESn_hourly[subway$rain==1],col="blue")

#section2
#Linear Regression
model1<-lm(ENTRIESn_hourly~UNIT+hour+weekday+rain+tempi+fog,data=subway)
summary(model1)
library(ggplot2)
ggplot(aes(x=hour,y=ENTRIESn_hourly),data=subway)+
  geom_point()
attach(rainy_days)
ggplot(subway,aes(x=ENTRIESn_hourly,fill=rain))+
  geom_histogram(binwidth=0.05,position="dodge")+
  labs(x="hours", y="rain", title="histogram")

#section3
#Visualization
range(subway$ENTRIESn_hourly)

ggplot(aes(x=ENTRIESn_hourly,fill=rain),data=subway)+
  geom_histogram(binwidth = 0.05)+coord_cartesian(xlim=c(0,6000))
attach(sub1)

ggplot(subway, aes(x=ENTRIESn_hourly, fill=rain))+
  geom_histogram(binwidth = 0.01,position="dodge")+
  labs(x="hour",y="")

