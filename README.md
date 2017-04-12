# NYC_subway
# Analyzing the subway dataset
subway<-read.csv(file.choose(),header=T)
head(subway)
summary(subway)
variable.names(subway)
str(subway)
summary(subway$ENTRIESn_hourly)
summary(subway$EXITSn_hourly)

#statiscal test(two sample t-test)
library(dplyr)
subway<-subway %>%
  mutate(new_rain_var = ifelse(precipi == 0,0,1))
t.test(filter(subway,precipi==0) %>% select (net_entries), 
       filter(subway,precipi>0) %>% select (net_entries),
       var.equal=FALSE)

list_time<-subway %>%
  select(TIMEn) %>%
  distinct(TIMEn)
list_time

list_time<-as.character(list_time$TIMEn)

weekday_test<-lapply(list_time, function(x) {
  t.test(filter(subway, TIMEn==x & weekday==1 & precipi==0) %>%
           select(net_entries), 
         filter(subway, TIMEn==x & weekday==1 & precipi>0) %>%
           select(net_entries))
})

p_val<-no_rain_mean <-rain_mean <-c()
for(m in 1:length(weekday_test)){
  no_rain_mean <- c(no_rain_mean,weekday_test[[m]]$estimate[1])
  rain_mean<-c(rain_mean,weekday_test[[m]]$estimate[2])
  p_val<-round(c(p_val,weekday_test[[m]]$p.value),3)
}

a<-data.table(list_time, p_val, no_rain_mean, rain_mean)


no_rain<-subway %>%
  filter(weekday == 0 & weekday == 0) %>%
  select(net_entries)


rain<-subway %>%
  filter(precipi > 0 & weekday == 0) %>%
  select(net_entries)

rain_data<-subway %>%
  mutate(new_rain_var = ifelse(precipi > 0,"Rain","No Rain")) %>%
  select(new_rain_var,net_entries)

rain_data_mean<-rain_data %>%
  group_by(new_rain_var)%>%
  summarize(mean_rain=mean(net_entries))

ggplot(rain_data, aes(x=net_entries, fill=new_rain_var))+
  geom_density(alpha=0.7)+
  xlim(c(-4000,4000))+
  ggtitle("density of rain versus no rain")+
  geom_vline(data=rain_data_mean, aes(xintercept=mean_rain, color=new_rain_var),
             linetype="dashed",size=1)

#Linear Regression

subway$hour<-factor(subway$hour)
subway$weekday.f<-factor(subway$weekday)

model1<-lm(ENTRIESn_hourly~hour+weekday.f+rain+tempi+fog,data=subway)
summary(model1)
plot(model1)

#Visualization

qplot(ENTRIESn_hourly,data=subway,fill=I("skyblue"),xlim=c(0,10000),geom="density",main="Hourly entries",xlab="entries per turnstile")

qplot(EXITSn_hourly,data=subway,fill=I("skyblue"),xlim=c(0,10000),geom="density",main="Hourly exits",xlab="exits per turnstile")

subway$net_entries<-subway$ENTRIESn_hourly-subway$EXITSn_hourly

qplot(net_entries,data=subway,fill=I("skyblue"),xlim=c(-7000,7000),geom="density",main="Net entries",xlab="Net entries per turnstile")

library(ggplot2)

ggplot(rain_data, aes(x=net_entries, fill=new_rain_var))+
  geom_histogram(position="dodge",alpha=1)+
  xlim(c(0,4000))+
  xlab("Hourly Enties")+ylab("Frequency")+
  ggtitle("Hourly entries histogram :rain vs. no rain")+
  guides(fill=guide_legend("weather condition"))
