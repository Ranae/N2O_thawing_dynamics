library(tidyverse)
library(agricolae)


flux<-read_csv("data/mesocosm_fluxes.csv")
head(flux)
colnames(flux)<-c("hour", "date", "core", "N", "ins")
flux<-filter(flux, N != "NA", hour > 59)  


dif<- function(df) {
  mod <- aov(df$N ~ df$ins)
  tuk <- TukeyHSD(x=mod, 'df$ins', conf.level=.95)
  pvalue <- tuk[[1]][,4]
  pvalue
}  

sas<- function(df) {
  mod <- lm(df$N ~ df$ins)
  tuk <- HSD.test(mod, 'df$ins')
  letter <- tuk$groups
  letter
}  

ttest<- function(df) {
  mod <- t.test(df$N ~ df$ins)
  mod[[3]]
}

now<-flux%>%
  group_by(hour)%>%
  do(pvalue=dif(.))

then<-flux%>%
  group_by(hour)%>%
  do(pvalue=sas(.))

try<-flux%>%
  group_by(hour)%>%
  do(pvalue=ttest(.))

best<-filter(flux, hour == 144)

ggplot(best, aes(x=ins, y=N))+
  geom_boxplot()

best_mod<-lm(N ~ ins, best)
