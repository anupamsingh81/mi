#Simulating data
#http://clayford.github.io/dwir/dwr_12_generating_data.html
library(tidyverse)

mi= tibble(
  age=rnorm(110,52,16),
  sex= sample(c("male","female"),110,replace = TRUE,prob=c(0.75,0.25)),
  Diabetic= sample(c("Diabetic","Non_Diabetic"),110,replace = TRUE,prob=c(0.30,0.70)),
  Hypertensive= sample(c("Hypertensive","Non_Hypertensive"),110,replace = TRUE,prob=c(0.6,0.4)),
  Smoker= sample(c("smoker","Non_smoker"),110,replace = TRUE,prob=c(0.40,0.60)),
  SBP=rnorm(110,120,20),
  HR=rnorm(110,72,15),
  time_to_treat= rnorm(110,180,60),
  therapy= sample(c("streptokinase","tenecteplase"),110,replace = TRUE,prob=c(0.7,0.3))
,
  location= sample(c("Anterior","Inferior","Other"),110,replace = TRUE,prob=c(0.55,0.35,0.10)))

glimpse(mi)
mi=mi %>% mutate_if(is.numeric,as.integer)

summary(factor(mi$therapy))

mi=mi %>% mutate(hr=case_when(location=="Inferior"~rnorm(n(),60,10),
                            TRUE~rnorm(n(),72,5))) %>% mutate(hr=as.integer(hr)) # use n()

mi %>% group_by(location,therapy) %>% count()
#https://stackoverflow.com/questions/31878389/dplyr-mutate-using-rbinom-do-not-return-random-numbers

mi=mi %>% mutate(resolution= case_when(
  location=="Anterior" &therapy=="streptokinase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.40,0.4,0.2)),
  location=="Inferior"& therapy=="streptokinase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.60,0.3,0.1)),
  location=="Anterior" & therapy=="tenecteplase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.5,0.4,0.1)),
  location=="Inferior" &therapy=="tenecteplase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.65,0.25,0.1)),
  location=="Other" & therapy=="tenecteplase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.5,0.45,0.05)),
  location=="Other" & therapy=="streptokinase" ~ sample(c("complete","patial","none"),n(),replace=TRUE,prob=c(0.45,0.35,0.2))
)) 

mi$thrombolysis = ifelse(mi$resolution=="complete",2,1)


  mi = mi %>% mutate(time=case_when(
    resolution=="complete" &therapy=="streptokinase" ~ sample(c(60,90,180),n(),replace=TRUE,prob=c(0.3,0.2,0.5)),
    resolution=="complete" &therapy=="tenecteplase" ~ sample(c(60,90,180),n(),replace=TRUE,prob=c(0.5,0.3,0.2)),
    TRUE~240
  ))
  
  

  
ggplot(mi,aes(therapy,fill=resolution))+geom_bar(position="dodge")


ggplot(mi,aes(therapy,fill=resolution))+geom_bar(position="fill")


ggplot(mi,aes(location,fill=resolution))+geom_bar(position="fill")
  


TukeyHSD(aov(mi$hr~mi$location))

####survival

library(survival)
library(survminer)


fit <- survfit(Surv(time, thrombolysis) ~ therapy, data = mi)
print(fit)

# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table

ggsurvplot(fit,
           pval = FALSE, conf.int = FALSE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("green","blue"),break.time.by=30)+
  labs(y="Failure percent")


mi$resolution=fct_recode(mi$resolution,partial="patial")

mi$resolution= factor(mi$resolution,levels=c("complete","partial","none"))

gg

summary(fisher.test(mi$therapy,mi$resolution))

fisher.test(mi$therapy,mi$location)

  jj=  glm(as.factor(thrombolysis)~therapy+location,family="binomial",data=mi)
  summary(jj)
  
  
 mi= mi %>% 
    mutate(circadian=case_when(
      resolution=="complete" ~ sample(c("0-4","4-8","8-12","12-16","16-20","20-24"),n(),replace=TRUE,prob=c(0.1,0.12,0.11,0.17,0.3,0.2)),
      TRUE ~ sample(c("0-4","4-8","8-12","12-16","16-20","20-24"),n(),replace=TRUE,prob=c(0.17,0.15,0.3,0.15,0.13,0.1))
    ))
 
 
 mi = mi %>% mutate(onset=case_when(
   Diabetic=="Diabetic" ~ sample(c("0-4","4-8","8-12","12-16","16-20","20-24"),n(),replace=TRUE,prob=c(0.18,0.14,0.08,0.24,0.20,0.16)),
   TRUE ~ sample(c("0-4","4-8","8-12","12-16","16-20","20-24"),n(),replace=TRUE,prob=c(0.12,0.25,0.12,0.16,0.16,0.19))
 ))
   
 positions=c("0-4","4-8","8-12","12-16","16-20","20-24")
 ggplot(mi,aes(circadian,fill=resolution))+geom_bar(position="fill")  + scale_x_discrete(limits = positions)
 
 ggplot(mi,aes(circadian,fill=Diabetic))+geom_bar(position="fill")  + scale_x_discrete(limits = positions)
 
 
 ggplot(mi,aes(circadian,color=resolution))+geom_freqpoly() + scale_x_discrete(limits = positions)
 
 mi$resolution
 mi %>%mutate(resolution=as.character(resolution)) %>%  group_by(circadian) %>% mutate(percentage=n())  %>%  group_by(resolution)%>% 
   transmute(percentage=percentage/sum(percentage))
 
 mi1=mi %>%mutate(resolution=as.character(resolution)) %>%  count(circadian,resolution)   %>%  group_by(resolution)%>% 
   transmute(circadian,percentage=n/sum(n)*100) %>% arrange(circadian)
 
 
 ggplot(mi1,aes(x=circadian,y=percentage,color=resolution,group=resolution))+geom_line() + scale_x_discrete(limits = positions)
 
 
 