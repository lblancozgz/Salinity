library(survival)
library(survminer)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(readxl)
library(lubridate)
library(fitdistrplus)
library(viridis)
library(ggfortify)
library(utils)
library(ggsignif)
library(RVAideMemoire)
library(ggpubr)
library(rstatix)
#uploading data
setwd("C:\\Users\\lblan\\OneDrive\\Escritorio\\ThirdCap")
df <- read_excel("C:/Users/lblan/OneDrive/Escritorio/ThirdCap/Water_salinity_data.xlsx", 
                   col_types = c("numeric", "numeric", "text", 
                                 "numeric", "date", "numeric", "date", 
                                 "numeric", "date", "numeric", "date", 
                                 "numeric", "date", "numeric", "date", 
                                 "numeric", "date", "text", "numeric", 
                                 "text", "numeric", "numeric"))

#transform variables which are factors
df$ID = factor(df$ID) # convert to nominal factor
df$sex = factor(df$sex)
df$GROUP = factor(df$GROUP)
df$GROUP2 = as.numeric(df$GROUP2) 


hist(df$total_lived) 
hist(df$days_adult)

#we separate the dataframe in two; one control will be distilled (df1) and the
#other will be Bottled
dfbottled <- subset(df, !GROUP == "Distilled")
dfdistilled <- subset(df, !GROUP == "Bottled")

##Order
dfbottled<- dfbottled %>% arrange(GROUP2)
dfdistilled<- dfdistilled %>% arrange(GROUP2)

##we create an L1 to L4 number of days lived as we want to measure survival during
##immature stage
dfdistilled$days_lived_im = rowSums(dfdistilled[ , c(6,8,10,12,14)], na.rm = T)
dfbottled$days_lived_im = rowSums(dfbottled[ , c(6,8,10,12,14)], na.rm = T)


#COX with DISTILLED

#immature stage -------------------------------
m0 <- coxph(Surv(days_lived_im, censored_im) ~ GROUP2, data = dfdistilled)
summary(m0)
cox.zph(m0)
ggcoxzph(cox.zph(m0))
ggcoxdiagnostics(m0, type = "dfbetas")

m0 <- coxph(Surv(total_lived, censored_ad) ~ GROUP2, data = dfdistilled)
summary(m0)
cox.zph(m0)
ggcoxzph(cox.zph(m0))
ggcoxdiagnostics(m0, type = "dfbetas")

#to obtain hr, instead of GROUP2, put factor(GROUP)
# HRdf<- as.data.frame(m2$coefficients)
# HRdf$group <- c("0.5", "1", "10", "12", "15", "2", "20", "30", "5", "Bottled", "Distilled")
# 
# plot <- ggplot(data=HRdf, aes(x=group, y= m2$coefficients)) + 
#   geom_point() 


m1 <- coxph(Surv(days_adult, censored_ad) ~ GROUP2, data = dfdistilled)
summary(m1)
cox.zph(m1)
ggcoxzph(cox.zph(m1))
ggcoxdiagnostics(m1, type = "dfbetas")

plot(cox.zph(m1))

m1_sex <- coxph(Surv(days_adult, censored_ad) ~  GROUP2 + sex, data = dfdistilled)
summary(m1_sex)
cox.zph(m1_sex)
ggcoxzph(cox.zph(m1_sex))
ggcoxdiagnostics(m1_sex, type = "dfbetas")


# m1_sexf <- coxph(Surv(days_adult, censored_ad) ~  GROUP2*factor(sex) +factor(sex), data = dfdistilled)
# summary(m1_sexf)
# cox.zph(m1_sexf)
# plot(cox.zph(m1_sexf))




#COx distilled vs bottled
df2 <- read_excel("Water_salinity_data.xlsx", 
                 col_types = c("numeric","text", "text", "text","date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "text", "numeric", "text", "numeric"))
control1 <- subset(dfdistilled, GROUP == "Distilled")
control2 <- subset(dfbottled, GROUP == "Bottled")

control <- rbind(control1, control2)
control$GROUP <- as.character(control$GROUP)

control$GROUP <- as.factor(control$GROUP)
control$sex <- as.factor(control$sex)
str(control$GROUP)
#comparing survival with immature stages
cox_control <- coxph(Surv(days_lived_im, censored_im) ~ GROUP, data = control)
summary(cox_control)
plot(cox.zph(cox_control))
cox.zph(cox_control)
ggcoxzph(cox.zph(cox_control))
ggcoxdiagnostics(cox_control, type = "dfbetas")
str(control$GROUP)

cox_control <- coxph(Surv(total_lived, censored_ad) ~ GROUP, data = control)
summary(cox_control)
plot(cox.zph(cox_control))
cox.zph(cox_control)
ggcoxzph(cox.zph(cox_control))
ggcoxdiagnostics(cox_control, type = "dfbetas")
str(control$GROUP)

cox_control_ad <- coxph(Surv(days_adult, censored_ad) ~  GROUP, data = control)
summary(cox_control_ad)
plot(cox.zph(cox_control_ad))
cox.zph(cox_control_ad)
ggcoxzph(cox.zph(cox_control_ad))
ggcoxdiagnostics(cox_control_ad, type = "dfbetas")


cox_control_ad_sex <- coxph(Surv(days_adult, censored_ad) ~  GROUP + factor(sex), data = control)
summary(cox_control_ad_sex)
plot(cox.zph(cox_control_ad_sex))
ggcoxzph(cox.zph(cox_control_ad_sex))
ggcoxdiagnostics(cox_control_ad_sex, type = "dfbetas")


par(mfrow=c(2,2))
plot(survfit(m0, newdata=data.frame(GROUP2=3)), main = "Prediction of survival with 3ppm",
             conf.int=T, xlab = "Longevity (days)") 
plot(survfit(m0, newdata=data.frame(GROUP2=6)), main = "Prediction of survival with 6ppm",
             conf.int=T, xlab = "Longevity (days)") 
plot(survfit(m0, newdata=data.frame(GROUP2=9)), main = "Prediction of survival with 9ppm",
             conf.int=T, xlab = "Longevity (days)") 
plot(survfit(m0, newdata=data.frame(GROUP2=11)), main = "Prediction of survival with 11ppm",
             conf.int=T, xlab = "Longevity (days)") 

