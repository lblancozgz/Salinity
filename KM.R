library(survival)
library(survminer)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(readxl)
library(lubridate)
library(fitdistrplus)
library(viridis)
library(ggfortify)
library(utils)
library(RColorBrewer)
library(ggsignif)
library(RVAideMemoire)
library(ggpubr)
library(rio) # for reading the data
library(tidyverse) # need for the functions as well
library(janitor) 
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
df$GROUP2 = factor(df$GROUP2) 
#we create a second column "GROUP" which will 
#be a factor, as we will analyze the different salinity groups as continuous and 
#categorical

hist(df$total_lived) 
hist(df$days_adult)

################################################################3
#Fit survival with Kaplan-Meier method
##Total of days lived (immature AND mature stages)
neworder <- c("0.2 ppt", "0.5 ppt", "1 ppt", "2 ppt", "5 ppt",
              "10 ppt", "12 ppt", "15 ppt", "20 ppt", "30 ppt",
              "Bottled", "Distilled")


library(plyr)  ## or dplyr (transform -> mutate)

df2 <- arrange(transform(df, GROUP=factor(GROUP,levels=neworder)),GROUP)


df <- df %>% 
  dplyr::rename(Salinity = "GROUP")
df$L1PUPA = rowSums(df[ , c(6,8,10,12,14)])
df$L1PUPA[is.na(df$L1PUPA)] <- 0

Fig1 <-  survfit(Surv(L1PUPA, censored_im) ~ Salinity, df, conf.type = "log-log") %>%
  ggsurvplot(
    conf.int = T,
    conf.int.alpha = 0.1,
    alpha = 0.8,
    xlab="Overall lifespan",
    surv.plot.heigh = 1.30,
    position = position_dodge(width=0.5),
    break.x.by = 5,
    font.tickslab = c(14),
    legend.title = "Salinity",
    legend.labs = c(
      "0.2 ppt", "0.5 ppt", "1 ppt", "2 ppt", "5 ppt", "10 ppt", "12 ppt", "15 ppt", 
      "20 ppt", "30 ppt","Bottled", "Distilled"),
    palette = c("darkblue","red", "#B1E71D","#C0007D","black","#338F71",
                          "gray60","gray60","gray60","gray60","skyblue","#F8B600"),
    font.y = c(14),
    font.x = c(14),
    pval = T,
    font.family = "Lato",
    ggtheme = theme_bw((base_size=15)))
Fig1$plot <- Fig1$plot+ 
  ggplot2::annotate("text", 
                    x = 23, y = 1, # x and y coordinates of the text
                    label = "a", size = 9)

Fig2 <-  survfit(Surv(days_adult, censored_ad) ~ Salinity, df, conf.type = "log-log") %>%
  ggsurvplot(
    conf.int = T,
    conf.int.alpha = 0.1,
    alpha = 0.8,
    xlab="Overall lifespan",
    ylab= NULL,
    surv.plot.heigh = 1.30,
    break.x.by = 20,
    pval = T,
    font.tickslab = c(14),
    legend.title = "Salinity",
    legend.labs = c(
      "0.2 ppt", "0.5 ppt", "1 ppt", "2 ppt", "5 ppt", "10 ppt", "Bottled", "Distilled"),
    palette = c("darkblue","red", "#B1E71D","#C0007D","black","#338F71","skyblue","#F8B600"),
                          font.y = c(14),
    font.x = c(14),
    font.family = "Lato",
    ggtheme = theme_bw((base_size=15)))
Fig2$plot <- Fig2$plot+ 
  ggplot2::annotate("text", 
                    x = 92, y = 1, # x and y coordinates of the text
                    label = "b", size = 9) + theme(axis.text.y = element_blank(),
                                                   axis.ticks.y= element_blank())
library(patchwork)
library(gridExtra)
Figkm <- (Fig1$plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")) + (Fig2$plot + theme(legend.position = "none"))
Figkm



#ESTIMATION OF THE MEAN, MEDIAN AND PERCENTILES
surv1_global <- survfit(Surv(L1PUPA, censored_im) ~ Salinity, df)  
surv2_adults <- survfit(Surv(days_adult, censored_ad) ~ GROUP2, df) 
surv2_adults_sex <- survfit(Surv(days_adult, censored_ad) ~ GROUP2 + strata(sex), df) 

print(surv1_global, print.rmean = TRUE)
print(surv2_adults, print.rmean = TRUE)
print(surv2_adults_sex, print.rmean = TRUE)


quantile(surv1_global, c(0.05, 0.5, 0.95))
quantile(surv2_adults, c(0.05, 0.5, 0.95))
quantile(surv2_adults_sex, c(0.05, 0.5, 0.95))


#LOG-RANK TEST
survdiff(Surv(L1PUPA, censored_im) ~ Salinity, data = df, rho = 0)  #Prueba log-rank all stages
survdiff(Surv(L1PUPA, censored_im) ~ Salinity, data = df, rho = 0)  #Prueba log-rank all stages

survdiff(Surv(days_adult, censored_ad) ~ Salinity, data = df, rho = 0) #Prueba log-rank adults
survdiff(Surv(days_adult, censored_ad) ~ Salinity, data = df, rho = 1)  #Prueba log-rank adults

survdiff(Surv(days_adult, censored_ad) ~ Salinity + strata(sex), data = df, rho = 0) #Prueba log-rank adults
survdiff(Surv(days_adult, censored_ad) ~ Salinity+ strata(sex), data = df, rho = 1)  #Prueba log-rank adults



#plots
Fig2 <-  survfit(Surv(days_adult, censored_ad) ~ Salinity + strata(sex), df, conf.type = "log-log") %>%
  ggsurvplot(
    conf.int = T,
    pval = T,
    legend.title = "",
    legend.position = "bottom",
    ylab="Survival probability", xlab="Longevity (days)",
    surv.plot.heigh = 1.30,
    break.x.by = 20,
    font.tickslab = c(15),
    font.y = c(15),
    font.x = c(15),
    font.family = "Lato",
    # palette = c("#861CCE", "#017984", "#FFBA00", "#9BF100", "#EB4CB7", "#0C7BD0",
    #             "#F10049", "#594900", "#257300", "#013D54", "#C05DF7", "#FF5600"),
    ggtheme = theme_bw((base_size=22)))
Fig2 <- Fig2$plot + theme_bw()+ facet_wrap(~strata)



