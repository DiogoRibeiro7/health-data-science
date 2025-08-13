setwd("/Users/brianajoy/OneDrive\ -\ Harvard\ University/NCDB/Data")

###########################
## load library packages ##
###########################

library(aod)

library(survival)
library(survminer)

library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(tidyverse)

#LOAD NCDB EARLY STAGE DATA
load('lca_earlypuf.Rdata')

## Define largest class 4 as the referent group ##
lca.pufdata$LCAprofile <-factor(lca.pufdata$class7)
lca.pufdata$LCAprofile <- relevel(lca.pufdata$LCAprofile,ref="4")

###########################################
## OUTCOME 1:                            ##
## RECEIPT OF MINIMUM EXPECTED TREATMENT ##
###########################################

mintx.logit <- glm(mintreat ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility + CDCC_TOTAL_BEST + FACILITY_LOCATION_CD+YEAR_OF_DIAGNOSIS, data = lca.pufdata, family = "binomial")
summary(mintx.logit)
mintx.aor<-exp(cbind(OR = coef(mintx.logit), confint(mintx.logit)))


mintx.logit0 <- glm(mintreat ~ LCAprofile, data = lca.pufdata, family = "binomial")
mintx.uor<-exp(cbind(OR=coef(mintx.logit0), confint(mintx.logit0)))

boxLabels = c("Profile 1","Profile 2","Profile 3","Profile 4","Profile 6","Profile 7")
  ## Plot of Odds ratios unadjusted and adjusted
lcaout1.aor <- data.frame(
  yAxis=1:length(boxLabels),
  boxOdds= mintx.aor[2:7,1],
  boxCIlow= mintx.aor[2:7,2],
  boxCIHigh= mintx.aor[2:7,3]
)
# Plot
p <- ggplot(lcaout1.aor, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCIlow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0,6,1) ) +
    scale_y_continuous(breaks = seq(1,6,1), labels = boxLabels) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)")+
  ggtitle("Odds of receiving minimum expected treatment")


## RECEIPT OF OPTIMAL CARE -- LOGISTIC REGRESSION ##
care.logit <- glm(optcare ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility + CDCC_TOTAL_BEST + FACILITY_LOCATION_CD+YEAR_OF_DIAGNOSIS, data = lca.pufdata, family = "binomial")
summary(care.logit)
#odds ratio and CI's
optcare.aor<-exp(cbind(OR = coef(care.logit), confint(care.logit)))
#UNADJUSTED ODDS RATIOS 
care.logit0 <- glm(optcare ~ LCAprofile,  data = lca.pufdata, family = "binomial")
summary(care.logit0)
exp(cbind(OR = coef(care.logit0), confint(care.logit0)))

lcaout2.aor<-data.frame(
  yAxis=1:length(boxLabels),
  boxOdds=optcare.aor[2:7,1],
  boxCIlow=optcare.aor[2:7,2],
  boxCIHigh=optcare.aor[2:7,3],
)
# Plot
p <- ggplot(lcaout2.aor, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCIlow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0,6,1) ) +
  scale_y_continuous(breaks = seq(1,6,1), labels = boxLabels) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)")+
  ggtitle("Odds of receipt of optimal care")






#####################################
##  TIME TO TREAT BY LC ASSIGNMENT ##
#####################################
txtime.median <- aggregate(lca.pufdata$DX_RX_STARTED_DAYS, list(lc=lca.pufdata$LCAprofile),na.rm=TRUE,median)
txtime.iqr <- aggregate(lca.pufdata$DX_RX_STARTED_DAYS, list(lc=lca.pufdata$LCAprofile),na.rm=TRUE,IQR)
txtime.q1q3 <- aggregate(lca.pufdata$DX_RX_STARTED_DAYS, list(lc=lca.pufdata$LCAprofile),na.rm=TRUE,quantile)

survfit(Surv(DX_RX_STARTED_DAYS, status) ~ LCAprofile, data = lca.pufdata)
quantile(survfit(Surv(DX_RX_STARTED_DAYS, status) ~ LCAprofile, data = lca.pufdata))


TXtime.lc <-ggplot(lca.pufdata, aes(x=factor(LCAprofile), y=DX_RX_STARTED_DAYS)) + geom_boxplot() 
print(TXtime.lc + ggtitle("Time to Treat by Latent Class"))

TXtime.lc <-ggplot(lca.pufdata, aes(x=factor(LCAprofile), y=log(DX_RX_STARTED_DAYS+0.1))) + geom_boxplot()
  #add reference line for 14 days
TXtime.lc  + geom_hline(yintercept=log(0+0.1), linetype="solid", color = "red") +
geom_hline(yintercept=log(14+0.1), linetype="dashed", color = "red") +
  #add reference line for 30 days
geom_hline(yintercept=log(30+0.1), linetype="dotted",color = "red") +
  #add reference line for 100 days
geom_hline(yintercept=log(100+0.1), linetype="dotdash", color = "red") + 
  #add titles and labels
ggtitle("Log-Time to Treat by Latent Class") + ylab("log(time to treat)") + xlab("LCA-derived profile")




n<-length(lca.pufdata$class7)
status <- rep(1,n)

km.lca <-survfit(Surv(DX_RX_STARTED_DAYS, status) ~ LCAprofile, data = lca.pufdata)
km.lcadif <-survdiff(Surv(DX_RX_STARTED_DAYS, status) ~ LCAprofile, data = lca.pufdata)

ggsurvplot(km.lca, data= lca.pufdata)

#Focus on the first 56 days
ggsurvplot(km.lca,data=lca.pufdata,
           risk.table=TRUE,
          # pval=TRUE,
           xlim=c(0,56),
           break.time.by=7
)
tim1.cox<-coxph(Surv(DX_RX_STARTED_DAYS,status)~LCAprofile+ ANALYTIC_STAGE_GROUP + facility + CDCC_TOTAL_BEST + YEAR_OF_DIAGNOSIS + FACILITY_LOCATION_CD:CROWFLY, data=lca.pufdata)
txtime.aor<-summary(tim1.cox)

coxout3.aor <- data.frame(
  yAxis=1:length(boxLabels),
  boxHazards=txtime.aor$conf.int[1:6,1],
  boxCIlow=txtime.aor$conf.int[1:6,3],
  boxCIHigh=txtime.aor$conf.int[1:6,4]
)

tp <- ggplot(coxout3.aor, aes(x = boxHazards, y = yAxis))
tp + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCIlow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3, color = "green") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0.75,1.25) ) +
  scale_y_continuous(breaks = seq(1,6,1), labels = boxLabels) +
 # coord_trans(x = "log10") +
  ylab("") +
  xlab("Hazards ratio")+
  ggtitle("Time to Treatment")

########################################
##  OVERALL SURVIVAL BY LC ASSIGNMENT ##
########################################
panc.data$race3<- NA
panc.data$race3[panc.data$RACE == 1] <- "White"
panc.data$race3[panc.data$RACE == 2] <- "Black"
panc.data$race3[panc.data$RACE > 2] <- "Other"
race <-as.factor(panc.data$race3)
#create tiered-treatment variable
lca.pufdata$treat_tier <-NA
lca.pufdata$treat_tier[lca.pufdata$mintreat==0] <-"No Surgery"
lca.pufdata$treat_tier[lca.pufdata$mintreat==1 & lca.pufdata$optcare==0] <- "Surgery"
lca.pufdata$treat_tier[lca.pufdata$optcare==1] <- "Surgery/Chemo"
lca.pufdata$treat_tier <- as.factor(lca.pufdata$treat_tier)
  
survtime.median <- aggregate(lca.pufdata$DX_LASTCONTACT_DEATH_MONTHS, list(lc=class7),na.rm=TRUE,median)
survfit(Surv(DX_LASTCONTACT_DEATH_MONTHS, PUF_VITAL_STATUS) ~ LCAprofile, data = lca.pufdata)
quantile(survfit(Surv(DX_LASTCONTACT_DEATH_MONTHS, PUF_VITAL_STATUS) ~ LCAprofile, data = lca.pufdata)
)

tim2.cox<-coxph(Surv(DX_LASTCONTACT_DEATH_MONTHS,PUF_VITAL_STATUS)~LCAprofile+ ANALYTIC_STAGE_GROUP + facility + CDCC_TOTAL_BEST + treat_tier + YEAR_OF_DIAGNOSIS + FACILITY_LOCATION_CD:CROWFLY, data=lca.pufdata)
survtime.aor<-summary(tim2.cox)

pufsurvfit <- survfit(Surv(DX_LASTCONTACT_DEATH_MONTHS, PUF_VITAL_STATUS) ~ LCAprofile , data = lca.pufdata)
ggsurvplot(pufsurvfit, data= lca.pufdata)

coxout4.aor <- data.frame(
  yAxis=1:length(boxLabels),
  boxHazards=survtime.aor$conf.int[1:6,1],
  boxCIlow=survtime.aor$conf.int[1:6,3],
  boxCIHigh=survtime.aor$conf.int[1:6,4]
)

sp <- ggplot(coxout4.aor, aes(x = boxHazards, y = yAxis))
sp + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCIlow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3, color = "purple") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0.75,1.75) ) +
  scale_y_continuous(breaks = seq(1,6,1), labels = boxLabels) +
  # coord_trans(x = "log10") +
  ylab("") +
  xlab("Hazards ratio")+
  ggtitle("Overall Survival Time")

#Who is seen in the first two weeks?
status14 <- ifelse(lca.pufdata$DX_RX_STARTED_DAYS<=14,1,0)
time14 <-ifelse(lca.pufdata$DX_RX_STARTED_DAYS <=14,lca.pufdata$DX_RX_STARTED_DAYS,14)

survfit(Surv(time14,status14)~class7,data=lca.pufdata)

####

#### End of file #####
