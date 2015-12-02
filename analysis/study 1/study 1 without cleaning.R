# Initilization -----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())
# set working directory to documents/home folder
setwd(normalizePath("~/"))

# load packages and install if nec ----------------------------------------

if(!require(httr)){install.packages('httr')}
library(httr)
if(!require(car)){install.packages('car')}
library(car)
if(!require(compute.es)){install.packages('compute.es')}
library(compute.es)
if(!require(reshape)){install.packages('reshape')}
library(reshape)
if(!require(reshape2)){install.packages('reshape2')}
library(reshape2)
if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(multcomp)){install.packages('multcomp')}
library(multcomp)
if(!require(pastecs)){install.packages('pastecs')}
library(pastecs)
if(!require(ez)){install.packages('ez')}
library(ez)
if(!require(doBy)){install.packages('doBy')}
library(doBy)
if(!require(lsr)){install.packages('lsr')}
library(lsr)
if(!require(WRS2)){install.packages('WRS2')}
library(WRS2)
if(!require(lavaan)){install.packages('lavaan')}
library(lavaan)
if(!require(mnormt)){install.packages('mnormt')}
library(mnormt)
if(!require(psych)){install.packages('psych')}
library(psych)
if(!require(lme4)){install.packages('lme4')}
library(lme4)
if(!require(nlme)){install.packages('nlme')}
library(nlme)
if(!require(gridExtra)){install.packages('gridExtra')}
library(gridExtra)

# download data -----------------------------------------------------------

GET('https://osf.io/x3vu6/?action=download',
    write_disk('Data_study_A_prepared.csv', overwrite = TRUE))

GET('https://osf.io/7hzg9/?action=download',
    write_disk('Data_study_A_prepared_international.csv', overwrite = TRUE))

# Read in data from csv ---------------------------------------------------

data <-read.csv(Data_study_A_file_name)
names(data)
data$X<-factor(data$X)

# Exclude Nobel prize laureates from analyses -----------------------------
# (they only rated scientists + very small small group)
# for their descriptives see later in the script for study 1

data<-subset(data, Respondent_group!="NPW")
data$Respondent_group<-factor(data$Respondent_group)

# Function to summarize data ----------------------------------------------

### from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  require(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Exclude incomplete cases ------------------------------------------------

#### Use complete cases only (as data collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data[,10:27]))
data<-data[-incomplete,]

# Create new variable: condition ------------------------------------------

data$Condition<-ifelse(data$Respondent_group=="Educated" & data$Target=="Educated", "Ed_Ed",
                       ifelse(data$Respondent_group=="Educated" & data$Target=="Scientists", "Ed_Sc",
                              ifelse(data$Respondent_group=="Scientists" & data$Target=="Educated", "Sc_Ed",
                                     ifelse(data$Respondent_group=="Scientists" & data$Target=="Scientists", "Sc_Sc", "unknown"))))
data$Condition<-as.factor(data$Condition)

# Find outliers -----------------------------------------------------------
## using Boxplot function from car package 
outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5)))

outliers<-as.numeric(outliers)
outliers

#  Remove outliers --------------------------------------------------------

data<-data[-outliers,]

# Sample descriptives -----------------------------------------------------

# Find them specified for asian and european in the supplement
# that is at the bottom of this file

# American educated
sum(data$Respondent_group=="Educated")
round(mean(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Educated"])), 2)

# American scientist
sum(data$Respondent_group=="Scientists")
round(mean(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists"])), 2)

# Scale reliabilities -----------------------------------------------------

Objectivity<-data[,c("ob1", "ob2", "ob3")]
alpha(Objectivity)

Rationality<-data[,c("ra1", "ra2", "ra3")]
alpha(Rationality)

Openness<-data[,c("op1", "op2", "op3")]
alpha(Openness)

Intelligence<-data[,c("iq1", "iq2", "iq3")]
alpha(Intelligence)

Integrity<-data[,c("in1", "in2", "in3")]
alpha(Integrity)

Communality<-data[,c("co1", "co2", "co3")] 
alpha(Communality)


# Correlations between features -------------------------------------------

names(data)
correlation_table_educated<-corr.test(data[data$Target=="Educated",34:39])
print(correlation_table_educated, short=F)

correlation_table_scientists<-corr.test(data[data$Target=="Scientists",34:39])
print(correlation_table_scientists, short=F)

correlation_table_overall<-corr.test(data[34:39])
print(correlation_table_overall, short=F)

# Analyses  -----------------------------------------------------

###### OBJECTIVITY ######

# Scientists-Scientists
length(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Objectivity_model) 

#check for effect of gender
#Objectivity_model_g<-Anova(lm(Objectivity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Objectivity_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target, data = data),type=3)
print(Objectivity_model) 
## t-test for Target
leveneTest(data$Objectivity, data$Condition, center=median)
t_test1 = t.test(data$Objectivity[data$Target == "Scientists"],
                 data$Objectivity[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Objectivity[data$Respondent_group == "Scientists"],
                  data$Objectivity[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)

###### RATIONALITY ######

# Scientists-Scientists
length(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Rationality_model<-Anova(lm(Rationality ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Rationality_model) 

#check for effect of gender
#Rationality_model_g<-Anova(lm(Rationality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Rationality_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Rationality_model<-Anova(lm(Rationality ~ Respondent_group + Target, data = data),type=3)
print(Rationality_model) 
## t-test for Target
leveneTest(data$Rationality, data$Condition, center=median)
t_test1 = t.test(data$Rationality[data$Target == "Scientists"],
                 data$Rationality[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Rationality[data$Respondent_group == "Scientists"],
                  data$Rationality[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)


###### OPENNESS ######
# Scientists-Scientists
length(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Openness[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Openness[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Openness[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Openness[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Openness[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Openness[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Openness_model<-Anova(lm(Openness ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Openness_model) 

#check for effect of gender
#Openness_model_g<-Anova(lm(Openness ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Openness_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Openness_model<-Anova(lm(Openness ~ Respondent_group + Target, data = data),type=3)
print(Openness_model) 
## t-test for Target
leveneTest(data$Openness, data$Condition, center=median)
t_test1 = t.test(data$Openness[data$Target == "Scientists"],
                 data$Openness[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Openness[data$Respondent_group == "Scientists"],
                  data$Openness[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)

##### INTELLIGENCE #####

# Scientists-Scientists
length(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Intelligence_model) 

#check for effect of gender
#Intelligence_model_g<-Anova(lm(Intelligence ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Intelligence_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target, data = data),type=3)
print(Intelligence_model) 
## t-test for Target
leveneTest(data$Intelligence, data$Condition, center=median)
t_test1 = t.test(data$Intelligence[data$Target == "Scientists"],
                 data$Intelligence[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Intelligence[data$Respondent_group == "Scientists"],
                  data$Intelligence[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)

##### INTEGRITY #####

# Scientists-Scientists
length(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Integrity_model) 

#check for effect of gender
#Integrity_model_g<-Anova(lm(Integrity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Integrity_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target, data = data),type=3)
print(Integrity_model) 
## t-test for Target
leveneTest(data$Integrity, data$Condition, center=median)
t_test1 = t.test(data$Integrity[data$Target == "Scientists"],
                 data$Integrity[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Integrity[data$Respondent_group == "Scientists"],
                  data$Integrity[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)

##### COMMUNALITY #####

# Scientists-Scientists
length(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Scientists"])
round(mean(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Scientists"]), 2)

# Scientists-Educated
length(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Educated"])
round(mean(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)
round(sd(data$Communality[data$Respondent_group == "Scientists" & data$Target == "Educated"]), 2)

# Educated-Scientists
length(data$Communality[data$Respondent_group == "Educated" & data$Target == "Scientists"])
round(mean(data$Communality[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Educated" & data$Target == "Scientists"]), 2)

# Educated-Educated
length(data$Communality[data$Respondent_group == "Educated" & data$Target == "Educated"])
round(mean(data$Communality[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)
round(sd(data$Communality[data$Respondent_group == "Educated" & data$Target == "Educated"]), 2)

#test
Communality_model<-Anova(lm(Communality ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Communality_model) 

#check for effect of gender
#Communality_model_g<-Anova(lm(Communality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type=3)
#print(Communality_model_g) 

# NO INTERACTION (alpha = 0.008),so model without interaction to report main effects
Communality_model<-Anova(lm(Communality ~ Respondent_group + Target, data = data),type=3)
print(Communality_model) 
## t-test for Target
leveneTest(data$Communality, data$Condition, center=median)
t_test1 = t.test(data$Communality[data$Target == "Scientists"],
                 data$Communality[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-t_test1$estimate[1]-t_test1$estimate[2]
difference
#effect size
tes(t=t_test1$statistic, n.1=n_target_S, n.2=n_target_E)


##t test for Respondent group
t_test_2 = t.test(data$Communality[data$Respondent_group == "Scientists"],
                  data$Communality[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
difference
#effect size
tes(t=t_test_2$statistic, n.1=n_resp_group_S, n.2=n_resp_group_E)

# ADD DESCRIPTIVES NOBEL PRIZE LAUREEATES FOR GRAPHS ------------------------------------------

Data_study_A_file_name<-"Data_study_A_prepared.csv"
data_2 <-read.csv(Data_study_A_file_name)

data_2$X<-factor(data_2$X)

### Use complete cases only (as data collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data_2[,10:27]))
data_2<-data_2[-incomplete,]

# Create variable: condition 
data_2$Respondent_group <- factor(data_2$Respondent_group,levels(data_2$Respondent_group)[c(1,3,2)])

data_2$Condition<-ifelse(data_2$Respondent_group=="Educated" & data_2$Target=="Educated", "Ed_Ed",
                         ifelse(data_2$Respondent_group=="Educated" & data_2$Target=="Scientists", "Ed_Sc",
                                ifelse(data_2$Respondent_group=="Scientists" & data_2$Target=="Educated", "Sc_Ed",
                                       ifelse(data_2$Respondent_group=="Scientists" & data_2$Target=="Scientists", "Sc_Sc", 
                                              ifelse(data_2$Respondent_group=="NPW" & data_2$Target=="Scientists", "NPW_Sc", "unknown")))))
data_2$Condition<-as.factor(data_2$Condition)

#  remove same outliers as in set without Nobel Prize Laureates 
outliers<-as.numeric(outliers)
data_2<-data_2[-outliers,]

# Sample descriptives INCLUDING NPL ---------------------------------------

length(data_2$Age[data_2$Respondent_group=="NPW"])
round(mean(data_2$Age[data_2$Respondent_group=="NPW"], na.rm=T),digits =1)
round(min(data_2$Age[data_2$Respondent_group=="NPW"], na.rm=T),digits =1)
round(max(data_2$Age[data_2$Respondent_group=="NPW"], na.rm=T),digits =1)
round(SD(data_2$Age[data_2$Respondent_group=="NPW"], na.rm=T),digits =1)
prop.table(table(data_2$Gender[data_2$Respondent_group=="NPW"]))

# GRAPHS (including Nobel Prize Laureates) --------------------------------
###### Objectivity ######
#barplot
data_2_Objectivity <- summarySE(data_2, measurevar="Objectivity", groupvars=c("Respondent_group","Target"))

Objectivity_plot_bar<-ggplot(data_2_Objectivity, aes(x=Target, y=Objectivity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar

ggsave("Objectivity_plot_bar.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Objectivity_plot_bar.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

###### Rationality ######
#barplot
data_2_Rationality <- summarySE(data_2, measurevar="Rationality", groupvars=c("Respondent_group","Target"))

Rationality_plot_bar<-ggplot(data_2_Rationality, aes(x=Target, y=Rationality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5, 6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar

ggsave("Rationality_plot_bar.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Rationality_plot_bar.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)


###### Openness ######

#barplot
data_2_Openness <- summarySE(data_2, measurevar="Openness", groupvars=c("Respondent_group","Target"))

Openness_plot_bar<-ggplot(data_2_Openness, aes(x=Target, y=Openness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5, 6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar

ggsave("Openness_plot_bar.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Openness_plot_bar.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)

###### Integrity ######

#barplot
data_2_Integrity <- summarySE(data_2, measurevar="Integrity", groupvars=c("Respondent_group","Target"))

Integrity_plot_bar<-ggplot(data_2_Integrity, aes(x=Target, y=Integrity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4, 6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar

ggsave("Integrity_plot_bar.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Integrity_plot_bar.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)

###### Intelligence ######

#barplot
data_2_Intelligence <- summarySE(data_2, measurevar="Intelligence", groupvars=c("Respondent_group","Target"))

Intelligence_plot_bar<-ggplot(data_2_Intelligence, aes(x=Target, y=Intelligence, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Intelligence_plot_bar

ggsave("Intelligence_plot_bar.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Intelligence_plot_bar.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)

###### Communality ######

#barplot
data_2_Communality <- summarySE(data_2, measurevar="Communality", groupvars=c("Respondent_group","Target"))

Communality_plot_bar<-ggplot(data_2_Communality, aes(x=Target, y=Communality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3, 5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated", "Scientists","NPW"), labels=c("Educated", "Scientists","Nobel Prize Laureates"))+
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Communality_plot_bar

ggsave("Communality_plot_bar.jpeg", Communality_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Communality_plot_bar.pdf", Communality_plot_bar,dpi=600, width = 9, height = 6)

# Create multipanel plot -------------------------------------------------
# multipanel plot bar plots
# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar)

jpeg("multipanel_plot_study_A.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Communality_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))

dev.off


# SUPPLEMENT --------------------------------------------------------------

# Read in data from csv ---------------------------------------------------

Data_study_A_file_name<-"Data_study_A_prepared_international.csv"
data <-read.csv(Data_study_A_file_name)

data$X<-factor(data$X)

# Exclude Nobel prize laureates from analyses -----------------------------
# (they only rated scientists + very small small group)

data<-subset(data, Respondent_group!="NPW")
data$Respondent_group<-factor(data$Respondent_group)

# Exclude incomplete cases ------------------------------------------------

#### Use complete cases only (as data collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data[,10:27]))
data<-data[-incomplete,]

# Create variable for contintent/worldpart --------------------------------

# create variable for continent/ worldpart

Europe <- c(2,4,8,10,11,16,17,22,26,42,44,45,48,57,60,61,64,65,67,76,77,82,84,94,99,100,107,113,115,122,128,137,138,141,142,143,149,153,157,158,163,168,169,173,179,183,185)
Africa <- c(3,5,19,23,27,28,30,32,33,34,38,39,41,49,53,55,56,58,62,63,66,70,71,89,96,97,98,102,103,106,109,110,116,117,119,125,126,144,150,152,154,155,160,161,165,167,175,178,182,186,580,1357)
Asia<-c(1,13,14,20,25,36,46,75,78,79,80,81,83,86,87,88,91,92,93,95,104,105,114,118,121,127,129,130,139,140,151,156,162,164,170,171,172,174,180,184,189,192,193)
North_America<-c(6,12,15,18,31,40,43,50,51,54,68,69,73,74,85,111,124,132,145,147,177,187)
South_America<-c(7,21,24,35,37,52,72,134,135,166,188,191)
Oceania<-c(9,59,90,108,112,120,123,131,133,1484,159,176,181,190)
USA<-187

worldpart <- numeric()

worldpart[data$Country%in%Europe] <- "Europe"
worldpart[data$Country%in%Africa] <- "Africa"
worldpart[data$Country%in%Asia] <- "Asia"
worldpart[data$Country%in%North_America] <- "North_America"
worldpart[data$Country%in%South_America] <- "South_America"
worldpart[data$Country%in%Oceania] <- "Oceania"
worldpart[data$Country%in%USA] <- "USA"

data$worldpart<-worldpart
data$worldpart
data$worldpart<-as.factor(data$worldpart)

# Only use Europe, Asia and USA (other groups too small)
data<-subset(data, worldpart=="Europe" | worldpart=="Asia" | worldpart=="USA")

# Create new variable: condition ------------------------------------------

data$Condition<-ifelse(data$Respondent_group=="Educated" & data$Target=="Educated", "Ed_Ed",
                       ifelse(data$Respondent_group=="Educated" & data$Target=="Scientists", "Ed_Sc",
                              ifelse(data$Respondent_group=="Scientists" & data$Target=="Educated", "Sc_Ed",
                                     ifelse(data$Respondent_group=="Scientists" & data$Target=="Scientists", "Sc_Sc", "unknown"))))
data$Condition<-as.factor(data$Condition)

# Find and remove outliers per worldpart -----------------------------------------------------------
## using Boxplot function from car package 

# Remove outliers per subgroup (worldparts)

## USA
data_USA<-subset(data, worldpart=="USA")

outliers<-unique(c(Boxplot(data_USA$Objectivity, data_USA$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_USA$Rationality, data_USA$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_USA$Openness, data_USA$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_USA$Intelligence, data_USA$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_USA$Integrity, data_USA$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_USA$Communality, data_USA$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_USA<-data_USA[-outliers,]

# ASIA
data_Asia<-subset(data, worldpart=="Asia")

outliers<-unique(c(Boxplot(data_Asia$Objectivity, data_Asia$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Asia$Rationality, data_Asia$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Asia$Openness, data_Asia$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Asia$Intelligence, data_Asia$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Asia$Integrity, data_Asia$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Asia$Communality, data_Asia$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_Asia<-data_Asia[-outliers,]

# EUROPE

data_Europe<-subset(data, worldpart=="Europe")

outliers<-unique(c(Boxplot(data_Europe$Objectivity, data_Europe$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Europe$Rationality, data_Europe$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Europe$Openness, data_Europe$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Europe$Intelligence, data_Europe$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Europe$Integrity, data_Europe$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5),
                   
                   Boxplot(data_Europe$Communality, data_Europe$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=1.5)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_Europe<-data_Europe[-outliers,]

# merge data again -----------------------------
# combine USA and Asia data

common.names <- intersect(colnames(data_USA), colnames(data_Asia))
data_usa_asia <- rbind(data_USA[, common.names], data_Asia[, common.names])

# combine USA, Asia and Europe data

common.names <- intersect(colnames(data_usa_asia), colnames(data_Europe))
data_combined <- rbind(data_usa_asia[, common.names], data_Europe[, common.names])

data<-data_combined

# Sample descriptives -----------------------------------------------------

# Educated (repeat)
sum(data$Respondent_group=="Educated")
round(mean(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Educated"])), 2)
# USA Scientists (repeat)
sum(data$Respondent_group=="Scientists"& data$worldpart=="USA")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="USA"])), 2)
# Asia
sum(data$Respondent_group=="Scientists"& data$worldpart=="Asia")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="Asia"])), 2)
# Europe
sum(data$Respondent_group=="Scientists"& data$worldpart=="Europe")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="Europe"])), 2)

#  plots per worldpart (SCIENTISTS ONLY!) ---------------------------------

# Use only scientist data
data<-subset(data, Respondent_group=="Scientists")

# OBJECTIVITY

#barplot
data_Objectivity <- summarySE(data, measurevar="Objectivity", groupvars=c("worldpart","Target"))

Objectivity_plot_bar<-ggplot(data_Objectivity, aes(x=Target, y=Objectivity, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5,5.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Objectivity_plot_bar

ggsave("Objectivity_plot_bar_sc_international.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Objectivity_plot_bar_sc_international.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

# Rationality

#barplot
data_Rationality <- summarySE(data, measurevar="Rationality", groupvars=c("worldpart","Target"))

Rationality_plot_bar<-ggplot(data_Rationality, aes(x=Target, y=Rationality, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5,6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Rationality_plot_bar

ggsave("Rationality_plot_bar_sc_international.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Rationality_plot_bar_sc_international.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)

# Openness

#barplot
data_Openness <- summarySE(data, measurevar="Openness", groupvars=c("worldpart","Target"))

Openness_plot_bar<-ggplot(data_Openness, aes(x=Target, y=Openness, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5,6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Openness_plot_bar

ggsave("Openness_plot_bar_sc_international.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Openness_plot_bar_sc_international.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)

# Intelligence

#barplot
data_Intelligence <- summarySE(data, measurevar="Intelligence", groupvars=c("worldpart","Target"))

Intelligence_plot_bar<-ggplot(data_Intelligence, aes(x=Target, y=Intelligence, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5,5.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Intelligence_plot_bar

ggsave("Intelligence_plot_bar_sc_international.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Intelligence_plot_bar_sc_international.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)

# Integrity

#barplot
data_Integrity <- summarySE(data, measurevar="Integrity", groupvars=c("worldpart","Target"))

Integrity_plot_bar<-ggplot(data_Integrity, aes(x=Target, y=Integrity, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.0,6.5)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Integrity_plot_bar

ggsave("Integrity_plot_bar_sc_international.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Integrity_plot_bar_sc_international.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)

# Communality

#barplot
data_Communality <- summarySE(data, measurevar="Communality", groupvars=c("worldpart","Target"))

Communality_plot_bar<-ggplot(data_Communality, aes(x=Target, y=Communality, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0,5.0)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  theme(legend.title = element_text(size=12, face="bold")) +
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks=c("Educated", "Scientists"), labels=c("Highly-educated people", "Scientists"))+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=14, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=24,vjust=1.2))

Communality_plot_bar

ggsave("Communality_plot_bar_sc_international.jpeg", Communality_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Communality_plot_bar_sc_international.pdf", Communality_plot_bar,dpi=600, width = 9, height = 6)




# Create MULTIPANEL plots -------------------------------------------------

# multipanel plot bar plots
# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar)


jpeg("multipanel_plot_study_A_bars.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Communality_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))

dev.off()

