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


data <-read.csv('Data_study_A_prepared.csv')
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
outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0)))

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


# cell sizes

# nr of respondents in the Educated group judging the Educated targets
sum(data$Condition=="Ed_Ed")

# nr of respondents in the Educated group judging the Scientist targets
sum(data$Condition=="Ed_Sc")

# #nr of respondents in the 'Scientist group judging the Educated targets
sum(data$Condition=="Sc_Ed")

# #nr of respondents in the 'Scientist group judging the Scientist targets
sum(data$Condition=="Sc_Sc")

# Create variables containing n of Respondent groups and Targets 


levels(data$Respondent_group)
n_resp_group_E<-sum(data$Respondent_group=="Educated")
n_resp_group_S<-sum(data$Respondent_group=="Scientists")

levels(data$Target)
n_target_E<-sum(sum(data$Target=="Educated"))
n_target_S<-sum(sum(data$Target=="Scientists"))


# create subsets to be able to look at simple effects 


data_Educated_respondents<-subset(data, Respondent_group=="Educated")
data_Scientist_respondents<-subset(data, Respondent_group=="Scientists")


n_resp_group_E_Target_E<-sum(data_Educated_respondents$Target=="Educated")
n_resp_group_E_Target_S<-sum(data_Educated_respondents$Target=="Scientists")
n_resp_group_S_Target_E<-sum(data_Scientist_respondents$Target=="Educated")
n_resp_group_S_Target_S<-sum(data_Scientist_respondents$Target=="Scientists")


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

#Test assumption of equal variances
leveneTest(data$Objectivity, data$Condition, center=median) # not significant

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
t_test1_Ob = t.test(data$Objectivity[data$Target == "Scientists"],
                 data$Objectivity[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_Ob
difference<-t_test1_Ob$estimate[1]-t_test1_Ob$estimate[2]
round(difference, 2)
p_value_main_effect_target_Ob <- t_test1_Ob$p.value
#effect size
tes(t=t_test1_Ob$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Ob_Target <- tes(t=t_test1_Ob$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Ob_Target_d <- ES_Ob_Target$d 
ES_Ob_Target_CI_lower <- ES_Ob_Target$l.d
ES_Ob_Target_CI_upper<- ES_Ob_Target$u.d

##t test for Respondent group
t_test_2 = t.test(data$Objectivity[data$Respondent_group == "Scientists"],
                  data$Objectivity[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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

# Test assumption of equal variances
leveneTest(data$Rationality, data$Condition, center=median) 
# significant, but largest group (n = 166) not more than 1.5 times size of smallest group (n = 153)
# so we can still use a parametric test 


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
t_test1_Ra = t.test(data$Rationality[data$Target == "Scientists"],
                 data$Rationality[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_Ra
difference<-t_test1_Ra$estimate[1]-t_test1_Ra$estimate[2]
round(difference, 2)
p_value_main_effect_target_Ra <- t_test1_Ra$p.value
#effect size
tes(t=t_test1_Ra$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Ra_Target <- tes(t=t_test1_Ra$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Ra_Target_d <- ES_Ra_Target$d 
ES_Ra_Target_CI_lower <- ES_Ra_Target$l.d
ES_Ra_Target_CI_upper<- ES_Ra_Target$u.d


##t test for Respondent group
t_test_2 = t.test(data$Rationality[data$Respondent_group == "Scientists"],
                  data$Rationality[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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


# Test assumption of equal variances
leveneTest(data$Openness, data$Condition, center=median) 
# not significant @ alpha .008333

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
t_test1_Op = t.test(data$Openness[data$Target == "Scientists"],
                 data$Openness[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_Op
difference<-t_test1_Op$estimate[1]-t_test1_Op$estimate[2]
round(difference, 2)
p_value_main_effect_target_Op <- t_test1_Op$p.value
#effect size
tes(t=t_test1_Op$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Op_Target <- tes(t=t_test1_Op$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Op_Target_d <- ES_Op_Target$d 
ES_Op_Target_CI_lower <- ES_Op_Target$l.d
ES_Op_Target_CI_upper<- ES_Op_Target$u.d


##t test for Respondent group
t_test_2 = t.test(data$Openness[data$Respondent_group == "Scientists"],
                  data$Openness[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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


# Test assumption of equal variances
leveneTest(data$Intelligence, data$Condition, center=median) # not significant @ alpha .008333

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
t_test1_Iq = t.test(data$Intelligence[data$Target == "Scientists"],
                 data$Intelligence[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_Iq
difference<-t_test1_Iq$estimate[1]-t_test1_Iq$estimate[2]
round(difference, 2)
p_value_main_effect_target_Iq <- t_test1_Iq$p.value
#effect size
tes(t=t_test1_Iq$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Iq_Target <- tes(t=t_test1_Iq$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Iq_Target_d <- ES_Iq_Target$d 
ES_Iq_Target_CI_lower <- ES_Iq_Target$l.d
ES_Iq_Target_CI_upper<- ES_Iq_Target$u.d


##t test for Respondent group
t_test_2 = t.test(data$Intelligence[data$Respondent_group == "Scientists"],
                  data$Intelligence[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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

# Test assumption of equal variances
leveneTest(data$Integrity, data$Condition, center=median) 
# significant, but largest group (n = 166) not more than 1.5 times size of smallest group (n = 153)
# so we can still use a parametric test 


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
t_test1_In = t.test(data$Integrity[data$Target == "Scientists"],
                 data$Integrity[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_In
difference<-t_test1_In$estimate[1]-t_test1_In$estimate[2]
round(difference, 2)
p_value_main_effect_target_In <- t_test1_In$p.value
#effect size
tes(t=t_test1_In$statistic, n.1=n_target_S, n.2=n_target_E)
ES_In_Target <- tes(t=t_test1_In$statistic, n.1=n_target_S, n.2=n_target_E)
ES_In_Target_d <- ES_In_Target$d 
ES_In_Target_CI_lower <- ES_In_Target$l.d
ES_In_Target_CI_upper<- ES_In_Target$u.d


##t test for Respondent group
t_test_2 = t.test(data$Integrity[data$Respondent_group == "Scientists"],
                  data$Integrity[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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

# Test assumption of equal variances
leveneTest(data$Communality, data$Condition, center=median) # not significant


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
t_test1_Co = t.test(data$Communality[data$Target == "Scientists"],
                 data$Communality[data$Target == "Educated"],
                 var.equal = TRUE, paired = FALSE)
t_test1_Co
difference<-t_test1_Co$estimate[1]-t_test1_Co$estimate[2]
round(difference, 2)
p_value_main_effect_target_Co <- t_test1_Co$p.value
#effect size
tes(t=t_test1_Co$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Co_Target <- tes(t=t_test1_Co$statistic, n.1=n_target_S, n.2=n_target_E)
ES_Co_Target_d <- ES_Co_Target$d 
ES_Co_Target_CI_lower <- ES_Co_Target$l.d
ES_Co_Target_CI_upper<- ES_Co_Target$u.d


##t test for Respondent group
t_test_2 = t.test(data$Communality[data$Respondent_group == "Scientists"],
                  data$Communality[data$Respondent_group == "Educated"],
                  var.equal = TRUE, paired = FALSE)
t_test_2
difference<-t_test_2$estimate[1]-t_test_2$estimate[2]
round(difference, 2)
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

w <- c(.9, .9, .9, .9, .45) 
Objectivity_plot_bar<-ggplot(data_2_Objectivity, aes(x=Respondent_group, y=Objectivity, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Objectivity 
ES_Ob_Target_d  
ES_Ob_Target_CI_lower 
ES_Ob_Target_CI_upper
p_value_main_effect_target_Ob
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Objectivity_plot_bar

#ggsave("Objectivity_plot_bar.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

###### Rationality ######
#barplot
data_2_Rationality <- summarySE(data_2, measurevar="Rationality", groupvars=c("Respondent_group","Target"))

w <- c(.9, .9, .9, .9, .45) 
Rationality_plot_bar<-ggplot(data_2_Rationality, aes(x=Respondent_group, y=Rationality, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5, 6.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Rationality 
ES_Ra_Target_d  
ES_Ra_Target_CI_lower 
ES_Ra_Target_CI_upper
p_value_main_effect_target_Ra
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Rationality_plot_bar

#ggsave("Rationality_plot_bar.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)


###### Openness ######

#barplot
data_2_Openness <- summarySE(data_2, measurevar="Openness", groupvars=c("Respondent_group","Target"))

w <- c(.9, .9, .9, .9, .45) 
Openness_plot_bar<-ggplot(data_2_Openness, aes(x=Respondent_group, y=Openness, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5, 6.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Openness 
ES_Op_Target_d  
ES_Op_Target_CI_lower 
ES_Op_Target_CI_upper
p_value_main_effect_target_Op
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Openness_plot_bar

#ggsave("Openness_plot_bar.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)


###### Intelligence ######

#barplot
data_2_Intelligence <- summarySE(data_2, measurevar="Intelligence", groupvars=c("Respondent_group","Target"))

w <- c(.9, .9, .9, .9, .45) 
Intelligence_plot_bar<-ggplot(data_2_Intelligence, aes(x=Respondent_group, y=Intelligence, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))


# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Intelligence 
ES_Iq_Target_d  
ES_Iq_Target_CI_lower 
ES_Iq_Target_CI_upper
p_value_main_effect_target_Iq
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Intelligence_plot_bar

#ggsave("Intelligence_plot_bar.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)



###### Integrity ######

#barplot
data_2_Integrity <- summarySE(data_2, measurevar="Integrity", groupvars=c("Respondent_group","Target"))

w <- c(.9, .9, .9, .9, .45) 
Integrity_plot_bar<-ggplot(data_2_Integrity, aes(x=Respondent_group, y=Integrity, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.0, 6.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Integrity 
ES_In_Target_d  
ES_In_Target_CI_lower 
ES_In_Target_CI_upper
p_value_main_effect_target_In
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Integrity_plot_bar

#ggsave("Integrity_plot_bar.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)


###### Communality ######

#barplot
data_2_Communality <- summarySE(data_2, measurevar="Communality", groupvars=c("Respondent_group","Target"))

w <- c(.9, .9, .9, .9, .45) 
Communality_plot_bar<-ggplot(data_2_Communality, aes(x=Respondent_group, y=Communality, fill=Target)) + 
  geom_bar(aes(width = w),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0, 5.0)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A Highly-Educated person", " A scientist"))+
  scale_x_discrete(breaks=c("Educated", "Scientists", "NPW"), labels=c("H-e", "Sc", "NPL"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

# Objects for effect size d, upper and lower CI bounds for main effect of target, and p-value for test:
# Communality 
ES_Co_Target_d  
ES_Co_Target_CI_lower 
ES_Co_Target_CI_upper
p_value_main_effect_target_Co
# (alpha for main effects = 0.0083333, so perhaps * = <.008333, # ** = <.001, *** = <.0001) 


Communality_plot_bar

#ggsave("Communality_plot_bar.jpeg", Communality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Communality_plot_bar.pdf", Communality_plot_bar,dpi=600, width = 9, height = 6)

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

dev.off()


# SUPPLEMENT --------------------------------------------------------------

# Read in data_international from csv ---------------------------------------------------

data_international_study_A_file_name<-"data_study_A_prepared_international.csv"
data_international <-read.csv(data_international_study_A_file_name)

data_international$X<-factor(data_international$X)

# Exclude Nobel prize laureates from analyses -----------------------------
# (they only rated scientists + very small small group)

data_international<-subset(data_international, Respondent_group!="NPW")
data_international$Respondent_group<-factor(data_international$Respondent_group)

# Exclude incomplete cases ------------------------------------------------

#### Use complete cases only (as data_international collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data_international[,10:27]))
data_international<-data_international[-incomplete,]

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

worldpart[data_international$Country%in%Europe] <- "Europe"
worldpart[data_international$Country%in%Africa] <- "Africa"
worldpart[data_international$Country%in%Asia] <- "Asia"
worldpart[data_international$Country%in%North_America] <- "North_America"
worldpart[data_international$Country%in%South_America] <- "South_America"
worldpart[data_international$Country%in%Oceania] <- "Oceania"
worldpart[data_international$Country%in%USA] <- "USA"

data_international$worldpart<-worldpart
data_international$worldpart
data_international$worldpart<-as.factor(data_international$worldpart)

# Only use Europe, Asia and USA (other groups too small)
data_international<-subset(data_international, worldpart=="Europe" | worldpart=="Asia" | worldpart=="USA")

# Create new variable: condition ------------------------------------------

data_international$Condition<-ifelse(data_international$Respondent_group=="Educated" & data_international$Target=="Educated", "Ed_Ed",
                       ifelse(data_international$Respondent_group=="Educated" & data_international$Target=="Scientists", "Ed_Sc",
                              ifelse(data_international$Respondent_group=="Scientists" & data_international$Target=="Educated", "Sc_Ed",
                                     ifelse(data_international$Respondent_group=="Scientists" & data_international$Target=="Scientists", "Sc_Sc", "unknown"))))
data_international$Condition<-as.factor(data_international$Condition)

# Find and remove outliers per worldpart -----------------------------------------------------------
## using Boxplot function from car package 

# Remove outliers per subgroup (worldparts)

## USA
data_international_USA<-subset(data_international, worldpart=="USA")

outliers<-unique(c(Boxplot(data_international_USA$Objectivity, data_international_USA$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_USA$Rationality, data_international_USA$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_USA$Openness, data_international_USA$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_USA$Intelligence, data_international_USA$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_USA$Integrity, data_international_USA$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_USA$Communality, data_international_USA$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_international_USA<-data_international_USA[-outliers,]

# ASIA
data_international_Asia<-subset(data_international, worldpart=="Asia")

outliers<-unique(c(Boxplot(data_international_Asia$Objectivity, data_international_Asia$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Asia$Rationality, data_international_Asia$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Asia$Openness, data_international_Asia$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Asia$Intelligence, data_international_Asia$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Asia$Integrity, data_international_Asia$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Asia$Communality, data_international_Asia$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_international_Asia<-data_international_Asia[-outliers,]

# EUROPE

data_international_Europe<-subset(data_international, worldpart=="Europe")

outliers<-unique(c(Boxplot(data_international_Europe$Objectivity, data_international_Europe$Condition,formula = Objectivity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Europe$Rationality, data_international_Europe$Condition,formula = Rationality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Europe$Openness, data_international_Europe$Condition,formula = Openness ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Europe$Intelligence, data_international_Europe$Condition,formula = Intelligence ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Europe$Integrity, data_international_Europe$Condition,formula = Integrity ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0),
                   
                   Boxplot(data_international_Europe$Communality, data_international_Europe$Condition,formula = Communality ~ Ed_Ed + Ed_Sc + Sc_Ed + Sc_Sc, range=2.0)))

outliers<-as.numeric(outliers)

#  Remove outliers 
data_international_Europe<-data_international_Europe[-outliers,]

# merge data_international again -----------------------------
# combine USA and Asia data_international

common.names <- intersect(colnames(data_international_USA), colnames(data_international_Asia))
data_international_usa_asia <- rbind(data_international_USA[, common.names], data_international_Asia[, common.names])

# combine USA, Asia and Europe data_international

common.names <- intersect(colnames(data_international_usa_asia), colnames(data_international_Europe))
data_international_combined <- rbind(data_international_usa_asia[, common.names], data_international_Europe[, common.names])

data_international<-data_international_combined

# Sample descriptives -----------------------------------------------------

# Educated (repeat)
sum(data_international$Respondent_group=="Educated")
round(mean(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Educated"])), 2)
# USA Scientists (repeat)
sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="USA")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"])), 2)
# Asia
sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="Asia")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"])), 2)
# Europe
sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="Europe")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"])), 2)

#  plots per worldpart (SCIENTISTS ONLY!) ---------------------------------

# Use only scientist data_international
data_international<-subset(data_international, Respondent_group=="Scientists")

# OBJECTIVITY

#barplot
data_international_Objectivity <- summarySE(data_international, measurevar="Objectivity", groupvars=c("worldpart","Target"))

Objectivity_plot_bar_international<-ggplot(data_international_Objectivity, aes(x=worldpart, y=Objectivity, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5,5.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))


Objectivity_plot_bar_international

#ggsave("Objectivity_plot_bar_international_sc_international.jpeg", Objectivity_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar_international_sc_international.pdf", Objectivity_plot_bar_international,dpi=600, width = 9, height = 6)

# Rationality

#barplot
data_international_Rationality <- summarySE(data_international, measurevar="Rationality", groupvars=c("worldpart","Target"))

Rationality_plot_bar_international<-ggplot(data_international_Rationality, aes(x=worldpart, y=Rationality, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5,6.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar_international

#ggsave("Rationality_plot_bar_international_sc_international.jpeg", Rationality_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar_international_sc_international.pdf", Rationality_plot_bar_international,dpi=600, width = 9, height = 6)

# Openness

#barplot
data_international_Openness <- summarySE(data_international, measurevar="Openness", groupvars=c("worldpart","Target"))

Openness_plot_bar_international<-ggplot(data_international_Openness, aes(x=worldpart, y=Openness, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.0,6.0)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar_international

#ggsave("Openness_plot_bar_international_sc_international.jpeg", Openness_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar_international_sc_international.pdf", Openness_plot_bar_international,dpi=600, width = 9, height = 6)

# Intelligence

#barplot
data_international_Intelligence <- summarySE(data_international, measurevar="Intelligence", groupvars=c("worldpart","Target"))

Intelligence_plot_bar_international<-ggplot(data_international_Intelligence, aes(x=worldpart, y=Intelligence, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0,5.0)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Intelligence_plot_bar_international

#ggsave("Intelligence_plot_bar_international_sc_international.jpeg", Intelligence_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar_international_sc_international.pdf", Intelligence_plot_bar_international,dpi=600, width = 9, height = 6)

# Integrity

#barplot
data_international_Integrity <- summarySE(data_international, measurevar="Integrity", groupvars=c("worldpart","Target"))

Integrity_plot_bar_international<-ggplot(data_international_Integrity, aes(x=worldpart, y=Integrity, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5,6.5)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar_international

#ggsave("Integrity_plot_bar_international_sc_international.jpeg", Integrity_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar_international_sc_international.pdf", Integrity_plot_bar_international,dpi=600, width = 9, height = 6)

# Communality

#barplot
data_international_Communality <- summarySE(data_international, measurevar="Communality", groupvars=c("worldpart","Target"))

Communality_plot_bar_international<-ggplot(data_international_Communality, aes(x=worldpart, y=Communality, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0,5.0)) +
  scale_fill_discrete(name="Target", breaks=c("Educated", "Scientists"), labels=c("A highly-educated person", "A scientist"))+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia", "Europe"),name= "Respondent group")+
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Communality_plot_bar_international

#ggsave("Communality_plot_bar_international_sc_international.jpeg", Communality_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Communality_plot_bar_international_sc_international.pdf", Communality_plot_bar_international,dpi=600, width = 9, height = 6)




# Create MULTIPANEL plots -------------------------------------------------

# multipanel plot bar plots
# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar_international)


jpeg("multipanel_plot_study_A_bars_international.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar_international + theme(legend.position="none"),
                                            Rationality_plot_bar_international + theme(legend.position="none"),
                                            Openness_plot_bar_international + theme(legend.position="none"),
                                            Intelligence_plot_bar_international + theme(legend.position="none"),
                                            Integrity_plot_bar_international + theme(legend.position="none"),
                                            Communality_plot_bar_international + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))

dev.off()

