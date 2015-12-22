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

GET('https://osf.io/cshxe/?action=download',
    write_disk('Data_study_B_prepared.csv', overwrite = TRUE))

# Read in data ------------------------------------------------------------

#read in data from csv
Data_study_B_file_name<-"Data_study_B_prepared.csv"
data <-read.csv(Data_study_B_file_name)

data$X<-factor(data$X)

# Create function to summarize data ---------------------------------------

# from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/ 

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


# Recode variables --------------------------------------------------------

data$Age <- 95-data$Year_born
data$Academic_age<-95-data$Year_phd

# Create new variable: Respondent_group -----------------------------------

data$Respondent_group<- ifelse(data$Check_phd ==2, "PhD Students",ifelse(data$Check_phd ==1 & data$Academic_age<=10 & data$Tenure==5, "Early-career Scientists", ifelse(data$Check_phd==1 & data$Academic_age>=10 & data$Tenure==4, "Established Scientists", "Unknown")))
data<-data[is.na(data$Respondent_group)==F,]
data<-data[data$Respondent_group!="Unknown",]
data$Respondent_group<-as.factor(data$Respondent_group)
data$Respondent_group <- factor(data$Respondent_group,levels(data$Respondent_group)[c(3,1:2)])

data$Target<-as.factor(data$Target)
data$Target <- factor(data$Target,levels(data$Target)[c(3,1:2)])

# Create new variables: outcome variable = mean per characteristic --------

data$Objectivity<-apply(data[,names(data)=="ob1"|
                                               names(data)=="ob2"|
                                               names(data)=="ob3"],1,mean, na.rm=T)

data$Rationality<-apply(data[,names(data)=="ra1"|
                                               names(data)=="ra2"|
                                               names(data)=="ra3"],1,mean, na.rm=T)

data$Openness<-apply(data[,names(data)=="op1"|
                                            names(data)=="op2"|
                                            names(data)=="op3"],1,mean, na.rm=T)

data$Intelligence<-apply(data[,names(data)=="iq1"|                                            
                                names(data)=="iq2"|                
                                names(data)=="iq3"],1,mean, na.rm=T)

data$Integrity<-apply(data[,names(data)=="in1"|
                                             names(data)=="in2"|
                                             names(data)=="in3"],1,mean, na.rm=T)

data$Communality<-apply(data[,names(data)=="co1"|
                               names(data)=="co2"|                
                               names(data)=="co3"],1,mean, na.rm=T)


# Remove PhD student respondents: group too small ------------------------

data<-subset(data, Respondent_group!= "PhD Students")
data$Respondent_group<-as.factor(data$Respondent_group)
data$Respondent_group<-droplevels(data$Respondent_group)

# Exclude incomplete cases ------------------------------------------------
incomplete<-which(!complete.cases(data[,13:30]))
data<-data[-incomplete,]

# Create new variable: condition ------------------------------------------

data$Condition<-ifelse(data$Respondent_group=="Early-career Scientists" & data$Target=="PhD Students", "Early_PhD",
                                            ifelse(data$Respondent_group=="Early-career Scientists" & data$Target=="Early-career Scientists", "Early_Early",
                                                   ifelse(data$Respondent_group=="Early-career Scientists" & data$Target=="Established Scientists", "Early_Established",
                                                          ifelse(data$Respondent_group=="Established Scientists" & data$Target=="PhD Students", "Established_PhD",
                                                                 ifelse(data$Respondent_group=="Established Scientists" & data$Target=="Early-career Scientists", "Established_Early",
                                                                        ifelse(data$Respondent_group=="Established Scientists" & data$Target=="Established Scientists", "Established_Established", "unknown"))))))
data$Condition<-as.factor(data$Condition)

# Find and remove outliers -----------------------------------------------------------

outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=2.0)))

outliers<-as.numeric(outliers)

data<-data[-outliers,]


# Sample descriptives -----------------------------------------------------

sum(data$Respondent_group=="Early-career Scientists")
round(mean(data$Age[data$Respondent_group=="Early-career Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Early-career Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Early-career Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Early-career Scientists"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Early-career Scientists"])), 2)

sum(data$Respondent_group=="Established Scientists")
round(mean(data$Age[data$Respondent_group=="Established Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Established Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Established Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Established Scientists"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Established Scientists"])),2)


# create variables containing cell sizes (for post-hoc tests/ effect sizes etc)
n_phd_students_Targets_all<-sum(data$Target=="PhD Students")
n_early_career_scientists_Targets_all<-sum(data$Target=="Early-career Scientists")
n_established_scientists_Targets_all<-sum(data$Target=="Established Scientists")

n_early_career_scientists_repondents_all<-sum(data$Respondent_group=="Early-career Scientists")
n_established_scientists_repondents_all<-sum(data$Respondent_group=="Established Scientists")

#create subsets to be able to look at simple effects
data_Early_career_scientists<-subset(data, Respondent_group=="Early-career Scientists")
data_Established_scientists<-subset(data, Respondent_group=="Established Scientists")

#create variables in subset to be able to conduct posthoc tests after looking at simple effects

n_phd_students_Target_EC_data<-sum(data_Early_career_scientists$Target=="PhD Students")
n_early_career_scientists_Target_EC_data<-sum(data_Early_career_scientists$Target=="Early-career Scientists")
n_established_scientists_Target_EC_data<-sum(data_Early_career_scientists$Target=="Established Scientists")


n_phd_students_Target_ES_data<-sum(data_Established_scientists$Target=="PhD Students")
n_early_career_scientists_Target_ES_data<-sum(data_Established_scientists$Target=="Early-career Scientists")
n_established_scientists_Target_ES_data<-sum(data_Established_scientists$Target=="Established Scientists")

# look at number of diffent countries
data$Country<-as.factor(data$Country)
table(data$Country[data$Respondent_group=="Early-career Scientists"]) # subtract nr of countries with 0
table(data$Country[data$Respondent_group=="Established Scientists"]) # subtract nr of countries with 0

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


# Correlation tables ------------------------------------------------------

correlation_table_early<-corr.test(data[data$Respondent_group=="Early-career Scientists",41:46])
print(correlation_table_early, short=F)

correlation_table_established<-corr.test(data[data$Respondent_group=="Established Scientists",41:46])
print(correlation_table_established, short=F)

correlation_table_overall<-corr.test(data[41:46])
print(correlation_table_overall, short=F)




# Analyses and graphs -----------------------------------------------------


###### Objectivity ######
leveneTest(data$Objectivity, data$Condition, center=median) 
# ! significant @ alpha = 0.008333
# ! smallest group: n = 167; largest group: n = 316 ->  largest = 1.89 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))

length(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Objectivity_model)

p_value_interaction_Objectivity <- Objectivity_model$`Pr(>F)`[4]

# check for effect of gender
#Objectivity_model_g<-Anova(lm(Objectivity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3, white.adjust = T)
#print(Objectivity_model_g)


#simple effects
# ! Here no correction for unequal variance necessary because 
# within groups the size difference is much smaller than 1.5 

#Early-career scientists
Objectivity_model_Early_career_scientists<-Anova(lm(Objectivity ~ Target, data = data_Early_career_scientists), type=3)
print(Objectivity_model_Early_career_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_Ob_1a = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Established Scientists"],
                  data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Early-career Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test_Ob_1a
difference<-as.numeric(t_test_Ob_1a$estimate[1])-as.numeric(t_test_Ob_1a$estimate[2])
difference
p_value_test_Ob_RespGroupEARLY_Est_Ea <- t_test_Ob_1a$p.value
# effect size
tes(t=t_test_Ob_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_Ob_RespGroupEARLY_Est_Ea <- tes(t=t_test_Ob_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_Ob_RespGroupEARLY_Est_Ea_d <- ES_Ob_RespGroupEARLY_Est_Ea$d 
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_lower <- ES_Ob_RespGroupEARLY_Est_Ea$l.d
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_upper<- ES_Ob_RespGroupEARLY_Est_Ea$u.d


ES_Ob_RespGroupEARLY_Est_Ea_d
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_lower
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Est_Ea


# b (EarlyCareer - PhD)
t_test_Ob_1b = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Early-career Scientists"],
                   data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ob_1b
difference<-as.numeric(t_test_Ob_1b$estimate[1])-as.numeric(t_test_Ob_1b$estimate[2])
difference
p_value_test_Ob_RespGroupEARLY_Ea_PhD <- t_test_Ob_1b$p.value
# effect size
tes(t=t_test_Ob_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ob_RespGroupEARLY_Ea_PhD <- tes(t=t_test_Ob_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ob_RespGroupEARLY_Ea_PhD_d <- ES_Ob_RespGroupEARLY_Ea_PhD$d 
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_lower <- ES_Ob_RespGroupEARLY_Ea_PhD$l.d
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_upper<- ES_Ob_RespGroupEARLY_Ea_PhD$u.d

ES_Ob_RespGroupEARLY_Ea_PhD_d
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Ea_PhD

# c (Established - PhD)
t_test_Ob_1c = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ob_1c
difference<-as.numeric(t_test_Ob_1c$estimate[1])-as.numeric(t_test_Ob_1c$estimate[2])
difference
p_value_test_Ob_RespGroupEARLY_Est_PhD <- t_test_Ob_1c$p.value
# effect size
tes(t=t_test_Ob_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ob_RespGroupEARLY_Est_PhD <- tes(t=t_test_Ob_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ob_RespGroupEARLY_Est_PhD_d <- ES_Ob_RespGroupEARLY_Est_PhD$d 
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_lower <- ES_Ob_RespGroupEARLY_Est_PhD$l.d
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_upper<- ES_Ob_RespGroupEARLY_Est_PhD$u.d

ES_Ob_RespGroupEARLY_Est_PhD_d
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_lower
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Est_PhD

#Established scientists
Objectivity_model_Established_scientists<-Anova(lm(Objectivity ~ Target, data = data_Established_scientists), type=3)
print(Objectivity_model_Established_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_Ob_2a = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ob_2a
difference<-as.numeric(t_test_Ob_2a$estimate[1])-as.numeric(t_test_Ob_2a$estimate[2])
difference
p_value_test_Ob_RespGroupESTABLISHED_Est_Ea <- t_test_Ob_2a$p.value
# effect size
tes(t=t_test_Ob_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

ES_Ob_RespGroupESTABLISHED_Est_Ea <- tes(t=t_test_Ob_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)
ES_Ob_RespGroupESTABLISHED_Est_Ea_d <- ES_Ob_RespGroupESTABLISHED_Est_Ea$d 
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_lower <- ES_Ob_RespGroupESTABLISHED_Est_Ea$l.d
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_upper<- ES_Ob_RespGroupESTABLISHED_Est_Ea$u.d


ES_Ob_RespGroupESTABLISHED_Est_Ea_d
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Est_Ea


# b (EarlyCareer - PhD)
t_test_Ob_2b = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ob_2b
difference<-as.numeric(t_test_Ob_2b$estimate[1])-as.numeric(t_test_Ob_2b$estimate[2])
difference
p_value_test_Ob_RespGroupESTABLISHED_Ea_PhD <- t_test_Ob_2b$p.value

# effect size
tes(t=t_test_Ob_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

ES_Ob_RespGroupESTABLISHED_Ea_PhD <- tes(t=t_test_Ob_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d <- ES_Ob_RespGroupESTABLISHED_Ea_PhD$d 
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_lower <- ES_Ob_RespGroupESTABLISHED_Ea_PhD$l.d
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_upper<- ES_Ob_RespGroupESTABLISHED_Ea_PhD$u.d


ES_Ob_RespGroupESTABLISHED_Ea_PhD_d
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Ea_PhD


# c (Established - PhD)
t_test_Ob_2c = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ob_2c
difference<-as.numeric(t_test_Ob_2c$estimate[1])-as.numeric(t_test_Ob_2c$estimate[2])
difference
p_value_test_Ob_RespGroupESTABLISHED_Est_PhD <- t_test_Ob_2c$p.value

# effect size
tes(t=t_test_Ob_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ob_RespGroupESTABLISHED_Est_PhD <- tes(t=t_test_Ob_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ob_RespGroupESTABLISHED_Est_PhD_d <- ES_Ob_RespGroupESTABLISHED_Est_PhD$d 
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_lower <- ES_Ob_RespGroupESTABLISHED_Est_PhD$l.d
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_upper<- ES_Ob_RespGroupESTABLISHED_Est_PhD$u.d


ES_Ob_RespGroupESTABLISHED_Est_PhD_d
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Est_PhD




#barplot
data_Objectivity <- summarySE(data, measurevar="Objectivity", groupvars=c("Respondent_group","Target"))

Objectivity_plot_bar<-ggplot(data_Objectivity, aes(x=Respondent_group, y=Objectivity, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar


# OBJECTS TO PLACE IN PLOTS:

# Object for Interaction:  = significant 
# p-value for interaction effect
p_value_interaction_Objectivity


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = EARLY-CAREER scientists
# difference between target Established and target Early:
ES_Ob_RespGroupEARLY_Est_Ea_d
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_lower
ES_Ob_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Est_Ea

# difference between target Early and target PhD:
ES_Ob_RespGroupEARLY_Ea_PhD_d
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_Ob_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Ea_PhD

# difference between target Established and target PhD:
ES_Ob_RespGroupEARLY_Est_PhD_d
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_lower
ES_Ob_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_Ob_RespGroupEARLY_Est_PhD

# Respondent group = ESTABLISHED scientists
# difference between target Established and target Early:
ES_Ob_RespGroupESTABLISHED_Est_Ea_d
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Est_Ea

# difference between target Early and target PhD:
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Est_Ea

# difference between target Established and target PhD:
ES_Ob_RespGroupESTABLISHED_Est_PhD_d
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Ob_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Ob_RespGroupESTABLISHED_Est_Ea


#ggsave("Objectivity_plot_bar.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

###### Rationality ######
leveneTest(data$Rationality, data$Condition, center=median) # not significant @ alpha = .008333

length(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Rationality_model<-Anova(lm(Rationality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Rationality_model)

p_value_interaction_Rationality <- Rationality_model$`Pr(>F)`[4]

# check for effect of gender
#Rationality_model_g<-Anova(lm(Rationality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Rationality_model_g)


#simple effects

#Early-career scientists
Rationality_model_Early_career_scientists<-Anova(lm(Rationality ~ Target, data = data_Early_career_scientists), type=3)
print(Rationality_model_Early_career_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_Ra_1a = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Established Scientists"],
                      data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Early-career Scientists"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_1a
difference<-as.numeric(t_test_Ra_1a$estimate[1])-as.numeric(t_test_Ra_1a$estimate[2])
difference
p_value_test_Ra_RespGroupEARLY_Est_Ea <- t_test_Ra_1a$p.value
# effect size
tes(t=t_test_Ra_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_Ra_RespGroupEARLY_Est_Ea <- tes(t=t_test_Ra_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_Ra_RespGroupEARLY_Est_Ea_d <- ES_Ra_RespGroupEARLY_Est_Ea$d 
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_lower <- ES_Ra_RespGroupEARLY_Est_Ea$l.d
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_upper<- ES_Ra_RespGroupEARLY_Est_Ea$u.d


ES_Ra_RespGroupEARLY_Est_Ea_d
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_lower
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Est_Ea


# b (EarlyCareer - PhD)
t_test_Ra_1b = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Early-career Scientists"],
                      data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_1b
difference<-as.numeric(t_test_Ra_1b$estimate[1])-as.numeric(t_test_Ra_1b$estimate[2])
difference
p_value_test_Ra_RespGroupEARLY_Ea_PhD <- t_test_Ra_1b$p.value
# effect size
tes(t=t_test_Ra_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ra_RespGroupEARLY_Ea_PhD <- tes(t=t_test_Ra_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ra_RespGroupEARLY_Ea_PhD_d <- ES_Ra_RespGroupEARLY_Ea_PhD$d 
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_lower <- ES_Ra_RespGroupEARLY_Ea_PhD$l.d
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_upper<- ES_Ra_RespGroupEARLY_Ea_PhD$u.d

ES_Ra_RespGroupEARLY_Ea_PhD_d
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Ea_PhD

# c (Established - PhD)
t_test_Ra_1c = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Established Scientists"],
                      data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_1c
difference<-as.numeric(t_test_Ra_1c$estimate[1])-as.numeric(t_test_Ra_1c$estimate[2])
difference
p_value_test_Ra_RespGroupEARLY_Est_PhD <- t_test_Ra_1c$p.value
# effect size
tes(t=t_test_Ra_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ra_RespGroupEARLY_Est_PhD <- tes(t=t_test_Ra_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Ra_RespGroupEARLY_Est_PhD_d <- ES_Ra_RespGroupEARLY_Est_PhD$d 
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_lower <- ES_Ra_RespGroupEARLY_Est_PhD$l.d
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_upper<- ES_Ra_RespGroupEARLY_Est_PhD$u.d

ES_Ra_RespGroupEARLY_Est_PhD_d
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_lower
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Est_PhD

#Established scientists
Rationality_model_Established_scientists<-Anova(lm(Rationality ~ Target, data = data_Established_scientists), type=3)
print(Rationality_model_Established_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_Ra_2a = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Rationality[data_Established_scientists$Target == "Early-career Scientists"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_2a
difference<-as.numeric(t_test_Ra_2a$estimate[1])-as.numeric(t_test_Ra_2a$estimate[2])
difference
p_value_test_Ra_RespGroupESTABLISHED_Est_Ea <- t_test_Ra_2a$p.value
# effect size
tes(t=t_test_Ra_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

ES_Ra_RespGroupESTABLISHED_Est_Ea <- tes(t=t_test_Ra_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)
ES_Ra_RespGroupESTABLISHED_Est_Ea_d <- ES_Ra_RespGroupESTABLISHED_Est_Ea$d 
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_lower <- ES_Ra_RespGroupESTABLISHED_Est_Ea$l.d
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_upper<- ES_Ra_RespGroupESTABLISHED_Est_Ea$u.d


ES_Ra_RespGroupESTABLISHED_Est_Ea_d
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Est_Ea




# b (EarlyCareer - PhD)
t_test_Ra_2b = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Early-career Scientists"],
                      data_Established_scientists$Rationality[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_2b
difference<-as.numeric(t_test_Ra_2b$estimate[1])-as.numeric(t_test_Ra_2b$estimate[2])
difference
p_value_test_Ra_RespGroupESTABLISHED_Ea_PhD <- t_test_Ra_2b$p.value

# effect size
tes(t=t_test_Ra_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

ES_Ra_RespGroupESTABLISHED_Ea_PhD <- tes(t=t_test_Ra_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d <- ES_Ra_RespGroupESTABLISHED_Ea_PhD$d 
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_lower <- ES_Ra_RespGroupESTABLISHED_Ea_PhD$l.d
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_upper<- ES_Ra_RespGroupESTABLISHED_Ea_PhD$u.d


ES_Ra_RespGroupESTABLISHED_Ea_PhD_d
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Ea_PhD


# c (Established - PhD)
t_test_Ra_2c = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Rationality[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Ra_2c
difference<-as.numeric(t_test_Ra_2c$estimate[1])-as.numeric(t_test_Ra_2c$estimate[2])
difference
p_value_test_Ra_RespGroupESTABLISHED_Est_PhD <- t_test_Ra_2c$p.value

# effect size
tes(t=t_test_Ra_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ra_RespGroupESTABLISHED_Est_PhD <- tes(t=t_test_Ra_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Ra_RespGroupESTABLISHED_Est_PhD_d <- ES_Ra_RespGroupESTABLISHED_Est_PhD$d 
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_lower <- ES_Ra_RespGroupESTABLISHED_Est_PhD$l.d
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_upper<- ES_Ra_RespGroupESTABLISHED_Est_PhD$u.d


ES_Ra_RespGroupESTABLISHED_Est_PhD_d
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Est_PhD




#barplot
data_Rationality <- summarySE(data, measurevar="Rationality", groupvars=c("Respondent_group","Target"))

Rationality_plot_bar<-ggplot(data_Rationality, aes(x=Respondent_group, y=Rationality, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.0, 6.0)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar


# OBJECTS TO PLACE IN PLOTS:

# Object for Interaction:  = significant 
# p-value for interaction effect
p_value_interaction_Rationality


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = EARLY-CAREER scientists
# difference between target Established and target Early:
ES_Ra_RespGroupEARLY_Est_Ea_d
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_lower
ES_Ra_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Est_Ea

# difference between target Early and target PhD:
ES_Ra_RespGroupEARLY_Ea_PhD_d
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_Ra_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Ea_PhD

# difference between target Established and target PhD:
ES_Ra_RespGroupEARLY_Est_PhD_d
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_lower
ES_Ra_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_Ra_RespGroupEARLY_Est_PhD

# Respondent group = ESTABLISHED scientists
# difference between target Established and target Early:
ES_Ra_RespGroupESTABLISHED_Est_Ea_d
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Est_Ea

# difference between target Early and target PhD:
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Ea_PhD

# difference between target Established and target PhD:
ES_Ra_RespGroupESTABLISHED_Est_PhD_d
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Ra_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Ra_RespGroupESTABLISHED_Est_PhD



#ggsave("Rationality_plot_bar.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)

###### Openness ######
leveneTest(data$Openness, data$Condition, center=median)
# ! significant @ alpha = 0.008333
# ! smallest group: n = 167; largest group: n = 316 ->  largest = 1.89 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))

length(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Openness[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Openness[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Openness_model<-Anova(lm(Openness ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Openness_model)

p_value_interaction_Openness <- Openness_model$`Pr(>F)`[4]

# check for effect of gender
#Openness_model_g<-Anova(lm(Openness ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3, white.adjust=T)
#print(Openness_model_g)


#simple effects

#Early-career scientists
Openness_model_Early_career_scientists<-Anova(lm(Openness ~ Target, data = data_Early_career_scientists), type=3)
print(Openness_model_Early_career_scientists) 

# NOT SIGNIFICANT
p_value_simple_effect_early_careers <- Openness_model_Early_career_scientists$`Pr(>F)`[2]
# # t-tests to see where differences lie
# # a (Established - EarlyCareer)
# t_test_Op_1a = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Established Scientists"],
#                       data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Early-career Scientists"],
#                       var.equal = TRUE, paired = FALSE)
# t_test_Op_1a
# difference<-as.numeric(t_test_Op_1a$estimate[1])-as.numeric(t_test_Op_1a$estimate[2])
# difference
# p_value_test_Op_RespGroupEARLY_Est_Ea <- t_test_Op_1a$p.value
# # effect size
# tes(t=t_test_Op_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
# ES_Op_RespGroupEARLY_Est_Ea <- tes(t=t_test_Op_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
# ES_Op_RespGroupEARLY_Est_Ea_d <- ES_Op_RespGroupEARLY_Est_Ea$d 
# ES_Op_RespGroupEARLY_Est_Ea_d_CI_lower <- ES_Op_RespGroupEARLY_Est_Ea$l.d
# ES_Op_RespGroupEARLY_Est_Ea_d_CI_upper<- ES_Op_RespGroupEARLY_Est_Ea$u.d
# 
# 
# ES_Op_RespGroupEARLY_Est_Ea_d
# ES_Op_RespGroupEARLY_Est_Ea_d_CI_lower
# ES_Op_RespGroupEARLY_Est_Ea_d_CI_upper
# p_value_test_Op_RespGroupEARLY_Est_Ea
# 
# 
# # b (EarlyCareer - PhD)
# t_test_Op_1b = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Early-career Scientists"],
#                       data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "PhD Students"],
#                       var.equal = TRUE, paired = FALSE)
# t_test_Op_1b
# difference<-as.numeric(t_test_Op_1b$estimate[1])-as.numeric(t_test_Op_1b$estimate[2])
# difference
# p_value_test_Op_RespGroupEARLY_Ea_PhD <- t_test_Op_1b$p.value
# # effect size
# tes(t=t_test_Op_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
# ES_Op_RespGroupEARLY_Ea_PhD <- tes(t=t_test_Op_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
# ES_Op_RespGroupEARLY_Ea_PhD_d <- ES_Op_RespGroupEARLY_Ea_PhD$d 
# ES_Op_RespGroupEARLY_Ea_PhD_d_CI_lower <- ES_Op_RespGroupEARLY_Ea_PhD$l.d
# ES_Op_RespGroupEARLY_Ea_PhD_d_CI_upper<- ES_Op_RespGroupEARLY_Ea_PhD$u.d
# 
# ES_Op_RespGroupEARLY_Ea_PhD_d
# ES_Op_RespGroupEARLY_Ea_PhD_d_CI_lower
# ES_Op_RespGroupEARLY_Ea_PhD_d_CI_upper
# p_value_test_Op_RespGroupEARLY_Ea_PhD
# 
# # c (Established - PhD)
# t_test_Op_1c = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Established Scientists"],
#                       data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "PhD Students"],
#                       var.equal = TRUE, paired = FALSE)
# t_test_Op_1c
# difference<-as.numeric(t_test_Op_1c$estimate[1])-as.numeric(t_test_Op_1c$estimate[2])
# difference
# p_value_test_Op_RespGroupEARLY_Est_PhD <- t_test_Op_1c$p.value
# # effect size
# tes(t=t_test_Op_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
# ES_Op_RespGroupEARLY_Est_PhD <- tes(t=t_test_Op_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
# ES_Op_RespGroupEARLY_Est_PhD_d <- ES_Op_RespGroupEARLY_Est_PhD$d 
# ES_Op_RespGroupEARLY_Est_PhD_d_CI_lower <- ES_Op_RespGroupEARLY_Est_PhD$l.d
# ES_Op_RespGroupEARLY_Est_PhD_d_CI_upper<- ES_Op_RespGroupEARLY_Est_PhD$u.d
# 
# ES_Op_RespGroupEARLY_Est_PhD_d
# ES_Op_RespGroupEARLY_Est_PhD_d_CI_lower
# ES_Op_RespGroupEARLY_Est_PhD_d_CI_upper
# p_value_test_Op_RespGroupEARLY_Est_PhD

#Established scientists
Openness_model_Established_scientists<-Anova(lm(Openness ~ Target, data = data_Established_scientists), type=3)
print(Openness_model_Established_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_Op_2a = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Openness[data_Established_scientists$Target == "Early-career Scientists"],
                      var.equal = TRUE, paired = FALSE)
t_test_Op_2a
difference<-as.numeric(t_test_Op_2a$estimate[1])-as.numeric(t_test_Op_2a$estimate[2])
difference
p_value_test_Op_RespGroupESTABLISHED_Est_Ea <- t_test_Op_2a$p.value
# effect size
tes(t=t_test_Op_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

ES_Op_RespGroupESTABLISHED_Est_Ea <- tes(t=t_test_Op_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)
ES_Op_RespGroupESTABLISHED_Est_Ea_d <- ES_Op_RespGroupESTABLISHED_Est_Ea$d 
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_lower <- ES_Op_RespGroupESTABLISHED_Est_Ea$l.d
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_upper<- ES_Op_RespGroupESTABLISHED_Est_Ea$u.d


ES_Op_RespGroupESTABLISHED_Est_Ea_d
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Est_Ea




# b (EarlyCareer - PhD)
t_test_Op_2b = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Early-career Scientists"],
                      data_Established_scientists$Openness[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Op_2b
difference<-as.numeric(t_test_Op_2b$estimate[1])-as.numeric(t_test_Op_2b$estimate[2])
difference
p_value_test_Op_RespGroupESTABLISHED_Ea_PhD <- t_test_Op_2b$p.value

# effect size
tes(t=t_test_Op_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

ES_Op_RespGroupESTABLISHED_Ea_PhD <- tes(t=t_test_Op_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Op_RespGroupESTABLISHED_Ea_PhD_d <- ES_Op_RespGroupESTABLISHED_Ea_PhD$d 
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_lower <- ES_Op_RespGroupESTABLISHED_Ea_PhD$l.d
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_upper<- ES_Op_RespGroupESTABLISHED_Ea_PhD$u.d


ES_Op_RespGroupESTABLISHED_Ea_PhD_d
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Ea_PhD


# c (Established - PhD)
t_test_Op_2c = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Openness[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_Op_2c
difference<-as.numeric(t_test_Op_2c$estimate[1])-as.numeric(t_test_Op_2c$estimate[2])
difference
p_value_test_Op_RespGroupESTABLISHED_Est_PhD <- t_test_Op_2c$p.value

# effect size
tes(t=t_test_Op_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Op_RespGroupESTABLISHED_Est_PhD <- tes(t=t_test_Op_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_Op_RespGroupESTABLISHED_Est_PhD_d <- ES_Op_RespGroupESTABLISHED_Est_PhD$d 
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_lower <- ES_Op_RespGroupESTABLISHED_Est_PhD$l.d
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_upper<- ES_Op_RespGroupESTABLISHED_Est_PhD$u.d


ES_Op_RespGroupESTABLISHED_Est_PhD_d
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Est_PhD




#barplot
data_Openness <- summarySE(data, measurevar="Openness", groupvars=c("Respondent_group","Target"))

Openness_plot_bar<-ggplot(data_Openness, aes(x=Respondent_group, y=Openness, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.0, 6.0)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar


# OBJECTS TO PLACE IN PLOTS:

# Object for Interaction:  = significant 
# p-value for interaction effect
p_value_interaction_Openness


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = EARLY-CAREER scientists
# ! simple effect not sifnificant in this respondent group
p_value_simple_effect_early_careers 

# Respondent group = ESTABLISHED scientists
# difference between target Established and target Early:
ES_Op_RespGroupESTABLISHED_Est_Ea_d
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_Op_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Est_Ea

# difference between target Early and target PhD:
ES_Op_RespGroupESTABLISHED_Ea_PhD_d
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_Op_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Est_Ea

# difference between target Established and target PhD:
ES_Op_RespGroupESTABLISHED_Est_PhD_d
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_Op_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_Op_RespGroupESTABLISHED_Est_Ea

#ggsave("Openness_plot_bar.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)


###### Intelligence ######
leveneTest(data$Intelligence, data$Condition, center=median)

length(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Intelligence_model)
# ! NOT SIGNIFICANT

p_value_interaction_Intelligence <- Intelligence_model$`Pr(>F)`[4]

# check for effect of gender
#Intelligence_model_g<-Anova(lm(Intelligence ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Intelligence_model_g)


# NO INTERACTION (alpha = 0.008), MAIN EFFECT of Respondent Group
Intelligence_model_main<-Anova(lm(Intelligence ~ Respondent_group + Target, data = data),type = 3)
print(Intelligence_model_main)

p_value_MAIN_EFFECT_Respondent_group_Intelligence_Anova <- Intelligence_model_main$`Pr(>F)`[2]

##t-test for Respondent_group
t_test_Iq_Resp_group = t.test(data$Intelligence[data$Respondent_group == "Established Scientists"],
                 data$Intelligence[data$Respondent_group == "Early-career Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test_Iq_Resp_group
difference<-as.numeric(t_test_Iq_Resp_group$estimate[1])-as.numeric(t_test_Iq_Resp_group$estimate[2])
difference

p_value_MAIN_EFFECT_Respondent_group_Intelligence_t_test <- t_test_Iq_Resp_group$p.value
#effect size
tes(t=t_test_Iq_Resp_group$statistic, n.1=n_established_scientists_repondents_all, n.2=n_early_career_scientists_repondents_all)

ES_Respondent_group <- tes(t=t_test_Iq_Resp_group$statistic, n.1=n_established_scientists_repondents_all, n.2=n_early_career_scientists_repondents_all)
ES_Respondent_group_d <- ES_Respondent_group$d 
ES_Respondent_group_d_CI_lower <- ES_Respondent_group$l.d
ES_Respondent_group_d_CI_upper<- ES_Respondent_group$u.d


ES_Respondent_group_d
ES_Respondent_group_d_CI_lower
ES_Respondent_group_d_CI_upper
p_value_MAIN_EFFECT_Respondent_group_Intelligence_t_test



#barplot
data_Intelligence <- summarySE(data, measurevar="Intelligence", groupvars=c("Respondent_group","Target"))

Intelligence_plot_bar<-ggplot(data_Intelligence, aes(x=Respondent_group, y=Intelligence, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0, 5.0)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))


Intelligence_plot_bar

# OBjects to place in graph:
# ! Here, MAIN EFFECT OF RESPONDENT GROUP only. # perhaps leave out of graph? Not mentioned in text 
ES_Respondent_group_d
ES_Respondent_group_d_CI_lower
ES_Respondent_group_d_CI_upper
p_value_MAIN_EFFECT_Respondent_group_Intelligence_t_test



#ggsave("Intelligence_plot_bar.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)




###### Integrity ######
leveneTest(data$Integrity, data$Condition, center=median)
# ! significant @ alpha = 0.008333
# ! smallest group: n = 167; largest group: n = 316 ->  largest = 1.89 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))

length(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Integrity_model)

p_value_interaction_Integrity <- Integrity_model$`Pr(>F)`[4]

# check for effect of gender
#Integrity_model_g<-Anova(lm(Integrity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3, white.adjust=T)
#print(Integrity_model_g)


#simple effects

#simple effects
# ! Here no correction for unequal variance necessary because 
# within groups the size difference is much smaller than 1.5 

#Early-career scientists
Integrity_model_Early_career_scientists<-Anova(lm(Integrity ~ Target, data = data_Early_career_scientists), type=3)
print(Integrity_model_Early_career_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_In_1a = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Established Scientists"],
                      data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Early-career Scientists"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_1a
difference<-as.numeric(t_test_In_1a$estimate[1])-as.numeric(t_test_In_1a$estimate[2])
difference
p_value_test_In_RespGroupEARLY_Est_Ea <- t_test_In_1a$p.value
# effect size
tes(t=t_test_In_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_In_RespGroupEARLY_Est_Ea <- tes(t=t_test_In_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
ES_In_RespGroupEARLY_Est_Ea_d <- ES_In_RespGroupEARLY_Est_Ea$d 
ES_In_RespGroupEARLY_Est_Ea_d_CI_lower <- ES_In_RespGroupEARLY_Est_Ea$l.d
ES_In_RespGroupEARLY_Est_Ea_d_CI_upper<- ES_In_RespGroupEARLY_Est_Ea$u.d


ES_In_RespGroupEARLY_Est_Ea_d
ES_In_RespGroupEARLY_Est_Ea_d_CI_lower
ES_In_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_In_RespGroupEARLY_Est_Ea


# b (EarlyCareer - PhD)
t_test_In_1b = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Early-career Scientists"],
                      data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_1b
difference<-as.numeric(t_test_In_1b$estimate[1])-as.numeric(t_test_In_1b$estimate[2])
difference
p_value_test_In_RespGroupEARLY_Ea_PhD <- t_test_In_1b$p.value
# effect size
tes(t=t_test_In_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_In_RespGroupEARLY_Ea_PhD <- tes(t=t_test_In_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_In_RespGroupEARLY_Ea_PhD_d <- ES_In_RespGroupEARLY_Ea_PhD$d 
ES_In_RespGroupEARLY_Ea_PhD_d_CI_lower <- ES_In_RespGroupEARLY_Ea_PhD$l.d
ES_In_RespGroupEARLY_Ea_PhD_d_CI_upper<- ES_In_RespGroupEARLY_Ea_PhD$u.d

ES_In_RespGroupEARLY_Ea_PhD_d
ES_In_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_In_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_In_RespGroupEARLY_Ea_PhD

# c (Established - PhD)
t_test_In_1c = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Established Scientists"],
                      data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_1c
difference<-as.numeric(t_test_In_1c$estimate[1])-as.numeric(t_test_In_1c$estimate[2])
difference
p_value_test_In_RespGroupEARLY_Est_PhD <- t_test_In_1c$p.value
# effect size
tes(t=t_test_In_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_In_RespGroupEARLY_Est_PhD <- tes(t=t_test_In_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_In_RespGroupEARLY_Est_PhD_d <- ES_In_RespGroupEARLY_Est_PhD$d 
ES_In_RespGroupEARLY_Est_PhD_d_CI_lower <- ES_In_RespGroupEARLY_Est_PhD$l.d
ES_In_RespGroupEARLY_Est_PhD_d_CI_upper<- ES_In_RespGroupEARLY_Est_PhD$u.d

ES_In_RespGroupEARLY_Est_PhD_d
ES_In_RespGroupEARLY_Est_PhD_d_CI_lower
ES_In_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_In_RespGroupEARLY_Est_PhD

#Established scientists
Integrity_model_Established_scientists<-Anova(lm(Integrity ~ Target, data = data_Established_scientists), type=3)
print(Integrity_model_Established_scientists) 
# t-tests to see where differences lie
# a (Established - EarlyCareer)
t_test_In_2a = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Integrity[data_Established_scientists$Target == "Early-career Scientists"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_2a
difference<-as.numeric(t_test_In_2a$estimate[1])-as.numeric(t_test_In_2a$estimate[2])
difference
p_value_test_In_RespGroupESTABLISHED_Est_Ea <- t_test_In_2a$p.value
# effect size
tes(t=t_test_In_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

ES_In_RespGroupESTABLISHED_Est_Ea <- tes(t=t_test_In_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)
ES_In_RespGroupESTABLISHED_Est_Ea_d <- ES_In_RespGroupESTABLISHED_Est_Ea$d 
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_lower <- ES_In_RespGroupESTABLISHED_Est_Ea$l.d
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_upper<- ES_In_RespGroupESTABLISHED_Est_Ea$u.d


ES_In_RespGroupESTABLISHED_Est_Ea_d
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Est_Ea




# b (EarlyCareer - PhD)
t_test_In_2b = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Early-career Scientists"],
                      data_Established_scientists$Integrity[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_2b
difference<-as.numeric(t_test_In_2b$estimate[1])-as.numeric(t_test_In_2b$estimate[2])
difference
p_value_test_In_RespGroupESTABLISHED_Ea_PhD <- t_test_In_2b$p.value

# effect size
tes(t=t_test_In_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

ES_In_RespGroupESTABLISHED_Ea_PhD <- tes(t=t_test_In_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_In_RespGroupESTABLISHED_Ea_PhD_d <- ES_In_RespGroupESTABLISHED_Ea_PhD$d 
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_lower <- ES_In_RespGroupESTABLISHED_Ea_PhD$l.d
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_upper<- ES_In_RespGroupESTABLISHED_Ea_PhD$u.d


ES_In_RespGroupESTABLISHED_Ea_PhD_d
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Ea_PhD


# c (Established - PhD)
t_test_In_2c = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Established Scientists"],
                      data_Established_scientists$Integrity[data_Established_scientists$Target == "PhD Students"],
                      var.equal = TRUE, paired = FALSE)
t_test_In_2c
difference<-as.numeric(t_test_In_2c$estimate[1])-as.numeric(t_test_In_2c$estimate[2])
difference
p_value_test_In_RespGroupESTABLISHED_Est_PhD <- t_test_In_2c$p.value

# effect size
tes(t=t_test_In_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_In_RespGroupESTABLISHED_Est_PhD <- tes(t=t_test_In_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)
ES_In_RespGroupESTABLISHED_Est_PhD_d <- ES_In_RespGroupESTABLISHED_Est_PhD$d 
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_lower <- ES_In_RespGroupESTABLISHED_Est_PhD$l.d
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_upper<- ES_In_RespGroupESTABLISHED_Est_PhD$u.d


ES_In_RespGroupESTABLISHED_Est_PhD_d
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Est_PhD




#barplot
data_Integrity <- summarySE(data, measurevar="Integrity", groupvars=c("Respondent_group","Target"))

Integrity_plot_bar<-ggplot(data_Integrity, aes(x=Respondent_group, y=Integrity, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4.5, 6.5)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar


# OBJECTS TO PLACE IN PLOTS:

# Object for Interaction:  = significant 
# p-value for interaction effect
p_value_interaction_Integrity


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = EARLY-CAREER scientists
# difference between target Established and target Early:
ES_In_RespGroupEARLY_Est_Ea_d
ES_In_RespGroupEARLY_Est_Ea_d_CI_lower
ES_In_RespGroupEARLY_Est_Ea_d_CI_upper
p_value_test_In_RespGroupEARLY_Est_Ea

# difference between target Early and target PhD:
ES_In_RespGroupEARLY_Ea_PhD_d
ES_In_RespGroupEARLY_Ea_PhD_d_CI_lower
ES_In_RespGroupEARLY_Ea_PhD_d_CI_upper
p_value_test_In_RespGroupEARLY_Ea_PhD

# difference between target Established and target PhD:
ES_In_RespGroupEARLY_Est_PhD_d
ES_In_RespGroupEARLY_Est_PhD_d_CI_lower
ES_In_RespGroupEARLY_Est_PhD_d_CI_upper
p_value_test_In_RespGroupEARLY_Est_PhD

# Respondent group = ESTABLISHED scientists
# difference between target Established and target Early:
ES_In_RespGroupESTABLISHED_Est_Ea_d
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_lower
ES_In_RespGroupESTABLISHED_Est_Ea_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Est_Ea

# difference between target Early and target PhD:
ES_In_RespGroupESTABLISHED_Ea_PhD_d
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_lower
ES_In_RespGroupESTABLISHED_Ea_PhD_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Est_Ea

# difference between target Established and target PhD:
ES_In_RespGroupESTABLISHED_Est_PhD_d
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_lower
ES_In_RespGroupESTABLISHED_Est_PhD_d_CI_upper
p_value_test_In_RespGroupESTABLISHED_Est_Ea


#ggsave("Integrity_plot_bar.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)


###### Communality ######
leveneTest(data$Communality, data$Condition, center=median)
# ! significant @ alpha = 0.008333
# ! smallest group: n = 167; largest group: n = 316 ->  largest = 1.89 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))

length(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"])
round(mean(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"])
round(mean(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Communality[data$Respondent_group == "Early-career Scientists" & data$Target == "PhD Students"]), 2)

length(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"])
round(mean(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Early-career Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"])
round(mean(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "Established Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"])
round(mean(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)
round(sd(data$Communality[data$Respondent_group == "Established Scientists" & data$Target == "PhD Students"]), 2)

#test
Communality_model<-Anova(lm(Communality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Communality_model)
# ! NOT SIGNIFICANT

p_value_interaction_Communality <- Communality_model$`Pr(>F)`[4]

# check for effect of gender
#Communality_model_g<-Anova(lm(Communality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Communality_model_g)


# NO INTERACTION (alpha = 0.008), check  main effects
Communality_model_main<-Anova(lm(Communality ~ Respondent_group + Target, data = data),type = 3)
print(Communality_model_main)
# Main effect of Target
p_value_MAIN_EFFECT_Target_Communality_Anova <- Communality_model_main$`Pr(>F)`[3]

# See where differences lie (in t-test, var.equal = false)
# a (Established - EarlyCareer)
t_test_Co_1a = t.test(data$Communality[data$Target == "Established Scientists"],
                      data$Communality[data$Target == "Early-career Scientists"],
                      var.equal = FALSE, paired = FALSE)
t_test_Co_1a
difference<-as.numeric(t_test_Co_1a$estimate[1])-as.numeric(t_test_Co_1a$estimate[2])
difference
p_value_test_Co_t_test_Target_Est_Ea <- t_test_Co_1a$p.value
# effect size
tes(t=t_test_Co_1a$statistic, n.1=n_established_scientists_Targets_all, n.2=n_early_career_scientists_Targets_all)
ES_Co_t_test_Target_Est_Ea <- tes(t=t_test_Co_1a$statistic, n.1=n_established_scientists_Targets_all, n.2=n_early_career_scientists_Targets_all)
ES_Co_t_test_Target_Est_Ea_d <- ES_Co_t_test_Target_Est_Ea$d 
ES_Co_t_test_Target_Est_Ea_d_CI_lower <- ES_Co_t_test_Target_Est_Ea$l.d
ES_Co_t_test_Target_Est_Ea_d_CI_upper<- ES_Co_t_test_Target_Est_Ea$u.d


ES_Co_t_test_Target_Est_Ea_d
ES_Co_t_test_Target_Est_Ea_d_CI_lower
ES_Co_t_test_Target_Est_Ea_d_CI_upper
p_value_test_Co_t_test_Target_Est_Ea


# b (EarlyCareer - PhD)
t_test_Co_1b = t.test(data$Communality[data$Target == "Early-career Scientists"],
                      data$Communality[data$Target == "PhD Students"],
                      var.equal = FALSE, paired = FALSE)
t_test_Co_1b
difference<-as.numeric(t_test_Co_1b$estimate[1])-as.numeric(t_test_Co_1b$estimate[2])
difference
p_value_test_Co_t_test_Target_Ea_PhD <- t_test_Co_1b$p.value
# effect size
tes(t=t_test_Co_1b$statistic, n.1=n_early_career_scientists_Targets_all, n.2=n_phd_students_Targets_all)
ES_Co_t_test_Target_Ea_PhD <- tes(t=t_test_Co_1b$statistic, n.1=n_early_career_scientists_Targets_all, n.2=n_phd_students_Targets_all)
ES_Co_t_test_Target_Ea_PhD_d <- ES_Co_t_test_Target_Ea_PhD$d 
ES_Co_t_test_Target_Ea_PhD_d_CI_lower <- ES_Co_t_test_Target_Ea_PhD$l.d
ES_Co_t_test_Target_Ea_PhD_d_CI_upper<- ES_Co_t_test_Target_Ea_PhD$u.d

ES_Co_t_test_Target_Ea_PhD_d
ES_Co_t_test_Target_Ea_PhD_d_CI_lower
ES_Co_t_test_Target_Ea_PhD_d_CI_upper
p_value_test_Co_t_test_Target_Ea_PhD

# c (Established - PhD)
t_test_Co_1c = t.test(data$Communality[data$Target == "Established Scientists"],
                      data$Communality[data$Target == "PhD Students"],
                      var.equal = FALSE, paired = FALSE)
t_test_Co_1c
difference<-as.numeric(t_test_Co_1c$estimate[1])-as.numeric(t_test_Co_1c$estimate[2])
difference
p_value_test_Co_t_test_Target_Est_PhD <- t_test_Co_1c$p.value
# effect size
tes(t=t_test_Co_1c$statistic, n.1=n_established_scientists_Targets_all, n.2=n_phd_students_Targets_all)
ES_Co_t_test_Target_Est_PhD <- tes(t=t_test_Co_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
ES_Co_t_test_Target_Est_PhD_d <- ES_Co_t_test_Target_Est_PhD$d 
ES_Co_t_test_Target_Est_PhD_d_CI_lower <- ES_Co_t_test_Target_Est_PhD$l.d
ES_Co_t_test_Target_Est_PhD_d_CI_upper<- ES_Co_t_test_Target_Est_PhD$u.d

ES_Co_t_test_Target_Est_PhD_d
ES_Co_t_test_Target_Est_PhD_d_CI_lower
ES_Co_t_test_Target_Est_PhD_d_CI_upper
p_value_test_Co_t_test_Target_Est_PhD


#barplot
data_Communality <- summarySE(data, measurevar="Communality", groupvars=c("Respondent_group","Target"))

Communality_plot_bar<-ggplot(data_Communality, aes(x=Respondent_group, y=Communality, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.0, 5.0)) +
  scale_x_discrete(breaks=c("Early-career Scientists", "Established Scientists"), labels=c("Early-career", "Established"), name = "Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))


Communality_plot_bar

# OBjects to place in graph:
# ! Here, MAIN EFFECT OF TARGET only. # perhaps leave out of graph? Not mentioned in text 
p_value_MAIN_EFFECT_Target_Communality_Anova <- Communality_model_main$`Pr(>F)`[3]

#difference Established - Early-career
ES_Co_t_test_Target_Est_Ea_d
ES_Co_t_test_Target_Est_Ea_d_CI_lower
ES_Co_t_test_Target_Est_Ea_d_CI_upper
p_value_test_Co_t_test_Target_Est_Ea

# difference EarlyCareer - PhD
ES_Co_t_test_Target_Ea_PhD_d
ES_Co_t_test_Target_Ea_PhD_d_CI_lower
ES_Co_t_test_Target_Ea_PhD_d_CI_upper
p_value_test_Co_t_test_Target_Ea_PhD

# difference Established - PhD
ES_Co_t_test_Target_Est_PhD_d
ES_Co_t_test_Target_Est_PhD_d_CI_lower
ES_Co_t_test_Target_Est_PhD_d_CI_upper
p_value_test_Co_t_test_Target_Est_PhD



#ggsave("Communality_plot_bar.jpeg", Communality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Communality_plot_bar.pdf", Communality_plot_bar,dpi=600, width = 9, height = 6)


# Create multipanel plot --------------------------------------------------



#install.packages("gridExtra")
library(gridExtra)

# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar)

# multipanel plot bar plots
jpeg("multipanel_plot_study_B_bars.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Communality_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))

dev.off()


