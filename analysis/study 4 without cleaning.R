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

GET('https://osf.io/sztvg/?action=download',
    write_disk('Data_study_C_prepared.csv', overwrite = TRUE))


# Read in data ------------------------------------------------------------

Data_study_C_file_name<-"Data_study_C_prepared.csv"
data <-read.csv(Data_study_C_file_name)

names(data)
data$X<-factor(data$X)

# Create function to summarize data ---------------------------------------

## from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/ 

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


# Recode variables and create new variables -------------------------------
# Recompute Age and Academic Age variables 
data$Age <- 95-data$Year_born
data$Academic_age<-95-data$Year_phd


# Create Respondent Group variable 
data$Respondent_group<- ifelse(data$Gender ==1, "Male Scientists",ifelse(data$Gender ==2, "Female Scientists", "Unknown"))
data<-data[is.na(data$Respondent_group)==F,]
data<-data[data$Respondent_group!="Unknown",]
data$Respondent_group<-as.factor(data$Respondent_group)
data$Respondent_group <- factor(data$Respondent_group,levels(data$Respondent_group)[c(3,1:2)])


# Make factor of Target variable 
data$Target<-as.factor(data$Target)


# Exclude incomplete cases ------------------------------------------------
names(data)
complete.cases(data[,12:29])

incomplete<-which(!complete.cases(data[,12:29]))

data<-data[-incomplete,]



# Create new variables: outcome variable = mean score on each feature ------------------------
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


#check
data$Objectivity

data$Rationality  

data$Openness

data$Intelligence

data$Integrity

data$Communality


# Create condition variable -----------------------------------------------
data$Condition<-ifelse(data$Respondent_group=="Female Scientists" & data$Target=="Female Scientists", "F_F",
                       ifelse(data$Respondent_group=="Female Scientists" & data$Target=="Male Scientists", "F_M",
                              ifelse(data$Respondent_group=="Male Scientists" & data$Target=="Male Scientists", "M_M",
                                     ifelse(data$Respondent_group=="Male Scientists" & data$Target=="Female Scientists", "M_F", "unknown"))))

data$Condition<-as.factor(data$Condition)



# Find and identify outliers ----------------------------------------------

hist(data$Objectivity)

hist(data$Rationality)

hist(data$Openness)

hist(data$Intelligence)

hist(data$Integrity)

hist(data$Communality)


outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ F_F + F_M + M_F + M_M, range=2.0),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ F_F + F_M + M_F + M_M, range=2.0),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ F_F + F_M + M_F + M_M, range=2.0),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ F_F + F_M + M_F + M_M, range=2.0),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ F_F + F_M + M_F + M_M, range=2.0),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ F_F + F_M + M_F + M_M, range=2.0)))

outliers<-as.numeric(outliers)



# Remove outliers from data -----------------------------------------------
data<-data[-outliers,]

# Sample descriptives -----------------------------------------------------

sum(data$Respondent_group=="Male Scientists")
round(mean(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)

# Age of 7 must be erroneous
data$Age[497]<-NA

round(mean(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)


sum(data$Respondent_group=="Female Scientists")
round(mean(data$Age[data$Respondent_group=="Female Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Female Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Female Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Female Scientists"], na.rm=T),digits =1)

n_resp_group_F<-sum(data$Respondent_group=="Female Scientists")
n_resp_group_M<-sum(data$Respondent_group=="Male Scientists")

# Create variables containing n of Respondent groups and Targets ----------
levels(data$Target)
n_target_F<-sum(sum(data$Target=="Female Scientists"))
n_target_M<-sum(sum(data$Target=="Male Scientists"))

# Create subsets to be able to look at simple effects ---------------------
data_female_respondents<-subset(data, Respondent_group=="Female Scientists")
data_male_respondents<-subset(data, Respondent_group=="Male Scientists")


n_resp_group_F_Target_F<-sum(data_female_respondents$Target=="Female Scientists")
n_resp_group_F_Target_M<-sum(data_female_respondents$Target=="Male Scientists")
n_resp_group_M_Target_F<-sum(data_male_respondents$Target=="Female Scientists")
n_resp_group_M_Target_M<-sum(data_male_respondents$Target=="Male Scientists")


# look at number of different countries
data$Country<-as.factor(data$Country)
table(data$Country[data$Respondent_group=="Female Scientists"]) # subtract nr of countries with 0
table(data$Country[data$Respondent_group=="Male Scientists"]) # subtract nr of countries with 0


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

correlation_table_male<-corr.test(data[data$Respondent_group=="Male Scientists",40:45])
print(correlation_table_male, short=F)

correlation_table_female<-corr.test(data[data$Respondent_group=="Female Scientists",40:45])
print(correlation_table_female, short=F)

correlation_table_overall<-corr.test(data[40:45])
print(correlation_table_overall, short=F)


# Analyses and graphs -----------------------------------------------------

###### Objectivity ######

leveneTest(data$Objectivity, data$Condition, center=median) # not significant @ alpha = .008333

length(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Objectivity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Objectivity_model) 

p_value_interaction_Objectivity <- Objectivity_model$`Pr(>F)`[4]

# NO INTERACTION (alpha = 0.008), MAIN EFFECT OF TARGET
Objectivity_model_main<-Anova(lm(Objectivity ~ Respondent_group + Target, data = data),type=3)
print(Objectivity_model_main) 

p_value_MAIN_EFFECT_Target_Objectivity_Anova <- Objectivity_model_main$`Pr(>F)`[3]


## t-test for Target
t_test_Ob_main_target = t.test(data$Objectivity[data$Target == "Female Scientists"],
                  data$Objectivity[data$Target == "Male Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test_Ob_main_target
difference<-as.numeric(t_test_Ob_main_target$estimate[1])-as.numeric(t_test_Ob_main_target$estimate[2])
difference
p_value_MAIN_EFFECT_Target_Objectivity_t_test <- t_test_Ob_main_target$p.value

#effect size
tes(t=t_test_Ob_main_target$statistic, n.1=n_target_F, n.2=n_target_M)
ES_Target_Ob <- tes(t=t_test_Ob_main_target$statistic, n.1=n_target_F, n.2=n_target_M)
ES_Target_Ob_d <- ES_Target_Ob$d 
ES_Target_Ob_d_CI_lower <- ES_Target_Ob$l.d
ES_Target_Ob_d_CI_upper<- ES_Target_Ob$u.d


ES_Target_Ob_d
ES_Target_Ob_d_CI_lower
ES_Target_Ob_d_CI_upper
p_value_MAIN_EFFECT_Target_Objectivity_t_test

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
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar

# OBjects to place in graph:
# ! Here, MAIN EFFECT OF TARGET only. 
ES_Target_Ob_d
ES_Target_Ob_d_CI_lower
ES_Target_Ob_d_CI_upper
p_value_MAIN_EFFECT_Target_Objectivity_t_test


#ggsave("Objectivity_plot_bar.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

###### Rationality ######

leveneTest(data$Rationality, data$Condition, center=median)
# ! significant @ alpha = 0.008333
# ! smallest group: n = 133; largest group: n = 362 ->  largest = 2.72 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))



length(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Rationality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Rationality_model<-Anova(lm(Rationality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Rationality_model)

p_value_interaction_Rationality <- Rationality_model$`Pr(>F)`[4]


#simple effects
# ! Here no correction for unequal variance necessary because 
# within groups the size difference is much smaller than 1.5 


#Female Scientists
Rationality_model_Female_scientists<-Anova(lm(Rationality ~ Target, data = data_female_respondents), type=3)
print(Rationality_model_Female_scientists) 


#t-test to check effect
t_test_Ra_female = t.test(data_female_respondents$Rationality[data_female_respondents$Target == "Female Scientists"],
                   data_female_respondents$Rationality[data_female_respondents$Target == "Male Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_Ra_female
difference<-as.numeric(t_test_Ra_female$estimate[1])-as.numeric(t_test_Ra_female$estimate[2])
difference

p_value_t_test_female_Ra <- t_test_Ra_female$p.value

#effect size
tes(t=t_test_Ra_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Ra_female <- tes(t=t_test_Ra_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Ra_female_d<-ES_Ra_female$d
ES_Ra_female_CI_lower<-ES_Ra_female$l.d
ES_Ra_female_CI_upper<-ES_Ra_female$u.d

ES_Ra_female_d
ES_Ra_female_CI_lower
ES_Ra_female_CI_upper
p_value_t_test_female_Ra

# Male scientists
Rationality_model_Male_scientists<-Anova(lm(Rationality ~ Target, data = data_male_respondents), type=3)
print(Rationality_model_Male_scientists) 

# not significant
#t-test to check effect
t_test_Ra_male = t.test(data_male_respondents$Rationality[data_male_respondents$Target == "Female Scientists"],
                 data_male_respondents$Rationality[data_male_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test_Ra_male
difference<-as.numeric(t_test_Ra_male$estimate[1])-as.numeric(t_test_Ra_male$estimate[2])
difference

p_value_t_test_male_Ra <- t_test_Ra_male$p.value

#effect size
tes(t=t_test_Ra_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Ra_male<-tes(t=t_test_Ra_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Ra_male_d<-ES_Ra_male$d
ES_Ra_male_CI_lower<-ES_Ra_male$l.d
ES_Ra_male_CI_upper<-ES_Ra_male$u.d

ES_Ra_male_d
ES_Ra_male_CI_lower
ES_Ra_male_CI_upper
p_value_t_test_male_Ra

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
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
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
p_value_interaction_Rationality <- Rationality_model$`Pr(>F)`[4]


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = FEMALE scientists
ES_Ra_female_d
ES_Ra_female_CI_lower
ES_Ra_female_CI_upper
p_value_t_test_female_Ra

# Respondent group = MALE scientists
# ! Not significant but still in plot because important finding that we find differences
# among females but not among males
ES_Ra_male_d
ES_Ra_male_CI_lower
ES_Ra_male_CI_upper
p_value_t_test_male_Ra


#ggsave("Rationality_plot_bar.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)



###### Openness ######

leveneTest(data$Openness, data$Condition, center=median) # not significant


length(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Openness[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Openness_model<-Anova(lm(Openness ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Openness_model)

p_value_interaction_Openness <- Openness_model$`Pr(>F)`[4]


#simple effects
# ! Here no correction for unequal variance necessary because 
# within groups the size difference is much smaller than 1.5 


#Female Scientists
Openness_model_Female_scientists<-Anova(lm(Openness ~ Target, data = data_female_respondents), type=3)
print(Openness_model_Female_scientists) 


#t-test to check effect
t_test_Op_female = t.test(data_female_respondents$Openness[data_female_respondents$Target == "Female Scientists"],
                          data_female_respondents$Openness[data_female_respondents$Target == "Male Scientists"],
                          var.equal = TRUE, paired = FALSE)
t_test_Op_female
difference<-as.numeric(t_test_Op_female$estimate[1])-as.numeric(t_test_Op_female$estimate[2])
difference

p_value_t_test_female_Ra <- t_test_Op_female$p.value

#effect size
tes(t=t_test_Op_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Op_female <- tes(t=t_test_Op_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Op_female_d<-ES_Op_female$d
ES_Op_female_CI_lower<-ES_Op_female$l.d
ES_Op_female_CI_upper<-ES_Op_female$u.d

ES_Op_female_d
ES_Op_female_CI_lower
ES_Op_female_CI_upper
p_value_t_test_female_Ra

# Male scientists
Openness_model_Male_scientists<-Anova(lm(Openness ~ Target, data = data_male_respondents), type=3)
print(Openness_model_Male_scientists) 

# not significant
#t-test to check effect
t_test_Op_male = t.test(data_male_respondents$Openness[data_male_respondents$Target == "Female Scientists"],
                        data_male_respondents$Openness[data_male_respondents$Target == "Male Scientists"],
                        var.equal = TRUE, paired = FALSE)
t_test_Op_male
difference<-as.numeric(t_test_Op_male$estimate[1])-as.numeric(t_test_Op_male$estimate[2])
difference

p_value_t_test_male_Ra <- t_test_Op_male$p.value

#effect size
tes(t=t_test_Op_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Op_male<-tes(t=t_test_Op_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Op_male_d<-ES_Op_male$d
ES_Op_male_CI_lower<-ES_Op_male$l.d
ES_Op_male_CI_upper<-ES_Op_male$u.d

ES_Op_male_d
ES_Op_male_CI_lower
ES_Op_male_CI_upper
p_value_t_test_male_Ra

#barplot
data_Openness <- summarySE(data, measurevar="Openness", groupvars=c("Respondent_group","Target"))

Openness_plot_bar<-ggplot(data_Openness, aes(x=Respondent_group, y=Openness, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
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
p_value_interaction_Openness <- Openness_model$`Pr(>F)`[4]


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = FEMALE scientists
ES_Op_female_d
ES_Op_female_CI_lower
ES_Op_female_CI_upper
p_value_t_test_female_Ra

# Respondent group = MALE scientists
# ! Not significant but still in plot because important finding that we find differences
# among females but not among males
ES_Op_male_d
ES_Op_male_CI_lower
ES_Op_male_CI_upper
p_value_t_test_male_Ra


#ggsave("Openness_plot_bar.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)




###### Intelligence ######

leveneTest(data$Intelligence, data$Condition, center=median) # not significant

length(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Intelligence[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target + Respondent_group:Target, data = data),type=3)
print(Intelligence_model) 

p_value_interaction_Intelligence <- Intelligence_model$`Pr(>F)`[4]

# NO INTERACTION (alpha = 0.008), MAIN EFFECT OF TARGET AND OF RESPONDENT GROUP
Intelligence_model_main<-Anova(lm(Intelligence ~ Respondent_group + Target, data = data),type=3)
print(Intelligence_model_main) 

p_value_MAIN_EFFECT_Target_Intelligence_Anova <- Intelligence_model_main$`Pr(>F)`[3]

##t_test test for Target
t_test_Iq_Target = t.test(data$Intelligence[data$Target == "Female Scientists"],
                  data$Intelligence[data$Target == "Male Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test_Iq_Target
difference<-as.numeric(t_test_Iq_Target$estimate[1])-as.numeric(t_test_Iq_Target$estimate[2])
difference

p_value_MAIN_EFFECT_Target_Intelligence_t_test <- t_test_Ob_main_target$p.value

#effect size
tes(t=t_test_Iq_Target$statistic, n.1=n_target_F, n.2=n_target_M)
ES_Target_Iq <- tes(t=t_test_Iq_Target$statistic, n.1=n_target_F, n.2=n_target_M)
ES_Target_Iq_d <- ES_Target_Iq$d 
ES_Target_Iq_d_CI_lower <- ES_Target_Iq$l.d
ES_Target_Iq_d_CI_upper<- ES_Target_Iq$u.d


ES_Target_Iq_d
ES_Target_Iq_d_CI_lower
ES_Target_Iq_d_CI_upper
p_value_MAIN_EFFECT_Target_Intelligence_t_test




#barplot
data_Intelligence <- summarySE(data, measurevar="Intelligence", groupvars=c("Respondent_group","Target"))

Intelligence_plot_bar<-ggplot(data_Intelligence, aes(x=Respondent_group, y=Intelligence, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Intelligence_plot_bar

# OBjects to place in graph:
# ! Here, MAIN EFFECT OF TARGET only. 

ES_Target_Iq_d
ES_Target_Iq_d_CI_lower
ES_Target_Iq_d_CI_upper
p_value_MAIN_EFFECT_Target_Intelligence_t_test


#ggsave("Intelligence_plot_bar.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)



###### Integrity ######

leveneTest(data$Integrity, data$Condition, center=median)
# ! significant @ alpha = 0.008333
# ! smallest group: n = 133; largest group: n = 362 ->  largest = 2.72 times as large as smallest group
# (this is more than 1.5 a large, so Welch correction @ Anova (= white.adjust = T))

length(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Integrity[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3, white.adjust = T)
print(Integrity_model)

p_value_interaction_Integrity <- Integrity_model$`Pr(>F)`[4]


#simple effects
# ! Here no correction for unequal variance necessary because 
# within groups the size difference is much smaller than 1.5 


#Female Scientists
Integrity_model_Female_scientists<-Anova(lm(Integrity ~ Target, data = data_female_respondents), type=3)
print(Integrity_model_Female_scientists) 


#t-test to check effect
t_test_In_female = t.test(data_female_respondents$Integrity[data_female_respondents$Target == "Female Scientists"],
                          data_female_respondents$Integrity[data_female_respondents$Target == "Male Scientists"],
                          var.equal = TRUE, paired = FALSE)
t_test_In_female
difference<-as.numeric(t_test_In_female$estimate[1])-as.numeric(t_test_In_female$estimate[2])
difference

p_value_t_test_female_Ra <- t_test_In_female$p.value

#effect size
tes(t=t_test_In_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_In_female <- tes(t=t_test_In_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_In_female_d<-ES_In_female$d
ES_In_female_CI_lower<-ES_In_female$l.d
ES_In_female_CI_upper<-ES_In_female$u.d

ES_In_female_d
ES_In_female_CI_lower
ES_In_female_CI_upper
p_value_t_test_female_Ra

# Male scientists
Integrity_model_Male_scientists<-Anova(lm(Integrity ~ Target, data = data_male_respondents), type=3)
print(Integrity_model_Male_scientists) 

# not significant
#t-test to check effect
t_test_In_male = t.test(data_male_respondents$Integrity[data_male_respondents$Target == "Female Scientists"],
                        data_male_respondents$Integrity[data_male_respondents$Target == "Male Scientists"],
                        var.equal = TRUE, paired = FALSE)
t_test_In_male
difference<-as.numeric(t_test_In_male$estimate[1])-as.numeric(t_test_In_male$estimate[2])
difference

p_value_t_test_male_Ra <- t_test_In_male$p.value

#effect size
tes(t=t_test_In_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_In_male<-tes(t=t_test_In_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_In_male_d<-ES_In_male$d
ES_In_male_CI_lower<-ES_In_male$l.d
ES_In_male_CI_upper<-ES_In_male$u.d

ES_In_male_d
ES_In_male_CI_lower
ES_In_male_CI_upper
p_value_t_test_male_Ra

#barplot
data_Integrity <- summarySE(data, measurevar="Integrity", groupvars=c("Respondent_group","Target"))

Integrity_plot_bar<-ggplot(data_Integrity, aes(x=Respondent_group, y=Integrity, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
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
p_value_interaction_Integrity <- Integrity_model$`Pr(>F)`[4]


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = FEMALE scientists
ES_In_female_d
ES_In_female_CI_lower
ES_In_female_CI_upper
p_value_t_test_female_Ra

# Respondent group = MALE scientists
# ! Not significant but still in plot because important finding that we find differences
# among females but not among males
ES_In_male_d
ES_In_male_CI_lower
ES_In_male_CI_upper
p_value_t_test_male_Ra


#ggsave("Integrity_plot_bar.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)


###### Communality ######

leveneTest(data$Communality, data$Condition, center=median) # not significant

length(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"])
round(mean(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"])
round(mean(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Female Scientists" & data$Target == "Male Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"])
round(mean(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Female Scientists"]), 2)

length(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"])
round(mean(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)
round(sd(data$Communality[data$Respondent_group == "Male Scientists" & data$Target == "Male Scientists"]), 2)

#test
Communality_model<-Anova(lm(Communality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Communality_model)

p_value_interaction_Communality <- Communality_model$`Pr(>F)`[4]


#simple effects

#Female Scientists
Communality_model_Female_scientists<-Anova(lm(Communality ~ Target, data = data_female_respondents), type=3)
print(Communality_model_Female_scientists) 


#t-test to check effect
t_test_Co_female = t.test(data_female_respondents$Communality[data_female_respondents$Target == "Female Scientists"],
                          data_female_respondents$Communality[data_female_respondents$Target == "Male Scientists"],
                          var.equal = TRUE, paired = FALSE)
t_test_Co_female
difference<-as.numeric(t_test_Co_female$estimate[1])-as.numeric(t_test_Co_female$estimate[2])
difference

p_value_t_test_female_Ra <- t_test_Co_female$p.value

#effect size
tes(t=t_test_Co_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Co_female <- tes(t=t_test_Co_female$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
ES_Co_female_d<-ES_Co_female$d
ES_Co_female_CI_lower<-ES_Co_female$l.d
ES_Co_female_CI_upper<-ES_Co_female$u.d

ES_Co_female_d
ES_Co_female_CI_lower
ES_Co_female_CI_upper
p_value_t_test_female_Ra

# Male scientists
Communality_model_Male_scientists<-Anova(lm(Communality ~ Target, data = data_male_respondents), type=3)
print(Communality_model_Male_scientists) 

# ! significant
#t-test to check effect
t_test_Co_male = t.test(data_male_respondents$Communality[data_male_respondents$Target == "Female Scientists"],
                        data_male_respondents$Communality[data_male_respondents$Target == "Male Scientists"],
                        var.equal = TRUE, paired = FALSE)
t_test_Co_male
difference<-as.numeric(t_test_Co_male$estimate[1])-as.numeric(t_test_Co_male$estimate[2])
difference

p_value_t_test_male_Ra <- t_test_Co_male$p.value

#effect size
tes(t=t_test_Co_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Co_male<-tes(t=t_test_Co_male$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
ES_Co_male_d<-ES_Co_male$d
ES_Co_male_CI_lower<-ES_Co_male$l.d
ES_Co_male_CI_upper<-ES_Co_male$u.d

ES_Co_male_d
ES_Co_male_CI_lower
ES_Co_male_CI_upper
p_value_t_test_male_Ra

#barplot
data_Communality <- summarySE(data, measurevar="Communality", groupvars=c("Respondent_group","Target"))

Communality_plot_bar<-ggplot(data_Communality, aes(x=Respondent_group, y=Communality, fill=Target)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(2.5, 5.0)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"), name="Respondent group (scientists)")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Communality_plot_bar

# OBJECTS TO PLACE IN PLOTS:

# Object for Interaction:  = significant 
# p-value for interaction effect
p_value_interaction_Communality <- Communality_model$`Pr(>F)`[4]


# Objects for simple effects:
# effect sizes, lower and upper bounds of CIs, and p-values for t-tests:

# Respondent group = FEMALE scientists
ES_Co_female_d
ES_Co_female_CI_lower
ES_Co_female_CI_upper
p_value_t_test_female_Ra

# Respondent group = MALE scientists
# ! Significant 
ES_Co_male_d
ES_Co_male_CI_lower
ES_Co_male_CI_upper
p_value_t_test_male_Ra


#ggsave("Communality_plot_bar.jpeg", Communality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Communality_plot_bar.pdf", Communality_plot_bar,dpi=600, width = 9, height = 6)


# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar)

# multipanel plot bar plots
jpeg("multipanel_plot_study_C_bars.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Communality_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))

dev.off()