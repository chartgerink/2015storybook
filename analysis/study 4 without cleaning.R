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


outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ F_F + F_M + M_F + M_M, range=1.5),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ F_F + F_M + M_F + M_M, range=1.5),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ F_F + F_M + M_F + M_M, range=1.5),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ F_F + F_M + M_F + M_M, range=1.5),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ F_F + F_M + M_F + M_M, range=1.5),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ F_F + F_M + M_F + M_M, range=1.5)))

outliers<-as.numeric(outliers)



# Remove outliers from data -----------------------------------------------
data<-data[-outliers,]

# Sample descriptives -----------------------------------------------------

sum(data$Respondent_group=="Male Scientists")
round(mean(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Male Scientists"], na.rm=T),digits =1)
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

leveneTest(data$Objectivity, data$Condition, center=median)

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
# NO INTERACTION (alpha = 0.008), MAIN EFFECT OF TARGET
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target, data = data),type=3)
print(Objectivity_model) 


## t-test for Target
t_test1 = t.test(data$Objectivity[data$Target == "Female Scientists"],
                  data$Objectivity[data$Target == "Male Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
tes(t=t_test1$statistic, n.1=n_target_F, n.2=n_target_M)

#barplot
data_Objectivity <- summarySE(data, measurevar="Objectivity", groupvars=c("Respondent_group","Target"))

Objectivity_plot_bar<-ggplot(data_Objectivity, aes(x=Target, y=Objectivity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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

leveneTest(data$Rationality, data$Condition, center=median)

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
Rationality_model<-Anova(lm(Rationality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Rationality_model)

#simple effects

#Female Scientists
Rationality_model_Female_scientists<-Anova(lm(Rationality ~ Target, data = data_female_respondents), type=3)
print(Rationality_model_Female_scientists) 
#t-test to check effect
t_test1 = t.test(data_female_respondents$Rationality[data_female_respondents$Target == "Female Scientists"],
                   data_female_respondents$Rationality[data_female_respondents$Target == "Male Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
effect1<-tes(t=t_test1$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
d1<-effect1$d
ld1<-effect1$l.d
ud1<-effect1$u.d
d1
ld1
ud1

# Male scientists
Rationality_model_Male_scientists<-Anova(lm(Rationality ~ Target, data = data_male_respondents), type=3)
print(Rationality_model_Male_scientists) 
# not significant
#t-test to check effect
t_test2 = t.test(data_male_respondents$Rationality[data_male_respondents$Target == "Female Scientists"],
                 data_male_respondents$Rationality[data_male_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test2
difference<-as.numeric(t_test2$estimate[1])-as.numeric(t_test2$estimate[2])
difference

#effect size
effect2<-tes(t=t_test2$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
d2<-effect2$d
ld2<-effect2$l.d
ud2<-effect2$u.d
d2
ld2
ud2

diff_d<-d1-d2
diff_d


#barplot
data_Rationality <- summarySE(data, measurevar="Rationality", groupvars=c("Respondent_group","Target"))
data$Rationality
Rationality_plot_bar<-ggplot(data_Rationality, aes(x=Target, y=Rationality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(4, 6)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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

leveneTest(data$Openness, data$Condition, center=median)

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


#simple effects

#Female Scientists
Openness_model_Female_scientists<-Anova(lm(Openness ~ Target, data = data_female_respondents), type=3)
print(Openness_model_Female_scientists) 
#t-test to check effect
t_test1 = t.test(data_female_respondents$Openness[data_female_respondents$Target == "Female Scientists"],
                 data_female_respondents$Openness[data_female_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
effect1<-tes(t=t_test1$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
d1<-effect1$d
ld1<-effect1$l.d
ud1<-effect1$u.d
d1
ld1
ud1

# Male scientists
Openness_model_Male_scientists<-Anova(lm(Openness ~ Target, data = data_male_respondents), type=3)
print(Openness_model_Male_scientists) 
# not significant
#t-test to check effect
t_test2 = t.test(data_male_respondents$Openness[data_male_respondents$Target == "Female Scientists"],
                 data_male_respondents$Openness[data_male_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test2
difference<-as.numeric(t_test2$estimate[1])-as.numeric(t_test2$estimate[2])
difference

#effect size
effect2<-tes(t=t_test2$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
d2<-effect2$d
ld2<-effect2$l.d
ud2<-effect2$u.d
d2
ld2
ud2

diff_d<-d1-d2
diff_d



#barplot
data_Openness <- summarySE(data, measurevar="Openness", groupvars=c("Respondent_group","Target"))
data$Openness
Openness_plot_bar<-ggplot(data_Openness, aes(x=Target, y=Openness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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




###### Intelligence ######

leveneTest(data$Intelligence, data$Condition, center=median)

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
# NO INTERACTION (alpha = 0.008), MAIN EFFECT OF TARGET AND OF RESPONDENT GROUP
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target, data = data),type=3)
print(Intelligence_model) 

##t_test test for Target
t_test1 = t.test(data$Intelligence[data$Target == "Female Scientists"],
                  data$Intelligence[data$Target == "Male Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
tes(t=t_test1$statistic, n.1=n_target_F, n.2=n_target_M)




#barplot
data_Intelligence <- summarySE(data, measurevar="Intelligence", groupvars=c("Respondent_group","Target"))

Intelligence_plot_bar<-ggplot(data_Intelligence, aes(x=Target, y=Intelligence, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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



###### Integrity ######

leveneTest(data$Integrity, data$Condition, center=median)

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
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Integrity_model)

#simple effects

#Female Scientists
Integrity_model_Female_scientists<-Anova(lm(Integrity ~ Target, data = data_female_respondents), type=3)
print(Integrity_model_Female_scientists) 
#t-test to check effect
t_test1 = t.test(data_female_respondents$Integrity[data_female_respondents$Target == "Female Scientists"],
                 data_female_respondents$Integrity[data_female_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
effect1<-tes(t=t_test1$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
d1<-effect1$d
ld1<-effect1$l.d
ud1<-effect1$u.d
d1
ld1
ud1

# Male scientists
Integrity_model_Male_scientists<-Anova(lm(Integrity ~ Target, data = data_male_respondents), type=3)
print(Integrity_model_Male_scientists) 
# not significant
#t-test to check effect
t_test2 = t.test(data_male_respondents$Integrity[data_male_respondents$Target == "Female Scientists"],
                 data_male_respondents$Integrity[data_male_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test2
difference<-as.numeric(t_test2$estimate[1])-as.numeric(t_test2$estimate[2])
difference

#effect size
effect2<-tes(t=t_test2$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
d2<-effect2$d
ld2<-effect2$l.d
ud2<-effect2$u.d
d2
ld2
ud2

diff_d<-d1-d2
diff_d


#barplot
data_Integrity <- summarySE(data, measurevar="Integrity", groupvars=c("Respondent_group","Target"))
data$Integrity
Integrity_plot_bar<-ggplot(data_Integrity, aes(x=Target, y=Integrity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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


###### Communality ######

leveneTest(data$Communality, data$Condition, center=median)

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

#simple effects

#Female Scientists
Communality_model_Female_scientists<-Anova(lm(Communality ~ Target, data = data_female_respondents), type=3)
print(Communality_model_Female_scientists) 
#t-test to check effect
t_test1 = t.test(data_female_respondents$Communality[data_female_respondents$Target == "Female Scientists"],
                 data_female_respondents$Communality[data_female_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference

#effect size
effect1<-tes(t=t_test1$statistic, n.1=n_resp_group_F_Target_F, n.2=n_resp_group_F_Target_M)
d1<-effect1$d
ld1<-effect1$l.d
ud1<-effect1$u.d
d1
ld1
ud1

# Male scientists
Communality_model_Male_scientists<-Anova(lm(Communality ~ Target, data = data_male_respondents), type=3)
print(Communality_model_Male_scientists) 
#t-test to check effect
t_test2 = t.test(data_male_respondents$Communality[data_male_respondents$Target == "Female Scientists"],
                 data_male_respondents$Communality[data_male_respondents$Target == "Male Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test2
difference<-as.numeric(t_test2$estimate[1])-as.numeric(t_test2$estimate[2])
difference

#effect size
effect2<-tes(t=t_test2$statistic, n.1=n_resp_group_M_Target_F, n.2=n_resp_group_M_Target_M)
d2<-effect2$d
ld2<-effect2$l.d
ud2<-effect2$u.d
d2
ld2
ud2

diff_d<-d1-d2
diff_d

#barplot
data_Communality <- summarySE(data, measurevar="Communality", groupvars=c("Respondent_group","Target"))
data$Communality
Communality_plot_bar<-ggplot(data_Communality, aes(x=Target, y=Communality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(2.5, 5)) +
  scale_x_discrete(breaks=c("Female Scientists", "Male Scientists"), labels=c("Female", "Male"))+
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