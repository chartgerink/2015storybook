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

outliers<-unique(c(Boxplot(data$Objectivity, data$Condition,formula = Objectivity ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5),
                   Boxplot(data$Rationality, data$Condition,formula = Rationality ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5),
                   Boxplot(data$Openness, data$Condition,formula = Openness ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5),
                   Boxplot(data$Intelligence, data$Condition,formula = Intelligence ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5),
                   Boxplot(data$Integrity, data$Condition,formula = Integrity ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5),
                   Boxplot(data$Communality, data$Condition,formula = Communality ~ Early_PhD + Early_Early + Early_Established + Established_PhD + Established_Early + Established_Established, range=1.5)))

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
Objectivity_model<-Anova(lm(Objectivity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Objectivity_model)

# check for effect of gender
#Objectivity_model_g<-Anova(lm(Objectivity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Objectivity_model_g)


#simple effects

#Early-career scientists
Objectivity_model_Early_career_scientists<-Anova(lm(Objectivity ~ Target, data = data_Early_career_scientists), type=3)
print(Objectivity_model_Early_career_scientists) 
# t-tests to see where differences lie
# a
t_test_1a = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Established Scientists"],
                  data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Early-career Scientists"],
                  var.equal = TRUE, paired = FALSE)
t_test_1a
difference<-as.numeric(t_test_1a$estimate[1])-as.numeric(t_test_1a$estimate[2])
difference
# effect size
tes(t=t_test_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)

# b
t_test_1b = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Early-career Scientists"],
                   data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1b
difference<-as.numeric(t_test_1b$estimate[1])-as.numeric(t_test_1b$estimate[2])
difference
# effect size
tes(t=t_test_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)

# c
t_test_1c = t.test(data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Objectivity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1c
difference<-as.numeric(t_test_1c$estimate[1])-as.numeric(t_test_1c$estimate[2])
difference
# effect size
tes(t=t_test_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)


#Established scientists
Objectivity_model_Established_scientists<-Anova(lm(Objectivity ~ Target, data = data_Established_scientists), type=3)
print(Objectivity_model_Established_scientists) 
# t-tests to see where differences lie
# a
t_test_2a = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_2a
difference<-as.numeric(t_test_2a$estimate[1])-as.numeric(t_test_2a$estimate[2])
difference
# effect size
tes(t=t_test_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

# b
t_test_2b = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2b
difference<-as.numeric(t_test_2b$estimate[1])-as.numeric(t_test_2b$estimate[2])
difference
# effect size
tes(t=t_test_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

# c
t_test_2c = t.test(data_Established_scientists$Objectivity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Objectivity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2c
difference<-as.numeric(t_test_2c$estimate[1])-as.numeric(t_test_2c$estimate[2])
difference
# effect size
tes(t=t_test_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)




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
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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

# check for effect of gender
#Rationality_model_g<-Anova(lm(Rationality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Rationality_model_g)


#simple effects

#Early-career scientists
Rationality_model_Early_career_scientists<-Anova(lm(Rationality ~ Target, data = data_Early_career_scientists), type=3)
print(Rationality_model_Early_career_scientists) 
# t-tests to see where differences lie
# a
t_test_1a = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_1a
difference<-as.numeric(t_test_1a$estimate[1])-as.numeric(t_test_1a$estimate[2])
difference
# effect size
tes(t=t_test_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)

# b
t_test_1b = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Early-career Scientists"],
                   data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1b
difference<-as.numeric(t_test_1b$estimate[1])-as.numeric(t_test_1b$estimate[2])
difference
# effect size
tes(t=t_test_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)

# c
t_test_1c = t.test(data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Rationality[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1c
difference<-as.numeric(t_test_1c$estimate[1])-as.numeric(t_test_1c$estimate[2])
difference
# effect size
tes(t=t_test_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)


#Established scientists
Rationality_model_Established_scientists<-Anova(lm(Rationality ~ Target, data = data_Established_scientists), type=3)
print(Rationality_model_Established_scientists) 
# t-tests to see where differences lie
# a
t_test_2a = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Rationality[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_2a
difference<-as.numeric(t_test_2a$estimate[1])-as.numeric(t_test_2a$estimate[2])
difference
# effect size
tes(t=t_test_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

# b
t_test_2b = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Rationality[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2b
difference<-as.numeric(t_test_2b$estimate[1])-as.numeric(t_test_2b$estimate[2])
difference
# effect size
tes(t=t_test_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

# c
t_test_2c = t.test(data_Established_scientists$Rationality[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Rationality[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2c
difference<-as.numeric(t_test_2c$estimate[1])-as.numeric(t_test_2c$estimate[2])
difference
# effect size
tes(t=t_test_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)




#barplot
data_Rationality <- summarySE(data, measurevar="Rationality", groupvars=c("Respondent_group","Target"))

Rationality_plot_bar<-ggplot(data_Rationality, aes(x=Target, y=Rationality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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
Openness_model<-Anova(lm(Openness ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Openness_model)

# check for effect of gender
#Openness_model_g<-Anova(lm(Openness ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Openness_model_g)


#simple effects

#Early-career scientists
Openness_model_Early_career_scientists<-Anova(lm(Openness ~ Target, data = data_Early_career_scientists), type=3)
print(Openness_model_Early_career_scientists) 
# # t-tests to see where differences lie
# # a
# t_test_1a = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Established Scientists"],
#                    data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Early-career Scientists"],
#                    var.equal = TRUE, paired = FALSE)
# t_test_1a
# difference<-as.numeric(t_test_1a$estimate[1])-as.numeric(t_test_1a$estimate[2])
# difference
# # effect size
# tes(t=t_test_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)
# 
# # b
# t_test_1b = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Early-career Scientists"],
#                    data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "PhD Students"],
#                    var.equal = TRUE, paired = FALSE)
# t_test_1b
# difference<-as.numeric(t_test_1b$estimate[1])-as.numeric(t_test_1b$estimate[2])
# difference
# # effect size
# tes(t=t_test_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)
# 
# # c
# t_test_1c = t.test(data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "Established Scientists"],
#                    data_Early_career_scientists$Openness[data_Early_career_scientists$Target == "PhD Students"],
#                    var.equal = TRUE, paired = FALSE)
# t_test_1c
# difference<-as.numeric(t_test_1c$estimate[1])-as.numeric(t_test_1c$estimate[2])
# difference
# # effect size
# tes(t=t_test_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)


#Established scientists
Openness_model_Established_scientists<-Anova(lm(Openness ~ Target, data = data_Established_scientists), type=3)
print(Openness_model_Established_scientists) 
# t-tests to see where differences lie
# a
t_test_2a = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Openness[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_2a
difference<-as.numeric(t_test_2a$estimate[1])-as.numeric(t_test_2a$estimate[2])
difference
# effect size
tes(t=t_test_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

# b
t_test_2b = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Openness[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2b
difference<-as.numeric(t_test_2b$estimate[1])-as.numeric(t_test_2b$estimate[2])
difference
# effect size
tes(t=t_test_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

# c
t_test_2c = t.test(data_Established_scientists$Openness[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Openness[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2c
difference<-as.numeric(t_test_2c$estimate[1])-as.numeric(t_test_2c$estimate[2])
difference
# effect size
tes(t=t_test_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

#barplot
data_Openness <- summarySE(data, measurevar="Openness", groupvars=c("Respondent_group","Target"))

Openness_plot_bar<-ggplot(data_Openness, aes(x=Target, y=Openness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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

# check for effect of gender
#Intelligence_model_g<-Anova(lm(Objectivity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Intelligence_model_g)


# NO INTERACTION (alpha = 0.008), MAIN EFFECT of Respondent Group
Intelligence_model<-Anova(lm(Intelligence ~ Respondent_group + Target, data = data),type = 3)
print(Intelligence_model)


##t-test for Respondent_group
t_test1 = t.test(data$Intelligence[data$Respondent_group == "Established Scientists"],
                 data$Intelligence[data$Respondent_group == "Early-career Scientists"],
                 var.equal = TRUE, paired = FALSE)
t_test1
difference<-as.numeric(t_test1$estimate[1])-as.numeric(t_test1$estimate[2])
difference
#effect size
tes(t=t_test1$statistic, n.1=n_established_scientists_repondents_all, n.2=n_early_career_scientists_repondents_all)


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
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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
Integrity_model<-Anova(lm(Integrity ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Integrity_model)

# check for effect of gender
#Integrity_model_g<-Anova(lm(Integrity ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Integrity_model_g)


#simple effects

#Early-career scientists
Integrity_model_Early_career_scientists<-Anova(lm(Integrity ~ Target, data = data_Early_career_scientists), type=3)
print(Integrity_model_Early_career_scientists) 
# t-tests to see where differences lie
# a
t_test_1a = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_1a
difference<-as.numeric(t_test_1a$estimate[1])-as.numeric(t_test_1a$estimate[2])
difference
# effect size
tes(t=t_test_1a$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_early_career_scientists_Target_EC_data)

# b
t_test_1b = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Early-career Scientists"],
                   data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1b
difference<-as.numeric(t_test_1b$estimate[1])-as.numeric(t_test_1b$estimate[2])
difference
# effect size
tes(t=t_test_1b$statistic, n.1=n_early_career_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)

# c
t_test_1c = t.test(data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "Established Scientists"],
                   data_Early_career_scientists$Integrity[data_Early_career_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_1c
difference<-as.numeric(t_test_1c$estimate[1])-as.numeric(t_test_1c$estimate[2])
difference
# effect size
tes(t=t_test_1c$statistic, n.1=n_established_scientists_Target_EC_data, n.2=n_phd_students_Target_EC_data)


#Established scientists
Integrity_model_Established_scientists<-Anova(lm(Integrity ~ Target, data = data_Established_scientists), type=3)
print(Integrity_model_Established_scientists) 
# t-tests to see where differences lie
# a
t_test_2a = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Integrity[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_2a
difference<-as.numeric(t_test_2a$estimate[1])-as.numeric(t_test_2a$estimate[2])
difference
# effect size
tes(t=t_test_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

# b
t_test_2b = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Integrity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2b
difference<-as.numeric(t_test_2b$estimate[1])-as.numeric(t_test_2b$estimate[2])
difference
# effect size
tes(t=t_test_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

# c
t_test_2c = t.test(data_Established_scientists$Integrity[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Integrity[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2c
difference<-as.numeric(t_test_2c$estimate[1])-as.numeric(t_test_2c$estimate[2])
difference
# effect size
tes(t=t_test_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)




#barplot
data_Integrity <- summarySE(data, measurevar="Integrity", groupvars=c("Respondent_group","Target"))

Integrity_plot_bar<-ggplot(data_Integrity, aes(x=Target, y=Integrity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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
Communality_model<-Anova(lm(Communality ~ Respondent_group + Target + Respondent_group:Target, data = data),type = 3)
print(Communality_model)

# check for effect of gender
#Communality_model_g<-Anova(lm(Communality ~ Respondent_group + Target + Gender + Respondent_group:Target, data = data),type = 3)
#print(Communality_model_g)


#simple effects

#Early-career scientists
Communality_model_Early_career_scientists<-Anova(lm(Communality ~ Target, data = data_Early_career_scientists), type=3)
print(Communality_model_Early_career_scientists) 

#Established scientists
Communality_model_Established_scientists<-Anova(lm(Communality ~ Target, data = data_Established_scientists), type=3)
print(Communality_model_Established_scientists) 
# t-tests to see where differences lie
# a
t_test_2a = t.test(data_Established_scientists$Communality[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Communality[data_Established_scientists$Target == "Early-career Scientists"],
                   var.equal = TRUE, paired = FALSE)
t_test_2a
difference<-as.numeric(t_test_2a$estimate[1])-as.numeric(t_test_2a$estimate[2])
difference
# effect size
tes(t=t_test_2a$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_early_career_scientists_Target_ES_data)

# b
t_test_2b = t.test(data_Established_scientists$Communality[data_Established_scientists$Target == "Early-career Scientists"],
                   data_Established_scientists$Communality[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2b
difference<-as.numeric(t_test_2b$estimate[1])-as.numeric(t_test_2b$estimate[2])
difference
# effect size
tes(t=t_test_2b$statistic, n.1=n_early_career_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)

# c
t_test_2c = t.test(data_Established_scientists$Communality[data_Established_scientists$Target == "Established Scientists"],
                   data_Established_scientists$Communality[data_Established_scientists$Target == "PhD Students"],
                   var.equal = TRUE, paired = FALSE)
t_test_2c
difference<-as.numeric(t_test_2c$estimate[1])-as.numeric(t_test_2c$estimate[2])
difference
# effect size
tes(t=t_test_2c$statistic, n.1=n_established_scientists_Target_ES_data, n.2=n_phd_students_Target_ES_data)




#barplot
data_Communality <- summarySE(data, measurevar="Communality", groupvars=c("Respondent_group","Target"))

Communality_plot_bar<-ggplot(data_Communality, aes(x=Target, y=Communality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Communality-ci, ymax=Communality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Communality") +
  ylab("Rating (1-7)") +
  coord_cartesian(ylim=c(3.5, 5.5)) +
  scale_x_discrete(breaks=c("PhD Students", "Early-career Scientists", "Established Scientists"), labels=c("PhD", "Early", "Established"))+
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


