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

GET('https://osf.io/ewz9h/?action=download',
    write_disk('Data_study_D_prepared.csv', overwrite = TRUE))

GET('https://osf.io/qst86/?action=download',
    write_disk('Data_study_D_prepared_international.csv', overwrite = TRUE))


# read in data ------------------------------------------------------------

Data_Study_D_file_name<-"Data_study_D_prepared.csv"
data <-read.csv(Data_Study_D_file_name)

data$X<-factor(data$X)

# Create function to summarize data ---------------------------------------

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
incomplete<-which(!complete.cases(data[,9:68]))
data<-data[-incomplete,]

# Create new variables: means of non-scientist professions  ---------------

# compute mean rating for 9 non-scientists professions
data$mean_non_scientists_Objectivity<-apply(data[,names(data)=="Ob_lawyers"|
                                                   names(data)=="Ob_politicians"|
                                                   names(data)=="Ob_journalists"|
                                                   names(data)=="Ob_med_doctors"|
                                                   names(data)=="Ob_accountants"|
                                                   names(data)=="Ob_army_lieutenants"|
                                                   names(data)=="Ob_bankers"|
                                                   names(data)=="Ob_judges"|
                                                   names(data)=="Ob_detectives"],1,mean, na.rm=T)

data$mean_non_scientists_Rationality<-apply(data[,names(data)=="Ra_lawyers"|
                                                   names(data)=="Ra_politicians"|
                                                   names(data)=="Ra_journalists"|
                                                   names(data)=="Ra_med_doctors"|
                                                   names(data)=="Ra_accountants"|
                                                   names(data)=="Ra_army_lieutenants"|
                                                   names(data)=="Ra_bankers"|
                                                   names(data)=="Ra_judges"|
                                                   names(data)=="Ra_detectives"],1,mean, na.rm=T)

data$mean_non_scientists_Openness<-apply(data[,names(data)=="Op_lawyers"|
                                                names(data)=="Op_politicians"|
                                                names(data)=="Op_journalists"|
                                                names(data)=="Op_med_doctors"|
                                                names(data)=="Op_accountants"|
                                                names(data)=="Op_army_lieutenants"|
                                                names(data)=="Op_bankers"|
                                                names(data)=="Op_judges"|
                                                names(data)=="Op_detectives"],1,mean, na.rm=T)


data$mean_non_scientists_Intelligence<-apply(data[,names(data)=="Iq_lawyers"|
                                                    names(data)=="Iq_politicians"|
                                                    names(data)=="Iq_journalists"|
                                                    names(data)=="Iq_med_doctors"|
                                                    names(data)=="Iq_accountants"|
                                                    names(data)=="Iq_army_lieutenants"|
                                                    names(data)=="Iq_bankers"|
                                                    names(data)=="Iq_judges"|
                                                    names(data)=="Iq_detectives"],1,mean, na.rm=T)

data$mean_non_scientists_Integrity<-apply(data[,names(data)=="In_lawyers"|
                                                 names(data)=="In_politicians"|
                                                 names(data)=="In_journalists"|
                                                 names(data)=="In_med_doctors"|
                                                 names(data)=="In_accountants"|
                                                 names(data)=="In_army_lieutenants"|
                                                 names(data)=="In_bankers"|
                                                 names(data)=="In_judges"|
                                                 names(data)=="In_detectives"],1,mean, na.rm=T)

data$mean_non_scientists_Competitiveness<-apply(data[,names(data)=="Co_lawyers"|
                                                       names(data)=="Co_politicians"|
                                                       names(data)=="Co_journalists"|
                                                       names(data)=="Co_med_doctors"|
                                                       names(data)=="Co_accountants"|
                                                       names(data)=="Co_army_lieutenants"|
                                                       names(data)=="Co_bankers"|
                                                       names(data)=="Co_judges"|
                                                       names(data)=="Co_detectives"],1,mean, na.rm=T)

# Create new variables: means of scientist profession  --------------------

## = renaming only

data$mean_scientists_Objectivity<-data$Ob_scientists
data$mean_scientists_Rationality<-data$Ra_scientists
data$mean_scientists_Openness<-data$Op_scientists
data$mean_scientists_Intelligence<-data$Iq_scientists
data$mean_scientists_Integrity<-data$In_scientists
data$mean_scientists_Competitiveness<-data$Co_scientists

# Find outliers -----------------------------------------------------------
# using Boxplot function from car package
# create variable containing outliers

outliers<-unique(c(Boxplot(data$mean_non_scientists_Objectivity, data$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Objectivity, data$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_non_scientists_Rationality, data$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Rationality, data$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_non_scientists_Openness, data$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Openness, data$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_non_scientists_Intelligence, data$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Intelligence, data$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_non_scientists_Integrity, data$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Integrity, data$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_non_scientists_Competitiveness, data$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data$mean_scientists_Competitiveness, data$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))



# display outliers
outliers<-as.numeric(outliers)

# Remove outliers ---------------------------------------------------------

data<-data[-outliers,]

# Sample descriptives -----------------------------------------------------
# see the "Sample descriptives" in the SUPPLEMENT for exact numbers.

# nr of respondents in the Educated group
sum(data$Respondent_group=="Educated")

# nr of respondents in the Scientist group
sum(data$Respondent_group=="Scientists")

round(mean(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
prop.table(table(data$Gender[data$Respondent_group=="Scientists"]))

round(mean(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(sd(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
prop.table(table(data$Gender[data$Respondent_group=="Educated"]))

# Scale reliabilities -----------------------------------------------------

# = check whether pooling non-scientist professions made sense: 
# can they indeed be used as a scale? 

Objectivity_non_scientists_scale<-data[,c("Ob_lawyers", "Ob_politicians", "Ob_journalists", "Ob_med_doctors", "Ob_accountants","Ob_army_lieutenants", "Ob_bankers","Ob_judges","Ob_detectives" )]
alpha(Objectivity_non_scientists_scale)

Rationality_non_scientists_scale<-data[,c("Ra_lawyers", "Ra_politicians", "Ra_journalists", "Ra_med_doctors", "Ra_accountants","Ra_army_lieutenants", "Ra_bankers","Ra_judges","Ra_detectives" )]
alpha(Rationality_non_scientists_scale)

Openness_non_scientists_scale<-data[,c("Op_lawyers", "Op_politicians", "Op_journalists", "Op_med_doctors", "Op_accountants","Op_army_lieutenants", "Op_bankers","Op_judges","Op_detectives" )]
alpha(Openness_non_scientists_scale)

Intelligence_non_scientists_scale<-data[,c("Iq_lawyers", "Iq_politicians", "Iq_journalists", "Iq_med_doctors", "Iq_accountants","Iq_army_lieutenants", "Iq_bankers","Iq_judges","Iq_detectives" )]
alpha(Intelligence_non_scientists_scale)

Integrity_non_scientists_scale<-data[,c("In_lawyers", "In_politicians", "In_journalists", "In_med_doctors", "In_accountants","In_army_lieutenants", "In_bankers","In_judges","In_detectives" )]
alpha(Integrity_non_scientists_scale)

Competitiveness_non_scientists_scale<-data[,c("Co_lawyers", "Co_politicians", "Co_journalists", "Co_med_doctors", "Co_accountants","Co_army_lieutenants", "Co_bankers","Co_judges","Co_detectives" )]
alpha(Competitiveness_non_scientists_scale)

# Correlations between the features ----------------------------------------

# for Educated professions
correlation_table_educated<-corr.test(data[75:80])
print(correlation_table_educated, short=F)
# for scientist profession
correlation_table_scientists<-corr.test(data[81:86])
print(correlation_table_scientists, short=F)

# Change dataframe into long format ---------------------------------------

# 2 rows per participant (data has one within-subjects variable)
#add id variable
data$id<-1:nrow(data)

#OBJECTIVITY
#long format
data_Objectivity<-melt(data[,c("id","mean_scientists_Objectivity",
                               "mean_non_scientists_Objectivity",
                               "Respondent_group",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)

# check long format
data_Objectivity <- rename(data_Objectivity, c(variable="Profession"))
data_Objectivity <- rename(data_Objectivity, c(value="Objectivity"))
data_Objectivity$Gender<-as.factor(data_Objectivity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Objectivity$profession_category<-as.factor(ifelse(data_Objectivity$Profession =="mean_scientists_Objectivity",1,0))

#RATIONALITY
#long format
data_Rationality<-melt(data[,c("id","mean_scientists_Rationality",
                               "mean_non_scientists_Rationality",
                               "Respondent_group",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)

# check long format
data_Rationality <- rename(data_Rationality, c(variable="Profession"))
data_Rationality <- rename(data_Rationality, c(value="Rationality"))
data_Rationality$Gender<-as.factor(data_Rationality$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Rationality$profession_category<-as.factor(ifelse(data_Rationality$Profession =="mean_scientists_Rationality",1,0))

# OPENNESS
#long format
data_Openness<-melt(data[,c("id","mean_scientists_Openness",
                            "mean_non_scientists_Openness",
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness")],
                    id=c("id",
                         "Respondent_group",
                         "Age",
                         "Gender",
                         "Religiousness"),na.rm=T)


# check long format
data_Openness <- rename(data_Openness, c(variable="Profession"))
data_Openness <- rename(data_Openness, c(value="Openness"))
data_Openness$Gender<-as.factor(data_Openness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Openness$profession_category<-as.factor(ifelse(data_Openness$Profession =="mean_scientists_Openness",1,0))

# INTELLIGENCE
#long format
data_Intelligence<-melt(data[,c("id","mean_scientists_Intelligence",
                                "mean_non_scientists_Intelligence",
                                "Respondent_group",
                                "Age",
                                "Gender",
                                "Religiousness")],
                        id=c("id",
                             "Respondent_group",
                             "Age",
                             "Gender",
                             "Religiousness"),na.rm=T)


# check long format
data_Intelligence <- rename(data_Intelligence, c(variable="Profession"))
data_Intelligence <- rename(data_Intelligence, c(value="Intelligence"))
data_Intelligence$Gender<-as.factor(data_Intelligence$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Intelligence$profession_category<-as.factor(ifelse(data_Intelligence$Profession =="mean_scientists_Intelligence",1,0))

# INTEGRITY
#long format
data_Integrity<-melt(data[,c("id","mean_scientists_Integrity",
                             "mean_non_scientists_Integrity",
                             "Respondent_group",
                             "Age",
                             "Gender",
                             "Religiousness")],
                     id=c("id",
                          "Respondent_group",
                          "Age",
                          "Gender",
                          "Religiousness"),na.rm=T)


# check long format
data_Integrity <- rename(data_Integrity, c(variable="Profession"))
data_Integrity <- rename(data_Integrity, c(value="Integrity"))
data_Integrity$Gender<-as.factor(data_Integrity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Integrity$profession_category<-as.factor(ifelse(data_Integrity$Profession =="mean_scientists_Integrity",1,0))

# COMPETITIVENESS
#long format
data_Competitiveness<-melt(data[,c("id","mean_scientists_Competitiveness",
                                   "mean_non_scientists_Competitiveness",
                                   "Respondent_group",
                                   "Age",
                                   "Gender",
                                   "Religiousness")],
                           id=c("id",
                                "Respondent_group",
                                "Age",
                                "Gender",
                                "Religiousness"),na.rm=T)

# check long format
data_Competitiveness <- rename(data_Competitiveness, c(variable="Profession"))
data_Competitiveness <- rename(data_Competitiveness, c(value="Competitiveness"))
data_Competitiveness$Gender<-as.factor(data_Competitiveness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Competitiveness$profession_category<-as.factor(ifelse(data_Competitiveness$Profession =="mean_scientists_Competitiveness",1,0))

# Create variables containing n of Respondent groups ----------------------
n_resp_group_E<-sum(data$Respondent_group=="Educated")
n_resp_group_S<-sum(data$Respondent_group=="Scientists")

# Create subsets to be able to look at simple effects ---------------------
data_Objectivity_Educated_respondents<-subset(data_Objectivity, Respondent_group=="Educated")
data_Objectivity_Scientist_respondents<-subset(data_Objectivity, Respondent_group=="Scientists")

data_Rationality_Educated_respondents<-subset(data_Rationality, Respondent_group=="Educated")
data_Rationality_Scientist_respondents<-subset(data_Rationality, Respondent_group=="Scientists")

data_Openness_Educated_respondents<-subset(data_Openness, Respondent_group=="Educated")
data_Openness_Scientist_respondents<-subset(data_Openness, Respondent_group=="Scientists")

data_Intelligence_Educated_respondents<-subset(data_Intelligence, Respondent_group=="Educated")
data_Intelligence_Scientist_respondents<-subset(data_Intelligence, Respondent_group=="Scientists")

data_Integrity_Educated_respondents<-subset(data_Integrity, Respondent_group=="Educated")
data_Integrity_Scientist_respondents<-subset(data_Integrity, Respondent_group=="Scientists")

data_Competitiveness_Educated_respondents<-subset(data_Competitiveness, Respondent_group=="Educated")
data_Competitiveness_Scientist_respondents<-subset(data_Competitiveness, Respondent_group=="Scientists")

# Analyses and graphs -----------------------------------------------------

# comparing MEAN OF OTHER PROFESSIONS TO ScIENTIST PROFESSION 
###### OBJECTIVITY #######

#Look at means per cell
means<-aggregate(data_Objectivity$Objectivity,by=list(profession_category=data_Objectivity$profession_category,respondent_group=data_Objectivity$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Objectivity[data_Objectivity$Respondent_group=="Scientists" & data_Objectivity$profession_category==1 ,"Objectivity"]),2)
round(stat.desc(data_Objectivity[data_Objectivity$Respondent_group=="Scientists" & data_Objectivity$profession_category==0 ,"Objectivity"]),2)
round(stat.desc(data_Objectivity[data_Objectivity$Respondent_group=="Educated" & data_Objectivity$profession_category==1 ,"Objectivity"]),2)
round(stat.desc(data_Objectivity[data_Objectivity$Respondent_group=="Educated" & data_Objectivity$profession_category==0 ,"Objectivity"]),2)

#fit model with interaction
m1.1 <-lme(fixed=Objectivity ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Objectivity)
summary(m1.1)

p_value_interaction_Objectivity <- summary(m1.1)$tTable[4,"p-value"]


#check for effect of gender
#m1.1g <-lme(fixed=Objectivity ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Objectivity)
#summary(m1.1g)

# If significant interaction: simple effects

# Scientists respondents
Objectivity_model_scientists <-lme(fixed=Objectivity ~ profession_category, random= ~ 1|id , data=data_Objectivity_Scientist_respondents)
summary(Objectivity_model_scientists)
# If significant, t-test to check effect
t_test1_Ob_Sc = t.test(data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 1],
                 data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1_Ob_Sc)

p_value_simple_eff_Ob_Sc<-t_test1_Ob_Sc$p.value
#effect size
t<-t_test1_Ob_Sc$statistic[[1]]
df<-t_test1_Ob_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Ob_Sc<-t*sqrt(1/(df+1))
mean(data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 1] - data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 0] )
r
d_Ob_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_Ob_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Objectivity_model_scientists)$varFix[2,2])))
lci_d_Ob_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Ob_Sc

t_uci_d<-(as.numeric(t_test1_Ob_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Objectivity_model_scientists)$varFix[2,2])))
uci_d_Ob_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Ob_Sc

# Educated respondents 
Objectivity_model_Educated <-lme(fixed=Objectivity ~ profession_category, random= ~ 1|id , data=data_Objectivity_Educated_respondents)
summary(Objectivity_model_Educated)

# If significant, t-test to check effect
t_test2_Ob_Ed = t.test(data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 1],
                 data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2_Ob_Ed)
p_value_simple_eff_Ob_Ed<-t_test2_Ob_Ed$p.value

#effect size
t<-t_test2_Ob_Ed$statistic[[1]]
df<-t_test2_Ob_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Ob_Ed<-t*sqrt(1/(df+1))
mean(data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 1] -
     data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 0])
r
d_Ob_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_Ob_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Objectivity_model_Educated)$varFix[2,2])))
lci_d_Ob_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Ob_Ed

t_uci_d<-(as.numeric(t_test2_Ob_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Objectivity_model_Educated)$varFix[2,2])))
uci_d_Ob_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Ob_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_Ob_Sc-d_Ob_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Objectivity ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Objectivity)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Objectivity$Objectivity[data_Objectivity$profession_category == 1],
#                  data_Objectivity$Objectivity[data_Objectivity$profession_category == 0],
#                  var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a
# 

#barplot

plot_data_Objectivity <- summarySE(data_Objectivity, measurevar="Objectivity", groupvars=c("Respondent_group","profession_category"))

Objectivity_plot_bar<-ggplot(plot_data_Objectivity, aes(x=Respondent_group, y=Objectivity, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar

# Object with p-value of interaction effect
p_value_interaction_Objectivity

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Objectivity:

# Respondent group: Scientists
d_Ob_Sc
lci_d_Ob_Sc
uci_d_Ob_Sc
p_value_simple_eff_Ob_Sc

# Respondent group: Educated
d_Ob_Ed
lci_d_Ob_Ed
uci_d_Ob_Ed
p_value_simple_eff_Ob_Ed



#ggsave("Objectivity_plot_bar.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)


###### RATIONALITY #######

#Look at means per cell
means<-aggregate(data_Rationality$Rationality,by=list(profession_category=data_Rationality$profession_category,respondent_group=data_Rationality$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Rationality[data_Rationality$Respondent_group=="Scientists" & data_Rationality$profession_category==1 ,"Rationality"]), 2)
round(stat.desc(data_Rationality[data_Rationality$Respondent_group=="Scientists" & data_Rationality$profession_category==0 ,"Rationality"]), 2)
round(stat.desc(data_Rationality[data_Rationality$Respondent_group=="Educated" & data_Rationality$profession_category==1 ,"Rationality"]), 2)
round(stat.desc(data_Rationality[data_Rationality$Respondent_group=="Educated" & data_Rationality$profession_category==0 ,"Rationality"]), 2)

#fit model with interaction
m2.1 <-lme(fixed=Rationality ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Rationality)
summary(m2.1)

p_value_interaction_Rationality <- summary(m2.1)$tTable[4,"p-value"]

#check for effect of gender
#m1.1g <-lme(fixed=Rationality ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Rationality)
#summary(m1.1g)

# If significant interaction: simple effects

# Scientists respondents
Rationality_model_scientists <-lme(fixed=Rationality ~ profession_category, random= ~ 1|id , data=data_Rationality_Scientist_respondents)
summary(Rationality_model_scientists)
# If significant, t-test to check effect
t_test1_Ra_Sc = t.test(data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 1],
                       data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test1_Ra_Sc)

p_value_simple_eff_Ra_Sc<-t_test1_Ra_Sc$p.value
#effect size
t<-t_test1_Ra_Sc$statistic[[1]]
df<-t_test1_Ra_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Ra_Sc<-t*sqrt(1/(df+1))
mean(data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 1] - data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 0] )
r
d_Ra_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_Ra_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Rationality_model_scientists)$varFix[2,2])))
lci_d_Ra_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Ra_Sc

t_uci_d<-(as.numeric(t_test1_Ra_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Rationality_model_scientists)$varFix[2,2])))
uci_d_Ra_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Ra_Sc

# Educated respondents 
Rationality_model_Educated <-lme(fixed=Rationality ~ profession_category, random= ~ 1|id , data=data_Rationality_Educated_respondents)
summary(Rationality_model_Educated)

# If significant, t-test to check effect
t_test2_Ra_Ed = t.test(data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 1],
                       data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test2_Ra_Ed)
p_value_simple_eff_Ra_Ed<-t_test2_Ra_Ed$p.value

#effect size
t<-t_test2_Ra_Ed$statistic[[1]]
df<-t_test2_Ra_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Ra_Ed<-t*sqrt(1/(df+1))
mean(data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 1] -
       data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 0])
r
d_Ra_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_Ra_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Rationality_model_Educated)$varFix[2,2])))
lci_d_Ra_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Ra_Ed

t_uci_d<-(as.numeric(t_test2_Ra_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Rationality_model_Educated)$varFix[2,2])))
uci_d_Ra_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Ra_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_Ra_Sc-d_Ra_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Rationality ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Rationality)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Rationality$Rationality[data_Rationality$profession_category == 1],
#                   data_Rationality$Rationality[data_Rationality$profession_category == 0],
#                   var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a
# 

#barplot
plot_data_Rationality <- summarySE(data_Rationality, measurevar="Rationality", groupvars=c("Respondent_group","profession_category"))

Rationality_plot_bar<-ggplot(plot_data_Rationality, aes(x=Respondent_group, y=Rationality, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar


# Object with p-value of interaction effect
p_value_interaction_Rationality

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Rationality:

# Respondent group: Scientists
d_Ra_Sc
lci_d_Ra_Sc
uci_d_Ra_Sc
p_value_simple_eff_Ra_Sc

# Respondent group: Educated
d_Ra_Ed
lci_d_Ra_Ed
uci_d_Ra_Ed
p_value_simple_eff_Ra_Ed



#ggsave("Rationality_plot_bar.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)


###### OPENNESS ########

#Look at means per cell
means<-aggregate(data_Openness$Openness,by=list(profession_category=data_Openness$profession_category,respondent_group=data_Openness$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Openness[data_Openness$Respondent_group=="Scientists" & data_Openness$profession_category==1 ,"Openness"]), 2)
round(stat.desc(data_Openness[data_Openness$Respondent_group=="Scientists" & data_Openness$profession_category==0 ,"Openness"]), 2)
round(stat.desc(data_Openness[data_Openness$Respondent_group=="Educated" & data_Openness$profession_category==1 ,"Openness"]), 2)
round(stat.desc(data_Openness[data_Openness$Respondent_group=="Educated" & data_Openness$profession_category==0 ,"Openness"]), 2)

#fit model with interaction
m3.1 <-lme(fixed=Openness ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Openness)
summary(m3.1)

p_value_interaction_Openness <- summary(m3.1)$tTable[4,"p-value"]

#check for effect of gender
# m3.1g <-lme(fixed=Openness ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Openness)
# summary(m3.1g)


# If significant interaction: simple effects

# Scientists respondents
Openness_model_scientists <-lme(fixed=Openness ~ profession_category, random= ~ 1|id , data=data_Openness_Scientist_respondents)
summary(Openness_model_scientists)
# If significant, t-test to check effect
t_test1_Op_Sc = t.test(data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 1],
                       data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test1_Op_Sc)

p_value_simple_eff_Op_Sc<-t_test1_Op_Sc$p.value
#effect size
t<-t_test1_Op_Sc$statistic[[1]]
df<-t_test1_Op_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Op_Sc<-t*sqrt(1/(df+1))
mean(data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 1] - data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 0] )
r
d_Op_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_Op_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Openness_model_scientists)$varFix[2,2])))
lci_d_Op_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Op_Sc

t_uci_d<-(as.numeric(t_test1_Op_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Openness_model_scientists)$varFix[2,2])))
uci_d_Op_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Op_Sc

# Educated respondents 
Openness_model_Educated <-lme(fixed=Openness ~ profession_category, random= ~ 1|id , data=data_Openness_Educated_respondents)
summary(Openness_model_Educated)

# If significant, t-test to check effect
t_test2_Op_Ed = t.test(data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 1],
                       data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test2_Op_Ed)
p_value_simple_eff_Op_Ed<-t_test2_Op_Ed$p.value

#effect size
t<-t_test2_Op_Ed$statistic[[1]]
df<-t_test2_Op_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Op_Ed<-t*sqrt(1/(df+1))
mean(data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 1] -
       data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 0])
r
d_Op_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_Op_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Openness_model_Educated)$varFix[2,2])))
lci_d_Op_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Op_Ed

t_uci_d<-(as.numeric(t_test2_Op_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Openness_model_Educated)$varFix[2,2])))
uci_d_Op_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Op_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_Op_Sc-d_Op_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Openness ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Openness)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Openness$Openness[data_Openness$profession_category == 1],
#                   data_Openness$Openness[data_Openness$profession_category == 0],
#                   var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a
# 


#barplot
plot_data_Openness <- summarySE(data_Openness, measurevar="Openness", groupvars=c("Respondent_group","profession_category"))


Openness_plot_bar<-ggplot(plot_data_Openness, aes(x=Respondent_group, y=Openness, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar


# Object with p-value of interaction effect
p_value_interaction_Openness

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Openness:

# Respondent group: Scientists
d_Op_Sc
lci_d_Op_Sc
uci_d_Op_Sc
p_value_simple_eff_Op_Sc

# Respondent group: Educated
d_Op_Ed
lci_d_Op_Ed
uci_d_Op_Ed
p_value_simple_eff_Op_Ed



#ggsave("Openness_plot_bar.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)



###### INTELLIGENCE #######

#Look at means per cell
means<-aggregate(data_Intelligence$Intelligence,by=list(profession_category=data_Intelligence$profession_category,respondent_group=data_Intelligence$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Intelligence[data_Intelligence$Respondent_group=="Scientists" & data_Intelligence$profession_category==1 ,"Intelligence"]), 2)
round(stat.desc(data_Intelligence[data_Intelligence$Respondent_group=="Scientists" & data_Intelligence$profession_category==0 ,"Intelligence"]), 2)
round(stat.desc(data_Intelligence[data_Intelligence$Respondent_group=="Educated" & data_Intelligence$profession_category==1 ,"Intelligence"]), 2)
round(stat.desc(data_Intelligence[data_Intelligence$Respondent_group=="Educated" & data_Intelligence$profession_category==0 ,"Intelligence"]), 2)

#fit model with interaction
m4.1 <-lme(fixed=Intelligence ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Intelligence)
summary(m4.1)

p_value_interaction_Intelligence <- summary(m4.1)$tTable[4,"p-value"]


#check for effect of gender
#m4.1g <-lme(fixed=Intelligence ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Intelligence)
#summary(m4.1g)


# If significant interaction: simple effects

# Scientists respondents
Intelligence_model_scientists <-lme(fixed=Intelligence ~ profession_category, random= ~ 1|id , data=data_Intelligence_Scientist_respondents)
summary(Intelligence_model_scientists)
# If significant, t-test to check effect
t_test1_Iq_Sc = t.test(data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 1],
                       data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test1_Iq_Sc)

p_value_simple_eff_Iq_Sc<-t_test1_Iq_Sc$p.value
#effect size
t<-t_test1_Iq_Sc$statistic[[1]]
df<-t_test1_Iq_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Iq_Sc<-t*sqrt(1/(df+1))
mean(data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 1] - data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 0] )
r
d_Iq_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_Iq_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Intelligence_model_scientists)$varFix[2,2])))
lci_d_Iq_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Iq_Sc

t_uci_d<-(as.numeric(t_test1_Iq_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Intelligence_model_scientists)$varFix[2,2])))
uci_d_Iq_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Iq_Sc

# Educated respondents 
Intelligence_model_Educated <-lme(fixed=Intelligence ~ profession_category, random= ~ 1|id , data=data_Intelligence_Educated_respondents)
summary(Intelligence_model_Educated)

# If significant, t-test to check effect
t_test2_Iq_Ed = t.test(data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 1],
                       data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test2_Iq_Ed)
p_value_simple_eff_Iq_Ed<-t_test2_Iq_Ed$p.value

#effect size
t<-t_test2_Iq_Ed$statistic[[1]]
df<-t_test2_Iq_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Iq_Ed<-t*sqrt(1/(df+1))
mean(data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 1] -
       data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 0])
r
d_Iq_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_Iq_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Intelligence_model_Educated)$varFix[2,2])))
lci_d_Iq_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Iq_Ed

t_uci_d<-(as.numeric(t_test2_Iq_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Intelligence_model_Educated)$varFix[2,2])))
uci_d_Iq_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Iq_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_Iq_Sc-d_Iq_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Intelligence ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Intelligence)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Intelligence$Intelligence[data_Intelligence$profession_category == 1],
#                   data_Intelligence$Intelligence[data_Intelligence$profession_category == 0],
#                   var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a
# 


#barplot
plot_data_Intelligence <- summarySE(data_Intelligence, measurevar="Intelligence", groupvars=c("Respondent_group","profession_category"))


Intelligence_plot_bar<-ggplot(plot_data_Intelligence, aes(x=Respondent_group, y=Intelligence, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))


Intelligence_plot_bar

# Object with p-value of interaction effect
p_value_interaction_Intelligence

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Intelligence:

# Respondent group: Scientists
d_Iq_Sc
lci_d_Iq_Sc
uci_d_Iq_Sc
p_value_simple_eff_Iq_Sc

# Respondent group: Educated
d_Iq_Ed
lci_d_Iq_Ed
uci_d_Iq_Ed
p_value_simple_eff_Iq_Ed



#ggsave("Intelligence_plot_bar.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)

###### INTEGRITY #######


#Look at means per cell
means<-aggregate(data_Integrity$Integrity,by=list(profession_category=data_Integrity$profession_category,respondent_group=data_Integrity$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Integrity[data_Integrity$Respondent_group=="Scientists" & data_Integrity$profession_category==1 ,"Integrity"]), 2)
round(stat.desc(data_Integrity[data_Integrity$Respondent_group=="Scientists" & data_Integrity$profession_category==0 ,"Integrity"]), 2)
round(stat.desc(data_Integrity[data_Integrity$Respondent_group=="Educated" & data_Integrity$profession_category==1 ,"Integrity"]), 2)
round(stat.desc(data_Integrity[data_Integrity$Respondent_group=="Educated" & data_Integrity$profession_category==0 ,"Integrity"]), 2)


#fit model with interaction
m5.1 <-lme(fixed=Integrity ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Integrity)
summary(m5.1)

p_value_interaction_Integrity <- summary(m5.1)$tTable[4,"p-value"]


#check for effect of gender
# m5.1g <-lme(fixed=Integrity ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Integrity)
# summary(m5.1g)


# If significant interaction: simple effects

# Scientists respondents
Integrity_model_scientists <-lme(fixed=Integrity ~ profession_category, random= ~ 1|id , data=data_Integrity_Scientist_respondents)
summary(Integrity_model_scientists)
# If significant, t-test to check effect
t_test1_In_Sc = t.test(data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 1],
                       data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test1_In_Sc)

p_value_simple_eff_In_Sc<-t_test1_In_Sc$p.value
#effect size
t<-t_test1_In_Sc$statistic[[1]]
df<-t_test1_In_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_In_Sc<-t*sqrt(1/(df+1))
mean(data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 1] - data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 0] )
r
d_In_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_In_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Integrity_model_scientists)$varFix[2,2])))
lci_d_In_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_In_Sc

t_uci_d<-(as.numeric(t_test1_In_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Integrity_model_scientists)$varFix[2,2])))
uci_d_In_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_In_Sc

# Educated respondents 
Integrity_model_Educated <-lme(fixed=Integrity ~ profession_category, random= ~ 1|id , data=data_Integrity_Educated_respondents)
summary(Integrity_model_Educated)

# If significant, t-test to check effect
t_test2_In_Ed = t.test(data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 1],
                       data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test2_In_Ed)
p_value_simple_eff_In_Ed<-t_test2_In_Ed$p.value

#effect size
t<-t_test2_In_Ed$statistic[[1]]
df<-t_test2_In_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_In_Ed<-t*sqrt(1/(df+1))
mean(data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 1] -
       data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 0])
r
d_In_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_In_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Integrity_model_Educated)$varFix[2,2])))
lci_d_In_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_In_Ed

t_uci_d<-(as.numeric(t_test2_In_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Integrity_model_Educated)$varFix[2,2])))
uci_d_In_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_In_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_In_Sc-d_In_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Integrity ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Integrity)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Integrity$Integrity[data_Integrity$profession_category == 1],
#                   data_Integrity$Integrity[data_Integrity$profession_category == 0],
#                   var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a



#barplot
plot_data_Integrity <- summarySE(data_Integrity, measurevar="Integrity", groupvars=c("Respondent_group","profession_category"))


Integrity_plot_bar<-ggplot(plot_data_Integrity, aes(x=Respondent_group, y=Integrity, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar

# Object with p-value of interaction effect
p_value_interaction_Integrity

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Integrity:

# Respondent group: Scientists
d_In_Sc
lci_d_In_Sc
uci_d_In_Sc
p_value_simple_eff_In_Sc

# Respondent group: Educated
d_In_Ed
lci_d_In_Ed
uci_d_In_Ed
p_value_simple_eff_In_Ed


#ggsave("Integrity_plot_bar.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)




###### COMPETITIVENESS #######

#Look at means per cell
means<-aggregate(data_Competitiveness$Competitiveness,by=list(profession_category=data_Competitiveness$profession_category,respondent_group=data_Competitiveness$Respondent_group),mean)
means
# descriptives per cell
round(stat.desc(data_Competitiveness[data_Competitiveness$Respondent_group=="Scientists" & data_Competitiveness$profession_category==1 ,"Competitiveness"]), 2)
round(stat.desc(data_Competitiveness[data_Competitiveness$Respondent_group=="Scientists" & data_Competitiveness$profession_category==0 ,"Competitiveness"]), 2)
round(stat.desc(data_Competitiveness[data_Competitiveness$Respondent_group=="Educated" & data_Competitiveness$profession_category==1 ,"Competitiveness"]), 2)
round(stat.desc(data_Competitiveness[data_Competitiveness$Respondent_group=="Educated" & data_Competitiveness$profession_category==0 ,"Competitiveness"]), 2)

#fit model with interaction
m6.1 <-lme(fixed=Competitiveness ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Competitiveness)
summary(m6.1)

p_value_interaction_Competitiveness <- summary(m6.1)$tTable[4,"p-value"]


#check for effect of gender #  small effect: women perceive higher competitiveness
# m6.1g <-lme(fixed=Competitiveness ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Competitiveness)
# summary(m6.1g)

# If significant interaction: simple effects

# Scientists respondents
Competitiveness_model_scientists <-lme(fixed=Competitiveness ~ profession_category, random= ~ 1|id , data=data_Competitiveness_Scientist_respondents)
summary(Competitiveness_model_scientists)
# If significant, t-test to check effect
t_test1_Co_Sc = t.test(data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 1],
                       data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test1_Co_Sc)

p_value_simple_eff_Co_Sc<-t_test1_Co_Sc$p.value
#effect size
t<-t_test1_Co_Sc$statistic[[1]]
df<-t_test1_Co_Sc$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Co_Sc<-t*sqrt(1/(df+1))
mean(data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 1] - data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 0] )
r
d_Co_Sc

#confidence interval for d
t_lci_d<-(as.numeric(t_test1_Co_Sc$conf.int[1]))/(as.numeric(sqrt(summary(Competitiveness_model_scientists)$varFix[2,2])))
lci_d_Co_Sc<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Co_Sc

t_uci_d<-(as.numeric(t_test1_Co_Sc$conf.int[2]))/(as.numeric(sqrt(summary(Competitiveness_model_scientists)$varFix[2,2])))
uci_d_Co_Sc<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Co_Sc

# Educated respondents 
Competitiveness_model_Educated <-lme(fixed=Competitiveness ~ profession_category, random= ~ 1|id , data=data_Competitiveness_Educated_respondents)
summary(Competitiveness_model_Educated)

# If significant, t-test to check effect
t_test2_Co_Ed = t.test(data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 1],
                       data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 0],
                       var.equal = TRUE, paired = TRUE)
print(t_test2_Co_Ed)
p_value_simple_eff_Co_Ed<-t_test2_Co_Ed$p.value

#effect size
t<-t_test2_Co_Ed$statistic[[1]]
df<-t_test2_Co_Ed$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d_Co_Ed<-t*sqrt(1/(df+1))
mean(data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 1] -
       data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 0])
r
d_Co_Ed


#confidence interval for d
t_lci_d<-(as.numeric(t_test2_Co_Ed$conf.int[1]))/(as.numeric(sqrt(summary(Competitiveness_model_Educated)$varFix[2,2])))
lci_d_Co_Ed<-t_lci_d*sqrt(1/(df+1)) 
lci_d_Co_Ed

t_uci_d<-(as.numeric(t_test2_Co_Ed$conf.int[2]))/(as.numeric(sqrt(summary(Competitiveness_model_Educated)$varFix[2,2])))
uci_d_Co_Ed<-t_uci_d*sqrt(1/(df+1)) 
uci_d_Co_Ed


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d_Co_Sc-d_Co_Ed
diff

# # if no interaction: look at main effects in model without interaction
# # fit model without interaction
# m1.1 <-lme(fixed=Competitiveness ~ profession_category + Respondent_group, random= ~ 1|id , data=data_Competitiveness)
# summary(m1.1)
# 
# 
# # effect of profession category
# t_test_a = t.test(data_Competitiveness$Competitiveness[data_Competitiveness$profession_category == 1],
#                   data_Competitiveness$Competitiveness[data_Competitiveness$profession_category == 0],
#                   var.equal = TRUE, paired = TRUE)
# t_test_a
# 
# t<-t_test_a$statistic[[1]]
# df<-t_test_a$parameter[[1]]
# r<-sqrt(t^2/(t^2+df))
# r<-round(r, 3)
# d_a<-t*sqrt(1/(df+1))
# d_a
# 

#barplot
plot_data_Competitiveness <- summarySE(data_Competitiveness, measurevar="Competitiveness", groupvars=c("Respondent_group","profession_category"))


Competitiveness_plot_bar<-ggplot(plot_data_Competitiveness, aes(x=Respondent_group, y=Competitiveness, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Competitiveness-ci, ymax=Competitiveness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Competitiveness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("Educated","Scientists"), labels=c("Highly-educated", "Scientists"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Competitiveness_plot_bar

# Object with p-value of interaction effect
p_value_interaction_Competitiveness

# Objects with effect sizes of the simple effects of Target per respondent group,
# the lower and upper bounds of the CI's of d, and the p-value of the t-test:
# Competitiveness:

# Respondent group: Scientists
d_Co_Sc
lci_d_Co_Sc
uci_d_Co_Sc
p_value_simple_eff_Co_Sc

# Respondent group: Educated
d_Co_Ed
lci_d_Co_Ed
uci_d_Co_Ed
p_value_simple_eff_Co_Ed



#ggsave("Competitiveness_plot_bar.jpeg", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)
#ggsave("Competitiveness_plot_bar.pdf", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)


# Create multipanel plot --------------------------------------------------

# create legend for multipanel bar plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar)


jpeg("multipanel_plot_study_D_bars.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Competitiveness_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()

pdf("multipanel_plot_study_D_bars.pdf")
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Competitiveness_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()

# SUPPLEMENT 1 - international sample --------------------------------------------------------------

# Read in data from csv ---------------------------------------------------

Data_study_D_file_name<-"Data_study_D_prepared_international.csv"
data_international <-read.csv(Data_study_D_file_name)

data_international$X<-factor(data_international$X)

# Exclude incomplete cases ------------------------------------------------

#### Use complete cases only (as data_international collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data_international[,9:68]))
data_international<-data_international[-incomplete,]

# Create variable for continent/worldpart ---------------------------------

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

sum(data_international$worldpart=="USA", na.rm=T)
data_international$worldpart[data_international$Respondent_group=="Educated"]<-"USA"

# Only use Europe, Asia and USA (other groups too small)
data_international<-subset(data_international, worldpart=="Europe" | worldpart=="Asia" | worldpart=="USA")


# Create new variables: means of non-scientist professions  ---------------

# compute mean rating for 9 non-scientists professions
data_international$mean_non_scientists_Objectivity<-apply(data_international[,names(data_international)=="Ob_lawyers"|
                                                   names(data_international)=="Ob_politicians"|
                                                   names(data_international)=="Ob_journalists"|
                                                   names(data_international)=="Ob_med_doctors"|
                                                   names(data_international)=="Ob_accountants"|
                                                   names(data_international)=="Ob_army_lieutenants"|
                                                   names(data_international)=="Ob_bankers"|
                                                   names(data_international)=="Ob_judges"|
                                                   names(data_international)=="Ob_detectives"],1,mean, na.rm=T)

data_international$mean_non_scientists_Rationality<-apply(data_international[,names(data_international)=="Ra_lawyers"|
                                                   names(data_international)=="Ra_politicians"|
                                                   names(data_international)=="Ra_journalists"|
                                                   names(data_international)=="Ra_med_doctors"|
                                                   names(data_international)=="Ra_accountants"|
                                                   names(data_international)=="Ra_army_lieutenants"|
                                                   names(data_international)=="Ra_bankers"|
                                                   names(data_international)=="Ra_judges"|
                                                   names(data_international)=="Ra_detectives"],1,mean, na.rm=T)

data_international$mean_non_scientists_Openness<-apply(data_international[,names(data_international)=="Op_lawyers"|
                                                names(data_international)=="Op_politicians"|
                                                names(data_international)=="Op_journalists"|
                                                names(data_international)=="Op_med_doctors"|
                                                names(data_international)=="Op_accountants"|
                                                names(data_international)=="Op_army_lieutenants"|
                                                names(data_international)=="Op_bankers"|
                                                names(data_international)=="Op_judges"|
                                                names(data_international)=="Op_detectives"],1,mean, na.rm=T)


data_international$mean_non_scientists_Intelligence<-apply(data_international[,names(data_international)=="Iq_lawyers"|
                                                    names(data_international)=="Iq_politicians"|
                                                    names(data_international)=="Iq_journalists"|
                                                    names(data_international)=="Iq_med_doctors"|
                                                    names(data_international)=="Iq_accountants"|
                                                    names(data_international)=="Iq_army_lieutenants"|
                                                    names(data_international)=="Iq_bankers"|
                                                    names(data_international)=="Iq_judges"|
                                                    names(data_international)=="Iq_detectives"],1,mean, na.rm=T)

data_international$mean_non_scientists_Integrity<-apply(data_international[,names(data_international)=="In_lawyers"|
                                                 names(data_international)=="In_politicians"|
                                                 names(data_international)=="In_journalists"|
                                                 names(data_international)=="In_med_doctors"|
                                                 names(data_international)=="In_accountants"|
                                                 names(data_international)=="In_army_lieutenants"|
                                                 names(data_international)=="In_bankers"|
                                                 names(data_international)=="In_judges"|
                                                 names(data_international)=="In_detectives"],1,mean, na.rm=T)

data_international$mean_non_scientists_Competitiveness<-apply(data_international[,names(data_international)=="Co_lawyers"|
                                                       names(data_international)=="Co_politicians"|
                                                       names(data_international)=="Co_journalists"|
                                                       names(data_international)=="Co_med_doctors"|
                                                       names(data_international)=="Co_accountants"|
                                                       names(data_international)=="Co_army_lieutenants"|
                                                       names(data_international)=="Co_bankers"|
                                                       names(data_international)=="Co_judges"|
                                                       names(data_international)=="Co_detectives"],1,mean, na.rm=T)

# Create new variables: means of scientist profession  --------------------

## = renaming only

data_international$mean_scientists_Objectivity<-data_international$Ob_scientists
data_international$mean_scientists_Rationality<-data_international$Ra_scientists
data_international$mean_scientists_Openness<-data_international$Op_scientists
data_international$mean_scientists_Intelligence<-data_international$Iq_scientists
data_international$mean_scientists_Integrity<-data_international$In_scientists
data_international$mean_scientists_Competitiveness<-data_international$Co_scientists

# Find and remove outliers  per worldpart -----------------------------------------------------------
# using Boxplot function from car package

# USA
data_international_USA<-subset(data_international, worldpart=="USA")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_international_USA$mean_non_scientists_Objectivity, data_international_USA$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Objectivity, data_international_USA$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_non_scientists_Rationality, data_international_USA$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Rationality, data_international_USA$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_non_scientists_Openness, data_international_USA$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Openness, data_international_USA$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_non_scientists_Intelligence, data_international_USA$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Intelligence, data_international_USA$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_non_scientists_Integrity, data_international_USA$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Integrity, data_international_USA$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_non_scientists_Competitiveness, data_international_USA$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_USA$mean_scientists_Competitiveness, data_international_USA$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))



# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 
data_international_USA<-data_international_USA[-outliers,]

# Asia
data_international_Asia<-subset(data_international, worldpart=="Asia")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_international_Asia$mean_non_scientists_Objectivity, data_international_Asia$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Objectivity, data_international_Asia$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_non_scientists_Rationality, data_international_Asia$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Rationality, data_international_Asia$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_non_scientists_Openness, data_international_Asia$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Openness, data_international_Asia$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_non_scientists_Intelligence, data_international_Asia$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Intelligence, data_international_Asia$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_non_scientists_Integrity, data_international_Asia$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Integrity, data_international_Asia$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_non_scientists_Competitiveness, data_international_Asia$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Asia$mean_scientists_Competitiveness, data_international_Asia$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))

# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 

data_international_Asia<-data_international_Asia[-outliers,]


# Europe
data_international_Europe<-subset(data_international, worldpart=="Europe")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_international_Europe$mean_non_scientists_Objectivity, data_international_Europe$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Objectivity, data_international_Europe$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_non_scientists_Rationality, data_international_Europe$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Rationality, data_international_Europe$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_non_scientists_Openness, data_international_Europe$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Openness, data_international_Europe$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_non_scientists_Intelligence, data_international_Europe$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Intelligence, data_international_Europe$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_non_scientists_Integrity, data_international_Europe$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Integrity, data_international_Europe$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_non_scientists_Competitiveness, data_international_Europe$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_international_Europe$mean_scientists_Competitiveness, data_international_Europe$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))



# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 
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

# nr of respondents in the Educated group
sum(data_international$Respondent_group=="Educated")

sum(data_international$Respondent_group=="Educated")
round(mean(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Educated"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Educated"])), 2)

# nr of respondents in the Scientist group
sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="USA")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="USA"])), 2)

sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="Asia")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Asia"])), 2)

sum(data_international$Respondent_group=="Scientists"& data_international$worldpart=="Europe")
round(mean(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(SD(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(min(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(max(data_international$Age[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"], na.rm=T),digits =1)
round(prop.table(table(data_international$Gender[data_international$Respondent_group=="Scientists" & data_international$worldpart=="Europe"])), 2)

# # use SCIENTIST data_international ONLY -----------------------------------------------
# Use only scientist data_international
data_international<-subset(data_international, Respondent_group=="Scientists")

# Change data_internationalframe into long format ---------------------------------------

# 2 rows per participant (data_international has one within-subjects variable)
#add id variable
data_international$id<-1:nrow(data_international)

#OBJECTIVITY
#long format
data_international_Objectivity<-melt(data_international[,c("id","mean_scientists_Objectivity",
                               "mean_non_scientists_Objectivity",
                               "worldpart",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "worldpart",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)

# check long format
data_international_Objectivity[1:10,]
data_international_Objectivity[175:185,]
data_international_Objectivity <- rename(data_international_Objectivity, c(variable="Profession"))
data_international_Objectivity <- rename(data_international_Objectivity, c(value="Objectivity"))
data_international_Objectivity$Gender<-as.factor(data_international_Objectivity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Objectivity$profession_category<-as.factor(ifelse(data_international_Objectivity$Profession =="mean_scientists_Objectivity",1,0))

#RATIONALITY
#long format
data_international_Rationality<-melt(data_international[,c("id","mean_scientists_Rationality",
                               "mean_non_scientists_Rationality",
                               "worldpart",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "worldpart",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)

# check long format
data_international_Rationality <- rename(data_international_Rationality, c(variable="Profession"))
data_international_Rationality <- rename(data_international_Rationality, c(value="Rationality"))
data_international_Rationality$Gender<-as.factor(data_international_Rationality$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Rationality$profession_category<-as.factor(ifelse(data_international_Rationality$Profession =="mean_scientists_Rationality",1,0))


# OPENNESS
#long format
data_international_Openness<-melt(data_international[,c("id","mean_scientists_Openness",
                            "mean_non_scientists_Openness",
                            "worldpart",
                            "Age",
                            "Gender",
                            "Religiousness")],
                    id=c("id",
                         "worldpart",
                         "Age",
                         "Gender",
                         "Religiousness"),na.rm=T)


# check long format
data_international_Openness <- rename(data_international_Openness, c(variable="Profession"))
data_international_Openness <- rename(data_international_Openness, c(value="Openness"))
data_international_Openness$Gender<-as.factor(data_international_Openness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Openness$profession_category<-as.factor(ifelse(data_international_Openness$Profession =="mean_scientists_Openness",1,0))

# INTELLIGENCE
#long format
data_international_Intelligence<-melt(data_international[,c("id","mean_scientists_Intelligence",
                                "mean_non_scientists_Intelligence",
                                "worldpart",
                                "Age",
                                "Gender",
                                "Religiousness")],
                        id=c("id",
                             "worldpart",
                             "Age",
                             "Gender",
                             "Religiousness"),na.rm=T)


# check long format
data_international_Intelligence <- rename(data_international_Intelligence, c(variable="Profession"))
data_international_Intelligence <- rename(data_international_Intelligence, c(value="Intelligence"))
data_international_Intelligence$Gender<-as.factor(data_international_Intelligence$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Intelligence$profession_category<-as.factor(ifelse(data_international_Intelligence$Profession =="mean_scientists_Intelligence",1,0))

# INTEGRITY
#long format
data_international_Integrity<-melt(data_international[,c("id","mean_scientists_Integrity",
                             "mean_non_scientists_Integrity",
                             "worldpart",
                             "Age",
                             "Gender",
                             "Religiousness")],
                     id=c("id",
                          "worldpart",
                          "Age",
                          "Gender",
                          "Religiousness"),na.rm=T)


# check long format
data_international_Integrity <- rename(data_international_Integrity, c(variable="Profession"))
data_international_Integrity <- rename(data_international_Integrity, c(value="Integrity"))
data_international_Integrity$Gender<-as.factor(data_international_Integrity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Integrity$profession_category<-as.factor(ifelse(data_international_Integrity$Profession =="mean_scientists_Integrity",1,0))

# COMPETITIVENESS
#long format
data_international_Competitiveness<-melt(data_international[,c("id","mean_scientists_Competitiveness",
                                   "mean_non_scientists_Competitiveness",
                                   "worldpart",
                                   "Age",
                                   "Gender",
                                   "Religiousness")],
                           id=c("id",
                                "worldpart",
                                "Age",
                                "Gender",
                                "Religiousness"),na.rm=T)


# check long format
data_international_Competitiveness <- rename(data_international_Competitiveness, c(variable="Profession"))
data_international_Competitiveness <- rename(data_international_Competitiveness, c(value="Competitiveness"))
data_international_Competitiveness$Gender<-as.factor(data_international_Competitiveness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_international_Competitiveness$profession_category<-as.factor(ifelse(data_international_Competitiveness$Profession =="mean_scientists_Competitiveness",1,0))

#  plots per worldpart ----------------------------------------------------

# OBJECTIVITY -------------------------------------------------------------
plot_data_international_Objectivity <- summarySE(data_international_Objectivity, measurevar="Objectivity", groupvars=c("worldpart","profession_category"))

Objectivity_plot_bar_international<-ggplot(plot_data_international_Objectivity, aes(x=worldpart, y=Objectivity, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar_international

#ggsave("Objectivity_plot_bar_international_international.jpeg", Objectivity_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar_international_international.pdf", Objectivity_plot_bar_international,dpi=600, width = 9, height = 6)

# RATIONALITY -------------------------------------------------------------
plot_data_international_Rationality <- summarySE(data_international_Rationality, measurevar="Rationality", groupvars=c("worldpart","profession_category"))

Rationality_plot_bar_international<-ggplot(plot_data_international_Rationality, aes(x=worldpart, y=Rationality, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar_international

#ggsave("Rationality_plot_bar_international_international.jpeg", Rationality_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar_international_international.pdf", Rationality_plot_bar_international,dpi=600, width = 9, height = 6)



# OPENNESS ----------------------------------------------------------------
plot_data_international_Openness <- summarySE(data_international_Openness, measurevar="Openness", groupvars=c("worldpart","profession_category"))

Openness_plot_bar_international<-ggplot(plot_data_international_Openness, aes(x=worldpart, y=Openness, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar_international

#ggsave("Openness_plot_bar_international_international.jpeg", Openness_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar_international_international.pdf", Openness_plot_bar_international,dpi=600, width = 9, height = 6)

# INTELLIGENCE ------------------------------------------------------------
plot_data_international_Intelligence <- summarySE(data_international_Intelligence, measurevar="Intelligence", groupvars=c("worldpart","profession_category"))

Intelligence_plot_bar_international<-ggplot(plot_data_international_Intelligence, aes(x=worldpart, y=Intelligence, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Intelligence_plot_bar_international

#ggsave("Intelligence_plot_bar_international_international.jpeg", Intelligence_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar_international_international.pdf", Intelligence_plot_bar_international,dpi=600, width = 9, height = 6)


# INTEGRITY ---------------------------------------------------------------
plot_data_international_Integrity <- summarySE(data_international_Integrity, measurevar="Integrity", groupvars=c("worldpart","profession_category"))

Integrity_plot_bar_international<-ggplot(plot_data_international_Integrity, aes(x=worldpart, y=Integrity, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar_international

#ggsave("Integrity_plot_bar_international_international.jpeg", Integrity_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar_international_international.pdf", Integrity_plot_bar_international,dpi=600, width = 9, height = 6)

# COMPETITIVENESS ---------------------------------------------------------
plot_data_international_Competitiveness <- summarySE(data_international_Competitiveness, measurevar="Competitiveness", groupvars=c("worldpart","profession_category"))

Competitiveness_plot_bar_international<-ggplot(plot_data_international_Competitiveness, aes(x=worldpart, y=Competitiveness, fill=profession_category)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Competitiveness-ci, ymax=Competitiveness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Competitiveness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Target", breaks=c(0,1), labels=c("Other professions", "Scientists"))+
  scale_x_discrete(breaks=c("USA", "Asia","Europe"), labels=c("USA", "Asia","Europe"), name="Respondent group")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Competitiveness_plot_bar_international

#ggsave("Competitiveness_plot_bar_international_international.jpeg", Competitiveness_plot_bar_international,dpi=600, width = 9, height = 6)
#ggsave("Competitiveness_plot_bar_international_international.pdf", Competitiveness_plot_bar_international,dpi=600, width = 9, height = 6)

# Create multipanel plot --------------------------------------------------

mylegend<-g_legend(Objectivity_plot_bar_international)

jpeg("multipanel_plot_study_D_bars_international.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar_international + theme(legend.position="none"),
                                            Rationality_plot_bar_international + theme(legend.position="none"),
                                            Openness_plot_bar_international + theme(legend.position="none"),
                                            Intelligence_plot_bar_international + theme(legend.position="none"),
                                            Integrity_plot_bar_international + theme(legend.position="none"),
                                            Competitiveness_plot_bar_international + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()


pdf("multipanel_plot_study_D_bars_international.pdf")
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar_international + theme(legend.position="none"),
                                            Rationality_plot_bar_international + theme(legend.position="none"),
                                            Openness_plot_bar_international + theme(legend.position="none"),
                                            Intelligence_plot_bar_international + theme(legend.position="none"),
                                            Integrity_plot_bar_international + theme(legend.position="none"),
                                            Competitiveness_plot_bar_international + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()


# SUPPLEMENT 2: graphs with professions separately -------------------


#OBJECTIVITY
#long format
data_all_professions_Objectivity<-melt(data[,c("id","Ob_lawyers",
                               "Ob_politicians",
                               "Ob_journalists",
                               "Ob_med_doctors",
                               "Ob_accountants",
                               "Ob_army_lieutenants",
                               "Ob_bankers",
                               "Ob_judges",
                               "Ob_detectives",
                               "Ob_scientists",
                               "Respondent_group",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)


# check long format
data_all_professions_Objectivity[1:10,]
data_all_professions_Objectivity[1000:1100,]
data_all_professions_Objectivity <- rename(data_all_professions_Objectivity, c(variable="Profession"))
data_all_professions_Objectivity <- rename(data_all_professions_Objectivity, c(value="Objectivity"))
is.data.frame(data_all_professions_Objectivity)
class(data_all_professions_Objectivity$Profession)
class(data_all_professions_Objectivity$Objectivity)
class(data_all_professions_Objectivity$Respondent_group)
class(data_all_professions_Objectivity$Age)
class(data_all_professions_Objectivity$Gender)
data_all_professions_Objectivity$Gender<-as.factor(data_all_professions_Objectivity$Gender)
class(data_all_professions_Objectivity$Gender)
class(data_all_professions_Objectivity$Religiousness)

data_all_professions_Objectivity$Profession<-as.character(data_all_professions_Objectivity$Profession)
class(data_all_professions_Objectivity$Profession)
data_all_professions_Objectivity$Profession<-gsub("Ob_","",data_all_professions_Objectivity$Profession)
data_all_professions_Objectivity$Profession<-as.factor(data_all_professions_Objectivity$Profession)
class(data_all_professions_Objectivity$Profession)
levels(data_all_professions_Objectivity$Profession)


#RATIONALITY
#long format
data_all_professions_Rationality<-melt(data[,c("id","Ra_lawyers",
                               "Ra_politicians",
                               "Ra_journalists",
                               "Ra_med_doctors",
                               "Ra_accountants",
                               "Ra_army_lieutenants",
                               "Ra_bankers",
                               "Ra_detectives",
                               "Ra_judges",
                               "Ra_scientists",
                               "Respondent_group",
                               "Age",
                               "Gender",
                               "Religiousness")],
                       id=c("id",
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness"),na.rm=T)


# check long format
data_all_professions_Rationality[1:10,]
data_all_professions_Rationality[1000:1100,]
data_all_professions_Rationality <- rename(data_all_professions_Rationality, c(variable="Profession"))
data_all_professions_Rationality <- rename(data_all_professions_Rationality, c(value="Rationality"))
is.data.frame(data_all_professions_Rationality)
class(data_all_professions_Rationality$Profession)
class(data_all_professions_Rationality$Rationality)
class(data_all_professions_Rationality$Respondent_group)
class(data_all_professions_Rationality$Age)
class(data_all_professions_Rationality$Gender)
data_all_professions_Rationality$Gender<-as.factor(data_all_professions_Rationality$Gender)
class(data_all_professions_Rationality$Gender)
class(data_all_professions_Rationality$Religiousness)

data_all_professions_Rationality$Profession<-as.character(data_all_professions_Rationality$Profession)
class(data_all_professions_Rationality$Profession)
data_all_professions_Rationality$Profession<-gsub("Ra_","",data_all_professions_Rationality$Profession)
data_all_professions_Rationality$Profession<-as.factor(data_all_professions_Rationality$Profession)
class(data_all_professions_Rationality$Profession)
levels(data_all_professions_Rationality$Profession)


# OPENNESS
#long format
data_all_professions_Openness<-melt(data[,c("id","Op_lawyers",
                            "Op_politicians",
                            "Op_journalists",
                            "Op_med_doctors",
                            "Op_accountants",
                            "Op_army_lieutenants",
                            "Op_bankers",
                            "Op_detectives",
                            "Op_judges",
                            "Op_scientists",                               
                            "Respondent_group",
                            "Age",
                            "Gender",
                            "Religiousness")],
                    id=c("id",
                         "Respondent_group",
                         "Age",
                         "Gender",
                         "Religiousness"),na.rm=T)


# check long format
data_all_professions_Openness[1:10,]
data_all_professions_Openness[1000:1100,]
data_all_professions_Openness <- rename(data_all_professions_Openness, c(variable="Profession"))
data_all_professions_Openness <- rename(data_all_professions_Openness, c(value="Openness"))
is.data.frame(data_all_professions_Openness)
class(data_all_professions_Openness$Profession)
class(data_all_professions_Openness$Openness)
class(data_all_professions_Openness$Respondent_group)
class(data_all_professions_Openness$Age)
class(data_all_professions_Openness$Gender)
data_all_professions_Openness$Gender<-as.factor(data_all_professions_Openness$Gender)
class(data_all_professions_Openness$Gender)
class(data_all_professions_Openness$Religiousness)

data_all_professions_Openness$Profession<-as.character(data_all_professions_Openness$Profession)
class(data_all_professions_Openness$Profession)
data_all_professions_Openness$Profession<-gsub("Op_","",data_all_professions_Openness$Profession)
data_all_professions_Openness$Profession<-as.factor(data_all_professions_Openness$Profession)
class(data_all_professions_Openness$Profession)
levels(data_all_professions_Openness$Profession)



# INTELLIGENCE
#long format
data_all_professions_Intelligence<-melt(data[,c("id","Iq_lawyers",
                                "Iq_politicians",
                                "Iq_journalists",
                                "Iq_med_doctors",
                                "Iq_accountants",
                                "Iq_army_lieutenants",
                                "Iq_bankers",
                                "Iq_detectives",
                                "Iq_judges",
                                "Iq_scientists",                               
                                "Respondent_group",
                                "Age",
                                "Gender",
                                "Religiousness")],
                        id=c("id",
                             "Respondent_group",
                             "Age",
                             "Gender",
                             "Religiousness"),na.rm=T)


# check long format
data_all_professions_Intelligence[1:10,]
data_all_professions_Intelligence[1000:1100,]
data_all_professions_Intelligence <- rename(data_all_professions_Intelligence, c(variable="Profession"))
data_all_professions_Intelligence <- rename(data_all_professions_Intelligence, c(value="Intelligence"))
is.data.frame(data_all_professions_Intelligence)
class(data_all_professions_Intelligence$Profession)
class(data_all_professions_Intelligence$Intelligence)
class(data_all_professions_Intelligence$Respondent_group)
class(data_all_professions_Intelligence$Age)
class(data_all_professions_Intelligence$Gender)
data_all_professions_Intelligence$Gender<-as.factor(data_all_professions_Intelligence$Gender)
class(data_all_professions_Intelligence$Gender)
class(data_all_professions_Intelligence$Religiousness)

data_all_professions_Intelligence$Profession<-as.character(data_all_professions_Intelligence$Profession)
class(data_all_professions_Intelligence$Profession)
data_all_professions_Intelligence$Profession<-gsub("Iq_","",data_all_professions_Intelligence$Profession)
data_all_professions_Intelligence$Profession<-as.factor(data_all_professions_Intelligence$Profession)
class(data_all_professions_Intelligence$Profession)
levels(data_all_professions_Intelligence$Profession)


# INTEGRITY
#long format
data_all_professions_Integrity<-melt(data[,c("id","In_lawyers",
                             "In_politicians",
                             "In_journalists",
                             "In_med_doctors",
                             "In_accountants",
                             "In_army_lieutenants",
                             "In_bankers",
                             "In_detectives",
                             "In_judges",
                             "In_scientists",                               
                             "Respondent_group",
                             "Age",
                             "Gender",
                             "Religiousness")],
                     id=c("id",
                          "Respondent_group",
                          "Age",
                          "Gender",
                          "Religiousness"),na.rm=T)


# check long format
data_all_professions_Integrity[1:10,]
data_all_professions_Integrity[1000:1100,]
data_all_professions_Integrity <- rename(data_all_professions_Integrity, c(variable="Profession"))
data_all_professions_Integrity <- rename(data_all_professions_Integrity, c(value="Integrity"))
is.data.frame(data_all_professions_Integrity)
class(data_all_professions_Integrity$Profession)
class(data_all_professions_Integrity$Integrity)
class(data_all_professions_Integrity$Respondent_group)
class(data_all_professions_Integrity$Age)
class(data_all_professions_Integrity$Gender)
data_all_professions_Integrity$Gender<-as.factor(data_all_professions_Integrity$Gender)
class(data_all_professions_Integrity$Gender)
class(data_all_professions_Integrity$Religiousness)


data_all_professions_Integrity$Profession<-as.character(data_all_professions_Integrity$Profession)
class(data_all_professions_Integrity$Profession)
data_all_professions_Integrity$Profession<-gsub("In_","",data_all_professions_Integrity$Profession)
data_all_professions_Integrity$Profession<-as.factor(data_all_professions_Integrity$Profession)
class(data_all_professions_Integrity$Profession)
levels(data_all_professions_Integrity$Profession)



# COMPETITIVENESS
#long format
data_all_professions_Competitiveness<-melt(data[,c("id","Co_lawyers",
                                   "Co_politicians",
                                   "Co_journalists",
                                   "Co_med_doctors",
                                   "Co_accountants",
                                   "Co_army_lieutenants",
                                   "Co_bankers",
                                   "Co_detectives",
                                   "Co_judges",
                                   "Co_scientists",
                                   "Respondent_group",
                                   "Age",
                                   "Gender",
                                   "Religiousness")],
                           id=c("id",
                                "Respondent_group",
                                "Age",
                                "Gender",
                                "Religiousness"),na.rm=T)


# check long format
data_all_professions_Competitiveness[1:10,]
data_all_professions_Competitiveness[1000:1100,]
data_all_professions_Competitiveness <- rename(data_all_professions_Competitiveness, c(variable="Profession"))
data_all_professions_Competitiveness <- rename(data_all_professions_Competitiveness, c(value="Competitiveness"))
is.data.frame(data_all_professions_Competitiveness)
class(data_all_professions_Competitiveness$Profession)
class(data_all_professions_Competitiveness$Competitiveness)
class(data_all_professions_Competitiveness$Respondent_group)
class(data_all_professions_Competitiveness$Age)
class(data_all_professions_Competitiveness$Gender)
data_all_professions_Competitiveness$Gender<-as.factor(data_all_professions_Competitiveness$Gender)
class(data_all_professions_Competitiveness$Gender)
class(data_all_professions_Competitiveness$Religiousness)

data_all_professions_Competitiveness$Profession<-as.character(data_all_professions_Competitiveness$Profession)
class(data_all_professions_Competitiveness$Profession)
data_all_professions_Competitiveness$Profession<-gsub("Co_","",data_all_professions_Competitiveness$Profession)
data_all_professions_Competitiveness$Profession<-as.factor(data_all_professions_Competitiveness$Profession)
class(data_all_professions_Competitiveness$Profession)
levels(data_all_professions_Competitiveness$Profession)




#barplot

plot_data_all_professions_Objectivity <- summarySE(data_all_professions_Objectivity, measurevar="Objectivity", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Objectivity$Profession)

Objectivity_plot_bar_all_professions<-ggplot(plot_data_all_professions_Objectivity, aes(x=Profession, y=Objectivity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Objectivity_plot_bar_all_professions

#ggsave("Objectivity_plot_bar_all_professions.jpeg", Objectivity_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Objectivity_plot_bar_all_professions.pdf", Objectivity_plot_bar_all_professions, dpi=600, width = 9, height = 6)



###### RATIONALITY #######


#barplot
plot_data_all_professions_Rationality <- summarySE(data_all_professions_Rationality, measurevar="Rationality", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Rationality$Profession)

Rationality_plot_bar_all_professions<-ggplot(plot_data_all_professions_Rationality, aes(x=Profession, y=Rationality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Rationality_plot_bar_all_professions

#ggsave("Rationality_plot_bar_all_professions.jpeg", Rationality_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Rationality_plot_bar_all_professions.pdf", Rationality_plot_bar_all_professions, dpi=600, width = 9, height = 6)



###### OPENNESS #######


#barplot
plot_data_all_professions_Openness <- summarySE(data_all_professions_Openness, measurevar="Openness", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Openness$Profession)

Openness_plot_bar_all_professions<-ggplot(plot_data_all_professions_Openness, aes(x=Profession, y=Openness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Open-mindedness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Openness_plot_bar_all_professions

#ggsave("Openness_plot_bar_all_professions.jpeg", Openness_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Openness_plot_bar_all_professions.pdf", Openness_plot_bar_all_professions, dpi=600, width = 9, height = 6)



###### INTELLIGENCE #######



#barplot
plot_data_all_professions_Intelligence <- summarySE(data_all_professions_Intelligence, measurevar="Intelligence", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Intelligence$Profession)

Intelligence_plot_bar_all_professions<-ggplot(plot_data_all_professions_Intelligence, aes(x=Profession, y=Intelligence, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Intelligence_plot_bar_all_professions

#ggsave("Intelligence_plot_bar_all_professions.jpeg", Intelligence_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Intelligence_plot_bar_all_professions.pdf", Intelligence_plot_bar_all_professions, dpi=600, width = 9, height = 6)




###### INTEGRITY #######


#barplot
plot_data_all_professions_Integrity <- summarySE(data_all_professions_Integrity, measurevar="Integrity", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Integrity$Profession)

Integrity_plot_bar_all_professions<-ggplot(plot_data_all_professions_Integrity, aes(x=Profession, y=Integrity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Integrity_plot_bar_all_professions

#ggsave("Integrity_plot_bar_all_professions.jpeg", Integrity_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Integrity_plot_bar_all_professions.pdf", Integrity_plot_bar_all_professions, dpi=600, width = 9, height = 6)





###### COMPETITIVENESS #######


#barplot
plot_data_all_professions_Competitiveness <- summarySE(data_all_professions_Competitiveness, measurevar="Competitiveness", groupvars=c("Respondent_group","Profession"))
levels(plot_data_all_professions_Competitiveness$Profession)

Competitiveness_plot_bar_all_professions<-ggplot(plot_data_all_professions_Competitiveness, aes(x=Profession, y=Competitiveness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Competitiveness-ci, ymax=Competitiveness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Competitiveness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("lawyers", "politicians", "journalists", "med_doctors", "accountants", "army_lieutenants", "bankers","judges","detectives","scientists"), labels=c("Lawyers", "Politicians", "Journalists", "Medical doctors", "Accountants", "Army-lieutenants", "Bankers","Judges","Detectives","Scientists"))+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12, angle = 90)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face="bold", size=18,vjust=1.2))

Competitiveness_plot_bar_all_professions

#ggsave("Competitiveness_plot_bar_all_professions.jpeg", Competitiveness_plot_bar_all_professions, dpi=600, width = 9, height = 6)
#ggsave("Competitiveness_plot_bar_all_professions.pdf", Competitiveness_plot_bar_all_professions, dpi=600, width = 9, height = 6)



# scientists vs other professions
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Objectivity_plot_bar_all_professions)

jpeg("multipanel_plot_2.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar_all_professions + theme(legend.position="none"),
                                            Rationality_plot_bar_all_professions + theme(legend.position="none"), Openness_plot_bar_all_professions + theme(legend.position="none"),Integrity_plot_bar_all_professions + theme(legend.position="none"), Intelligence_plot_bar_all_professions + theme(legend.position="none"),Competitiveness_plot_bar_all_professions + theme(legend.position="none"),
                                            nrow=3),
                                mylegend, nrow=2,heights=c(10, 3))

dev.off()




