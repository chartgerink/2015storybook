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

Data_Study_D_file_name<-"Data_Study_D_prepared.csv"
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
round(SD(data$Age[data$Respondent_group=="Scientists"], na.rm=T),digits =1)
prop.table(table(data$Gender[data$Respondent_group=="Scientists"]))

round(mean(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
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

#check for effect of gender
#m1.1g <-lme(fixed=Objectivity ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Objectivity)
#summary(m1.1g)

# If significant interaction: simple effects

# Scientists respondents
Objectivity_model_scientists <-lme(fixed=Objectivity ~ profession_category, random= ~ 1|id , data=data_Objectivity_Scientist_respondents)
summary(Objectivity_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 1],
                 data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 1] - data_Objectivity_Scientist_respondents$Objectivity[data_Objectivity_Scientist_respondents$profession_category == 0] )
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Objectivity_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Objectivity_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d

# Educated respondents 
Objectivity_model_Educated <-lme(fixed=Objectivity ~ profession_category, random= ~ 1|id , data=data_Objectivity_Educated_respondents)
summary(Objectivity_model_Educated)

# If significant, t-test to check effect
t_test2 = t.test(data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 1],
                 data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 1] -
     data_Objectivity_Educated_respondents$Objectivity[data_Objectivity_Educated_respondents$profession_category == 0])
r
d2


#confidence interval for d
t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Objectivity_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Objectivity_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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

Objectivity_plot_bar<-ggplot(plot_data_Objectivity, aes(x=profession_category, y=Objectivity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
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

#check for effect of gender
#m1.1g <-lme(fixed=Rationality ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Rationality)
#summary(m1.1g)

# If significant interaction: simple effects

# Scientists respondents
Rationality_model_scientists <-lme(fixed=Rationality ~ profession_category, random= ~ 1|id , data=data_Rationality_Scientist_respondents)
summary(Rationality_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 1],
                 data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 1] - 
     data_Rationality_Scientist_respondents$Rationality[data_Rationality_Scientist_respondents$profession_category == 0])
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Rationality_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Rationality_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d

# Educated respondents 
Rationality_model_Educated <-lme(fixed=Rationality ~ profession_category, random= ~ 1|id , data=data_Rationality_Educated_respondents)
summary(Rationality_model_Educated)


# If significant, t-test to check effect
t_test2 = t.test(data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 1],
                 data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 1] - 
     data_Rationality_Educated_respondents$Rationality[data_Rationality_Educated_respondents$profession_category == 0])
r
d2

t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Rationality_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Rationality_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d

# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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
levels(plot_data_Rationality$profession_category)


Rationality_plot_bar<-ggplot(plot_data_Rationality, aes(x=profession_category, y=Rationality, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
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
m1.1 <-lme(fixed=Openness ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Openness)
summary(m1.1)

#check for effect of gender
# m1.1g <-lme(fixed=Openness ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Openness)
# summary(m1.1g)


# If significant interaction: simple effects

# Scientists respondents
Openness_model_scientists <-lme(fixed=Openness ~ profession_category, random= ~ 1|id , data=data_Openness_Scientist_respondents)
summary(Openness_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 1],
                 data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 1] - 
     data_Openness_Scientist_respondents$Openness[data_Openness_Scientist_respondents$profession_category == 0])
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Openness_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Openness_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d


# Educated respondents 
Openness_model_Educated <-lme(fixed=Openness ~ profession_category, random= ~ 1|id , data=data_Openness_Educated_respondents)
summary(Openness_model_Educated)


# If significant, t-test to check effect
t_test2 = t.test(data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 1],
                 data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 1] - 
     data_Openness_Educated_respondents$Openness[data_Openness_Educated_respondents$profession_category == 0])
r
d2

t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Openness_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Openness_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d

# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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
levels(plot_data_Openness$profession_category)


Openness_plot_bar<-ggplot(plot_data_Openness, aes(x=profession_category, y=Openness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
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
m1.1 <-lme(fixed=Intelligence ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Intelligence)
summary(m1.1)


#check for effect of gender
#m1.1g <-lme(fixed=Intelligence ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Intelligence)
#summary(m1.1g)


# If significant interaction: simple effects

# Scientists respondents
Intelligence_model_scientists <-lme(fixed=Intelligence ~ profession_category, random= ~ 1|id , data=data_Intelligence_Scientist_respondents)
summary(Intelligence_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 1],
                 data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 1] - 
     data_Intelligence_Scientist_respondents$Intelligence[data_Intelligence_Scientist_respondents$profession_category == 0])
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Intelligence_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Intelligence_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d


# Educated respondents 
Intelligence_model_Educated <-lme(fixed=Intelligence ~ profession_category, random= ~ 1|id , data=data_Intelligence_Educated_respondents)
summary(Intelligence_model_Educated)


# If significant, t-test to check effect
t_test2 = t.test(data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 1],
                 data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 1] - 
     data_Intelligence_Educated_respondents$Intelligence[data_Intelligence_Educated_respondents$profession_category == 0])
r
d2

t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Intelligence_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Intelligence_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d



# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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
levels(plot_data_Intelligence$profession_category)


Intelligence_plot_bar<-ggplot(plot_data_Intelligence, aes(x=profession_category, y=Intelligence, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
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
m1.1 <-lme(fixed=Integrity ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Integrity)
summary(m1.1)

#check for effect of gender
# m1.1g <-lme(fixed=Integrity ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Integrity)
# summary(m1.1g)


# If significant interaction: simple effects

# Scientists respondents
Integrity_model_scientists <-lme(fixed=Integrity ~ profession_category, random= ~ 1|id , data=data_Integrity_Scientist_respondents)
summary(Integrity_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 1],
                 data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 1] - 
     data_Integrity_Scientist_respondents$Integrity[data_Integrity_Scientist_respondents$profession_category == 0])
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Integrity_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Integrity_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d


# Educated respondents 
Integrity_model_Educated <-lme(fixed=Integrity ~ profession_category, random= ~ 1|id , data=data_Integrity_Educated_respondents)
summary(Integrity_model_Educated)


# If significant, t-test to check effect
t_test2 = t.test(data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 1],
                 data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 1] -
     data_Integrity_Educated_respondents$Integrity[data_Integrity_Educated_respondents$profession_category == 0])
r
d2

t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Integrity_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Integrity_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d



# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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
levels(plot_data_Integrity$profession_category)


Integrity_plot_bar<-ggplot(plot_data_Integrity, aes(x=profession_category, y=Integrity, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
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
m1.1 <-lme(fixed=Competitiveness ~ profession_category + Respondent_group + profession_category*Respondent_group, random= ~ 1|id , data=data_Competitiveness)
summary(m1.1)

#check for effect of gender #  small effect: women perceive higher competitiveness
# m1.1g <-lme(fixed=Competitiveness ~ profession_category + Respondent_group + Gender + profession_category*Respondent_group, random= ~ 1|id , data=data_Competitiveness)
# summary(m1.1g)

# If significant interaction: simple effects
# Scientists respondents
Competitiveness_model_scientists <-lme(fixed=Competitiveness ~ profession_category, random= ~ 1|id , data=data_Competitiveness_Scientist_respondents)
summary(Competitiveness_model_scientists)
# If significant, t-test to check effect
t_test1 = t.test(data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 1],
                 data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test1)
#effect size
t<-t_test1$statistic[[1]]
df<-t_test1$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d1<-t*sqrt(1/(df+1))
mean(data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 1] -
     data_Competitiveness_Scientist_respondents$Competitiveness[data_Competitiveness_Scientist_respondents$profession_category == 0])
r
d1

#confidence interval for d
t_lci_d<-(as.numeric(t_test1$conf.int[1]))/(as.numeric(sqrt(summary(Competitiveness_model_scientists)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test1$conf.int[2]))/(as.numeric(sqrt(summary(Competitiveness_model_scientists)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d

# Educated respondents 
Competitiveness_model_Educated <-lme(fixed=Competitiveness ~ profession_category, random= ~ 1|id , data=data_Competitiveness_Educated_respondents)
summary(Competitiveness_model_Educated)


# If significant, t-test to check effect
t_test2 = t.test(data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 1],
                 data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 0],
                 var.equal = TRUE, paired = TRUE)
print(t_test2)
#effect size
t<-t_test2$statistic[[1]]
df<-t_test2$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r<-round(r, 3)
d2<-t*sqrt(1/(df+1))
mean(data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 1] -
     data_Competitiveness_Educated_respondents$Competitiveness[data_Competitiveness_Educated_respondents$profession_category == 0])
r
d2

t_lci_d<-(as.numeric(t_test2$conf.int[1]))/(as.numeric(sqrt(summary(Competitiveness_model_Educated)$varFix[2,2])))
lci_d<-t_lci_d*sqrt(1/(df+1)) 
lci_d

t_uci_d<-(as.numeric(t_test2$conf.int[2]))/(as.numeric(sqrt(summary(Competitiveness_model_Educated)$varFix[2,2])))
uci_d<-t_uci_d*sqrt(1/(df+1)) 
uci_d


# difference in effect size of profession category between scientist respondents and educated respondents
diff<-d1-d2
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
levels(plot_data_Competitiveness$profession_category)


Competitiveness_plot_bar<-ggplot(plot_data_Competitiveness, aes(x=profession_category, y=Competitiveness, fill=Respondent_group)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Competitiveness-ci, ymax=Competitiveness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Competitiveness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("Educated","Scientists"), labels=c("Educated", "Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Competitiveness_plot_bar

ggsave("Competitiveness_plot_bar.jpeg", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Competitiveness_plot_bar.pdf", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)


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

# SUPPLEMENT --------------------------------------------------------------

# Read in data from csv ---------------------------------------------------

Data_study_D_file_name<-"Data_study_D_prepared_international.csv"
data <-read.csv(Data_study_D_file_name)

data$X<-factor(data$X)

# Exclude incomplete cases ------------------------------------------------

#### Use complete cases only (as data collected through Qualtrics sample only contained complete resonses)
incomplete<-which(!complete.cases(data[,9:68]))
data<-data[-incomplete,]

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

sum(data$worldpart=="USA", na.rm=T)
data$worldpart[data$Respondent_group=="Educated"]<-"USA"

# Only use Europe, Asia and USA (other groups too small)
data<-subset(data, worldpart=="Europe" | worldpart=="Asia" | worldpart=="USA")


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

# Find and remove outliers  per worldpart -----------------------------------------------------------
# using Boxplot function from car package

# USA
data_USA<-subset(data, worldpart=="USA")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_USA$mean_non_scientists_Objectivity, data_USA$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Objectivity, data_USA$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_non_scientists_Rationality, data_USA$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Rationality, data_USA$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_non_scientists_Openness, data_USA$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Openness, data_USA$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_non_scientists_Intelligence, data_USA$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Intelligence, data_USA$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_non_scientists_Integrity, data_USA$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Integrity, data_USA$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_non_scientists_Competitiveness, data_USA$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_USA$mean_scientists_Competitiveness, data_USA$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))



# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 
data_USA<-data_USA[-outliers,]

# Asia
data_Asia<-subset(data, worldpart=="Asia")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_Asia$mean_non_scientists_Objectivity, data_Asia$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Objectivity, data_Asia$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_non_scientists_Rationality, data_Asia$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Rationality, data_Asia$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_non_scientists_Openness, data_Asia$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Openness, data_Asia$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_non_scientists_Intelligence, data_Asia$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Intelligence, data_Asia$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_non_scientists_Integrity, data_Asia$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Integrity, data_Asia$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_non_scientists_Competitiveness, data_Asia$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_Asia$mean_scientists_Competitiveness, data_Asia$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))

# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 

data_Asia<-data_Asia[-outliers,]


# Europe
data_Europe<-subset(data, worldpart=="Europe")

# create variable containing outliers

outliers<-unique(c(Boxplot(data_Europe$mean_non_scientists_Objectivity, data_Europe$Respondent_group,formula = mean_non_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Objectivity, data_Europe$Respondent_group,formula = mean_scientists_Objectivity ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_non_scientists_Rationality, data_Europe$Respondent_group,formula = mean_non_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Rationality, data_Europe$Respondent_group,formula = mean_scientists_Rationality ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_non_scientists_Openness, data_Europe$Respondent_group,formula = mean_non_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Openness, data_Europe$Respondent_group,formula = mean_scientists_Openness ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_non_scientists_Intelligence, data_Europe$Respondent_group,formula = mean_non_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Intelligence, data_Europe$Respondent_group,formula = mean_scientists_Intelligence ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_non_scientists_Integrity, data_Europe$Respondent_group,formula = mean_non_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Integrity, data_Europe$Respondent_group,formula = mean_scientists_Integrity ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_non_scientists_Competitiveness, data_Europe$Respondent_group,formula = mean_non_scientists_Communality ~ Educated + Scientists, range=2),
                   Boxplot(data_Europe$mean_scientists_Competitiveness, data_Europe$Respondent_group,formula = mean_scientists_Communality ~ Educated + Scientists, range=2)))



# display outliers
outliers<-as.numeric(outliers)

# Remove outliers 
data_Europe<-data_Europe[-outliers,]

# merge data again -----------------------------
# combine USA and Asia data

common.names <- intersect(colnames(data_USA), colnames(data_Asia))
data_usa_asia <- rbind(data_USA[, common.names], data_Asia[, common.names])

# combine USA?Asia and Erope data

common.names <- intersect(colnames(data_usa_asia), colnames(data_Europe))
data_combined <- rbind(data_usa_asia[, common.names], data_Europe[, common.names])

data<-data_combined

# Sample descriptives -----------------------------------------------------

# nr of respondents in the Educated group
sum(data$Respondent_group=="Educated")

sum(data$Respondent_group=="Educated")
round(mean(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Educated"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Educated"])), 2)

# nr of respondents in the Scientist group
sum(data$Respondent_group=="Scientists"& data$worldpart=="USA")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="USA"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="USA"])), 2)

sum(data$Respondent_group=="Scientists"& data$worldpart=="Asia")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Asia"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="Asia"])), 2)

sum(data$Respondent_group=="Scientists"& data$worldpart=="Europe")
round(mean(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(SD(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(min(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(max(data$Age[data$Respondent_group=="Scientists" & data$worldpart=="Europe"], na.rm=T),digits =1)
round(prop.table(table(data$Gender[data$Respondent_group=="Scientists" & data$worldpart=="Europe"])), 2)

# # use SCIENTIST DATA ONLY -----------------------------------------------
# Use only scientist data
data<-subset(data, Respondent_group=="Scientists")

# Change dataframe into long format ---------------------------------------

# 2 rows per participant (data has one within-subjects variable)
#add id variable
data$id<-1:nrow(data)

#OBJECTIVITY
#long format
data_Objectivity<-melt(data[,c("id","mean_scientists_Objectivity",
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
data_Objectivity[1:10,]
data_Objectivity[175:185,]
data_Objectivity <- rename(data_Objectivity, c(variable="Profession"))
data_Objectivity <- rename(data_Objectivity, c(value="Objectivity"))
data_Objectivity$Gender<-as.factor(data_Objectivity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Objectivity$profession_category<-as.factor(ifelse(data_Objectivity$Profession =="mean_scientists_Objectivity",1,0))

#RATIONALITY
#long format
data_Rationality<-melt(data[,c("id","mean_scientists_Rationality",
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
data_Rationality <- rename(data_Rationality, c(variable="Profession"))
data_Rationality <- rename(data_Rationality, c(value="Rationality"))
data_Rationality$Gender<-as.factor(data_Rationality$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Rationality$profession_category<-as.factor(ifelse(data_Rationality$Profession =="mean_scientists_Rationality",1,0))


# OPENNESS
#long format
data_Openness<-melt(data[,c("id","mean_scientists_Openness",
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
data_Openness <- rename(data_Openness, c(variable="Profession"))
data_Openness <- rename(data_Openness, c(value="Openness"))
data_Openness$Gender<-as.factor(data_Openness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Openness$profession_category<-as.factor(ifelse(data_Openness$Profession =="mean_scientists_Openness",1,0))

# INTELLIGENCE
#long format
data_Intelligence<-melt(data[,c("id","mean_scientists_Intelligence",
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
data_Intelligence <- rename(data_Intelligence, c(variable="Profession"))
data_Intelligence <- rename(data_Intelligence, c(value="Intelligence"))
data_Intelligence$Gender<-as.factor(data_Intelligence$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Intelligence$profession_category<-as.factor(ifelse(data_Intelligence$Profession =="mean_scientists_Intelligence",1,0))

# INTEGRITY
#long format
data_Integrity<-melt(data[,c("id","mean_scientists_Integrity",
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
data_Integrity <- rename(data_Integrity, c(variable="Profession"))
data_Integrity <- rename(data_Integrity, c(value="Integrity"))
data_Integrity$Gender<-as.factor(data_Integrity$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Integrity$profession_category<-as.factor(ifelse(data_Integrity$Profession =="mean_scientists_Integrity",1,0))

# COMPETITIVENESS
#long format
data_Competitiveness<-melt(data[,c("id","mean_scientists_Competitiveness",
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
data_Competitiveness <- rename(data_Competitiveness, c(variable="Profession"))
data_Competitiveness <- rename(data_Competitiveness, c(value="Competitiveness"))
data_Competitiveness$Gender<-as.factor(data_Competitiveness$Gender)

#code professions as scientist (1) or other highy-educated profession targets (0)
data_Competitiveness$profession_category<-as.factor(ifelse(data_Competitiveness$Profession =="mean_scientists_Competitiveness",1,0))

#  plots per worldpart ----------------------------------------------------

# OBJECTIVITY -------------------------------------------------------------
plot_data_Objectivity <- summarySE(data_Objectivity, measurevar="Objectivity", groupvars=c("worldpart","profession_category"))

Objectivity_plot_bar<-ggplot(plot_data_Objectivity, aes(x=profession_category, y=Objectivity, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Objectivity-ci, ymax=Objectivity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Objectivity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Objectivity_plot_bar

ggsave("Objectivity_plot_bar_international.jpeg", Objectivity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Objectivity_plot_bar_international.pdf", Objectivity_plot_bar,dpi=600, width = 9, height = 6)

# RATIONALITY -------------------------------------------------------------
plot_data_Rationality <- summarySE(data_Rationality, measurevar="Rationality", groupvars=c("worldpart","profession_category"))
levels(plot_data_Rationality$profession_category)


Rationality_plot_bar<-ggplot(plot_data_Rationality, aes(x=profession_category, y=Rationality, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rationality-ci, ymax=Rationality+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Rationality") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Rationality_plot_bar

ggsave("Rationality_plot_bar_international.jpeg", Rationality_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Rationality_plot_bar_international.pdf", Rationality_plot_bar,dpi=600, width = 9, height = 6)



# OPENNESS ----------------------------------------------------------------
plot_data_Openness <- summarySE(data_Openness, measurevar="Openness", groupvars=c("worldpart","profession_category"))
levels(plot_data_Openness$profession_category)

Openness_plot_bar<-ggplot(plot_data_Openness, aes(x=profession_category, y=Openness, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Openness-ci, ymax=Openness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Openness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Openness_plot_bar

ggsave("Openness_plot_bar_international.jpeg", Openness_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Openness_plot_bar_international.pdf", Openness_plot_bar,dpi=600, width = 9, height = 6)

# INTELLIGENCE ------------------------------------------------------------
plot_data_Intelligence <- summarySE(data_Intelligence, measurevar="Intelligence", groupvars=c("worldpart","profession_category"))
levels(plot_data_Intelligence$profession_category)

Intelligence_plot_bar<-ggplot(plot_data_Intelligence, aes(x=profession_category, y=Intelligence, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intelligence-ci, ymax=Intelligence+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Intelligence") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Intelligence_plot_bar

ggsave("Intelligence_plot_bar_international.jpeg", Intelligence_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Intelligence_plot_bar_international.pdf", Intelligence_plot_bar,dpi=600, width = 9, height = 6)


# INTEGRITY ---------------------------------------------------------------
plot_data_Integrity <- summarySE(data_Integrity, measurevar="Integrity", groupvars=c("worldpart","profession_category"))
levels(plot_data_Integrity$profession_category)

Integrity_plot_bar<-ggplot(plot_data_Integrity, aes(x=profession_category, y=Integrity, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Integrity-ci, ymax=Integrity+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Integrity") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Integrity_plot_bar

ggsave("Integrity_plot_bar_international.jpeg", Integrity_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Integrity_plot_bar_international.pdf", Integrity_plot_bar,dpi=600, width = 9, height = 6)

# COMPETITIVENESS ---------------------------------------------------------
plot_data_Competitiveness <- summarySE(data_Competitiveness, measurevar="Competitiveness", groupvars=c("worldpart","profession_category"))
levels(plot_data_Competitiveness$profession_category)

Competitiveness_plot_bar<-ggplot(plot_data_Competitiveness, aes(x=profession_category, y=Competitiveness, fill=worldpart)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Competitiveness-ci, ymax=Competitiveness+ci),
                width=.2, 
                position=position_dodge(.9)) +
  ggtitle("Competitiveness") +
  ylab("Rating (0-100)") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_discrete(name="Respondent group", breaks=c("USA", "Asia","Europe"), labels=c("USA Scientists", "Asian Scientists", "European Scientists"))+
  scale_x_discrete(breaks=c(0,1), labels=c("Other", "Scientists"), name="Profession category")+
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x = element_text(size=12, vjust = -0.2),
        axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12, vjust=1.5),
        axis.text.y  = element_text(size=12)) +
  theme(plot.title = element_text(face=c("bold"), size=18,vjust=1.2))

Competitiveness_plot_bar

ggsave("Competitiveness_plot_bar_international.jpeg", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)
ggsave("Competitiveness_plot_bar_international.pdf", Competitiveness_plot_bar,dpi=600, width = 9, height = 6)

# Create multipanel plot --------------------------------------------------

mylegend<-g_legend(Objectivity_plot_bar)

jpeg("multipanel_plot_study_D_bars_international.jpeg", width = 24, height = 30, units = "cm", quality = 100, res =300)
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Competitiveness_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()


pdf("multipanel_plot_study_D_bars_international.pdf")
multipanel_plot <- grid.arrange(arrangeGrob(Objectivity_plot_bar + theme(legend.position="none"),
                                            Rationality_plot_bar + theme(legend.position="none"),
                                            Openness_plot_bar + theme(legend.position="none"),
                                            Intelligence_plot_bar + theme(legend.position="none"),
                                            Integrity_plot_bar + theme(legend.position="none"),
                                            Competitiveness_plot_bar + theme(legend.position="none"),
                                            nrow=3),mylegend, nrow=2,heights=c(10, 3))


dev.off()
