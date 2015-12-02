
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Data_study_A_file_name<-"Data_Study_A_combined_international.csv"
Data_study_A <-read.csv(Data_study_A_file_name)



# Check dataframe ---------------------------------------------------------

#check names
names(Data_study_A)

#Remove two reduntant variables (resulting from merging)
Data_study_A<-Data_study_A[,-c(1:2)]
names(Data_study_A)


# Recode variables --------------------------------------------------------

# Install the car package for easy recoding
if(!require(car)){install.packages('car')}

# Load the car package
library(car)

Data_study_A$Religiousness<-recode(Data_study_A$Religiousness, "38=1;39=2;40=3;41=4;42=5;43=6;44=7")
Data_study_A$Age <- 95-Data_study_A$Year_born
Data_study_A$Age
names(Data_study_A)
Data_study_A<-Data_study_A[,-28]
names(Data_study_A)


# Create outcome variables: mean per characteristic ----------------------------------------------------

# compute variables per characteristic
Data_study_A$Objectivity<-apply(Data_study_A[,names(Data_study_A)=="ob1"|
                                               names(Data_study_A)=="ob2"|
                                               names(Data_study_A)=="ob3"],1,mean, na.rm=T)

Data_study_A$Rationality<-apply(Data_study_A[,names(Data_study_A)=="ra1"|
                                               names(Data_study_A)=="ra2"|
                                               names(Data_study_A)=="ra3"],1,mean, na.rm=T)

Data_study_A$Openness<-apply(Data_study_A[,names(Data_study_A)=="op1"|
                                            names(Data_study_A)=="op2"|
                                            names(Data_study_A)=="op3"],1,mean, na.rm=T)

Data_study_A$Intelligence<-apply(Data_study_A[,names(Data_study_A)=="iq1"|
                                                names(Data_study_A)=="iq2"|
                                                names(Data_study_A)=="iq3"],1,mean, na.rm=T)

Data_study_A$Integrity<-apply(Data_study_A[,names(Data_study_A)=="in1"|
                                                names(Data_study_A)=="in2"|
                                                names(Data_study_A)=="in3"],1,mean, na.rm=T)

Data_study_A$Communality<-apply(Data_study_A[,names(Data_study_A)=="co1"|
                                               names(Data_study_A)=="co2"|
                                               names(Data_study_A)=="co3"],1,mean, na.rm=T)


# Write to file -----------------------------------------------------------

write.csv(Data_study_A, "Data_study_A_prepared_international.csv")
