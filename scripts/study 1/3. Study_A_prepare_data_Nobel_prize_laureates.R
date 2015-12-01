
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Study_A_Npw_data_file_name<-"Study_A_Sample_NPW.csv"
Study_A_Npw_varnames_file_name <-"Study_A_Sample_Npw_new_variable_names.csv"
Data_Study_A_Npw <-read.csv(Study_A_Npw_data_file_name)
Varnames_Study_A_Npw<-read.csv2(Study_A_Npw_varnames_file_name)


# Check dataframe ---------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_A_Npw) 
head(Varnames_Study_A_Npw)

Data_Study_A_Npw<-Data_Study_A_Npw[-1,]
head(Data_Study_A_Npw)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_A_Npw)

#replace variable names
colnames(Data_Study_A_Npw)<-Varnames_Study_A_Npw$New_name

#check new variable names
names(Data_Study_A_Npw)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_A_Npw <- Data_Study_A_Npw[,-grep("delete",names(Data_Study_A_Npw))]
names(Data_Study_A_Npw)

#remove empty variable
Data_Study_A_Npw<-Data_Study_A_Npw[,-34]
names(Data_Study_A_Npw)



# Remove invalid cases ----------------------------------------------------

Data_Study_A_Npw<-subset(Data_Study_A_Npw, Finished==1)
Data_Study_A_Npw<-subset(Data_Study_A_Npw, Screening!="Screened_out")
Data_Study_A_Npw<-subset(Data_Study_A_Npw, Consent==1)



# Replace empty cells by NAs ----------------------------------------------

Data_Study_A_Npw[Data_Study_A_Npw==""] <- NA

# Replace -99 by NA
Data_Study_A_Npw[Data_Study_A_Npw==-99] <- NA


# Add Target variable  ----------------------------------------------------

#create variable 'Target' (to be able to merge this file with the two data sets that do have conditions)
Data_Study_A_Npw$Target<- ifelse(Data_Study_A_Npw$Respondent_group == "Scientists", "Scientists", "Scientists")



# Remove S_ prefix from variable names ------------------------------------

#create variable containing all variables starting with 'S_ 
S_ <- grep("S_",names(Data_Study_A_Npw))

# remove the S_ prefix so that the variable name is no longer specific for Scientists as target
split_names <- unlist(strsplit(names(Data_Study_A_Npw)[grep("S_",names(Data_Study_A_Npw))],"_"))
new_names <- split_names[1:length(split_names)%%2==0]

names(Data_Study_A_Npw)[grep("S_",names(Data_Study_A_Npw))] <- new_names

# show how it looks now 
head(Data_Study_A_Npw)
View(Data_Study_A_Npw)



# Correct variable naming error in survey programming ------------------------------
# change 'Respondent Group' from Scientists to NPW (error in survey file) 
Data_Study_A_Npw$Respondent_group<-ifelse(Data_Study_A_Npw$Respondent_group=="Scientists", "NPW", "NPW")


# Write to file -----------------------------------------------------------


write.csv(Data_Study_A_Npw, "Data_Study_A_Npw_prepared.csv")

