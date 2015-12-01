
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Study_D_Sc_data_file_name<-"Study_D_Sample_Sc.csv"
Study_D_Sc_varnames_file_name <-"Study_D_Sample_Sc_new_variable_names.csv"
Data_Study_D_Sc <-read.csv(Study_D_Sc_data_file_name)
Varnames_Study_D_Sc<-read.csv2(Study_D_Sc_varnames_file_name)



# Check dataframe ---------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_D_Sc) 
head(Varnames_Study_D_Sc)

Data_Study_D_Sc<-Data_Study_D_Sc[-1,]
head(Data_Study_D_Sc)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_D_Sc)

#replace variable names
colnames(Data_Study_D_Sc)<-Varnames_Study_D_Sc$New_name

#check new variable names
names(Data_Study_D_Sc)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_D_Sc <- Data_Study_D_Sc[,-grep("delete",names(Data_Study_D_Sc))]
names(Data_Study_D_Sc)

#remove redundant variable'V1'
Data_Study_D_Sc<-Data_Study_D_Sc[,-1]
names(Data_Study_D_Sc)
#remove empty variable
Data_Study_D_Sc<-Data_Study_D_Sc[,-75]
names(Data_Study_D_Sc)





# Remove invalid cases ----------------------------------------------------

Data_Study_D_Sc<-subset(Data_Study_D_Sc, Finished==1)
Data_Study_D_Sc<-subset(Data_Study_D_Sc, Screening!="Screened_out")
Data_Study_D_Sc<-subset(Data_Study_D_Sc, Consent==1)



# Replace empty cells by NAs ----------------------------------------------


Data_Study_D_Sc[Data_Study_D_Sc==""] <- NA

# Replace -99 by NA
Data_Study_D_Sc[Data_Study_D_Sc==-99] <- NA


# show how it looks now 
head(Data_Study_D_Sc)
View(Data_Study_D_Sc)




# Write to file -----------------------------------------------------------

# all respondents
write.csv(Data_Study_D_Sc, "Data_Study_D_Sc_all_countries_prepared.csv")

# USA respondents only
Data_Study_D_Sc_USA_only<-subset(Data_Study_D_Sc, Country==187)
write.csv(Data_Study_D_Sc_USA_only, "Data_Study_D_Sc_USA_only_prepared.csv")
