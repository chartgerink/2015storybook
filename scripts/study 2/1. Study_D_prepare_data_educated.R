
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

# read in data from csv
Study_D_Ed_data_file_name<-"Study_D_Sample_Ed.csv"
Study_D_Ed_varnames_file_name <-"Study_D_Sample_Ed_new_variable_names.csv"
Data_Study_D_Ed <-read.csv(Study_D_Ed_data_file_name)
Varnames_Study_D_Ed<-read.csv2(Study_D_Ed_varnames_file_name)


# Check data frame --------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_D_Ed) 
head(Varnames_Study_D_Ed)

Data_Study_D_Ed<-Data_Study_D_Ed[-1,]
head(Data_Study_D_Ed)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_D_Ed)

#replace variable names
colnames(Data_Study_D_Ed)<-Varnames_Study_D_Ed$New_name

#check new variable names
names(Data_Study_D_Ed)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_D_Ed <- Data_Study_D_Ed[,-grep("delete",names(Data_Study_D_Ed))]
names(Data_Study_D_Ed)

#remove empty variable
Data_Study_D_Ed<-Data_Study_D_Ed[,-76]
names(Data_Study_D_Ed)



# Remove invalid cases ----------------------------------------------------

Data_Study_D_Ed<-subset(Data_Study_D_Ed, Finished==1)
Data_Study_D_Ed<-subset(Data_Study_D_Ed, Screening!="Screened_out")
Data_Study_D_Ed<-subset(Data_Study_D_Ed, Consent==1)



# Replace empty cells by NAs ----------------------------------------------

Data_Study_D_Ed[Data_Study_D_Ed==""] <- NA

# Replace -99 by NA
Data_Study_D_Ed[Data_Study_D_Ed==-99] <- NA




# Write to file -----------------------------------------------------------

write.csv(Data_Study_D_Ed, "Data_Study_D_Ed_prepared.csv")
