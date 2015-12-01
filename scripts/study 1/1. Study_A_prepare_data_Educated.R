
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())



# Read in data -----------------------------------------------------------

# read in data from csv
Study_A_Ed_data_file_name<-"Study_A_Sample_Ed.csv"
Study_A_Ed_varnames_file_name <-"Study_A_Sample_Ed_new_variable_names.csv"
Data_Study_A_Ed <-read.csv(Study_A_Ed_data_file_name)
Varnames_Study_A_Ed<-read.csv2(Study_A_Ed_varnames_file_name)


# Check dataframe ---------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_A_Ed) 
head(Varnames_Study_A_Ed)

# remove first row of dataframe as file contains two rows of variable names
Data_Study_A_Ed<-Data_Study_A_Ed[-1,]
head(Data_Study_A_Ed)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_A_Ed)

#replace variable names
colnames(Data_Study_A_Ed)<-Varnames_Study_A_Ed$New_name

#check new variable names
names(Data_Study_A_Ed)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_A_Ed <- Data_Study_A_Ed[,-grep("delete",names(Data_Study_A_Ed))]
names(Data_Study_A_Ed)

#remove empty variable
Data_Study_A_Ed<-Data_Study_A_Ed[,-53]
names(Data_Study_A_Ed)


# Delete invalid cases ----------------------------------------------------

Data_Study_A_Ed<-subset(Data_Study_A_Ed, Finished==1)
Data_Study_A_Ed<-subset(Data_Study_A_Ed, Screening!="Screened_out")
Data_Study_A_Ed<-subset(Data_Study_A_Ed, Consent==1)


# Replace empty cells by NAs ----------------------------------------------

Data_Study_A_Ed[Data_Study_A_Ed==""] <- NA

# Replace -99 by NA
Data_Study_A_Ed[Data_Study_A_Ed==-99] <- NA


# merge variables that are the same but had different names -----------
# in different conditions

#create variable 'Target' (scientists or highly-educated people judged by participant)
Data_Study_A_Ed$Target<- ifelse(Data_Study_A_Ed$Condition ==  "Educated_Educated", "Educated", "Scientists")

#replace empty cells by NAs
Data_Study_A_Ed[Data_Study_A_Ed==""] <- NA

#create variable containing all variables starting with 'S_' or 'H_' 
S_ <- grep("S_",names(Data_Study_A_Ed))
H_ <- grep("H_",names(Data_Study_A_Ed))

#make all of these numeric variables
Data_Study_A_Ed[,c(S_,H_)] <- apply(Data_Study_A_Ed[,c(S_,H_)],2,as.numeric)

# move data in H_ columns (i.e. Target is Highly Educated people) to S_columns
# so that only the S_columns are used and can than be assigned a new name
# common to both columns. 
for(i in 1:nrow(Data_Study_A_Ed)){
  if(Data_Study_A_Ed$Target[i]=="Educated"){
    Data_Study_A_Ed[i,S_] <- Data_Study_A_Ed[i,H_]
  }
}

Data_Study_A_Ed <- Data_Study_A_Ed[,-H_]

# remove the S_ prefix so that the variable name is no longer specific for Scientists as target
split_names <- unlist(strsplit(names(Data_Study_A_Ed)[grep("S_",names(Data_Study_A_Ed))],"_"))
new_names <- split_names[1:length(split_names)%%2==0]

names(Data_Study_A_Ed)[grep("S_",names(Data_Study_A_Ed))] <- new_names

# show how it looks now 
head(Data_Study_A_Ed)
View(Data_Study_A_Ed)


# Write to file -----------------------------------------------------------

write.csv(Data_Study_A_Ed, "Data_Study_A_Ed_prepared.csv")
