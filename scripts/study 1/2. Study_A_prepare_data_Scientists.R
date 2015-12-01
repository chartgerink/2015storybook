
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Study_A_Sc_data_file_name<-"Study_A_Sample_Sc.csv"
Study_A_Sc_varnames_file_name <-"Study_A_Sample_Sc_new_variable_names.csv"
Data_Study_A_Sc <-read.csv(Study_A_Sc_data_file_name)
Varnames_Study_A_Sc<-read.csv2(Study_A_Sc_varnames_file_name)


# Check dataframe ---------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_A_Sc) 
head(Varnames_Study_A_Sc)

Data_Study_A_Sc<-Data_Study_A_Sc[-1,]
head(Data_Study_A_Sc)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_A_Sc)

#replace variable names
colnames(Data_Study_A_Sc)<-Varnames_Study_A_Sc$New_name

#check new variable names
names(Data_Study_A_Sc)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_A_Sc <- Data_Study_A_Sc[,-grep("delete",names(Data_Study_A_Sc))]
names(Data_Study_A_Sc)

#remove redundant variable'V1'
Data_Study_A_Sc<-Data_Study_A_Sc[,-1]
names(Data_Study_A_Sc)
#remove empty variable
Data_Study_A_Sc<-Data_Study_A_Sc[,-53]
names(Data_Study_A_Sc)


# Remove invalid cases ----------------------------------------------------

Data_Study_A_Sc<-subset(Data_Study_A_Sc, Finished==1)
Data_Study_A_Sc<-subset(Data_Study_A_Sc, Screening!="Screened_out")
Data_Study_A_Sc<-subset(Data_Study_A_Sc, Consent==1)



# Replace empty cells by NAs ----------------------------------------------

Data_Study_A_Sc[Data_Study_A_Sc==""] <- NA

# Replace -99 by NA
Data_Study_A_Sc[Data_Study_A_Sc==-99] <- NA


# merge variables that are the same but had  different --------------------
# names in different conditions

#create variable 'Target' (scientists or highly-educated people judged by participant)
Data_Study_A_Sc$Target<- ifelse(Data_Study_A_Sc$Condition ==  "Scientists_Educated", "Educated", "Scientists")


#create variable containing all variables starting with 'S_' or 'H_' 
S_ <- grep("S_",names(Data_Study_A_Sc))
H_ <- grep("H_",names(Data_Study_A_Sc))

#make all of these numeric variables
Data_Study_A_Sc[,c(S_,H_)] <- apply(Data_Study_A_Sc[,c(S_,H_)],2,as.numeric)

# move data in H_ columns (i.e. Target is Highly Educated people) to S_columns
# so that only the S_columns are used and can than be assigned a new name
# common to both columns. 
for(i in 1:nrow(Data_Study_A_Sc)){
  if(Data_Study_A_Sc$Target[i]=="Educated"){
    Data_Study_A_Sc[i,S_] <- Data_Study_A_Sc[i,H_]
  }
}

Data_Study_A_Sc <- Data_Study_A_Sc[,-H_]

# remove the S_ prefix so that the variable name is no longer specific for Scientists as target
split_names <- unlist(strsplit(names(Data_Study_A_Sc)[grep("S_",names(Data_Study_A_Sc))],"_"))
new_names <- split_names[1:length(split_names)%%2==0]

names(Data_Study_A_Sc)[grep("S_",names(Data_Study_A_Sc))] <- new_names

# show how it looks now 
head(Data_Study_A_Sc)
View(Data_Study_A_Sc)


# Write to file -----------------------------------------------------------


# all respondents
write.csv(Data_Study_A_Sc, "Data_Study_A_Sc_all_countries_prepared.csv")

# USA respondents only
Data_Study_A_Sc_USA_only<-subset(Data_Study_A_Sc, Country==187)
write.csv(Data_Study_A_Sc_USA_only, "Data_Study_A_Sc_USA_only_prepared.csv")