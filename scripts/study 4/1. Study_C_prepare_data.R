
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Study_C_data_file_name<-"Study_C.csv"
Study_C_varnames_file_name <-"Study_C_new_variable_names.csv"
Data_Study_C <-read.csv(Study_C_data_file_name)
Varnames_Study_C<-read.csv2(Study_C_varnames_file_name)


# Check dataframe ---------------------------------------------------------


#show first 5 rows and all columns to check if it looks okay
head(Data_Study_C) 
head(Varnames_Study_C)
names(Data_Study_C)
Varnames_Study_C

Data_Study_C<-Data_Study_C[-1,]
head(Data_Study_C)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_C)

#replace variable names
colnames(Data_Study_C)<-Varnames_Study_C$New_name

#check new variable names
names(Data_Study_C)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_C <- Data_Study_C[,-grep("delete",names(Data_Study_C))]
names(Data_Study_C)



# Merge variable that are the same but had different ----------------------
# names in different conditions

#create variable 'Target' (scientists or highly-educated people judged by participant)
Data_Study_C$Target<- ifelse(Data_Study_C$Condition ==  "Male", "Male Scientists", "Female Scientists")
Data_Study_C<-Data_Study_C[is.na(Data_Study_C$Target)==F,]

#create variable containing all variables starting with 'ECS' or 'ES' or PHD' 
FS_ <- (grep("FS_",names(Data_Study_C)))
MS_ <- grep("MS_",names(Data_Study_C))


#make all of these numeric variables
Data_Study_C[,c(FS_,MS_)] <- apply(Data_Study_C[,c(FS_,MS_)],2,as.numeric)

# move data in MS_ columns (i.e. Target is Established Scientists) to FS_columns
# so that only the FS_columns are used and can than be assigned a new name
# common to both columns. 
for(i in 1:nrow(Data_Study_C)){
  if(Data_Study_C$Target[i]=="Male Scientists"){
    Data_Study_C[i,FS_] <- Data_Study_C[i,MS_]}} 

Data_Study_C <- Data_Study_C[,-MS_]   


# remove the S_ prefix so that the variable name is no longer specific for Scientists as target
split_names <- unlist(strsplit(names(Data_Study_C)[grep("FS_",names(Data_Study_C))],"_"))
new_names <- split_names[1:length(split_names)%%2==0]

names(Data_Study_C)[grep("FS",names(Data_Study_C))] <- new_names

# show how it looks now 
names(Data_Study_C)
View(Data_Study_C)



# Remove invalid cases ----------------------------------------------------

Data_Study_C<-subset(Data_Study_C, Finished==1)
Data_Study_C<-subset(Data_Study_C, Screening!="Screened_out")
Data_Study_C<-subset(Data_Study_C, Consent==1)
Data_Study_C<-subset(Data_Study_C, Check_phd!=3)
Data_Study_C<-subset(Data_Study_C, Check_work_status!=5)



#check
Data_Study_C$Finished
Data_Study_C$Screening
Data_Study_C$Consent
Data_Study_C$Check_phd
Data_Study_C$Check_work_status



# remove respondents for whom work status is unknown 
Data_Study_C<-Data_Study_C[is.na(Data_Study_C$Check_work_status)==F,]



# Write to file -----------------------------------------------------------

write.csv(Data_Study_C, "Data_Study_C_prepared.csv")

