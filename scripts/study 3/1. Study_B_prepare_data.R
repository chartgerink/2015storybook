
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Study_B_data_file_name<-"Study_B.csv"
Study_B_varnames_file_name <-"Study_B_new_variable_names.csv"
Data_Study_B <-read.csv(Study_B_data_file_name)
Varnames_Study_B<-read.csv2(Study_B_varnames_file_name)


# Check dataframe ---------------------------------------------------------

#show first 5 rows and all columns to check if it looks okay
head(Data_Study_B) 
head(Varnames_Study_B)

Data_Study_B<-Data_Study_B[-1,]
head(Data_Study_B)


# Change variable names ---------------------------------------------------

#show original column names
names(Data_Study_B)

#replace variable names
colnames(Data_Study_B)<-Varnames_Study_B$New_name

#check new variable names
names(Data_Study_B)



# Delete redundant variables ----------------------------------------------

#delete variables to be deleted renamed as 'delete1', 'delete2' etc.
Data_Study_B <- Data_Study_B[,-grep("delete",names(Data_Study_B))]
names(Data_Study_B)


#remove empty variable
Data_Study_B<-Data_Study_B[,-72]
names(Data_Study_B)





# Remove invalid cases ----------------------------------------------------

Data_Study_B<-subset(Data_Study_B, Finished==1)
Data_Study_B<-subset(Data_Study_B, Screening!="Screened_out")
Data_Study_B<-subset(Data_Study_B, Consent==1)
Data_Study_B<-subset(Data_Study_B, Check_phd!=3)
Data_Study_B<-subset(Data_Study_B, Check_work_status!=5)
Data_Study_B<-subset(Data_Study_B, Check_phd_research!=2)


#check
Data_Study_B$Finished
Data_Study_B$Screening
Data_Study_B$Consent
Data_Study_B$Check_phd
Data_Study_B$Check_work_status
Data_Study_B$Check_phd_research



# Replace empty cells by NAs ----------------------------------------------

Data_Study_B[Data_Study_B==""] <- NA

# Replace -99 by NA
Data_Study_B[Data_Study_B==-99] <- NA


# Merge variables that are the same but had different ---------------------
# names in different conditions

#create variable 'Target' (scientists or highly-educated people judged by participant)
Data_Study_B$Target<- ifelse(Data_Study_B$Condition ==  "Established", "Established Scientists", ifelse(Data_Study_B$Condition ==  "Early", "Early-career Scientists", "PhD Students"))
Data_Study_B<-Data_Study_B[is.na(Data_Study_B$Target)==F,]

#create variable containing all variables starting with 'ECS' or 'ES' or PHD' 
ECS_ <- (grep("ECS_",names(Data_Study_B)))
ES_ <- grep("ES_",names(Data_Study_B))


#make all of these numeric variables
Data_Study_B[,c(ECS_,ES_)] <- apply(Data_Study_B[,c(ECS_,ES_)],2,as.numeric)

# move data in ES_ columns (i.e. Target is Established Scientists) to ECS_columns
# so that only the ECH_columns are used and can than be assigned a new name
# common to both columns. 
for(i in 1:nrow(Data_Study_B)){
  if(Data_Study_B$Target[i]=="Established Scientists"){
    Data_Study_B[i,ECS_] <- Data_Study_B[i,ES_]}} 

Data_Study_B <- Data_Study_B[,-ES_]   


PHD_ <- (grep("PHD_",names(Data_Study_B)))
Data_Study_B[,PHD_] <- apply(Data_Study_B[,PHD_],2,as.numeric)


for(i in 1:nrow(Data_Study_B)){
  if(Data_Study_B$Target[i]=="PhD Students"){
        Data_Study_B[i,ECS_] <- Data_Study_B[i,PHD_]}}



Data_Study_B <- Data_Study_B[,-PHD_]



# remove the S_ prefix so that the variable name is no longer specific for Scientists as target
split_names <- unlist(strsplit(names(Data_Study_B)[grep("ECS_",names(Data_Study_B))],"_"))
new_names <- split_names[1:length(split_names)%%2==0]

names(Data_Study_B)[grep("ECS",names(Data_Study_B))] <- new_names

# show how it looks now 
names(Data_Study_B)
View(Data_Study_B)



# Write to file -----------------------------------------------------------

# all respondents
write.csv(Data_Study_B, "Data_Study_B_prepared.csv")

