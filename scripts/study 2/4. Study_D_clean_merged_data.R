# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Data_Study_D_file_name<-"Data_Study_D_combined.csv"
Data_Study_D <-read.csv(Data_Study_D_file_name)


# Check data frame --------------------------------------------------------

#check names
names(Data_Study_D)

#Remove two reduntant variables (resulting from merging)
Data_Study_D<-Data_Study_D[,-c(1:2)]
names(Data_Study_D)

# Install the car package for easy recoding
if(!require(car)){install.packages('car')}


# Recode variables --------------------------------------------------------

# Load the car package
library(car)

Data_Study_D$Religiousness<-recode(Data_Study_D$Religiousness, "38=1;39=2;40=3;41=4;42=5;43=6;44=7")
Data_Study_D$Age <- 95-Data_Study_D$Year_born
Data_Study_D$Age
names(Data_Study_D)


# Write to file -----------------------------------------------------------

write.csv(Data_Study_D, "Data_Study_D_prepared.csv")
