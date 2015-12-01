
# Initialization ----------------------------------------------------------

# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Educated_sample_data_file_name<-"Data_Study_D_Ed_prepared.csv"
Scientist_sample_data_file_name <-"Data_Study_D_Sc_USA_only_prepared.csv"
Educated_sample_data <-read.csv(Educated_sample_data_file_name)
Scientist_sample_data<-read.csv(Scientist_sample_data_file_name)



# Merge data files --------------------------------------------------------

# combine data files Educated respondents & Scientist respondents (USA only)

common.names <- intersect(colnames(Educated_sample_data), colnames(Scientist_sample_data))
Data_Ed_and_Sc_combined <- rbind(Educated_sample_data[, common.names], Scientist_sample_data[, common.names])

# Write to file -----------------------------------------------------------
write.csv(Data_Ed_and_Sc_combined, "Data_Study_D_combined.csv")


