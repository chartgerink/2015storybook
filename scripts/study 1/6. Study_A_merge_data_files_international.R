
# Initialization ----------------------------------------------------------


# start clean by removing variables in workspace
rm(list=ls())


# Read in data ------------------------------------------------------------

#read in data from csv
Educated_sample_data_file_name<-"Data_Study_A_Ed_prepared.csv"
Scientist_sample_data_file_name <-"Data_Study_A_Sc_all_countries_prepared.csv"
Nobel_prize_winners_data_file_name <-"Data_Study_A_Npw_prepared.csv"
Educated_sample_data <-read.csv(Educated_sample_data_file_name)
Scientist_sample_data<-read.csv(Scientist_sample_data_file_name)
Nobel_prize_winners_data<-read.csv(Nobel_prize_winners_data_file_name)



# Merge datafiles ---------------------------------------------------------

# combine data files Educated respondents & Scientist respondents (USA only)

common.names <- intersect(colnames(Educated_sample_data), colnames(Scientist_sample_data))
Data_Ed_and_Sc <- rbind(Educated_sample_data[, common.names], Scientist_sample_data[, common.names])

# combine data files Data_Ed_and_Sc  & Nobel Prize winners repondents 

common.names <- intersect(colnames(Data_Ed_and_Sc), colnames(Nobel_prize_winners_data))
Data_Study_A_combined <- rbind(Data_Ed_and_Sc[, common.names], Nobel_prize_winners_data[, common.names])


# Write to file -----------------------------------------------------------

write.csv(Data_Study_A_combined, "Data_Study_A_combined_international.csv")