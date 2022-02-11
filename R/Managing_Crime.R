
# Managing Crime Data from NIBRS

library(tidyverse)

library(readr)

NIBRS_incident <- read_csv("Crime_Data_NIBRS/AL/NIBRS_incident.csv")
View(NIBRS_incident)

NIBRS_VICTIM <- read_csv("Crime_Data_NIBRS/AL/NIBRS_VICTIM.csv")

agencies <- read_csv("Crime_Data_NIBRS/AL/agencies.csv")

# make empty data set for incident data
incidents <- data.frame(matrix(ncol = 15, nrow = 0))
names(incidents) <- c("DATA_YEAR", "AGENCY_ID", "INCIDENT_ID", "NIBRS_MONTH_ID",    
                       "CARGO_THEFT_FLAG", "SUBMISSION_DATE", "INCIDENT_DATE", "REPORT_DATE_FLAG",  
                        "INCIDENT_HOUR", "CLEARED_EXCEPT_ID", "CLEARED_EXCEPT_DATE", "INCIDENT_STATUS",   
                        "DATA_HOME", "ORIG_FORMAT" , "DID")

# get a list of the folders in Crime_Data_NIBRS
files <- list.files(path="C:\\Users\\streh\\OneDrive\\Documents\\Crime_Data_NIBRS")

# go through each folder in Crime_Data_NIBRS and find corresponding incident 
# table to bind it to incidents data frame
for (i in files){
  NIBRS_incident <- read_csv(gsub(' ', '', paste("Crime_Data_NIBRS/", i, "/NIBRS_incident.csv")))
  incidents <- rbind(incidents, NIBRS_incident)
}


# make empty data set for victim data
victims <- data.frame(matrix(ncol = 16, nrow = 0))
names(victims) <- c("DATA_YEAR", "VICTIM_ID", "INCIDENT_ID", "VICTIM_SEQ_NUM",   
                    "VICTIM_TYPE_ID", "ASSIGNMENT_TYPE_ID", "ACTIVITY_TYPE_ID", "OUTSIDE_AGENCY_ID",  
                    "AGE_ID", "AGE_NUM", "SEX_CODE", "RACE_ID",           
                    "ETHNICITY_ID", "RESIDENT_STATUS_CODE", "AGE_RANGE_LOW_NUM", "AGE_RANGE_HIGH_NUM")  

for (i in files){
  NIBRS_VICTIM <- read_csv(gsub(' ', '', paste("Crime_Data_NIBRS/", i, "/NIBRS_VICTIM.csv")))
  victims <- rbind(victims, NIBRS_VICTIM)
}
  

write_csv(x = incidents, file = "nibrs_incidents_2020.csv")
write_csv(x = victims, file = "nibrs_victims_2020.csv")
# need: agencies.csv, NIBRS_CRIMINAL_ACT.csv, 

  






  
