
# Managing Crime Data from NIBRS

library(tidyverse)

library(data.table)

library(stats)

library(dplyr)

library(readr)

NIBRS_incident <- read_csv("Crime_Data_2018/AR/NIBRS_incident.csv")


NIBRS_VICTIM <- read_csv("Crime_Data_2018/AR/NIBRS_VICTIM.csv")

agencies <- read_csv("Crime_Data_2018/AR/agencies.csv")

# make empty data set for incident data
incidents <- data.frame(matrix(ncol = 15, nrow = 0))
names(incidents) <- c("DATA_YEAR", "AGENCY_ID", "INCIDENT_ID", "NIBRS_MONTH_ID",    
                      "CARGO_THEFT_FLAG", "SUBMISSION_DATE", "INCIDENT_DATE", "REPORT_DATE_FLAG",  
                      "INCIDENT_HOUR", "CLEARED_EXCEPT_ID", "CLEARED_EXCEPT_DATE", "INCIDENT_STATUS",   
                      "DATA_HOME", "ORIG_FORMAT" , "DID")

# get a list of the folders in Crime_Data_NIBRS
files <- list.files(path="CRIME_DATA_2018")

# go through each folder in Crime_Data_NIBRS and find corresponding incident 
# table to bind it to incidents data frame
for (i in files){
  NIBRS_incident <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_incident.csv")))
  incidents <- rbind(incidents, NIBRS_incident)
}


incidents <- incidents[ , c("DATA_YEAR", "AGENCY_ID", "INCIDENT_ID",
                            "INCIDENT_DATE", "INCIDENT_HOUR")]

# make empty data set for victim data
victims <- data.frame(matrix(ncol = 16, nrow = 0))
names(victims) <- c("DATA_YEAR", "VICTIM_ID", "INCIDENT_ID", "VICTIM_SEQ_NUM",   
                    "VICTIM_TYPE_ID", "ASSIGNMENT_TYPE_ID", "ACTIVITY_TYPE_ID", "OUTSIDE_AGENCY_ID",  
                    "AGE_ID", "AGE_NUM", "SEX_CODE", "RACE_ID",           
                    "ETHNICITY_ID", "RESIDENT_STATUS_CODE", "AGE_RANGE_LOW_NUM", "AGE_RANGE_HIGH_NUM")  

for (i in files){
  NIBRS_VICTIM <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_VICTIM.csv")))
  victims <- rbind(victims, NIBRS_VICTIM)
}


victims <- victims[ , c("VICTIM_ID", "INCIDENT_ID", 
                        "AGE_NUM", "SEX_CODE")]


agency_list <- data.frame(matrix(ncol = 59, nrow = 0))

names(agency_list) <- names(agencies)

for (i in files){
  agencies <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/agencies.csv")))
  agency_list <- rbind(agency_list, agencies)
}

agency_list <- agency_list[ , c("YEARLY_AGENCY_ID", "AGENCY_ID", "STATE_NAME", "STATE_ABBR",
                                "AGENCY_TYPE_NAME", "UCR_AGENCY_NAME", "COUNTY_NAME")]

NIBRS_OFFENDER <- read_csv("Crime_Data_2018/AL/NIBRS_OFFENDER.csv")

offenders <- data.frame(matrix(ncol = 11, nrow = 0))

names(offenders) <- names(NIBRS_OFFENDER)



for (i in files){
  NIBRS_OFFENDER <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_OFFENDER.csv")))
  offenders <- rbind(offenders, NIBRS_OFFENDER)
}

offenders <- offenders[ , c("OFFENDER_ID", "INCIDENT_ID", 
                            "AGE_NUM", "SEX_CODE")]

NIBRS_OFFENSE <- read_csv("Crime_Data_2018/AL/NIBRS_OFFENSE.csv")

offense <- data.frame(matrix(ncol = 8, nrow = 0))

names(offense) <- names(NIBRS_OFFENSE)

for (i in files){
  NIBRS_OFFENSE <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_OFFENSE.csv")))
  offense <- rbind(offense, NIBRS_OFFENSE)
}

offense <- offense[ , c("OFFENSE_ID", "INCIDENT_ID", 
                        "OFFENSE_TYPE_ID", "LOCATION_ID")]

NIBRS_VICTIM_OFFENDER_REL <- read_csv("Crime_data_2018/AL/NIBRS_VICTIM_OFFENDER_REL.csv")

victim_offender_rel <- data.frame(matrix(ncol = 5, nrow = 0))

names(victim_offender_rel) <- names(NIBRS_VICTIM_OFFENDER_REL)


for (i in files){
  NIBRS_VICTIM_OFFENDER_REL <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_VICTIM_OFFENDER_REL.csv")))
  victim_offender_rel <- rbind(victim_offender_rel, NIBRS_VICTIM_OFFENDER_REL)
}

victim_offender_rel <- victim_offender_rel[ , c("VICTIM_ID", "OFFENDER_ID",
                                                "RELATIONSHIP_ID")]

# now we have incidents, victims, offenders, victim offender rel, and offense

# so now merge incidents with victims
inc_vic <- left_join(victims, incidents, by = "INCIDENT_ID")
inc_vic <- na.omit(inc_vic)

# now merge inc_vic with offender
# note: 'x' specifications refer to victim details
# 'y' specifications refer to offender details
vic_off <- left_join(inc_vic, offenders, by = "INCIDENT_ID")
vic_off <- na.omit(vic_off)

# add vic off rel
vic_off_rel <- left_join(vic_off, victim_offender_rel, by = c("VICTIM_ID", 
                                                              "OFFENDER_ID"))
vic_off_rel <- na.omit(vic_off_rel)

# now we need offense and agency data added
almost_there <- left_join(vic_off_rel, offense, by = "INCIDENT_ID")
almost_there <- na.omit(almost_there)

# now add agency data
woohoo <- left_join(almost_there, agency_list, by = "AGENCY_ID")

# there seems to be a lot of situations where the relationship ID is
# NA, so we are going to filter these out.

woohoo <- na.omit(woohoo)

# make woohoo a little more manageable for the time being
itscrime <- woohoo[ ,c("DATA_YEAR", "COUNTY_NAME","YEARLY_AGENCY_ID", "STATE_ABBR", "INCIDENT_DATE",
                       "RELATIONSHIP_ID", "OFFENSE_TYPE_ID", "LOCATION_ID")]

# Now lets change the date of the incident to a numbered month

# first get rid of all numbers as they correspond to year and day
itscrime$INCIDENT_DATE <- gsub('[[:digit:]]+','', as.character(itscrime$INCIDENT_DATE))

# then get rid of the dashes
itscrime$INCIDENT_DATE <- gsub('-','', as.character(itscrime$INCIDENT_DATE))

#vector of capitalized MMM
cap_mmm <- toupper(month.abb)

# replace MMM with numbers
itscrime$INCIDENT_DATE <- match(itscrime$INCIDENT_DATE, cap_mmm)

#change name of INCIDENT_DATE to INCIDENT_MONTH
colnames(itscrime)[4] <- "INCIDENT_MONTH"



write_csv(x = itscrime, file = "incidents_2018.csv")

















