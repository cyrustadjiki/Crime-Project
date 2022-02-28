# CRIME - PROJECT
# Analyzing crime data from NIBRS, Census data, and Safegraph Data
# Managing Crime Data from NIBRS 2018

# loading packages
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, 
       skimr, broom, tidyr, stringr, stats)

# Cyrus
setwd("C:/Users/cyrus/Downloads/Crime Data")
# Alex
# setwd("~")

# pull example files to get col names list
NIBRS_incident <- read_csv("Crime_Data_2018/AR/NIBRS_incident.csv")
NIBRS_VICTIM <- read_csv("Crime_Data_2018/AR/NIBRS_VICTIM.csv")
agencies <- read_csv("Crime_Data_2018/AR/agencies.csv")
unique_agency_list = read_csv("unique_agency_list.csv")
# unique_agency_list

#INCIDENTS
# make empty data set for incident data
incidents <- data.frame(matrix(ncol = 15, nrow = 0))
names(incidents) <- names(NIBRS_incident)
# get a list of the folders in Crime_Data_NIBRS
files <- list.files(path="CRIME_DATA_2018")

# go through each folder in Crime_Data_NIBRS and find corresponding incident 
# table to bind it to incidents data frame

for (i in files){
  NIBRS_incident <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_incident.csv")))
  incidents <- rbind(incidents, NIBRS_incident)
}
# pull out unnecessary columns for incidents data set
incidents <- incidents[ , c("DATA_YEAR", "AGENCY_ID", "INCIDENT_ID",
                            "INCIDENT_DATE", "INCIDENT_HOUR")]

# [1] "DATA_YEAR"          
# [2] "AGENCY_ID"          
# [3] "INCIDENT_ID"        
# [4] "NIBRS_MONTH_ID"        # DROPPED   
# [5] "CARGO_THEFT_FLAG"      # DROPPED
# [6] "SUBMISSION_DATE"       # DROPPED
# [7] "INCIDENT_DATE"         
# [8] "REPORT_DATE_FLAG"      # DROPPED
# [9] "INCIDENT_HOUR"         
# [10] "CLEARED_EXCEPT_ID"    # DROPPED
# [11] "CLEARED_EXCEPT_DATE"  # DROPPED
# [12] "INCIDENT_STATUS"      # DROPPED
# [13] "DATA_HOME"            # DROPPED
# [14] "ORIG_FORMAT"          # DROPPED
# [15] "DID"                  # DROPPED

#VICTIMS
#make empty data set for victim data
victims <- data.frame(matrix(ncol = 16, nrow = 0))
names(victims) <- names(NIBRS_VICTIM)
#go through each folder and find corresponding  
for (i in files){
  NIBRS_VICTIM <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_VICTIM.csv")))
  victims <- rbind(victims, NIBRS_VICTIM)
}
# pull out necessary column names from victims
victims <- victims[ , c("VICTIM_ID", "INCIDENT_ID", 
                        "AGE_NUM", "SEX_CODE")]

# [1] "DATA_YEAR"                 # DROPPED
# [2] "VICTIM_ID"           
# [3] "INCIDENT_ID"         
# [4] "VICTIM_SEQ_NUM"            # DROPPED 
# [5] "VICTIM_TYPE_ID"            # DROPPED
# [6] "ASSIGNMENT_TYPE_ID"        # DROPPED
# [7] "ACTIVITY_TYPE_ID"          # DROPPED
# [8] "OUTSIDE_AGENCY_ID"         # DROPPED
# [9] "AGE_ID"                    # DROPPED
# [10] "AGE_NUM"                  
# [11] "SEX_CODE"                 
# [12] "RACE_ID"                  # DROPPED
# [13] "ETHNICITY_ID"             # DROPPED
# [14] "RESIDENT_STATUS_CODE"     # DROPPED
# [15] "AGE_RANGE_LOW_NUM"        # DROPPED
# [16] "AGE_RANGE_HIGH_NUM"       # DROPPED

# AGENCIES
# make empty dataset for agencies list and set column names
agency_list <- data.frame(matrix(ncol = 59, nrow = 0))
names(agency_list) <- names(agencies)
# run through each file to bind all the agencies list
for (i in files){
  agencies <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/agencies.csv")))
  agency_list <- rbind(agency_list, agencies)
}
# isolate the necessary columns
agency_list <- agency_list[ , c( "AGENCY_ID", 
                                 "STATE_ABBR",
                                 "COUNTY_NAME")]

# OFFENDER
# pull example file to get the names of the data set
NIBRS_OFFENDER <- read_csv("Crime_Data_2018/AL/NIBRS_OFFENDER.csv")
# make empty data set for offenders files and set the names
offenders <- data.frame(matrix(ncol = 11, nrow = 0))
names(offenders) <- names(NIBRS_OFFENDER)
# run through each file to bind all the offenders lists
for (i in files){
  NIBRS_OFFENDER <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_OFFENDER.csv")))
  offenders <- rbind(offenders, NIBRS_OFFENDER)
}
# isolate important column names of offenders
offenders <- offenders[ , c("OFFENDER_ID", 
                            "INCIDENT_ID",
                            "AGE_NUM",
                            "SEX_CODE")]

#OFFENSE
# read NIBRS_OFFENSE file for example
NIBRS_OFFENSE <- read_csv("Crime_Data_2018/AL/NIBRS_OFFENSE.csv")
# make empty data fram for offense
offense <- data.frame(matrix(ncol = 8, nrow = 0))
# set name fo offense frame
names(offense) <- names(NIBRS_OFFENSE)
# for loop to bind all offense datasets
for (i in files){
  NIBRS_OFFENSE <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_OFFENSE.csv")))
  offense <- rbind(offense, NIBRS_OFFENSE)
}
beepr::beep(sound = 3)
# isolate necessary columns for offense
offense <- offense[ , c("OFFENSE_ID", 
                        "INCIDENT_ID", 
                        "OFFENSE_TYPE_ID",
                        "LOCATION_ID")]


#VICTIM OFFENDER REL
# Read example file
NIBRS_VICTIM_OFFENDER_REL <- read_csv("Crime_data_2018/AL/NIBRS_VICTIM_OFFENDER_REL.csv")
# make empty data frame
victim_offender_rel <- data.frame(matrix(ncol = 5, nrow = 0))
# set names of data frame
names(victim_offender_rel) <- names(NIBRS_VICTIM_OFFENDER_REL)
# for loop to bind all victim offender rel files
for (i in files){
  NIBRS_VICTIM_OFFENDER_REL <- read_csv(gsub(' ', '', paste("Crime_Data_2018/", i, "/NIBRS_VICTIM_OFFENDER_REL.csv")))
  victim_offender_rel <- rbind(victim_offender_rel, NIBRS_VICTIM_OFFENDER_REL)
}
# isolate necessary column names
victim_offender_rel <- victim_offender_rel[ , c("VICTIM_ID", "OFFENDER_ID",
                                                "RELATIONSHIP_ID")]

# now we have incidents, victims, offenders, victim offender rel, and offense

beepr::beep(sound = 3)


# t1 = left_join(victims)

m1 <- left_join(victims, incidents, by = "INCIDENT_ID")
m2 <- left_join(m1, offenders, by = "INCIDENT_ID")
rm(m1)
m3 <- left_join(m2, victim_offender_rel, by = c("VICTIM_ID", "OFFENDER_ID"))
rm(m2)
m4 <- left_join(m3, offense, by = "INCIDENT_ID")
rm(m3)
m5 <- left_join(m4, agency_list, by = "AGENCY_ID")
rm(m4)

names(m5)[names(m5) == 'SEX_CODE.x'] <- "SEX_VICTIM"
names(m5)[names(m5) == 'SEX_CODE.y'] <- "SEX_OFFENDER"
names(m5)[names(m5) == 'AGE_NUM.x'] <- "AGE_VICTIM"
names(m5)[names(m5) == 'AGE_NUM.y'] <- "AGE_OFFENDER"




# find list of unique agency ids and filter out agency
# that don't match that

m5 = m5 %>% filter(m5$AGENCY_ID %in% unique_agency_list$AGENCY_ID)
identical(length(unique(m5$AGENCY_ID)),length(unique_agency_list$AGENCY_ID))


m5 <- m5[ ,c("DATA_YEAR",
             "COUNTY_NAME",
             "STATE_ABBR",
             "INCIDENT_DATE",
             "RELATIONSHIP_ID", 
             "OFFENSE_TYPE_ID", 
             "LOCATION_ID",
             "AGE_VICTIM",
             "SEX_OFFENDER",
             "SEX_VICTIM",
             "AGE_OFFENDER")]

rm(list= ls()[!(ls() %in% c("m5","df"))])
beepr::beep(sound = 2)


df = m5
# rm(m5)


# Child Relationship
df$child_rel = ifelse(df$RELATIONSHIP_ID ==  4|
                      df$RELATIONSHIP_ID ==  5|
                      df$RELATIONSHIP_ID == 10|
                      df$RELATIONSHIP_ID == 20|
                      df$RELATIONSHIP_ID == 23, 1,0)
# Partner Relationship
df$partner_rel = ifelse(df$RELATIONSHIP_ID ==  3|
                        df$RELATIONSHIP_ID == 27|
                        df$RELATIONSHIP_ID == 12|
                        df$RELATIONSHIP_ID == 21|
                        df$RELATIONSHIP_ID == 26, 1,0)
# Property Relationship
df$property_rel = ifelse( df$RELATIONSHIP_ID ==  1|
                          df$RELATIONSHIP_ID ==  2|
                          df$RELATIONSHIP_ID ==  4|
                          df$RELATIONSHIP_ID ==  7|
                          df$RELATIONSHIP_ID ==  8|
                          df$RELATIONSHIP_ID ==  8|
                          df$RELATIONSHIP_ID ==  9|
                          df$RELATIONSHIP_ID == 14|
                          df$RELATIONSHIP_ID == 16|
                          df$RELATIONSHIP_ID == 18|
                          df$RELATIONSHIP_ID == 24, 1,0)
# Stranger Relationship
df$stranger_rel = ifelse( df$RELATIONSHIP_ID ==  1|
                          df$RELATIONSHIP_ID ==  2|
                          df$RELATIONSHIP_ID ==  7|
                          df$RELATIONSHIP_ID ==  8|
                          df$RELATIONSHIP_ID ==  8|
                          df$RELATIONSHIP_ID ==  9|
                          df$RELATIONSHIP_ID == 14|
                          df$RELATIONSHIP_ID == 16|
                          df$RELATIONSHIP_ID == 18|
                          df$RELATIONSHIP_ID == 24|
                          df$RELATIONSHIP_ID == 25, 1,0)
# Burglary at home part
df$burglary_home_stranger = ifelse(  df$RELATIONSHIP_ID == 24 &
                                     df$OFFENSE_TYPE_ID == 49 &
                                     df$LOCATION_ID == 20, 1, 0)
# Crimes against children
df$child_crime = ifelse(
    df$child_rel == 1 &
    df$OFFENSE_TYPE_ID == 51| # Simple Assault 
    df$OFFENSE_TYPE_ID == 56| # Fondling
    df$OFFENSE_TYPE_ID == 36| # Rape
    df$OFFENSE_TYPE_ID ==  1| # Homicide  
    df$OFFENSE_TYPE_ID ==  3| # Statutory Rape
    df$OFFENSE_TYPE_ID ==  4| # Sexual Assault With An Object
    # df$OFFENSE_TYPE_ID ==  6| # Family Offenses, Nonviolent
    df$OFFENSE_TYPE_ID == 15| # Assisting or Promoting Prostitution
    df$OFFENSE_TYPE_ID == 19| # Run Away
    df$OFFENSE_TYPE_ID == 19| # Runaway
    df$OFFENSE_TYPE_ID == 27| # Aggravated Assault
    df$OFFENSE_TYPE_ID == 29| # Kidnapping/Abduction
    df$RELATIONSHIP_ID == 30| # Prostitution Offenses
    df$OFFENSE_TYPE_ID == 32| # Murder and Nonnegligent Manslaughter
    df$OFFENSE_TYPE_ID == 38| # Negligent Manslaughter 
    df$OFFENSE_TYPE_ID == 43| # Sodomy
    df$OFFENSE_TYPE_ID == 44| # Intimidation
    df$OFFENSE_TYPE_ID == 55| # Incest
    df$OFFENSE_TYPE_ID == 59| # Human Trafficking, Commercial Sex Acts
    df$OFFENSE_TYPE_ID == 60,1, 0) # Human Trafficking, Involuntary Servitude
# Crimes against partners
df$partner_crime = ifelse(
    df$partner_rel == 1 &
    df$OFFENSE_TYPE_ID == 51| # Simple Assault 
    df$OFFENSE_TYPE_ID == 56| # Fondling
    df$OFFENSE_TYPE_ID == 36| # Rape
    df$OFFENSE_TYPE_ID ==  1| # Homicide  
    # df$OFFENSE_TYPE_ID ==  3| # Statutory Rape
    df$OFFENSE_TYPE_ID ==  4| # Sexual Assault With An Object
    # df$OFFENSE_TYPE_ID ==  6| # Family Offenses, Nonviolent
    df$OFFENSE_TYPE_ID == 15| # Assisting or Promoting Prostitution
    # df$OFFENSE_TYPE_ID == 19| # Run Away
    df$OFFENSE_TYPE_ID == 19| # Runaway
    df$OFFENSE_TYPE_ID == 27| # Aggravated Assault
    df$OFFENSE_TYPE_ID == 29| # Kidnapping/Abduction
    df$RELATIONSHIP_ID == 30| # Prostitution Offenses
    df$OFFENSE_TYPE_ID == 32| # Murder and Nonnegligent Manslaughter
    df$OFFENSE_TYPE_ID == 38| # Negligent Manslaughter 
    df$OFFENSE_TYPE_ID == 43| # Sodomy
    df$OFFENSE_TYPE_ID == 59| # Human Trafficking, Commercial Sex Acts
    df$OFFENSE_TYPE_ID == 60,1, 0) # Human Trafficking, Involuntary Servitude
# Crimes against strangers
df$stranger_crime = ifelse(
    df$stranger_rel == 1 &
    df$OFFENSE_TYPE_ID == 51| # Simple Assault 
    df$OFFENSE_TYPE_ID == 56| # Fondling
    df$OFFENSE_TYPE_ID == 36| # Rape
    df$OFFENSE_TYPE_ID ==  1| # Homicide  
    df$OFFENSE_TYPE_ID ==  3| # Statutory Rape
    df$OFFENSE_TYPE_ID ==  4| # Sexual Assault With An Object
    df$OFFENSE_TYPE_ID == 19| # Runaway
    df$OFFENSE_TYPE_ID == 27| # Aggravated Assault
    df$RELATIONSHIP_ID == 30| # Prostitution Offenses
    df$OFFENSE_TYPE_ID == 32| # Murder and Nonnegligent Manslaughter
    df$OFFENSE_TYPE_ID == 38| # Negligent Manslaughter 
    df$OFFENSE_TYPE_ID == 55| # Incest
    df$OFFENSE_TYPE_ID == 59| # Human Trafficking, Commercial Sex Acts
    df$OFFENSE_TYPE_ID == 60,1,0) # Human Trafficking, Involuntary Servitude

# VICTIM SEX
df$victim_male = ifelse(df$SEX_VICTIM == "M",1,0)
df$victim_female = ifelse(df$SEX_VICTIM == "F",1,0)
df$victim_unknown_sex = ifelse(df$SEX_VICTIM == "U",1,0)

# OFFENDER SEX
df$offender_male = ifelse(df$SEX_OFFENDER == "M",1,0)
df$offender_female = ifelse(df$SEX_OFFENDER == "F",1,0)
df$offender_unknown_sex = ifelse(df$SEX_OFFENDER == "U",1,0)

# VICTIM AGE
df$victim_minor = ifelse(df$AGE_VICTIM < 18,1,0)
df$victim_adult = ifelse(df$AGE_VICTIM < 18,0,1)

# OFFENDER AGE
df$offender_minor = ifelse(df$AGE_VICTIM < 18,1,0)
df$offender_adult = ifelse(df$AGE_VICTIM < 18,0,1)

#CHANGE
#Property Crime
prop_crime_id_list=c(46,50,58,2,5,7,11,12,13,14,
                     17,18,20,21,23,25,26,28,37,
                     40,41,45,47,49,57,63,64)

df$property_crime = ifelse(df$OFFENSE_TYPE_ID %in% prop_crime_id_list,1,0)


# People Crime
people_crime_id_list=c(51,56,36,1,3,4,6,27,29,
                       32,33,38,43,44,55,59,60,19)

df$person_crime = ifelse(df$OFFENSE_TYPE_ID %in% people_crime_id_list,1,0)

# Society Crime
society_crime_id_list=setdiff(1:86,c(people_crime_id_list,
                                     prop_crime_id_list))

df$society_crime = ifelse(df$OFFENSE_TYPE_ID %in% society_crime_id_list,1,0)

# beepr::beep(sound = 8)


#dropping relationship columns and age/sex/victim/offender columns
df = select(df, -c(child_rel,
                   partner_rel,
                   # CHANGE
                   # property_rel,
                   stranger_rel,
                   SEX_OFFENDER,
                   SEX_VICTIM,
                   AGE_OFFENDER,
                   AGE_VICTIM)) 

# sum(is.na(df$partner_crime))
df = mutate_at(df, c( "partner_crime",
                      "child_crime",
                      "stranger_crime",
                      "burglary_home_stranger",
                      "offender_male",         
                      "offender_female",        
                      "offender_unknown_sex",  
                      "victim_minor",
                      "victim_adult",         
                      "victim_male",            
                      "victim_female",         
                      "victim_unknown_sex",     
                      "offender_minor",        
                      "offender_adult"), 
               ~replace(., is.na(.), 0))

# beepr::beep(sound = 8)

# Sys.sleep(120)
df =separate(
  data = df,
  col = COUNTY_NAME,
  sep = "; ",
  into = 
    c("county_1", 
      "county_2", 
      "county_3", 
      "county_4",
      "county_5"),
  remove = FALSE)

# beepr::beep(sound = 3)
# # A tibble: 8 x 8
# skim_variable n_missing complete_rate   min   max empty n_unique
# * <chr>             <int>         <dbl> <int> <int> <int>    <int>
#   1 COUNTY_NAME      133070        0.989      3    43     0     1858
# 2 county_1         133070        0.989      3    21     0     1491
# 3 county_2       10127605        0.155      3    15     0      266
# 4 county_3       11001952        0.0820     4    12     0       33
# 5 county_4       11839600        0.0121     4     8     0        5
# 6 county_5       11985211        0         NA    NA     0        0
# 7 STATE_ABBR            0        1          2     2     0       46
# 8 INCIDENT_DATE         0        1          9     9     0      366



df = select(df, -c(COUNTY_NAME,
                   county_2,
                   county_3,
                   county_4,
                   county_5))


new_df <- na.omit(df, c("county_1")) # Remove NA on specific variables
# DONT RUN
# df = df %>% na.omit(county_1)



# This was how we use to subset to jusyt the columns we needed 
# but we have a lot more now

# df <- df[ , c("DATA_YEAR", "COUNTY_NAME","AGENCY_ID",
#               "STATE_ABBR", "INCIDENT_DATE",
#               "RELATIONSHIP_ID", "OFFENSE_TYPE_ID", 
#               "LOCATION_ID", "AGE_NUM")]



# Renaming Columns
names(df)[names(df) == 'county_1'] <- "COUNTY_NAME"
names(df)[names(df) == 'DATA_YEAR'] <- "year"
df = clean_names(df)

df$month = gsub("[^a-zA-Z]", "", df$incident_date)

df$month =  ifelse(df$month == "JAN",1,
            ifelse(df$month == "FEB",2,
            ifelse(df$month == "MAR",3,
            ifelse(df$month == "APR",4,
            ifelse(df$month == "MAY",5,
            ifelse(df$month == "JUN",6,
            ifelse(df$month == "JUL",7,
            ifelse(df$month == "AUG",8,
            ifelse(df$month == "SEP",9,
            ifelse(df$month == "OCT",10,
            ifelse(df$month == "NOV",11,12)))))))))))



df$combo = str_c(df$county_name,"-",
                 df$state_abbr,"-",
                 df$year, "-",
                 df$month)
df = df %>% 
  group_by(combo) %>% 
  summarise(partner_crime = sum(partner_crime),
            child_crime = sum(child_crime),
            stranger_crime = sum(stranger_crime),
            burglary_home_stranger = sum(burglary_home_stranger),
            #CHANGE
            property_rel = sum(property_rel),
            # VICTIM SEX
            victim_male = sum(victim_male),
            victim_female = sum(victim_female),
            victim_unknown_sex = sum(victim_unknown_sex),
            
            # OFFENDER SEX
            offender_male = sum(offender_male),
            offender_female = sum(offender_female),
            offender_unknown_sex = sum(offender_unknown_sex),
            
            # VICTIM AGE
            victim_minor = sum(victim_minor),
            victim_adult = sum(victim_adult),
            
            # OFFENDER AGE
            offender_minor = sum(offender_minor),
            offender_adult = sum(offender_adult),
            
            #CHANGE
            # Type of crime
            property_crime = sum(property_crime),
            person_crime = sum(person_crime),
            society_crime = sum(society_crime)
            )


beepr::beep(sound = 8)



# Saving out data frame as a .csv file
write_csv(x = df, file = "final_2018.csv")
path_out = "C:/Users/cyrus/OneDrive/EC 419/Crime-Project/data"
write.csv(df,paste(path_out,"final_2018.csv",sep = ''))


