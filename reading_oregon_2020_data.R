
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, skimr, broom)

files <-  list.files(pattern = "*.csv")
multiple_csv <- sapply(files, read.csv)

agencies=multiple_csv[[1]]
activity_type=multiple_csv[[2]]
age=multiple_csv[[3]]
arrest_type=multiple_csv[[4]]
arrestee=multiple_csv[[5]]
arrestee_weapon=multiple_csv[[6]]
assignment_type=multiple_csv[[7]]
bias_list=multiple_csv[[8]]
bias_motivation=multiple_csv[[9]]
circumstances=multiple_csv[[10]]
cleared_except=multiple_csv[[11]]
criminal_act=multiple_csv[[12]]
criminal_act_type=multiple_csv[[13]]
drug_measure_type=multiple_csv[[14]]
ethnicity=multiple_csv[[15]]
incident=multiple_csv[[16]]
injury=multiple_csv[[17]]
justifiable_force=multiple_csv[[18]]
location_type=multiple_csv[[19]]
month=multiple_csv[[20]]
offender=multiple_csv[[21]]
offense=multiple_csv[[22]]
offense_type=multiple_csv[[23]]
prop_desc_type=multiple_csv[[24]]
prop_loss_type=multiple_csv[[25]]
property=multiple_csv[[26]]
property_desc=multiple_csv[[27]]
relationship=multiple_csv[[28]]
suspect_using=multiple_csv[[29]]
suspected_drug=multiple_csv[[30]]
suspected_drug_type=multiple_csv[[31]]
using_list=multiple_csv[[32]]
victim=multiple_csv[[33]]
victim_circumstances=multiple_csv[[34]]
victim_injury=multiple_csv[[35]]
victim_offender_rel=multiple_csv[[36]]
victim_offense=multiple_csv[[37]]
victim_type=multiple_csv[[38]]
weapon=multiple_csv[[39]]
weapon_type=multiple_csv[[40]]
ref_race=multiple_csv[[41]]
ref_state=multiple_csv[[42]]


# joined_df = left_join(incident, OFFENSE, by = "INCIDENT_ID")

types_of_realtionships = relationship$REALTIONSHIP_NAME
locations = location_type$LOCATION_NAME
type_of_crime = offense_type$OFFENSE_NAME
crime_against = offense_type$CRIME_AGAINST


funct = function(ARGUMENT){
  VALUE = c(ARGUMENT, rep(NA, 16 - length(ARGUMENT)))
  return(VALUE)
}

incident_names = names(incident)
offense_names = names(offense)
offense_type_names = names(offense_type)
relationship_names = names(relationship)
victim_names = names(victim)
victim_circumstances_names = names(victim_circumstances)
offender_names = names(offender)
victim_type_names = names(victim_type)
victim_offender_rel_names = names(victim_offender_rel)

incident_names = funct(incident_names)
offense_names = funct(offense_names)
offense_type_names = funct(offense_type_names)
relationship_names = funct(relationship_names)
victim_names = funct(victim_names)
victim_circumstances_names = funct(victim_circumstances_names)
offender_names = funct(offender_names)
victim_type_names = funct(victim_type_names)
victim_offender_rel_names = funct(victim_offender_rel_names)



col_names = data.frame( incident_names,
                        offense_names,
                        offense_type_names,
                        relationship_names,
                        victim_names,
                        victim_circumstances_names,
                        offender_names,
                        victim_type_names,
                        victim_offender_rel_names
                        )



length(unique(offense_type_df$OFFENSE_TYPE_ID))
length(unique(offense_type_df$OFFENSE_CATEGORY_NAME))
length(unique(offense_type_df$OFFENSE_NAME))
length(unique(offense_type_df$OFFENSE_CODE))

# TEXT


# offense_names = c(offense_names, rep(NA, 15 - length(offense_names)))
# offense_type_names = c(offense_type_names, rep(NA, 15 - length(offense_type_names)))

# funct(incident_names)
# funct(offense_names)
# funct(offense_type_names)
# funct(relationship_names)
# funct(victim_names)
# funct(victim_circumstances_names)
# funct(offender_names)
# funct(victim_type_names)
# funct(victim_offender_rel_names)

# agencies=multiple_csv[[1]]                
# NIBRS_ACTIVITY_TYPE=multiple_csv[[2]]       
# NIBRS_AGE=multiple_csv[[3]]                 
# NIBRS_ARREST_TYPE=multiple_csv[[4]]         
# NIBRS_ARRESTEE=multiple_csv[[5]]            
# NIBRS_ARRESTEE_WEAPON=multiple_csv[[6]]     
# NIBRS_ASSIGNMENT_TYPE=multiple_csv[[7]]     
# NIBRS_BIAS_LIST=multiple_csv[[8]]           
# NIBRS_BIAS_MOTIVATION=multiple_csv[[9]]     
# NIBRS_CIRCUMSTANCES=multiple_csv[[10]]       
# NIBRS_CLEARED_EXCEPT=multiple_csv[[11]]      
# NIBRS_CRIMINAL_ACT=multiple_csv[[12]]        
# NIBRS_CRIMINAL_ACT_TYPE=multiple_csv[[13]]   
# NIBRS_DRUG_MEASURE_TYPE=multiple_csv[[14]]   
# NIBRS_ETHNICITY=multiple_csv[[15]]           
# NIBRS_incident=multiple_csv[[16]]            
# NIBRS_INJURY=multiple_csv[[17]]              
# NIBRS_JUSTIFIABLE_FORCE=multiple_csv[[18]]   
# NIBRS_LOCATION_TYPE=multiple_csv[[19]]       
# NIBRS_month=multiple_csv[[20]]               
# NIBRS_OFFENDER=multiple_csv[[21]]            
# NIBRS_OFFENSE=multiple_csv[[22]]             
# NIBRS_OFFENSE_TYPE=multiple_csv[[23]]        
# NIBRS_PROP_DESC_TYPE=multiple_csv[[24]]      
# NIBRS_PROP_LOSS_TYPE=multiple_csv[[25]]      
# NIBRS_PROPERTY=multiple_csv[[26]]            
# NIBRS_PROPERTY_DESC=multiple_csv[[27]]       
# NIBRS_RELATIONSHIP=multiple_csv[[28]]        
# NIBRS_SUSPECT_USING=multiple_csv[[29]]       
# NIBRS_SUSPECTED_DRUG=multiple_csv[[30]]      
# NIBRS_SUSPECTED_DRUG_TYPE=multiple_csv[[31]] 
# NIBRS_USING_LIST=multiple_csv[[32]]          
# NIBRS_VICTIM=multiple_csv[[33]]              
# NIBRS_VICTIM_CIRCUMSTANCES=multiple_csv[[34]]
# NIBRS_VICTIM_INJURY=multiple_csv[[35]]       
# NIBRS_VICTIM_OFFENDER_REL=multiple_csv[[36]] 
# NIBRS_VICTIM_OFFENSE=multiple_csv[[37]]      
# NIBRS_VICTIM_TYPE=multiple_csv[[38]]         
# NIBRS_WEAPON=multiple_csv[[39]]              
# NIBRS_WEAPON_TYPE=multiple_csv[[40]]         
# REF_RACE=multiple_csv[[41]]                  
# REF_STATE=multiple_csv[[42]]  








