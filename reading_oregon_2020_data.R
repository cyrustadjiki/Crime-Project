
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, skimr, broom)


# OFFENSEloading data
incident_df = read.csv("NIBRS_incident.csv")
offense_df = read.csv("NIBRS_OFFENSE.csv")
offense_type_df = read.csv("NIBRS_OFFENSE_TYPE.csv")
relationship_df = read.csv("NIBRS_RELATIONSHIP.csv")
victim_df = read.csv("NIBRS_VICTIM.csv")
victim_circumstances_df = read.csv("NIBRS_VICTIM_CIRCUMSTANCES.csv")
offender_df = read.csv("NIBRS_OFFENDER.csv")
victim_type_df = read.csv("NIBRS_VICTIM_TYPE.csv")
victim_offender_rel_df = read.csv("NIBRS_VICTIM_OFFENDER_REL.csv")
# _df = read.csv("NIBRS_.csv")
# _df = read.csv("NIBRS_.csv")

# joined_df = left_join(incident_df, offense_df, by = "INCIDENT_ID")

funct = function(ARGUMENT){
  VALUE = c(ARGUMENT, rep(NA, 16 - length(ARGUMENT)))
  return(VALUE)
}

incident_names = names(incident_df)
offense_names = names(offense_df)
offense_type_names = names(offense_type_df)
relationship_names = names(relationship_df)
victim_names = names(victim_df)
victim_circumstances_names = names(victim_circumstances_df)
offender_names = names(offender_df)
victim_type_names = names(victim_type_df)
victim_offender_rel_names = names(victim_offender_rel_df)

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

# NIBRS_ACTIVITY_TYPE.csv
# NIBRS_AGE.csv
# NIBRS_ARRESTEE.csv
# NIBRS_ARRESTEE_WEAPON.csv
# NIBRS_ARREST_TYPE.csv
# NIBRS_ASSIGNMENT_TYPE.csv
# NIBRS_BIAS_LIST.csv
# NIBRS_BIAS_MOTIVATION.csv
# NIBRS_CIRCUMSTANCES.csv
# NIBRS_CLEARED_EXCEPT.csv
# NIBRS_CRIMINAL_ACT.csv
# NIBRS_CRIMINAL_ACT_TYPE.csv
# NIBRS_DRUG_MEASURE_TYPE.csv
# NIBRS_ETHNICITY.csv
# NIBRS_INJURY.csv
# NIBRS_JUSTIFIABLE_FORCE.csv
# NIBRS_LOCATION_TYPE.csv
# NIBRS_OFFENDER.csv
# NIBRS_OFFENSE.csv
# NIBRS_OFFENSE_TYPE.csv
# NIBRS_PROPERTY.csv
# NIBRS_PROPERTY_DESC.csv
# NIBRS_PROP_DESC_TYPE.csv
# NIBRS_PROP_LOSS_TYPE.csv
# NIBRS_RELATIONSHIP.csv
# NIBRS_SUSPECTED_DRUG.csv
# NIBRS_SUSPECTED_DRUG_TYPE.csv
# NIBRS_SUSPECT_USING.csv
# NIBRS_USING_LIST.csv
# NIBRS_VICTIM.csv
# NIBRS_VICTIM_CIRCUMSTANCES.csv
# NIBRS_VICTIM_INJURY.csv
# NIBRS_VICTIM_OFFENDER_REL.csv
# NIBRS_VICTIM_OFFENSE.csv
# NIBRS_VICTIM_TYPE.csv
# NIBRS_WEAPON.csv
# NIBRS_WEAPON_TYPE.csv
# NIBRS_incident.csv
# NIBRS_month.csv
# REF_RACE.csv
# REF_STATE.csv
# agencies.csv






