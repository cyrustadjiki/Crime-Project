
# Loading Packages
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, maps,
       skimr, broom, tidyr, stringr, stats)

# Setting Working Directory
setwd("C:/Users/cyrus/OneDrive/EC 419/Crime-Project/data")
# setwd("C:/Users/cyrus/Downloads/EC 419/Crime Data")
getwd()

crime_2018 = read.csv("final_2018.csv")
crime_2019 = read.csv("final_2019.csv")
crime_2020 = read.csv("final_2020.csv")
sc = read.csv("school_closure_data1.csv")


# Combining data sets
bound = rbind(crime_2018,crime_2019,crime_2020)
# Sanity Check
identical(nrow(bound),sum(nrow(crime_2018),nrow(crime_2019),nrow(crime_2020)))
sc = sc %>% filter(year < 2021) 
# sc = as.tibble(sc) %>% select(sc, -c(X, 
#                           statefips,
#                           countyfips, 
#                           countyfips2,
#                           countyfips3,
#                           year,
#                           state_abb,
#                           county_name)) 

# Leriche Method ORCID: 100000000000789
sc = sc %>% mutate(at_home_low = share_all_closed_75*0.75*total_students +
       (share_all_closed_50-share_all_closed_75)*0.50*total_students +
       (share_all_closed_25-share_all_closed_50)*0.25*total_students) %>% 
            mutate(at_home_high = share_all_closed_75*1.00*total_students +
       (share_all_closed_50-share_all_closed_75)*0.75*total_students +
       (share_all_closed_25-share_all_closed_50)*0.50*total_students)




# Joining School Closure Data
# Using a full_join is BAD here
df4 = left_join(bound,sc, by = c("combo"))
dim(df4)

# Old School Closure interpretation
##################
# df = df %>% 
#       mutate(stud_per_school = total_students/number_schools) %>% 
#       mutate(closed25 = 0.25 * (share_all_closed_25*total_students)) %>% 
#       mutate(closed50 = 0.5  * (share_all_closed_50*total_students)) %>% 
#       mutate(closed75 = 0.75 * (share_all_closed_75*total_students)) %>% 
#       mutate(students_home = (closed25 + closed50 + closed75))
#####################


df4 =separate(
  data = df4,
  col = combo,
  sep = "-",
  into = 
    c("county", 
      "state", 
      "year", 
      "month"),
  remove = FALSE)

df4$year = as.numeric(df4$year)
df4$month = as.numeric(df4$month)
df4$year_month <- zoo::as.yearmon(paste(df4$year, df4$month), "%Y %m")



# Old way of finding time
# df = df %>% 
#   # fix this
#   mutate(time = year - 2017 + month) %>% 
#   # times by 100
#   mutate(ten = 100*share_all_closed_25)

# df = select(df, -c(month))

#I NEED TO SORT and make table correlcty
# fips_dt = data.frame(df$countyfips3,df$county) %>% 
#           na.omit() %>% 
#           distinct()

# names(fips_dt)[names(fips_dt) == 'df4.countyfips3'] <- "countyfips3"
# names(fips_dt)[names(fips_dt) == 'df4.county'] <- "county"


# This is the ugliest code ever I will delete once I'm done debugging
# df4 = select(df4, -c(
                    # combo                 ,
                    # property_rel          ,
                    # property_crime        ,
                    # person_crime          ,
                    # society_crime         ,
                    # year_month            ,
                    # at_home_low           ,
                    # at_home_high          ,
                    # county                ,
                    # state                 ,
                    # year                  ,
                    # month                 ,
                    # partner_crime         ,
                    # child_crime           ,
                    # stranger_crime        ,
                    # burglary_home_stranger,
                    # victim_male           ,
                    # victim_female         ,
                    # victim_unknown_sex    ,
                    # offender_male         ,
                    # offender_female       ,
                    # offender_unknown_sex  ,
                    # victim_minor          ,
                    # victim_adult          ,
                    # offender_minor        ,
                    # offender_adult        ,
                    # number_schools        ,
                    # share_all_closed_50   ,
                    # share_all_closed_25   ,
                    # share_all_closed_75   ,
                    # total_students
                    # ))


df4[is.na(df4)] <- 0
census = read.csv("acs.csv")
# census <- na.omit(census)
# census = select(census, -c(County, GEOID, STATE))
fk = select(census, c(fips,county,state)) %>% 
      na.omit()

df4 = full_join(fk,df4, by = c("county", "state"))

result <- left_join(df4, census, by = c("county", "state"))
skim(result)

res = result


# DROP FIPS NOT YEAR

# res = select(res, -c(STATEA,
#                    countyfips3,
#                    fips.x,
#                    year.y,
#                    STATE,
#                    fips,
#                    COUNTYA,
#                    GEOID,
#                    population.y))
# 
# df4 = select(df4, -c(month))

#include population density to the data set
# people per square kilometer
pop_density <- read_csv("population_density.csv")




pop_density$State <- state.abb[match(pop_density$State,state.name)]
colnames(pop_density)[4] <- "ignore"
colnames(pop_density)[5] <- "state"

#join df1 with pop_density
pop_density <- pop_density[ , c("county", "state", "pop_dens")]
df1 <- left_join(df1, pop_density, by = c("county","state"))

df1 <- df1 %>% mutate(percent_child = 100*(child_crime /total_pop))

df1 <- df1 %>% mutate(percent_partner = 100*(partner_crime /total_pop))

df1 <- df1 %>% mutate(percent_stranger = 100*(stranger_crime /total_pop))

df1 <- df1 %>% mutate(percent_burglarly = 100*(burglary_home_stranger /total_pop))

df1 <- df1 %>% mutate(percent_closed_25 = share_all_closed_25 * 100)

df1 <- df1 %>% mutate(tfHH_18_percent = (tfHH_18 / tHH)*100)

df1 <- df1 %>% mutate(tfHH_65_percent = (tfHH_65 / tHH)*100)

df1 <- df1 %>% mutate(clf_percent = (clf/total_pop)*100)

df1 <- df1 %>% mutate(clf_ue_percent = (clf_ue/total_pop)*100)



# Garbage Code
#####
# test = inner_join(df4, fips_dt, by = "county")
# 
# test = test %>% distinct()
# 
# county.fips2 = fread('fips_code_state_county.csv')
# county.fips2[, V5 := NULL]
# setnames(county.fips2, c('state_alpha', 'state_fips', 'county_fips', 'county'))
# 
# county.fips2 = unite_(data = county.fips2, col = 'fips', from = c('state_fips', 'county_fips'), sep = '', remove = T)
# 
# state_names = fread(str_c(files_dir, 'state_names.csv'))
# county.fips2 = merge(county.fips2, state_names, by = 'state_alpha')
# 
# setnames(county.fips2, 'state_name', 'state')
# county.fips2 = county.fips2[, c(1,4:5,3,2)]
# 
# county_clean = county.fips2 %>% 
#   select(county, state) %>%
#   map_df(.f = str_to_lower) %>%
#   map_df(.f = str_replace_all, pattern = '[[:punct:]]', replacement = '') %>%
#   map_df(.f = str_replace_all, pattern = '[[:space:]]', replacement = '')
# 
# county.fips2$county = county_clean$county
# county.fips2$state = county_clean$state
# 
# county.fips2[, fips := as.integer(fips)]

# Check for duplicates
# county.fips2[duplicated(county.fips2$fips), ]
#####


# write_csv(x = df, file = "data_draft_1.csv")
# write_csv(x = df, file = "data_draft_2.csv")
# write_csv(x = df, file = "data_draft_3.csv")
# write_csv(x = df, file = "data_draft_4.csv")
# NEXT
# write_csv(x = df, file = "data_draft_5.csv")
# write_csv(x = df, file = "data_draft_6.csv")
# write_csv(x = df, file = "data_draft_7.csv")
# write_csv(x = df, file = "data_draft_8.csv")

