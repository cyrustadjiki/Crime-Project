
# Loading Packages
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, 
       skimr, broom, tidyr, stringr, stats)

# Setting Working Directory
setwd("C:/Users/cyrus/OneDrive/EC 419/Crime-Project")
setwd("C:/Users/cyrus/Downloads/EC 419/Crime Data")
getwd()

crime_2018 = read.csv("final_2018.csv")
crime_2019 = read.csv("final_2019.csv")
crime_2020 = read.csv("final_2020.csv")
sc = read.csv("school_closure_data1.csv")
# census = read.csv(".csv")


# Combining data sets
bound = rbind(crime_2018,crime_2019,crime_2020)
# Sanity Check
identical(nrow(bound),sum(nrow(crime_2018),nrow(crime_2019),nrow(crime_2020)))
sc = sc %>% filter(year < 2021)
sc = select(sc, -c( X,statefips,
                    countyfips,
                    countyfips2,
                    countyfips3,
                    year,
                    state_abb,
                    county_name)) 

# Joining School Closure Data
# Using a full_join is BAD here
df = left_join(bound,sc, by = "combo")

# Old School Closure interpretation

# df = df %>% 
#       mutate(stud_per_school = total_students/number_schools) %>% 
#       mutate(closed25 = 0.25 * (share_all_closed_25*total_students)) %>% 
#       mutate(closed50 = 0.5  * (share_all_closed_50*total_students)) %>% 
#       mutate(closed75 = 0.75 * (share_all_closed_75*total_students)) %>% 
#       mutate(students_home = (closed25 + closed50 + closed75))

df =separate(
  data = df,
  col = combo,
  sep = "-",
  into = 
    c("county", 
      "state", 
      "year", 
      "month"),
  remove = FALSE)

df$year = as.numeric(df$year)
df$month = as.numeric(df$month)

# Old way of finding time
# df = df %>% 
#   # fix this
#   mutate(time = year - 2017 + month) %>% 
#   # times by 100
#   mutate(ten = 100*share_all_closed_25)

df$year_month <- zoo::as.yearmon(paste(df$year, df$month), "%Y %m")
df = select(df, -c(month))



# write_csv(x = df, file = "data_draft_1.csv")
write_csv(x = df, file = "data_draft_2.csv")
# write_csv(x = df, file = "data_draft_3.csv")
# write_csv(x = df, file = "data_draft_4.csv")
# write_csv(x = df, file = "data_draft_5.csv")
# write_csv(x = df, file = "data_draft_6.csv")
# write_csv(x = df, file = "data_draft_7.csv")
# write_csv(x = df, file = "data_draft_8.csv")

