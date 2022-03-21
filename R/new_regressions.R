setwd("C:/Users/cyrus/OneDrive/EC 419/Crime-Project/data")
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, zoo,
       skimr, broom, tidyr, stringr, stats,
       ggthemes, fixest, lfe)
df = read.csv("data_draft_5.csv")
df$year = as.numeric(df$year)
df$month = as.numeric(df$month)
df$year_month <- zoo::as.yearmon(paste(df$year, df$month), "%Y %m")
# Maybe don't do this
df[is.na(df)] = 0
# df2020 = df %>% filter(df$year == 2020)
# df = df %>% mutate(x = at_home_low/total_students)

# need to look at crimes as a percent of household
df <- df %>% mutate(percent_child = 100*(child_crime /total_pop))
df <- df %>% mutate(percent_partner = 100*(partner_crime /total_pop))
df <- df %>% mutate(percent_stranger = 100*(stranger_crime /total_pop))
df <- df %>% mutate(percent_burglarly = 100*(burglary_home_stranger /total_pop))
df <- df %>% mutate(percent_property = 100*(property_crime /total_pop))

# make fixed effects a percent
df <- df %>% mutate(tfHH_18_percent = (tfHH_18 / tHH)*100)
df <- df %>% mutate(tfHH_65_percent = (tfHH_65 / tHH)*100) # We only use this one
df <- df %>% mutate(unemployed = (clf_ue/clf)*100)

# make at_home_low and at_home_high percentages of students
# df <- df %>% mutate(at_home_low_percent = ifelse(
#   total_students == 0, 0, (at_home_low/total_students)*100))
# df <- df %>% mutate(at_home_high_percent = ifelse(
#   total_students == 0, 0, (at_home_high/total_students)*100))

# make a new share_all_closed_25 that is multiplied by 100
df <- df %>% mutate(pct_25pct_red = share_all_closed_25*100)
names(df)[names(df) == 'at_home_low'] <- "students_home"

# find and replace
# at_least_25 ---> pct_25pct_red
# at_home_low ---> students_home
# Not this!!!
# percent_child ---> child_crime_pct

## Models

# Percent of schools reduced at least 25% capacity
mod_pct = list(
"Child Crime" = feols(
  child_crime ~ pct_25pct_red  + 
                pop_dens + 
                HH_inc + 
                tfHH_65_percent + 
                total_pop +
                clf_ue_percent + 
                pct_25pct_red:tfHH_65_percent | 
                month, data=df),

"Partner Crime" = feols(
  partner_crime ~ pct_25pct_red  +
                  pop_dens +
                  HH_inc + 
                  tfHH_65_percent +
                  total_pop +
                  clf_ue_percent + 
                  pct_25pct_red:tfHH_65_percent |
                  month, data=df),

"Stranger Crime" = feols(
  stranger_crime ~  pct_25pct_red  +
                    pop_dens +
                    HH_inc + 
                    tfHH_65_percent +
                    total_pop +
                    clf_ue_percent + 
                    pct_25pct_red:tfHH_65_percent |
                    month, data=df),

"Percent of Child Crime" = feols(
  percent_child ~ pct_25pct_red  + 
                  pop_dens + 
                  HH_inc + 
                  tfHH_65_percent +
                  clf_ue_percent + 
                  pct_25pct_red:tfHH_65_percent |
                  month, data=df),

"Percent of Partner Crime" = feols(
  percent_partner ~ pct_25pct_red  + 
                    pop_dens + 
                    HH_inc + 
                    tfHH_65_percent + 
                    clf_ue_percent + 
                    pct_25pct_red:tfHH_65_percent |
                    month, data=df),

"Percent of Stranger Crime" = feols(
  percent_stranger ~  pct_25pct_red  + 
                      pop_dens + 
                      HH_inc + 
                      tfHH_65_percent + 
                      clf_ue_percent + 
                      pct_25pct_red:tfHH_65_percent |
                      month, data=df)

)
# TODO
# Add Property

# Students home
mod_students = list(
"Child Crime" = feols(
  child_crime ~ students_home  + 
                pop_dens + 
                HH_inc + 
                tfHH_65_percent + 
                total_pop + 
                clf_ue_percent + 
                students_home:tfHH_65_percent |
                month, data=df),

"Partner Crime" = feols(
  partner_crime ~ students_home  +
                  pop_dens + 
                  HH_inc + 
                  tfHH_65_percent + 
                  total_pop +
                  clf_ue_percent + 
                  students_home:tfHH_65_percent |
                  month, data=df),

"Stranger Crime" = feols(
  stranger_crime ~  students_home  +
                    pop_dens + 
                    HH_inc + 
                    tfHH_65_percent + 
                    total_pop +
                    clf_ue_percent + 
                    students_home:tfHH_65_percent |
                    month, data=df),

"Percent of Child Crime" = feols(
  percent_child ~ students_home  +
                  pop_dens + 
                  HH_inc + 
                  tfHH_65_percent + 
                  clf_ue_percent + 
                  students_home:tfHH_65_percent |
                  month, data=df),

"Percent of Partner Crime" = feols(
  percent_partner ~ students_home  + 
                    pop_dens + 
                    HH_inc + 
                    tfHH_65_percent + 
                    clf_ue_percent + 
                    students_home:tfHH_65_percent |
                    month, data=df),

"Percent of Stranger Crime" = feols(
  percent_stranger ~  students_home  + 
                      pop_dens + 
                      HH_inc + 
                      tfHH_65_percent + 
                      clf_ue_percent + 
                      students_home:tfHH_65_percent |
                      month, data=df)

)
# TODO
# Add Property

# Note!
# If there is percentage we don't want total_pop
library(modelsummary)
modelsummary(
  mod_pct, 
  fmt = "%.5f",
  coef_rename = c(
    "pct_25pct_red" = "Share all 25",
    "pop_dens" = "Population Denisty",
    "HH_inc" = "Household Income",
    "tfHH_65_percent" = "Percent of elderly people in households",
    "total_pop" = "Total population",
    "clf_ue_percent" = "Unemployment Rate",
    "pct_25pct_red:tfHH_65_percent" = "Interaction"
    # "pct_25pct_red:tfHH_65_percent" = "Percent of schools that expierenced 25% reduction and percent of family households with people over 65"
    ),
   estimate = "{estimate}{stars}",
   # statistic = "({Std.Error})",
   gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo|R2 Adj."
  )


modelsummary(
  mod_students, 
  fmt = "%.5f",
  coef_rename = c(
    "students_home" = "Minimum Students Home",
    "pop_dens" = "Population Denisty",
    "HH_inc" = "Household Income",
    "tfHH_65_percent" = "Percent of elderly people in households",
    "total_pop" = "Total population",
    "clf_ue_percent" = "Unemployment Rate",
    "students_home:tfHH_65_percent" = "Interaction"
    # "pct_25pct_red:tfHH_65_percent" = "Percent of schools that expierenced 25% reduction and percent of family households with people over 65"
  ),
  estimate = "{estimate}{stars}",
  # statistic = "({Std.Error})",
  gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo|R2 Adj."
)


summary(feols(
  percent_partner ~ pct_25pct_red  + 
    pop_dens + 
    HH_inc + 
    tfHH_65_percent + 
    total_pop +
    clf_ue_percent + 
    pct_25pct_red:tfHH_65_percent | 
    month, data=df)
)

kableExtra::kable(summary(df))
