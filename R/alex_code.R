library(pacman)

p_load(tidyverse, rvest, lubridate, janitor,
       data.table, readr, readxl, dplyr, skimr,
       broom, tidyr, stringr, stats)


# school_closure = read_csv("School_closure.csv")
# school_closure = sc# <-read_csv("School_closure.csv")
# identical(sc,)

# rm(sc)


df <- read_csv("data_draft_4.csv")



# data <- c("county", "state", "year", "month")

# data <- str_split_fixed(df$combo, "-", 4)













# data <- data.table(data)





# df$county <- data$V1
# 
# df$state <- data$V2
# 
# df$year <- data$V3
# 
# df$month <- data$V4



# df$combo <- NULL



# df2018 <- filter(df, df$year == 2018)
# df2019 <- filter(df, df$year == 2019)
# df2020 <- filter(df, df$year == 2020)



# unique_agency_2020 <- data.table(unique(df2020$county))
# unique_agency_2019 <- data.table(unique(df2019$county))
# unique_agency_2018 <- data.table(unique(df2018$county))

# county_list <- inner_join(unique_agency_2018, unique_agency_2019, by = "V1")
# county_list <- inner_join(county_list, unique_agency_2020, by = "V1")
# names(county_list) <- c("county")
# df <- left_join(county_list, df, by = "county")








# df$year_month <- zoo::as.yearmon(df$year_month, "%Y %m")
df = df %>% mutate(year_month = zoo::as.yearmon(year_month))






df[is.na(df)] <- 0


# sum(is.na(df$partner_crime))
# df = mutate_at(df, c("share_all_closed_25",
#                       "share_all_closed_50",
#                       "share_all_closed_75",
#                      "total_students",
#                      "number_schools"), 
#                ~replace(., is.na(.), 0))








census <- read_csv("acs.csv")
census <- na.omit(census)


df1 <- left_join(df, census, by = c("county", "state"))
skim(df1)




df2  <- left_join(census, df, by = c("county", "state"))