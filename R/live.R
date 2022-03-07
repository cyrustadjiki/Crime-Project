df = read.csv("data_draft_5.csv")
df$year = as.numeric(df$year)
df$month = as.numeric(df$month)
df$year_month <- zoo::as.yearmon(paste(df$year, df$month), "%Y %m")
# Maybe don't do this
df[is.na(df)] = 0
df2020 = df %>% filter(df$year == 2020)
df = df %>% mutate(x = at_home_low/total_students) %>% 
            mutate(y = child_crime/tHH)



df[is.na(df)] = 0

mean(df$y)
mean(df$tHH)
mean(df$child_crime)/mean(df$total_students)

order(unique(df$y))

v1 = df$child_crime
v2 = df$tHH
v3 = v1/v2
mean(v3)
mean(df$x)

df2019 = df %>% filter(df$year == 2019) %>% 
                  group_by(state, county) %>% 
                  summarise(
                    gr_child_crime = sum(child_crime),
                    gr_partner_crime = sum(partner_crime),
                    gr_stranger_crime = sum(stranger_crime),
                  )
census = read_csv("acs.csv")
# census <- na.omit(census) # I don't think we need to do this
census = select(census, -c(County, GEOID, STATE,year,fips))
df2019 <- left_join(df2019, census, by = c("county", "state"))


pop_density <- read.csv("population_density.csv")
pop_density$State <- state.abb[match(pop_density$State,state.name)]
colnames(pop_density)[4] <- "ignore"
colnames(pop_density)[5] <- "state"
pop_density <- pop_density[ , c("county", "state", "pop_dens")]
pop_density = na.omit(pop_density)


#join df1 with pop_density
df2019 <- left_join(df2019, pop_density, by = c("county","state"))

# library(lfe)
# summary()
# felm(partner_crime~at_home_low | 0| month , data = df)

summary(lm(gr_child_crime~pop_denHH_inc+ tfHH_65+tfHH_18, data = df2019))

df2018 = df %>% filter(year == 2018)

summary(lm(child_crime~at_home_low, data = df2020))
# Fuck 2020
summary(lm(child_crime~at_home_low, data = df))
#Beta = 0.003999


#Child
summary(feols(partner_crime~at_home_low| pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))
# Beta = 0.000453
# mean(df$child_crime)/mean(df$at_home_low)
# mean(df$child_crime)
# mean(df$at_home_low)
# not sure what mean to compare it to

summary(feols(partner_crime~at_home_high| pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))

# mean(df$child_crime)/mean(df$at_home_high)
# mean(df$child_crime)
# mean(df$at_home_high)

#Partner
summary(feols(partner_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))

summary(feols(partner_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))


#Property
summary(feols(property_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))

summary(feols(property_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))

#Stranger
summary(feols(stranger_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))

summary(feols(stranger_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df))








summary(feols(log(child_crime)~log(at_home_low) | pop_dens + HH_inc + 
                tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df2020))
















