
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, zoo,
       skimr, broom, tidyr, stringr, stats,
       ggthemes, fixest, lfe, modelsummary)



mod_low = list(
child_low = feols(child_crime~at_home_low| pop_dens + HH_inc + tfHH_65_percent
                   + clf_ue/clf + month + tfHH_18_percent , data = df),
partner_low = feols(partner_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                     + clf_ue/clf + month + tfHH_18_percent , data = df),
property_low = feols(property_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                      + clf_ue/clf + month + tfHH_18_percent , data = df),
stranger_low = feols(stranger_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                      + clf_ue/clf + month + tfHH_18_percent , data = df)
)

mod_high = list(
child_high = feols(child_crime~at_home_high| pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
partner_high = feols(partner_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
property_high = feols(property_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
stranger_high = feols(stranger_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df)
)
summary(feols(child_crime~at_home_low+at_home_low:tfHH_65_percent + pop_dens + 
                HH_inc + tfHH_65_percent+
                          tHH + clf_ue/clf +
                tfHH_18_percent| month , data = df))

#1% increase in thh_18percent increases child_crime by beta


mean(df$child_crime)/mean(df$tHH)

#Low
modelsummary(mod_low, 
             fmt = "%.3e",
             coef_rename = c("at_home_low" = "Maximum Students at Home",
                             "Std.Error" = "Standard Error"),
             estimate = "{estimate}{stars}",
             statistic = "({estimate})",
             gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo")

#High
modelsummary(mod_high, 
             fmt = "%.3e",
             coef_rename = c("at_home_high" = "Minimum Students at Home",
                             "Std.Error" = "Standard Error"),
             estimate = "{estimate}{stars}",
             # statistic = "Std.Error: {std.error}",
             gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo")



# this is the good one
summary(feols(child_crime ~ share_all_closed_25 +pop_dens +total_pop + HH_inc +
                tfHH_65_percent + share_all_closed_25:tfHH_65_percent| month, data=df))


# this is the good one
summary(feols(partner_crime ~ share_all_closed_25  + pop_dens + HH_inc + 
                tfHH_65_percent + total_pop
              + clf_ue_percent + share_all_closed_25:tfHH_65_percent | month, data=df))



summary(lm(stranger_crime~at_home_low, data = df))

summary(feols(stranger_crime ~ share_all_closed_25 + pop_dens + HH_inc + 
                + pop_dens + total_pop | month, data=df))




#USING share_all_closed_25
#Run initial regressions without fixed effects
#Child
summary(feols(percent_child~ share_all_closed_25 + as.factor(month)-1, data=df))

#Partner
summary(feols(percent_partner~ share_all_closed_25 + as.factor(month)-1, data=df))

#Stranger
summary(feols(percent_stranger~ share_all_closed_25 + as.factor(month)-1, data=df))


#Now run regressions with fixed effects
summary(lm(child_crime ~ share_all_closed_25 + as.factor(month) + pop_dens + HH_inc + 
             tfHH_65_percent + tfHH_18_percent + total_pop + total_students, data=df))

summary(lm(partner_crime ~ share_all_closed_25 + as.factor(month) + pop_dens + HH_inc + 
             tfHH_65_percent  + tfHH_18_percent + total_pop + total_students, data=df))

summary(lm(stranger_crime ~ share_all_closed_25 + as.factor(month) + pop_dens + HH_inc + 
             tfHH_65_percent  + tfHH_18_percent + total_pop + total_students, data=df))


