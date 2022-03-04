
library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, zoo,
       skimr, broom, tidyr, stringr, stats,
       ggthemes, fixest, lfe, modelsummary)



mod_low = list(
child_low = feols(partner_crime~at_home_low| pop_dens + HH_inc + tfHH_65_percent
                   + clf_ue/clf + month + tfHH_18_percent , data = df),
partner_low = feols(partner_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                     + clf_ue/clf + month + tfHH_18_percent , data = df),
property_low = feols(property_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                      + clf_ue/clf + month + tfHH_18_percent , data = df),
stranger_low = feols(stranger_crime~at_home_low | pop_dens + HH_inc + tfHH_65_percent
                      + clf_ue/clf + month + tfHH_18_percent , data = df)
)

mod_high = list(
child_high = feols(partner_crime~at_home_high| pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
partner_high = feols(partner_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
property_high = feols(property_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df),
stranger_high = feols(stranger_crime~at_home_high | pop_dens + HH_inc + tfHH_65_percent
              + clf_ue/clf + month + tfHH_18_percent , data = df)
)

#Low
modelsummary(mod_low, 
             fmt = "%.3e",
             coef_rename = c("at_home_low" = "Maximum Students at Home",
                             "Std.Error" = "Standard Error"),
             estimate = "{estimate}{stars}",
             statistic = "({estimate}/1)",
             gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo")

#High
modelsummary(mod_high, 
             fmt = "%.3e",
             coef_rename = c("at_home_high" = "Minimum Students at Home",
                             "Std.Error" = "Standard Error"),
             estimate = "{estimate}{stars}",
             # statistic = "Std.Error: {std.error}",
             gof_omit = "^(?!.*R2)|R2 Within|R2 Pseudo")

