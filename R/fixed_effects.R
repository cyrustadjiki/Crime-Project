library(pacman)
p_load(tidyverse, rvest, lubridate, janitor, 
       data.table, readr, readxl, dplyr, zoo,
       skimr, broom, tidyr, stringr, stats,
       ggthemes, fixest)

reg1 = feols(child_crime/100~share_all_closed_25 | month, data = df)
summary(reg1)
tidy(reg1)

reg2 = lm(child_crime/100~share_all_closed_25, data = df)
summary(reg2)

# stargazer::stargazer(reg1,reg2, type = "text")

mod = list(
  reg1 = feols(child_crime/100~share_all_closed_25 | year_month, data = df),
  reg2 = lm(child_crime/100~share_all_closed_25, data = df)
  )

library(modelsummary)
modelsummary(mod, coef_omit = "^(?!.*child_crime)")







