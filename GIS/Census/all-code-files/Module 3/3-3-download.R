library(choroplethr)
library(choroplethrMaps)

# get data from 2010 and 2015
df_2010 = get_state_demographics(2010)
df_2015 = get_state_demographics(2015)

# set the "value" column of each to be per-capita income
df_2010$value = df_2010$per_capita_income
df_2015$value = df_2015$per_capita_income

# now we can calculate the percent change in per-capita income easily
df_change = calculate_percent_change(df_2010, df_2015)
View(df_change)
