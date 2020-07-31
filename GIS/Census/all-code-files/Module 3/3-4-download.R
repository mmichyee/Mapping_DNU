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

# map the percent change using the default color scheme
state_choropleth(df_change)

# now map the percent change using a divergent scale
# note that negative values are now negative, zero is white and positive values are blue
state_choropleth(df_change, num_colors = 0)

# Adding the title and legend
state_choropleth(df_change,
                 num_colors = 0,
                 title = "Percent Change in Estimated Per-Capita Income, 2010-2015",
                 legend = "Percent")
