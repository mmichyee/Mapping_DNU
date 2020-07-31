library(choroplethr)
library(choroplethrMaps)

?df_state_demographics
data(df_state_demographics)
View(df_state_demographics)

df_state_demographics$value = 
  df_state_demographics$per_capita_income

View(df_state_demographics)

state_choropleth(df_state_demographics,
                 num_colors = 2,
                 title  = "2013 State Per Capita Income Estimates",
                 legend = "Dollars")