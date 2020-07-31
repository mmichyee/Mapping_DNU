library(choroplethr)
library(choroplethrMaps)

# get a census key at https://api.census.gov/data/key_signup.html
# then set it like this
api.key.install("<your key here>")

?get_state_demographics

# get and view demographics from 2010
df_2010 = get_state_demographics(2010)
View(df_2010)

# map 2010 per capita income like this
df_2010$value = df_2010$per_capita_income
state_choropleth(df_2010)

# get and map 2015 income like this
df_2015 = get_state_demographics(2015)
df_2015$value = df_2015$per_capita_income
state_choropleth(df_2015)
