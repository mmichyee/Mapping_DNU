library(choroplethr)
library(choroplethrMaps)

# if you haven't already, get a census key at: 
# https://api.census.gov/data/key_signup.html
# and then install it by typing:
# api.key.install("<your key>")

# this returns the table as a list
# the first element is a dataframe with the estimates
# the second element is the title of table
get_acs_data("B19301",
              "state", 
              endyear = 2015, 
              span = 5)

# you can get the first element by typing [[1]] after the list
df_income = get_acs_data("B19301", "state")[[1]]

# the dataframe is in a form that choroplethr can map
state_choropleth(df_income)
