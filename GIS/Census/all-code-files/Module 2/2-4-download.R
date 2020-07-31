library(choroplethr)
library(choroplethrMaps)

# create default map
data(df_pop_state)
state_choropleth(df_pop_state)

# customize map with title and legend
state_choropleth(df_pop_state,
                 title  = "2012 State Population Estimates",
                 legend = "Population")

# change scale
# the default is 7 colors - these maps are identical
state_choropleth(df_pop_state)
state_choropleth(df_pop_state, num_colors = 7)

# 2 colors shows which states are above / below median value
state_choropleth(df_pop_state, num_colors=2)

# 1 color uses a continuous scale - useful for seeing outliers
state_choropleth(df_pop_state, num_colors = 1)

# use the "zoom" parameter to zoom in on certain states
# remember: all choroplethr requires lower-case state names
# combine multiple states using ?c
state_choropleth(df_pop_state, 
                 zoom = c("california", "oregon", "washington"))

# combine choropleth map and reference map by setting the "reference_map"
# parameter to TRUE
state_choropleth(df_pop_state, 
                 num_colors = 1,
                 zoom = c("california", "oregon", "washington"),
                 reference_map = TRUE)
