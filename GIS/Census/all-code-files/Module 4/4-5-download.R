library(choroplethr)
library(choroplethrMaps)

# This will get both the estimate and margin of error from table B19301
get_acs_data("B19301",
              "state", 
              endyear = 2015, 
              span = 5,
              include_moe = TRUE)