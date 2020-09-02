setwd("~/2018_NYU GPH/Applied Practice")

# Importing land use data for join -------------------------------------------------
MapPLUTO_Geo <- read.csv("GIS/TRIMMED MAP/MapPLUTO_Geo.csv")
str(MapPLUTO_Geo)

# Filtering and sorting to create initial table to join to  -------------------------------------------------
#creating summary table
##NOTE DO NOT USE CT2010 - USE ctlabel to find actual location of features that crosse multiple tracts

install.packages("dplyr")
library(dplyr)
table1 <- MapPLUTO_Geo %>% 
  filter(LandUse == 1) %>% 
  group_by(ctlabel) %>% 
  summarise(sum1 = sum(PERCTAREA))
print(table1)

table23 <- MapPLUTO_Geo %>% 
  filter(LandUse == 23) %>% 
  group_by(ctlabel) %>% 
  summarise(sum23 = sum(PERCTAREA))
print(table23)

table4 <- MapPLUTO_Geo %>% 
  filter(LandUse == 4) %>% 
  group_by(ctlabel) %>% 
  summarise(sum4 = sum(PERCTAREA))
print(table4)

table5 <- MapPLUTO_Geo %>% 
  filter(LandUse == 5) %>% 
  group_by(ctlabel) %>% 
  summarise(sum5 = sum(PERCTAREA))
print(table5)

table6 <- MapPLUTO_Geo %>% 
  filter(LandUse == 6) %>% 
  group_by(ctlabel) %>% 
  summarise(sum6 = sum(PERCTAREA))
print(table6)

table7 <- MapPLUTO_Geo %>% 
  filter(LandUse == 7) %>% 
  group_by(ctlabel) %>% 
  summarise(sum7 = sum(PERCTAREA))
print(table7)

table8 <- MapPLUTO_Geo %>% 
  filter(LandUse == 8) %>% 
  group_by(ctlabel) %>% 
  summarise(sum8 = sum(PERCTAREA))
print(table8)

table9 <- MapPLUTO_Geo %>% 
  filter(LandUse == 9) %>% 
  group_by(ctlabel) %>% 
  summarise(sum9 = sum(PERCTAREA))
print(table9)

table10 <- MapPLUTO_Geo %>% 
  filter(LandUse == 10) %>% 
  group_by(ctlabel) %>% 
  summarise(sum10 = sum(PERCTAREA))
print(table10)

table11 <- MapPLUTO_Geo %>% 
  filter(LandUse == 11) %>% 
  group_by(ctlabel) %>% 
  summarise(sum11 = sum(PERCTAREA))
print(table11)

ctlabel <- unique(MapPLUTO_Geo$ctlabel)
table <- data.frame(ctlabel)
  

library(plyr)
landuse <- join_all(list(table, table1, table23, table4, table5, table6, table7, table8, table9, table10, table11), by='ctlabel', type='left')
write.csv(landuse, file = "GIS/TRIMMED MAP/landusejoin.csv")
detach("package:plyr", unload=TRUE)


# Importing full join CSV from QGIS -------------------------------------------------
fulljoin <- read.csv("GIS/TRIMMED MAP/SVI_ACS_500Cities_TrimmedJoinShort.csv")
str(fulljoin)
fulljoin$ACS_agesex_CTLABEL <- as.character(fulljoin$ACS_agesex_CTLABEL)

fulljoinclean <- fulljoin[-c(88),] # DROP ctract 319 (no people)


# Running correlation matrix on risk factors -------------------------------------------------
install.packages("Hmisc")
library("Hmisc")
corr <- rcorr(as.matrix(fulljoinclean))


df.corr=data.frame(corr$r)
df.corrsig=data.frame(corr$P)
df.corrsigcode=data.frame(corr$P)


write.csv(df.corr, file = "GIS/TRIMMED MAP/fulljoincorrclean.csv")
write.csv(df.corrsig, file = "GIS/TRIMMED MAP/fulljoincorrsigclean.csv")

# Running correlation matrix with COVID outcome data
covidmay <- read.csv("GIS/ArealInterp/areal_20200518.csv")
covidaug <- read.csv("GIS/ArealInterp/areal_20200806.csv")

fulljoincovid <- read.csv("GIS/TRIMMED MAP/SVI_ACS_500Cities_TrimmedJoinCovid.csv")
covidcorr <- rcorr(as.matrix(fulljoincovid))

df.covidcorr=data.frame(covidcorr$r)
df.covidcorrsig=data.frame(covidcorr$P)
df.covidcorrsigcode=data.frame(covidcorr$P)

write.csv(df.covidcorr, file = "GIS/TRIMMED MAP/fulljoincorr_covid.csv")
write.csv(df.covidcorrsig, file = "GIS/TRIMMED MAP/fulljoincorrsig_covid.csv")

# Factor analysis (backwards) -------------------------------------------------
sigonly <- read.csv("GIS/TRIMMED MAP/SVI_ACS_500Cities_TrimmedJoinSIGONLY.csv")

cleandata <- sigonly[-c(1)] # drop census tract column

#finding # of factors (HORN'S PARALLEL ANALYSIS)
install.packages("paran")
library(paran)
paran(cleandata, iterations=0, centile=0, quietly=FALSE, 
      status=TRUE, all=FALSE, cfa=FALSE, graph=FALSE, 
      color=TRUE, col=c("black","red","blue"), 
      lty=c(1,2,3), lwd=1, legend=TRUE, file="", 
      width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)
##3 factor groups


#also check scree plott with psych package
install.packages('psych')
library(psych)
library(ggplot2)
factors <- fa.parallel(cleandata,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)
##3 factor groups


#try out 2 groups? for comparison
fit <- factanal(cleandata, factors = 2, rotaion ="varimax", lower = 0.02)
print(fit)
#visualization
fitplot <- fit$loadings[,1:2] 
plot(fitplot,type="n") # set up plot 
text(fitplot,labels=names(cleandata),cex=.7) # add variable names

fit3 <- factanal(cleandata, factors = 3, rotaion ="varimax", lower = 0.02)
print(fit3)

fit4 <- factanal(cleandata, factors = 3, rotaion ="varimax", lower = 0.02)
print(fit4)

fit3export <- fit3$loadings[,1:3] 
write.csv(fit4export, file = "GIS/TRIMMED MAP/fulljoin_factorloading.csv")


# Factor analysis (forwards) -------------------------------------------------
# START WITH POVERTY - ADD NEW FACTORS BASED ON CORR AND MORAN'S I
sigonly <- read.csv("GIS/TRIMMED MAP/SVI_ACS_500Cities_TrimmedJoinFORWARD.csv")

cleandata <- sigonly[-c(1)] # drop census tract column

#check # of factor groups - only with many factors
factors <- fa.parallel(cleandata,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)

#try out 2 groups? for comparison
fitfwd <- factanal(cleandata, factors = 2, rotaion ="varimax", lower = 0.02)
print(fitfwd)
#visualization
fitplotfwd <- fitfwd$loadings[,1:2] 
plot(fitplotfwd,type="n") # set up plot 
text(fitplotfwd,labels=names(cleandata),cex=.7) # add variable names

fit3 <- factanal(cleandata, factors = 3, rotaion ="varimax", lower = 0.02)
print(fit3)

fit4 <- factanal(cleandata, factors = 3, rotaion ="varimax", lower = 0.02)
print(fit4)

fit3export <- fit3$loadings[,1:3] 
write.csv(fit4export, file = "GIS/TRIMMED MAP/fulljoin_factorloading.csv")
