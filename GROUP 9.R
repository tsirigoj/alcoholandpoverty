#GROUP 9
#HOW ALCOHOLISM AFFECT POVERTY LEVELS

#Use NHANES dataset
library('NHANES')
df<-NHANES

#CLEAN DATA
#Dichotomize GENDER variable
df$female<-ifelse(df$Gender=="female",1,0)

#Subset AGE variable to 20-69
df_new<-subset(df, Age>=20 & Age<=69)


#Delete observations with NA for Poverty
library(tidyverse)
df_new<-df_new %>% drop_na(AlcoholDay,Poverty)

hist(df$Poverty)


#TREAT ALCOHOL VARIABLE BY TAKING MEDIAN AND USING STANDARD DEVIATION
alc_median<-median(df_new$AlcoholDay)
alc_median #The median drinks per person is 2

alc_sd<-sd(df_new$AlcoholDay)
alc_sd #The standard deviation for daily drinks is 3.323637

lowDrinker<-alc_median+((1/2)*(alc_sd))
medDrinker<-alc_median+((3/2)*(alc_sd))
highDrinker<-alc_median+((5/2)*(alc_sd))
extremeDrinker<-alc_median+((7/2)*(alc_sd))

k=0
df_new$alc_level<-alc_median

for (k in 1:length(df_new$AlcoholDay)){
  if (df_new$AlcoholDay[k]<=lowDrinker){
    df_new$alc_level[k]=1
  }
  else if ((df_new$AlcoholDay[k]>lowDrinker) && (df_new$AlcoholDay[k]<=medDrinker)){
    df_new$alc_level[k]=2
  }
  else if ((df_new$AlcoholDay[k]>medDrinker) && (df_new$AlcoholDay[k]<=highDrinker)){
    df_new$alc_level[k]=3
  }
  else{
    df_new$alc_level[k]=4
  }
  k=k+1
}

#USE SEQUENTIAL TESTS TO DECIDE COVARIATES TO USE
#Start with alc_level & age
#nullh: age does not change the model
#alth: model with age is significantly better
final_model<-lm(Poverty~alc_level+Age+Race1+Education+female,data=df_new)
summary(final_model)   

final_model2<-lm(Poverty~alc_level+Age+Race1+Education+female+(Age*Race1)+(Age*female)+(Race1*Education)+(Race1*female),data=df_new)
anova(final_model2,final_model)#p-value=<0.001
summary(final_model2)

###########################################
######CHECKING REGRESSION ASSUMPTIONS######
###########################################
plot(df_new$alc_level,df_new$Poverty)
simple_reg<-lm(Poverty~alc_level,data=df_new)
summary(simple_reg)

#Add regression line
abline(simple_reg)

#Now, lets investigate the residuals of this model -- we will use our whole model
#for this
plot(final_model2)

#We don't have equal variance among residuals, so lets try robust standard errors
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)
w<-coeftest(final_model2,vcov=sandwich)
plot(w)

###########################################
###########RUN DIAGNOSTICS#################
###########################################

#COOKS DISTANCE
#Will give us strongly influential points
plot(final_model2,4) #Points 1909, 2145, and 3612 are influential
library(olsrr)
ols_plot_cooksd_bar(final_model2)


#3612 seems to be the largest of the three, so lets remove it
c1_df<-df_new[-c(3612),]

#Lets run our regression with the data frame with n-1 observations
c1_reg<-lm(Poverty~alc_level+Age+Race1+Education+female+(Age*Race1)+(Age*female)+(Race1*Education)+(Race1*female),data=c1_df)
summary(c1_reg) 
summary(final_model2) #alc_level variable becomes LESS statistically significant
#when we remove his observation, we will leave it in

#STUDENTIZED RESIDUALS PLOT
ols_plot_resid_stud(final_model2)#Outliers include observations 1678, 1915
#3907 and 4000 where 1678 appears to be the biggest outlier

#Run a regression without the outlier
c2_df<-df_new[-c(1678),]
c2_reg<-lm(Poverty~alc_level+Age+Race1+Education+female+(Age*Race1)+(Age*female)+(Race1*Education)+(Race1*female),data=c2_df)
summary(c2_reg)
summary(final_model2)

#FOR PRESENTATION
par(mfrow=c(2,2))

hist(df_new$AlcoholDay)
hist(df_new$alc_level)
