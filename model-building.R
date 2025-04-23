#load libraries
library(tidyverse)
library(GGally)
library(MASS)

#read in cleaned data
pollution_deaths <- read.csv("pollution_deaths.csv")

#null and full models for poisson modeling
#did not end up using these
null_poiss = glm(Deaths_Per_100000 ~ 1, family="poisson", data=pollution_deaths)
full_poiss = glm(Deaths_Per_100000 ~ Year * Clean_Fuel_Access_Percent * 
             GDP_Per_Capita * Population, data=pollution_deaths, 
           family="poisson")


#forward selection by AIC
stepAIC(null_poiss, scope = list(upper = full_poiss), 
        direction = "forward", k = 2)
#backward selection by AIC
stepAIC(full_poiss, direction = "backward", k = 2)
#stepwise seleciton by AIC
stepAIC(null_poiss, scope = list(upper = full_poiss), 
        direction="both", k=2)

#results: selecting full model or nearly full model


#quasi-poisson model for just original variables 
original_poiss = glm(formula = Deaths_Per_100000 ~ Clean_Fuel_Access_Percent + 
              GDP_Per_Capita + Year + Population, family = "quasipoisson", 
            data = pollution_deaths)
summary(original_poiss)

#results: summary shows dispersion parameter of 10, highly overdispersed


#examining data: finding a lot of 0s, likely causing overdispersion
hist(pollution_deaths$Deaths_Per_100000)
count(pollution_deaths, Deaths_Per_100000)

#3044 observations
length(pollution_deaths$Deaths_Per_100000)
#181 unique countries
length(unique(pollution_deaths$Country))



#Solution: make it a bernoullli variable
pollution_deaths <- pollution_deaths %>% mutate(Deaths = as.integer(Deaths_Per_100000>0))
View(pollution_deaths)


#null and full models for bernoulli model
bern_null = glm(Deaths ~ 1, family="binomial", data=pollution_deaths)
bern_full = glm(Deaths ~ Year * Clean_Fuel_Access_Percent * 
                  GDP_Per_Capita * Population, data=pollution_deaths, 
                family="binomial")


#forward selection by AIC
stepAIC(bern_null, scope = list(upper = bern_full), 
        direction = "forward", k = 2)
#backward selection by AIC
stepAIC(bern_full, direction = "backward", k = 2)
#stepwise selction by AIC
stepAIC(bern_null, scope = list(upper = bern_full), 
        direction="both", k=2)

#results: stepwise and forward selection agree, discarding backward for parsimony


#fitting model from stepwise selection on AIC
bern_AIC = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                   Year + Clean_Fuel_Access_Percent:Year + Clean_Fuel_Access_Percent:GDP_Per_Capita, 
                 family = "binomial", data = pollution_deaths)
summary(bern_AIC)
#fitting quasi-binomial to check dispersion - appears to be fine
bern_quasi = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                   Year + Clean_Fuel_Access_Percent:Year + Clean_Fuel_Access_Percent:GDP_Per_Capita, 
                 family = "quasibinomial", data = pollution_deaths)
summary(bern_quasi)



#need length of data for BIC
n = length(pollution_deaths$Deaths)

#forward selection by BIC
stepAIC(bern_null, scope=list(upper=bern_full), direction="forward", k=log(n))
#backward selection by BIC
stepAIC(bern_full, direction="backward", k=log(n))
#stepwise selection by BIC
stepAIC(bern_null, scope=list(upper=bern_full), direction="both", k=log(n))


#results: stepwise and forward selection agree, discarding backward for parsimony


#fitting model from stepwise BIC
bern_BIC = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                 Year + Clean_Fuel_Access_Percent:Year, family = "binomial", 
               data = pollution_deaths)
summary(bern_BIC)


#looking at parameters - don't need the interaction term from the AIC stepwise, BIC works
anova(bern_AIC, test="Chisq")
anova(bern_BIC, test="Chisq")


#Best model so far - stepwise BIC on a Bernoulli model of deaths
model_final = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                   Year + Clean_Fuel_Access_Percent:Year, family = "binomial", 
                 data = pollution_deaths)



#get deviance residuals
model_resid <- resid(model_final, type="deviance")

#residuals vs. fitted values plot looks fine
plot(model_final$fitted.values, model_resid, xlab="Fitted Values", ylab="Deviance Residuals")

#issues - more high fuel access percent in data, fanning in residuals there
hist(pollution_deaths$Clean_Fuel_Access_Percent, xlab = "Clean Fuel Access %", main="Histogram of Clean Fuel Access %")
plot(pollution_deaths$Clean_Fuel_Access_Percent, model_resid, xlab="Clean Fuel Access %", ylab="Deviance Residuals")

#issues - more high gdp per capita in data, fanning in residuals there
hist(pollution_deaths$GDP_Per_Capita, xlab="GDP Per Capita", main="Histogram of GDP Per Capita")
plot(pollution_deaths$GDP_Per_Capita, model_resid, xlab="GDP Per Capita", ylab="Deviance Residuals")


#residuals vs. year looks fine
plot(pollution_deaths$Year, model_resid, xlab="Year", ylab="Deviance Residuals")

#coefficients for final model
model_final$coefficients

