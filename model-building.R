library(tidyverse)
library(GGally)

#read in cleaned data
pollution_deaths <- read.csv("pollution_deaths.csv")

#exploring poisson models - did not use
library(MASS)
null = glm(Deaths_Per_100000 ~ 1, family=poisson, data=pollution_deaths)
null

full = glm(Deaths_Per_100000 ~ Year * Clean_Fuel_Access_Percent * 
             GDP_Per_Capita * Population, data=pollution_deaths, 
           family=poisson)
full

partial = glm(Deaths_Per_100000 ~ Year + Clean_Fuel_Access_Percent + 
             GDP_Per_Capita + Population + Year:Clean_Fuel_Access_Percent + 
               Year:GDP_Per_Capita + Year:Population + Clean_Fuel_Access_Percent:GDP_Per_Capita +
               Clean_Fuel_Access_Percent:Population + GDP_Per_Capita:Population, data=pollution_deaths, 
           family=poisson)
partial


#check diagnostics for dispersion parameter
#check about how to fit GLM - scale, as integer (counts vs. rates)

#can use stepAIC, look at plots to verify
#report selection method
#stepAIC(null, scope = list(upper = full), 
#        direction = "forward", k = 2)
#stepAIC(full, direction = "backward", k = 2)
#stepAIC(null, scope = list(upper = full), 
#        direction="both", k=2)
#AIC appears to just be selecting the full model or nearly full

#original poisson model
model = glm(formula = Deaths_Per_100000 ~ Clean_Fuel_Access_Percent + 
              GDP_Per_Capita + Year + Population, family = "quasipoisson", 
            data = pollution_deaths)
summary(model)

#motivating factor - there are a lot of 0s in the data
hist(pollution_deaths$Deaths_Per_100000)
count(pollution_deaths, Deaths_Per_100000)
length(pollution_deaths$Deaths_Per_100000)
#181 unique countries
length(unique(pollution_deaths$Country))



#Solution: make it a bernoullli variable
pollution_deaths <- pollution_deaths %>% mutate(Deaths = as.integer(Deaths_Per_100000>0))
View(pollution_deaths)

#fit bernoulli model 
#null
bern_null = glm(Deaths ~ 1, family="binomial", data=pollution_deaths)
#full
bern_full = glm(Deaths ~ Year * Clean_Fuel_Access_Percent * 
                  GDP_Per_Capita * Population, data=pollution_deaths, 
                family="binomial")


summary(bern_null)
summary(bern_full)


stepAIC(bern_null, scope = list(upper = bern_full), 
        direction = "forward", k = 2)
stepAIC(bern_full, direction = "backward", k = 2)
stepAIC(bern_null, scope = list(upper = bern_full), 
        direction="both", k=2)

View(pollution_deaths)

#fitting model from stepAIC
bern_model = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                   Year + Clean_Fuel_Access_Percent:Year + Clean_Fuel_Access_Percent:GDP_Per_Capita, 
                 family = "binomial", data = pollution_deaths)
#dispersion check - looks fine
bern_quasi = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                   Year + Clean_Fuel_Access_Percent:Year + Clean_Fuel_Access_Percent:GDP_Per_Capita, 
                 family = "quasibinomial", data = pollution_deaths)
summary(bern_model)
summary(bern_quasi)




#looking at BIC measure
n = length(pollution_deaths$Deaths)
stepAIC(bern_null, scope=list(upper=bern_full), direction="forward", k=log(n))
stepAIC(bern_full, direction="backward", k=log(n))
stepAIC(bern_null, scope=list(upper=bern_full), direction="both", k=log(n))



#Best model so far - stepwise BIC on a Bernoulli model of deaths
model_final = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
               Year + Clean_Fuel_Access_Percent:Year, family = "binomial", 
             data = pollution_deaths)
summary(model_final)

#looking at parameters - don't need the interaction term from the AIC stepwise, BIC works
anova(bern_model, test="Chisq")
anova(model_final, test="Chisq")


#get residuals
model_resid <- resid(model_final, type="deviance")

#residuals vs. fitted looks fine
plot(model_final$fitted.values, model_resid)


#issues - more high fuel access percent in data, fanning in residuals there
hist(pollution_deaths$Clean_Fuel_Access_Percent)
plot(pollution_deaths$Clean_Fuel_Access_Percent, model_resid)

#issues - more high gdp per capita in data, fanning in residuals there
hist(pollution_deaths$GDP_Per_Capita)
plot(pollution_deaths$GDP_Per_Capita, model_resid)

#residuals vs. year looks fine
plot(pollution_deaths$Year, model_resid)


model_final$coefficients

