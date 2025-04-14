library(tidyverse)
library(GGally)

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

fuel_gdp <- tuesdata$fuel_gdp
death_source <- tuesdata$death_source
#fuel_gdp
#death_source

#View(fuel_gdp)

#get data together
pollution_deaths <- merge(fuel_gdp, death_source, by=c('Entity', 'Year', 'Code'))
pollution_deaths <- pollution_deaths %>% rename('Clean_Fuel_Access_Percent' = 'Access to clean fuels and technologies for cooking (% of population)',  
                                                'GDP_Per_Capita' = 'GDP per capita, PPP (constant 2017 international $)' ,
                                                'Population'= 'Population (historical estimates)', 
                                                'Deaths_Per_100000' = 'Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)', 
                                                'Country' = 'Entity')
pollution_deaths <- subset(pollution_deaths, select = -c(Continent))




#deaths is per 100000
#pollution_deaths <- pollution_deaths[!is.na(pollution_deaths$Code),]
pollution_deaths <- na.omit(pollution_deaths)


#scale GDP by thousands
#scale population by millions
#truncate decimal portion of deaths per 100000
pollution_deaths <- pollution_deaths %>% mutate(Population = Population/1000000, GDP_Per_Capita = GDP_Per_Capita/1000, Deaths_Per_100000 = as.integer(Deaths_Per_100000))
pollution_deaths <- subset(pollution_deaths, select = -Code)

View(pollution_deaths)


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


#stepAIC(null, scope = list(upper = partial), 
#        direction = "forward", k = 2)
#stepAIC(partial, direction = "backward", k = 2)
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
plot(pollution_deaths$Year, bern_2_resid)


