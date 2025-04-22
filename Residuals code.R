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



#Solution: make it a bernoullli variable
pollution_deaths <- pollution_deaths %>% mutate(Deaths = as.integer(Deaths_Per_100000>0))
View(pollution_deaths)


#Best model so far - stepwise BIC on a Bernoulli model of deaths
model_final = glm(formula = Deaths ~ Clean_Fuel_Access_Percent + GDP_Per_Capita + 
                    Year + Clean_Fuel_Access_Percent:Year, family = "binomial", 
                  data = pollution_deaths)
summary(model_final)







# Deviance residuals (default)
resid_deviance <- residuals(model_final, type = "deviance")

# Pearson residuals
resid_pearson <- residuals(model_final, type = "pearson")

# Response residuals
resid_response <- residuals(model_final, type = "response")


# Fitted probabilities
fitted_vals <- fitted(model_final)

# Deviance residuals plot
plot(fitted_vals, resid_deviance, 
     main = "Deviance Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)

# Histogram of deviance residuals
hist(resid_deviance, breaks = 30, 
     main = "Histogram of Deviance Residuals",
     xlab = "Deviance Residuals")

# QQ plot
qqnorm(resid_deviance)
qqline(resid_deviance, col = "red")
