library(tidyverse)
library(GGally)

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

fuel_gdp <- tuesdata$fuel_gdp
death_source <- tuesdata$death_source
fuel_gdp
death_source

View(fuel_gdp)

pollution_deaths <- merge(fuel_gdp, death_source, by=c('Entity', 'Year', 'Code'))
pop = 'Population (historical estimates'
pollution_deaths <- pollution_deaths %>% rename('Clean_Fuel_Access_Percent' = 'Access to clean fuels and technologies for cooking (% of population)',  
                                                'GDP_Per_Capita' = 'GDP per capita, PPP (constant 2017 international $)' ,
                                                'Population'= 'Population (historical estimates)', 
                                                'Deaths_Per_100000' = 'Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)')
pollution_deaths <- subset(pollution_deaths, select = -c(Continent))

View(pollution_deaths)

#deaths is per 100000
#pollution_deaths <- pollution_deaths[!is.na(pollution_deaths$Code),]
pollution_deaths <- na.omit(pollution_deaths)
pollution_deaths <- subset(pollution_deaths, select = -Code)

justFew = pollution_deaths[pollution_deaths$Entity %in% c('Afghanistan', 'Belarus', 'Armenia', 'Argentina'),]
View(justFew)
ggpairs(justFew)

#rename Entity to Country
library(MASS)
null = glm(Deaths_Per_100000 ~ 1, family=poisson, data=pollution_deaths, offset=rep(log(100000), nrow(pollution_deaths)))
summary(null)

full = glm(Deaths_Per_100000 ~ Year * Clean_Fuel_Access_Percent * GDP_Per_Capita * Population, data=pollution_deaths, family=poisson, offset=rep(log(100000), nrow(pollution_deaths)))
summary(full)



stepAIC(null, scope = list(upper = full), 
        direction = "forward", k = 2)







