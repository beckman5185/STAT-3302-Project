library(tidyverse)

#load in tidy tuesday dataset
tuesdata <- tidytuesdayR::tt_load('2022-04-12')

#get relevant tables
fuel_gdp <- tuesdata$fuel_gdp
death_source <- tuesdata$death_source


#merge tables
pollution_deaths <- merge(fuel_gdp, death_source, by=c('Entity', 'Year', 'Code'))

#rename columns
pollution_deaths <- pollution_deaths %>% rename('Clean_Fuel_Access_Percent' = 'Access to clean fuels and technologies for cooking (% of population)',  
                                                'GDP_Per_Capita' = 'GDP per capita, PPP (constant 2017 international $)' ,
                                                'Population'= 'Population (historical estimates)', 
                                                'Deaths_Per_100000' = 'Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)', 
                                                'Country' = 'Entity')

#remove irrelevant columns, rows missing values
pollution_deaths <- subset(pollution_deaths, select = -c(Continent))
pollution_deaths <- na.omit(pollution_deaths)
pollution_deaths <- subset(pollution_deaths, select = -c(Code))

#scale GDP by thousands
#scale population by millions
#truncate decimal portion of deaths per 100000
pollution_deaths <- pollution_deaths %>% mutate(Population = Population/1000000, GDP_Per_Capita = GDP_Per_Capita/1000, Deaths_Per_100000 = as.integer(Deaths_Per_100000))

#write file to csv
write.csv(pollution_deaths, "pollution_deaths.csv", row.names=FALSE)
