library(tidyverse)
library(GGally)

#read in cleaned data
pollution_deaths <- read.csv("pollution_deaths.csv")
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
