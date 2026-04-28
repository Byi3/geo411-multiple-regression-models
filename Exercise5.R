#Multivariate Statistics for Geographers
#Homework #5 

#Read in datafile
sales <- read.csv("homeSales.csv")
names(sales)

#Estimate Model for part a (base model)
base.model <- lm(price ~ bedrooms + bathrooms + sqft_living + yr_built + HS + SC, data = sales)

summary(base.model)

#Perform the diagnostic plots
plot(base.model)

#Independence and right hand side variables
ResidualVsXPlots <- function(mod.in){
	var.names <- names(mod.in$coefficients)
	n.x.vars <- length(var.names)
	mod.e <- residuals(mod.in)

	for (i in 2:n.x.vars){
		plot (mod.in$model[,var.names[i]], mod.e, xlab = var.names[i], ylab = "residuals")
		lines(lowess(mod.in$model[,var.names[i]],mod.e, f=3/4), col="red")
		locator(1)
		}
	}

ResidualVsXPlots(base.model)


#Homoskedasticity and right hand side variables
XScaleLocationPlots <- function(mod.in){
	var.names <- names(mod.in$coefficients)
	n.x.vars <- length(var.names)
	std.residuals <- sqrt(abs(rstandard(mod.in)))

	for (i in 2:n.x.vars){
		plot (mod.in$model[,var.names[i]], std.residuals, xlab = var.names[i], 
			ylab = "Square root of Absolute Standardized Residuals")
		lines(lowess(mod.in$model[,var.names[i]], std.residuals, f=3/4), col="red")
		locator(1)
		}
	}

XScaleLocationPlots(base.model)


#Calculating VIF

#Install the package
#install.packages(c("car"))

#Load the package
library(car)

#Calculate VIF
vif(base.model)



#New Model
#Estimate Model for part a (base model)
base.model_improved <- lm(price ~ bedrooms + sqft_living + sqft_lot + waterfront + yr_built + pop_estimate + HS + SC, data = sales)

summary(base.model_improved)

vif(base.model_improved)

#log transformation
improved_log <- lm(log(price) ~ bedrooms + (sqft_living)^2 + yr_built + log(pop_estimate)+ HS, data = sales)

summary(improved_log)

vif(improved_log)

plot(improved_log)

ResidualVsXPlots(improved_log)

XScaleLocationPlots(improved_log)

#Upload to GitHub and paste your link. 
