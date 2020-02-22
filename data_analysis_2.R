# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#                                   Data Analysis with R
#                                      Khagendra Adhikari
#                                             2016
#
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Use packages 
library(DAAG)
library(lattice)
library(MASS)
library(leaps)


# *************************************************************************************
# Q.1
#   The following code is designed to explore effects that can result from the omission
#   of explanatory varaibles:
#   > x1<-runif(10)
#   > x2<-rbinom(10,1,1-x1)
#   > y<-5*x1+x2+rnorm(10,sd=0.1) # simulated model, coef of x2 is positive
#   > y.lm<-lm(y~factor(x2))      # model fitting to observed data without x1
#   > coef(y.lm)
#   (Intercept) factor(x2)1
#   3.2429990 -0.3789021
#   > y.lm2<-lm(y~x1+factor(x2))  # correct model
#   > coef(y.lm2)
#   (Intercept) x1 factor(x2)1
#   0.034054df.b 5.0017938 1.0232510
#
#   What happens if x2 is generated according to
#   x2 <- rbinom(10,1,x1)?
#   x2 <- rbinom(10,1, .5)?
# ******************************************************************************************

set.seed(100)
x1.0 <- runif(10)
x2.0 <- rbinom(10, 1, 1-x1.0)

##  This function comapreModels compare the coefficent of omitted
##  model with the full model.

compareModels <- function(x2, x1 = x1.0)
{
  y <- 5*x1 + x2 + rnorm(10, sd=0.1)  # simulated model, coef of x2 is positive
  y.lm <- lm(y ~ factor(x2))          # model fitting to observed data without x1
  print("Omited model")
  print(coef(y.lm))
  y.lm2 <- lm(y ~ x1 + factor(x2))    # correct model
  print("correct model")
  print(coef(y.lm2))
}

compareModels(x2.0)                  # given in question

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    The coefficient of x2.0 has opposite sign.
##  
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

x2.1 <- rbinom(10, 1, x1.0)          # First model
compareModels(x2.1)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    Here, x2.1 depends on x1.0. So, the cofficient of x2.1 has correct sign
##    but not the value.The omitted variable bias.
##  "
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

x2.2 <- rbinom(10, 1, 0.5)         # Second model
compareModels(x2.2)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    Here, x2.2 does not depent on x1.0. So, the coeficient of estimation
##    of x2.2 is reasonable in comparision to correct(full) model. 
##  "
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **************************************************************************************
# Q.2
#   For the data frame oddbooks (DAAG),
#   (a) Use the function pairs( ) to display the scatterplot matrix. Which paris of 
#       variables show evidence of a strong relationships?
# ***************************************************************************************

pairs(oddbooks)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    Height and breadth shows the evidence of strong relationship. And other variables
##    have also significant correlation.
##  "
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##  Full  model
oddbooks.all.lm <- lm(weight ~ thick + height + breadth, data =  oddbooks)

# *************************************************************************************
# (b) In each panel of the scatterplot matrix, record the correlation for the panel and
#     add the smooth fitting curve in the panel. Hint: look for functions in help page
#     of pairs().
# *************************************************************************************

## This function panel.cor is imported from lecture code multilr.R. 
panel.cor <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x,y)
  ar <- abs(r)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * ar)
}

pairs(oddbooks,lower.panel = panel.smooth, upper.panel = panel.cor)

# *************************************************************************************
# (c) Use the backward selection method to select the best linear regression model
#     based on AIC.
# *************************************************************************************

##  stepAIC {MASS}:  Performs stepwise model selection by AIC.
##  smallar AIC value gives better model.

stepAIC(oddbooks.all.lm, direction = "backward")    # backward selection

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    Based on AIC, weight ~ breadth is the best linear regression model.
##  "
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ***************************************************************************************
#   (d) Search for the best model on all possible models based on the criterion adjust R^2.
# ***************************************************************************************

##  The error of regression goes down as adjusted R^ goes up.
##  uses regression subset selection packages leaps based on adjusted R^2.

precditors.matrix <- data.matrix(oddbooks)[, 1:3]
leaps(precditors.matrix, oddbooks$weight, method = "adjr2")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    The observation shows that the that includes thick (column 1) and
##    breadth (columns 3) of precditors matrix has larger(close to 1). 
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## compare adj.r.sruqred with the summary of that model.
oddbooks.tb.lm <- lm(weight ~ thick + breadth, data =  oddbooks)
summary(oddbooks.tb.lm)$adj.r.squared

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##  Based on adjusted R square, the model weight ~ thick + breadth is the best.
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ***************************************************************************************
# (e) Fit the following regression relationships with log transformations. To do such
#     transformation is due to that we might expect weight proportional to volume(tbh).
# ***************************************************************************************

##  scatterplot matrix in log scale
splom(~log(oddbooks))

# *****************************************************************************************
# e.i. log(weight) on log(thick), log(height) and log(breadth).
# ******************************************************************************************

oddbooks.log.lm <- lm(log(weight) ~ log(thick) + log(height) + log(breadth), data = oddbooks)
summary(oddbooks.log.lm)

# *******************************************************************************************
# e.ii. log(weight) on log(thick) and 0.5*(log(height)+log(breadth)).
#     Hint: the model formal is log(weight) ∼ log(thick) +I(0.5*(log(height)+log(breadth))).
#     The reason for the use of the wrapper function I( ) is to prevent the parser
#     from giving * the special meaning that it would otherwise have in a model formula.
# *********************************************************************************************
oddbooks.log.lm1 <- lm(log(weight) ~ log(thick) 
                       + I(0.5*(log(height) + log(breadth))), data = oddbooks)
summary(oddbooks.log.lm1)

# What feature of the scatterplot matrix suggests that this might make sense to use
# this form of equation?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    The height and breath are strongly correlated. From scatter plot, we can see that
##    weight has similar corelation with both height and breadth, so we don't need to
##    include both as different predictor variables. It seems to consider the geometric  
##    mean of height and breadth as a single variable.
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Removing the intercept term gives better fittings? Explain why?
oddbooks.log.lm1.no.intercept <- lm(log(weight)~ -1 + log(thick) 
                                    + I(0.5*(log(height) + log(breadth))), data = oddbooks)
summary(oddbooks.log.lm1.no.intercept)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    Yes, removing the intercept term gives better fitting. The p small value of the model 
##    without intercept inticates that the estimated coefficeitns are signigicant. And, this
##    model has also smallar Std. Error compare to the previous model.
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *******************************************************************************************
# (f) Check multicolinearity with variance inflation factor. If the multicolinearity problem
#     exists, remove some variable to deal with it.
# ******************************************************************************************

vif(oddbooks.all.lm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## "
##    The scatter plot shows that height and breadth are highly correlated. 
##    So, include one of these. Here, remove height variable with big vif.
##    i.e Weight ~ thick + breadth
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vif(oddbooks.tb.lm)
# here vif are ok.

# *******************************************************************************************
# Q.3.
#   The following compares regression splines with locally weighted regression (lowess), for
#   scatterplot smoothing, using the fruitohms data frame in DAAG.
library(splines)
plot(ohms ~ juice, cex = 0.8, xlab = "Apparent juice content (%)",
     ylab = "Resistance (ohms)", data = fruitohms)
hat <- with(fruitohms, fitted(lm(ohms ~ ns(juice, 4))))
with(fruitohms, lines(juice, hat, col=3))

attributes(ns(fruitohms$juice, 4))$knots
with(fruitohms, lines(lowess(juice, ohms), col = "red"))
with(fruitohms, lines(lowess(juice, ohms, f=0.2), col = "cyan"))
# ***********************************************************************************************
# (a) Experiment with different choices for number of degrees of freedom.
# ************************************************************************************************
plot(ohms ~ juice, cex=0.8, xlab="Apparent juice content (%)",
     ylab="Resistance (ohms)", data=fruitohms)
dF <- c(1:7)                              # list of degree of freedom
for (dOf in dF) {
  hat <- with(fruitohms, fitted(lm(ohms ~ ns(juice, df = dOf))))
  with(fruitohms, lines(juice, hat, col=dOf, lty=dOf, lwd = 1.5))
}
legend("topright", legend = dF, col = dF, lty = dF, title = "df" )

# ********************************************************************************************
# (b) How many degrees of freedom seem needed to adequately capture the pattern of change?
# ********************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  "
##    6 degrees of freedom seem needed to adequately capture the pattern of the change.
##  "
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df.b <- 6                                # best degree of freedom
plot(ohms ~ juice, cex = 0.8, xlab = "Apparent juice content (%)",
     ylab = "Resistance (ohms)", data = fruitohms)
hat.df.b <- with(fruitohms, fitted(lm(ohms ~ ns(juice, df=df.b))))
with(fruitohms, lines(juice, hat.df.b, col="blue", lty=1, lwd = 2))
knots <- as.data.frame(attributes(ns(fruitohms$juice,df=df.b))$knots)
for (knot in knots) {abline(v=knot)}

# *************************************************************************************************
# (c) Plot the spline basic functions. Add vertical lines to the plot that show the knot
#     locations.
# **********************************************************************************************
## natural splines basis curve.

fruit.best.df.lm <- lm(ohms ~ ns(juice, df.b), data = fruitohms)
model.m <- model.matrix(fruit.best.df.lm)
for(index in 2 : df.b + 1){
  plot(model.m[, index] ~ juice,  xlab = "Apparent Juice Content (%)",
      ylab="Spline basis functions", type="l", lty= index-1, lwd=1.5, 
      col = index-1, data=fruitohms, ylim=c(-0.35,1.2))
  par(new=T)
}
for (knot in knots) {abline(v=knot)}  # verticle lines at knot locations.
legend("top",legend=1:df.b, col=1:df.b, lty = 1:df.b, seg.len = 1.3, 
       horiz = TRUE, title = "basis index" )

##  Alternative method gives the same plot.
## basis plot using lattice.
nsdf.b <- as.data.frame(with(fruitohms, ns(juice, df.b))[, 1:df.b])
names(nsdf.b) <- c("f1", "f2", "f3","f4", "f5", "f6")
nsdf.b$juice <- fruitohms$juice
xyplot(f1 + f2 + f3 + f4 + f5 + f6 ~ juice, type="l", data= nsdf.b,
       auto.key = list(columns=3, points=FALSE, lines=TRUE))

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

