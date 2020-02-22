# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#                                     Data Analysis with R                          #
#                                      Khagendra Adhikari                           #
#                                           2016                                    #
#                                                                                   #
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#   Use packages 
library(MASS)
library(ggplot2)
library(lmtest)
library(boot)
library(caret)
library(DAAG)
#                                         birthwt {MAAS}
#   Risk Factors Associated with Low Infant Birth Weight.
# ************************************************************************************
##  The birthwt data frame has 189 rows and 10 columns. 
##  The data were collected at Baystate Medical Center, Springfield, Mass during 1986.
# ************************************************************************************

# structure of dataset birthwt.
str(birthwt)
# low:    indicator of birth weight less than 2.5 kg.
# 
# age:    mother's age in years.
# 
# low:    mother's weight in pounds at last menstrual period.
# 
# race:   mother's race (1 = white, 2 = black, 3 = other).
# 
# smoke:  smoking status during pregnancy.
# 
# ptl:    number of previous premature labours.
# 
# ht:     history of hypertension.
# 
# ui:     presence of uterine irritability.
# 
# ftv:    number of physician visits during the first trimester.
# 
# bwt:    birth weight in grams.



# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&
#                                         Ans.1                                     #
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&



attach(birthwt)

#   Attach dataset birthwt, so the objects in the database can be accessed
#   by simply giving their names.

(race <- factor(race, labels = c("white", "black", "other")))

#   The factor function creates factor variable from numeric variable.
#   The levels in variable race has been modified as "white" for 1, "black" 
#   for 2, and "other" for 3.

(ptd <- factor(ptl > 0))

#   Create new logic variable from  numeric variable  with
#   TRUE for  ptl > 0 and FALSE otherwise.

table(ftv)

#   Display variable ftv in tabulated form. i.e. elements with corresponding frequency.  

(ftv <- factor(ftv))

#   Convert variable ftv of type numeric to factor.

levels(ftv) # Orignial levels.
(levels(ftv)[-(1:2)] <- "2+")

#   New level "2+" to all the levels of ftv except 1st ("0") and 2nd ("1") levels.
#   i.e. replace levels 2 or highter with new level "2+".

levels(ftv) # new levels after rename.

table(ftv)

#   Display the factor variable ftv in tabulated form.

bw <- data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
                 ptd, ht = (ht > 0), ui = (ui > 0), ftv)

#   reate new data frame bw updaing variable "low" as a factor,
#   and variable "smoke", "ht", and "ui" as logic.

detach()

#   Detach the attachment birthwt such that dataset birthwt should be
#   referenced in order to access the variable. i.e. birthwt$race.

rm(race, ptd, ftv)

#   Remove objects in lists (race, ptd, ftv).


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&
#                                         Ans.2                                     #
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&

model <- glm(low ~ age * ftv, family = binomial(link = logit), data = bw)
summary(model)

#   Yes, the data from this study confirm that ftv is an important
#   predicator of low birthweight. The interation effect is significant
#   between age of mother and "2+" number of physician visits during
#   the first trimester.


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&
#                                         Ans.3                                     #
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&

ftv.level <- levels(bw$ftv)

est.prob.age20 <- predict.glm(model, 
                       newdata = data.frame(age = 20, ftv = ftv.level), 
                       type = "response", se.fit=TRUE)

est.prob.age30 <- predict.glm(model, 
                       newdata = data.frame(age = 30, ftv = ftv.level), 
                       type = "response", se.fit=TRUE)

#   Estimated probability of low birthweight for 20 and 30 years mothers 
#   for each ftv-catagories
(est.prob.low.bw <- data.frame(ftv = ftv.level, est.prob.age20$fit, est.prob.age30$fit))

#   Fitted values vs. age plot for each ftv-grups.
ggplot(bw, aes(bw$age, model$fitted.values, colour = ftv))+
      geom_point()+
      labs(x = "Mother's age", y = "Probability of low birthweight")

#   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#   Interpretation based on the plot: #
#   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##   The mothers, who had no physician visit (ftv = 0), have a chance of having
##   uderweight baby is low but increase with the mother's age.

##   The mothers, who had one physician visit (ftv = 1), have a chance of having
##   uderweight baby is low and also decrease with the mother's age.

##   The young mothers, who had 2+ physician visits (ftv = 2+), have a higher chance of
##   having uderweight baby and but decrease sharply with the mother's age.


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&
#                                         Ans.4                                     #
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&

p30 <- est.prob.age30
lower.limit <- p30$fit - 1.96*p30$se.fit
upper.limit <- p30$fit + 1.96*p30$se.fit

#   95% cofidence interval of PI for 30 years old women in each ftv group.
(conf.interval <- data.frame( ftv.level, p30$fit, lower.limit, upper.limit))

#   The probabilty can't be zero. 
#   Set the lower.limit  to zero if it is negative.

conf.interval$lower.limit[conf.interval$lower.limit < 0] <- 0.0
conf.interval


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&
#                                         Ans.5                                     #
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&*********************************&&&&&&&&&&&&&&&&&&&&&

#   *****************************???????????????????????????*************************
##   The likelihood ratio test (LR-test) can be performed either using lrtest()
##   from lmtest package or using anova() function from base R. The reduced
##   model has less log likelihood but it is necessary to test whether the observed
##   difference is significan or not?. Here, the null hypothesis is that the
##   reduced model is true.
#   ********************************************************************************
# %%%%%%%%%
# Ans.5.a #*************************************************************************
# %%%%%%%%%

# model.a includes all explanatory variables from datset bw.
model.a <- glm(low ~ ., family = binomial(link = logit), data = bw)

# %%%%%%%%%
# Ans.5.b #**************************************************************************
# %%%%%%%%%

#   model.b include interaction effect to model.a.
model.b <- update(model.a,~.+age:ftv)

#   compare two models uisng anova. We use chi-squared test as our 
#   models are binomial fits.
anova(model.a, model.b, test = "Chisq")

# Alternative method gives the same result.
lrtest(model.a, model.b)

#   Here, p value shows that the model.b does singnificantly improve the model fit.

# %%%%%%%%%
# Ans.5.c #***************************************************************************
# %%%%%%%%%

summary(model.a)

#   From summary of model.a, we can see that explanatory variables 
#   "lwt", "race", "ptd", and "ht"  had a significant efffect 
#   using singificance level 0.05.

model.c <- glm(low ~ lwt + race+ ptd + ht, 
               family = binomial(link = logit), data = bw)

#   compare model.c with  model.a using anova.
anova(model.c, model.a, test = "Chisq")

#   compare model.c with model.a using anova.
anova(model.c, model.b, test = "Chisq")

#   According to LR-test, model.b is preferred.
AIC(model.a, model.b, model.c)

#   According to AIC, model.b with smalest AIC is the best.

#   The residuals also suggest model.b but they are not signigicant enough
#   to select one model over other. i.e ther are of same order.


# %%%%%%%%%
# Ans.5.d #****************************************************************************
# %%%%%%%%%

model.d <- stepAIC(model.a, scope = ~. ^2)

#   The final model with smallest AIC is
#   Step:  AIC=207.94
#   low ~ age + lwt + race + smoke + ptd + ht + ui + ftv + age:ftv + smoke:ui
#   Based on AIC value, we can conclude that this is the optimal model.

optimal.m <- glm(low ~ age + lwt + race + smoke + ptd + ht + ui + ftv + age:ftv + smoke:ui, 
                 family = binomial(link = logit), data = bw)

# %%%%%%%%%
# Ans.5.e #******************************************************************************
# %%%%%%%%%

#   The prediction accuracy based on 10-fold cross validation is
CVbinary(optimal.m, print.details = FALSE)$acc.cv

# Average of 10 prediction accuracy rates based on 5-fold cross validation.
acc.cv.list <- NULL
for (j in 1:10)
  {
     acc.cv.list[j] <- CVbinary(optimal.m, nfolds = 5, 
                             print.details = FALSE)$acc.cv
  }
ave.predict.acc <- mean(acc.cv.list)

paste("Average predication accuracy is", round(mean(ave.predict.acc)*100, 1), "%.")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

