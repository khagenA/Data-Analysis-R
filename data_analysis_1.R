# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#                                     Data Analysis with R
#                                      Khagendra Adhikari
#                                           2016
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Use packages 
library(DAAG)
library(lattice)
library(ggplot2)
library(reshape2)
library(latticeExtra)
library(boot)

# *****************************************************************************************
# Q.1.
# Draw four random samples, each of size 5. One from Normal distribution, one from Chi-
# square distribution, one from Gamma-distribution and one from Cauchy-distribution.
# Plot the samples so that all of them are in one graph sheet. Use different point
# characters (symbols) and colors in each of the plot. 
# (You can use your own selection of distributional parameters when drawing the samples)
# ******************************************************************************************

set.seed(50)
df <- data.frame(x.value = 1:5, normal = rnorm(5), chi.sq = rchisq(5,3),
                  gamma = rgamma(5,1), cauchy = rcauchy(5))

require(reshape2) 
melt.df <- melt(df,  id.vars = 'x.value', 
                variable.name = 'Distributions', value.name = 'Random.Numbers')

require(ggplot2)
ggplot(melt.df, aes(x.value, Random.Numbers )) + 
       geom_line(aes(colour = Distributions), lwd = 0.8) + 
       geom_point(aes(shape = Distributions, colour = Distributions), size = 2)+
       labs(x= 'Random Samples', y = 'Random Numbers')

# Q.2.
# Data frame possum (DAAG package)
# The possum data frame consists of nine morphometric measurements on each 
# of 104 mountain brushtail possums, trapped at seven sites from Southern 
# Victoria to central Queensland.

# ***********************************************************************************
# Q.2.a
# Use the lattice function bwplot () to display the distribution of ages for each
# combination of site and sex. Show the different sites on the same panel, with
# different panels for different sexes.
# **********************************************************************************

bwplot(age ~ site | sex, data = possum)

# *********************************************************************************
# Q.2.b
# plot a histogram and density estimation curve on the earconch measurement.
# The distribution should appear bimodal (two peaks). This is a simple indication
# of clustering, possibly due to sex differences.
# **********************************************************************************

hist(possum$earconch, main = "", xlab = "earconch length", col ='skyblue', prob = TRUE)
lines(density(possum$earconch), col = 'red', lwd = 2)
text(49, 0.12, "Bimodal")
arrows(48, 0.115, x1 = 46.5, y1 = 0.10)
arrows(50, 0.115, x1 = 51.5, y1 = 0.10)

# ************************************************************************************
# Q.2.c
# Obtain side-by-side boxplots of the male and female earconch measurements. How
# do these measurement distributions differ? Can you predict what the corresponding
# histogram would like? Plot them to check your answer.
# ************************************************************************************

boxplot(earconch ~ sex, data = possum, boxwex = 0.5, horizontal = TRUE)

# A.2.c
"
  The side-by-side boxplots of the male and female enconch measurement shows that
  the median earconch length of female is greater than that of male. In contrast,
  female has minimum earconch length and male has maximum earconch length.
  The corresponding histogram should have more frequencies in the region of
  long earconch length for the female and vice versa for the male.
"

histogram(~ earconch | sex, data = possum)

"
  This histogram shows that the bimodality in the distribution is not becouse of sex.
"

# ****************************************************************************************
# Q.3
# For the data frame ais (DAAG package), draw graphs that show how the values of the
# hematological measures (red cell count, hemoglobin concentration, hematocrit, white
# cell count and plasma ferritin concentration) vary with the sport and sex of the athlete.
# *****************************************************************************************

bwplot(sport ~ rcc | sex, data = ais)
bwplot(sport ~ hg | sex, data = ais)
bwplot(sport ~ hc | sex, data = ais)
bwplot(sport ~ wcc | sex, data = ais)
bwplot(sport ~ ferr | sex, data = ais)


# Q.4.
# DATA
# The Effect of Vitamin C on Tooth Growth in Guinea Pigs.
# The response is the length of odontoblasts (cells responsible for tooth growth)
# in 60 guinea pigs. Each animal received one of three dose levels of vitamin 
# C (0.5, 1, and 2 mg/day) by one of two delivery methods, 
# (orange juice or ascorbic acid (a form of vitamin C and coded as VC).
                                                                                                                                                                                                                      
# For the built-in data ToothGrowth, the plot from the following code indicates some
# tooth length differences between ‘VC’ and ‘OJ’ groups and each dose level.    

require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

# *************************************************************************************
# Q.4.a
# Compare the difference between ‘VC’ and ‘OJ’ groups by t-test. First you have
# to test can the variances of the groups be assumed equal, then based on the result
# you can do t-test correctly.
# ************************************************************************************
#  Use F test two compare to variances.

var.test(ToothGrowth$len[ToothGrowth$supp  == "VC"],
         ToothGrowth$len[ToothGrowth$supp == "OJ"])

"
  Here, variances are equal based on p-vlaue and 95 percent confidence interval.
  So, we do two samples t-test with equal variance.
"
t.test(ToothGrowth$len[ToothGrowth$supp == "VC"], 
       ToothGrowth$len[ToothGrowth$supp == "OJ"], 
       variance.equal = TRUE)
"
  We can not reject the null hypothesis. It shows that orange juice (OJ) and
  ascorbic acid (VC) have not different impact on the tooth growth of Guinea Pigs.
"
#*****************************************************************************************
# Q.4.b
# Construct the 95% confidence interval for the mean of the tooth length of guinea
# pigs under each dose level.
# *************************************************************************************
                                                                                                        
# Here, we do all the t-test assuming unequal variance (default) for each dose level.

dose.level <- unique(ToothGrowth$dose)
for (dl in dose.level){
  print(sprintf ("For the given dose level dl = %0.1f",dl))
  
   print(t.test(ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == dl], 
                ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == dl]))
}

# ***********************************************************************************
# Q.4.c
# Interpret your results.
# ***********************************************************************************
# A.4.c
"
  For lowest dose level 0.5 and intermediate dose level 1.0, we can reject the
  null hypothesis. So, one suppliment is more effective than the other.

  For highet dose leve 2.0, we can not reject the null hypothesis and it shows that
  the effect of both supplyments are similar.
"


# Q.5
# The multi-way table UCBAdmissions has admission frequencies, by sex, for the six
# largest departments at the University of California at Berkeley in 1973. The following
# gives a table that adds the 2 × 2 tables of admission data overall all departments:
## For each combination of margins 1 and 2, calculate the sum
UCBtotal <- apply(UCBAdmissions, c(1,2), sum)
# *****************************************************************************************
# Q.5.a
# What are the names of the two dimensions of this table?
# ****************************************************************************************

dimnames(UCBtotal)
# A.5.a
"
  The names of the two dimensions are Admit and Gender.
"

# *****************************************************************************************
# 5.b
# From the table UCBAdmissions, create mosaic plot for each faculty separately.
# (If necessary refer to the code given in the help page for USBAdmissions.)
# *****************************************************************************************

par(mfrow=c(2,3))
for(i in 1:6) mosaicplot(t(UCBAdmissions[,,i]),main = paste("Dept", LETTERS[i]), color = TRUE)
par(mfrow=c(1,1))

# ********************************************************************************************
# 5.c
# Compare the information in the table UCBtotal with the result from applying the
# function mantelhaen.test( ) to the table UCBAdmissions. Compare the two sets
# of results, and comments on difference.
# ********************************************************************************************
UCBtotal
mantelhaen.test(UCBAdmissions)
# A.5.c
"
  The information in the UCBtotal table shows that the male admitted rate is higher
  than that of female admitted rate. But, mantelhaen.test( ) to the table UCBAdmissions
  shows no evidence for association between gender and admission.
"
# ******************************************************************************************
# 5.d
# The Mantel-Haenzel test is valid only if the male to female odds ratio for admission
# is similar across departments. The following code calculates the relevant odds ratios:
  apply(UCBAdmissions, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
# Is the odds ratio consistent across department? Which department(s) stand(s)
# out as different? What is the nature of the difference?
# *******************************************************************************************

# A.5.d
  "
    The odds ratio is not consistent across the department, lowest for department A
    and highest for department E.
  "

# ********************************************************************************************
# Q.6.
  # P142 15 For constructing bootstrap confidence intervals for the correlation coefficient,
  # it is advisable to work with the Fisher z-transformation of the correlation coefficient.
  # The following lines of R code show how to obtain a bootstrap confidence interval
  # for the z-transformed correlation between chest and belly in the possum data frame.
  # The last step of the procedure is to apply the inverse of the z-transformation to the
  # confidence interval to return it to the original scale. Run the following code and
  # compare the resulting interval with the one computed without transformation. Is the
  # z-transformation necessary here?
  
  z.transform <- function(r) .5 * log((1 + r)/(1 - r))
  z.inverse <- function(z) (exp(2 * z) -1)/(exp(2 * z) + 1)
  possum.fun <- function(data, indices) {
    chest <- data$chest[indices]
    belly <- data$belly[indices]
    z.transform(cor(belly, chest))}
  possum.boot <- boot(possum, possum.fun, R = 1000)
  z.inverse(boot.ci(possum.boot, type = "perc")$percent[4:5])
  # See help(bootci.object)
  
# *********************************************************************************************
# Without Fisher z-transformation, the bootstrap confidence interval is
  possum.fun0 <- function(data, indices) {
    chest <- data$chest[indices]
    belly <- data$belly[indices]
    cor(belly, chest)}
  possum.boot0 <- boot(possum, possum.fun0, R = 1000)
  boot.ci(possum.boot0, type = "perc")$percent[4:5]
"
  There is no significant difference in the bootsrap confidence interval for the
  correlation coefficient between with transformation and without transformation.
"
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  