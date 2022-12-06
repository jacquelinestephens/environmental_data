#Lecture assignment- using models 2
#recap of t-tests: T-tests are univariate tests that can determine
#whether there is good evidence that:
#The mean of one sample is different from a fixed value.
#The means of two samples are different from each other.

#1-sample t-test
require(here)
penguins = require(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm) #compare avg flipper length to zero
#the null hypothesis is that there is that the avg flipper length is equal to zero 
#this is not a sensible null hypothesis because it means all the lengths would be zero

#instead of comparing to zero, test wheter they are equal too 218mm
t.test(x = subset(penguins, species == "Gentoo")$flipper_length_mm, mu = 218)
#the null hypothesis this time was that avg flipper length is 218mm
#the pvalue of 0.1669 tells you that the avg is not sign different than 218

#one-tailed alt hyp: Gentoo penguins flippers are smaller than 218mm
t.test(x = subset(penguins, species == "Gentoo")$flipper_length_mm, 
       mu = 218, alternative = "less")
#evidence is stronger but not completely supported


#two sampled t-test: instead of comparing flipper length of gentoo penguins to  a fixed value
#compare the flipper length of two species
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

#t-tests are useful when we have categorical predictor with two levels and a contin response variabe
#Parametric Two-Sample Test
#alternative hypothesis that A.penguins have shorter flippers than Gentoo
dat_pen = droplevels(subset(penguins, species != "Chinstrap"))
t.test(flipper_length_mm ~ species, 
       data = dat_pen, alternative = "less")
#take note of the CI and p value and group means

#1-Analysis of variance (ANOVA)
#Model 1: Body mass explained by species
#response variable: body mass, continous, ratio
#predictor: species, categorical variable, nominal

#to perform an ANOVA in R
#perform a graphical and numerical data exploration
#fit a linear model using lm()
#examine the model coefficient table using summary()
#conduct the ANOVA using anova()

#Data exploration
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

par(mfrow = c(1, 1))
boxplot(body_mass_g ~ species, data = penguins)

#Numerical exploration: for the assumption of normality
#test whether within group data are normally distributed 
#extract measurements for each species, caluclate mean for species, conduct shapiro 
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)
#shapiro null: data are drawn from normally dist pop
#shortcut for calucating species mean body mass using aggregate()
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

#adelie appears to be the only species not normally distributed

#Fit a linear model
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

#Conduct the anova
anova(fit_species)
#simple ANOVA belongs to the Group 1 analyses: its a linear model

#Two tables: model coefficients and ANOVA
#Model coefficients table: base case is the intercept, slope coefficients are the adjustments 
#the pvalues in each row tell us if the coefficient in the row is different from zero
#note the pvalues do no tell us whether any of the coefficients are sign diff from each other

#ANOVA table: predictor variable p-values

#Degrees of freedom: represent the number of levels within a categorical variable


#sum of squares: how much of the total data variability is explained by each of the predictor variables
#in our case we only have one predictor variable: species
#resids sum of squares:info about the variation that our model couldn't explain
#total sum of squares: measure of the total variability in the data

#mean squares: variability explained by each factor, allows us to compare the relatire amount of info that each factor explains

#F-statistic: how much adding a variable to the model improves the model fit
#null: adding pedictor x to the model does no improve the model fit"

#difference between ANOVA and model coefficients tables:
#Model coefficient showed three species coefficients but anova combins all species into a single row
#categorical varaibles are hanlded slightly differently in the model coefficient and ANOVA


#Two way additive ANOVA
boxplot(body_mass_g ~ sex*species, data = penguins)


#fit a 2-way additive model
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

summary(fit_interactive)#model coefficient

anova(fit_interactive)

#Simple linear regression: penguin bills and body mass
lm(bill_length_mm ~ body_mass_g, data = penguins)


################################################################################

#Question 1
#droplevels() to remove unused factor levels from a dataframe
labels = c("female \n Adelie", "male \n Adelie", "female \n Chinstrap",
           "male \n Chinsstrap", "female \n Gentoo", "male \n Gentoo")
boxplot(body_mass_g ~ sex*species, data = penguins,
        ylab = "Body mass (grams)",
        xlab = "",
        names = labels,
        las = 2)

#Question 4

fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

#Question 8
dat_chinstrap = subset(penguins, species == "Chinstrap")
dat_chinstrap_fem = subset(dat_chinstrap, sex == "female")
mean(dat_chinstrap_fem$body_mass_g, na.rm = TRUE)
