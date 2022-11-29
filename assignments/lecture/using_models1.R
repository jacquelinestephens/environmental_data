#Using Models 1 Lecture Assignment

#Marbled Salamander observed reproductive success in 13 vernal pools over a epriod of 7 years
require(here)
catrate = read.csv(here("data","catrate.csv"))
head(catrate)
#use the summary function to view numerical summaries of all the columns
summary(catrate)
hist(catrate$cat.rate, main = "Histogram of Catastrophe Rates",
     xlab = "Catastrophe")
#Checck for normality: Shapiro-Wilk test
shapiro.test(catrate$cat.rate)
#the null hypothesis for the SW test is that the data were sampled from a normally distributed population

#recall that low p-values suggest that there is good evidence against the null hypothesis

#Other Tests for normality: package nortest contains many other functions 
#including ad.test(), lillie.test() etc

#One-sample Tests: tests for difference from expectation
#one of the simplest one sample tests is whether the observed mean (or meadian)
#is significantly different from a specified value 
#default null is that the mean is equal to zero

#observed late-filling rate is 2/7 = 0.28 and the obs mean catastrophe rate is 0.54
#to test the alt hyp that the obs mean cat.rate is sign diff from an expected value of 0.28

#Parametric One-Sample test: the Student's t-test
#recall that for a one-sampled test, null is "the mean of pop from which the data 
#were collected is not different from x"

#t.test() arguments you need to use for this test are x and mu
#mu is the null hyp you want to test against

t.test(x = catrate$cat.rate, mu = 2/7)
# the mu should be 2/7 because it is the observed late filling rate
#this student t-test says that alt hyp is that the obs mean is not = to the expected mean,
#which can mean less than or greater than the expected mean

#one-sided alternative hypothesis
t.test(x = catrate$cat.rate, mu = 2/7, alternative = "greater")
t.test(x = catrate$cat.rate, mu = 2/7, alternative = "less")

#non-parametric one sample test: the wilcoxon rank sum test
#non-normal distribution, use the Mann Whitney test= wilcoxons signed rank test 
wilcox.test(catrate$cat.rate, mu = 2/7) 
#the syntax for conducting a 1 tailed test is exactly the same

#comparing two sample means
# in the one-sample tests, we compared a sample mean with a fixed value that we specified
#if you need to compare the means of two groups of observations, use two sample test

#null hyp = there is no difference in mean values between the two groups

require(palmerpenguins)
## Loading required package: palmerpenguins
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")
#testing for normality using the shapiro test to determine flipper length norm dist
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

#Parametric and nonparametric tests
t.test(flipper_length_mm ~ species, 
       data = penguin_dat)
summary(penguin_dat$species)
wilcox.test(flipper_length_mm ~ species, 
            data = penguin_dat)
levels(penguin_dat$species)
_______________________________________________________________________

#Question 1
hist( catrate$cat.rate , main = "Reproduction Catastrophic Rates \n of Marbled Salamanders",
     xlab = "Reproductive Catastrophic Rate")
shapiro.test(catrate$cat.rate)

#Question 18

png(
  filename = here("using_models1_Q18.png"), 
  width = 1600, 
  height = 800,
  res = 180, 
  units = "px" )
par(mfrow = c(1, 2))
hist(dat_adelie$flipper_length_mm,
  xlab = "Flipper length (mm)",
  main = "Adelie")
hist(dat_chinstrap$flipper_length_mm,
  xlab = "Flipper length (mm)", 
  main = "Chinstrap")
dev.off()
