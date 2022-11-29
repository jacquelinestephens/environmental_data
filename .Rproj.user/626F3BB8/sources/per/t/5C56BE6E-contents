#lab 6 Introduction to Inference
#Every sample statistic (mean, standard deviation, etc) has a formula.

#sample standard error of the mean = sample standard deviation / by the square root of the sample size

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)

#The penguin data

boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")

#They look different,
#but how could we back up our visual assessment?
#answer: simple Analysis of Variance (ANOVA) to perform a parametric Frequentist analysis
#also do a resampling simulation to estimate how likely the observed differences 
#in flipper length would be if there truly was no difference.

#2-speccies data (one species removed)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

#droplevels() to remove unused factor levels from a dataframe

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)")
}

#Resampling (not parametric in sense of linear regress or t-test)
#Resampling with replacement (shuffle flipper length)
# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

#Flipper length: original and resampled data
#monte Carlo Resampling and Null Hypothesis
#flipper length was shuffled separately from species labels above
#breaks the structure = destroys any association between FL and SL

#there is no differencein flipper length between the species
#Monte Carlo resampling = simulate what would happen if the null hypothesis
#were true = simulate a null distribution


#Bootstrap Resampling and Alternative Hypotheses
#primary difference between bootstrap and MC is that bootstrapping does 
#not destroy the associations in the data (it samples entire rows of your data)
#it shuffles rows (with replacemnt), but does not shuffle the values in columns

penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}

#In contrast to MC resampling
#bootstrap resampling simulates an alternative hypothesis.

#RepeatedMC Resampling
#What if we repeated the MC process many times?
par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

#T-tests - A frequentist approach (Classical t-test: penguins)

#two-sample t-test to compare the mean values of two groups
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
#there is good evidence that FL is different between the two species

#two sample resampling with replacement
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

#difference seems to disappear visually

#Classical test on resampled data: what if we reshuffled the data and re-ran the t test?
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#does not support rejecting a null hypothesis that the two flipper lengths 
#are the same between the two species.
#Difference of means
t_test= t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
#gives you the different between the two means


#Using aggregate() to calculate the difference in means 
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#Sample Sizes
#use table() ti show the number of individuals of each species
table(dat_pen$species)

#Resampling with replacement is the same thing as randomly sampling 
#68 flipper lengths in one group and 152 in another.
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
print(c(observed = diff_observed, simulated = diff_simulated))

#Simulation function 

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

x= penguins$body_mass_g

is.na(x)


two_group_resample_diff = function(x, n_1, n_2) 
{
  x = x[!is.na(x)]
  resamp_1 = sample(x, n_1, replace = TRUE)
  resamp_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = 
    mean(resamp_1, na.rm = TRUE) - mean(resamp_2, na.rm = TRUE)
  
  return(difference_in_means)
}

set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)

?mean()

#Resampling experiment: if function ran many times, how often would 
#you see a mean difference greater than diff_observed: try it 200 times


#Question 4-5

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

require("here")
png(filename = here("lab6_histogram.png"), width = 800, height = 600)
hist(mean_differences)
dev.off


sum(abs(mean_differences) >= diff_observed)

sum(abs(mean_differences) >= 5.8)




sum(!is.na(penguins$bill_depth_mm))
#this is the sum of all FALSE values aka numeric values instead of NA
# without the ! it would give you all the TRUE values aka the NA valules present

sse_mean= function(x) 
{ 
  n = sum(!is.na(x))
  SSE= (sd(x, na.rm = TRUE)) / sqrt(n)
  return(SSE)
}
require(palmerpenguins, mtcars)
sse_mean(penguins$bill_depth_mm)

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)


#question 1

sse_mean= function(x) 
{ 
  n = sum(!is.na(x))
  SSE= (sd(x, na.rm = TRUE)) / sqrt(n)
  return(SSE)
}
require(palmerpenguins, mtcars)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Question 2

two_group_resample_diff = function(x, n_1, n_2) 
{
  x = x[!is.na(x)]
  resamp_1 = sample(x, n_1, replace = TRUE)
  resamp_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = 
    mean(resamp_1, na.rm = TRUE) - mean(resamp_2, na.rm = TRUE)
  
  return(difference_in_means)
}

#Question 7

boxplot(body_mass_g~ species, data = dat_pen, 
        main = "Body Mass by Species" , 
        ylab = "Body mass (g)", xlab = "Species")

#question 8
agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

#Question 9
t_test = t.test(dat_pen$body_mass_g ~ dat_pen$species)
t_test$estimate


table(dat_pen$species)
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$body_mass_g, n_1, replace = TRUE)
dat_2 = sample(dat_pen$body_mass_g, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

set.seed(54321)
two_group_resample_diff(dat_pen$body_mass_g, 68,152)

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$body_mass_g, 68, 152)
  )
}
hist(mean_differences, main = "Resampled Mean Differences",
     xlab = "Mean Differences")

sum(abs(mean_differences) >= diff_observed)

