#Lab 07: the bootstrapping

# the apply() attempts to replace the need for for-loop

#apply() works on data frames or other 2D arrays of data such as matrices
#applies a function to either the rows or columns of the input data

#important arguments
#x - data frame or matrix
#margin - whether to apply the function to rows (margin = 1) or columns (margin = 2)
#FUN- func to apply to the rows or columns

#basic examples: Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)
#min and max values in each row
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
#note used margin = 1 to indicate rows

#mean values in each column
apply(dat, MARGIN = 2, FUN = mean)
#note used margin = 2 to indicate columns and FUN = mean

#more advanced usage
#If the task requires several steps, and perhaps some
#intermediate variables a for-loop is often better.

#Data Files
#dataset containing sd abund est of 10 rare moth species 
#across 24 plots in the pine barrens of southeast MA
require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)


#bootstrap - resampling technique with replacment (getting something from nothing)
#one of the most important uses of BS is calculating non-parametric CIs
#simulates Frequenist ideal of obtaining estimates from repeated similar experiments

#CLT - upon repeated sampling, sample mean will be approx norm dist
#t-distr is a standard normal dist that has been adjusted for sample size
# t dist has heavy tails compared to norm (represent increased uncertainty)
#t dist described with single parameter - degrees of freedom (sample size minus 1)

#safer to use t-dist over standard normal dis for CI calucations
#if your sample has 30 or fewer individuals you should use a t-distribution

#Calculating the Parametric CI
# Choose significance level (usually 95%, value of alpha is just 1 - SL)
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(moths$anst))
sse = sd(moths$anst, na.rm = TRUE) / sqrt(n)

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))

# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
anst_ci = c(
  lower = mean(moths$anst) - ci_radius,
  upper = mean(moths$anst) + ci_radius)

print(round(anst_ci, 4))

#standard way to construct CIs for the mean and test 
#the hypothesis that the mean differs from zero

#if the CI does not contain 0, then the mean is unlikely
# to have come from a dist with a mean of 0
#in other words, pop mean is likely to be diff than zero


#Bootstrap Interval Using boot()

install.packages("boot")
require(boot)
#boot(data, statistic, R)
#R is the  number of resamplings you want to 
#data is the data object you want to resample (vector, matrix, data frame)

#statistics is a function that returns the stats of interest

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
#modified version above is needed 
myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#original is the original mean of the whole sample: mean (moths$anst)
# bias = diff between original mean and mean of BS
# std.error = sd of the simulated values

str(myboot)
mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)


#extract our bootstrap CI as follows:
quantile(
  myboot$t,
  c(0.025, 0.975))


#Setting up the Bootstrap
#first remove the first column which represents an arbitrary site id
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#NOTE: each row corresponds to a single simulation (the outer loop)
#each col represents a different level of sampling effort (inner loop)
#values in each cell are the number of species observed

#Packaging your code into a function
#First Draft: copy all codee inside the func and propose argument names 
#based on function inputs 
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#Second Draft; check that body of the function does not make ref to any varaibles that are not defined
#m was defined already outside the function so replace with n_iterations

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n_iterations)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_iterations, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#Check in a fresh environment

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
require(here)
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

# added " n = n_input_rows in the function

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = n_input_rows
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)


moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 1000)
head(rarefact)



rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

?matplot

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main="Creating a Rarefaction Curve for Moths")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))











#lab questions 1-5
require(palmerpenguins)
dat_Gentoo = subset(penguins, species == "Gentoo")
n = sum(!is.na(dat_Gentoo$bill_length_mm))
n

sd(dat_Gentoo$bill_length_mm, na.rm = TRUE)

sse = sd(dat_Gentoo$bill_length_mm, na.rm = TRUE)/sqrt(n)
t_crit = abs(qt(alpha / 2, df = n - 1))
ci_radius = sse * t_crit
mean(na.omit(dat_Gentoo$bill_length_mm))
lower = mean(na.omit(dat_Gentoo$bill_length_mm)) - ci_radius
upper = mean(na.omit(dat_Gentoo$bill_length_mm)) + ci_radius
GentooBL_ci = c(lower - ci_radius, upper + ci_radius)
GentooBL_ci

#questions 6-8
install.packages("boot")
require(boot)
#boot(data, statistic, R)

Gentoo_boot = 
  boot(
    data = dat_Gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(Gentoo_boot)
str(Gentoo_boot)
mean(Gentoo_boot)
Gentoo_boot$t0
mean(Gentoo_boot$t) - Gentoo_boot$t0
sd(Gentoo_boot$t)
quantile(Gentoo_boot$t, c(0.025, 0.975))

#Questions 9-13

rm(list = ls())

# Re-read my data:
require(here)
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

# added " n = n_input_rows in the function

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = n_input_rows
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {

    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      
      
      t1 = input_dat[rows_j, ]
      
     
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
?adjustcolor()

moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))
head(rare)
matplot(
  rare,
  type='l',
  xlab='Number of Sampling Plots',
  ylab='Species Richness',
  main="Species-Sampling Intensity for Moths")
polygon(x = c(1:24,24:1),y = c(rare[,2],rev(rare[,3])), 
        col= adjustcolor("dodgerblue", alpha.f = 0.10),
                         border= NA)
legend(
  'bottomright',
  legend=c('Mean','%95 CI Lower','%95 CI Upper'),
  lty=c(1,2,3),lwd=c(2,2,2), col=c(1,2,3), inset=c(0.1,0.25),
  text.col = c(1,2,3))
?legend
