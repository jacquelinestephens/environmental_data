#Inclass: Populations, samples, and sampling
#Populations - infinitely large and unknowable and follows a parametric distribtuion

#Assumptions of the binomial
#A set of n trials with a binary outcome.
#The trials are independent.
#Each trial has the same probability of success.
#binomial dist has 2 parameters: 
#n (number of trials) and p (probability of success).

x = 0:20

barplot(
  dbinom(x, size = 20, prob = 0.1),
  names.arg = x, space = 0,
  main = "Binomial PMF: n = 20, p = 0.1",
  ylab = "Pr(x)", xlab = "x = n successes")
#rightly skewed and discrete

#Create a "Population" - make a pop of 1 million binom-dist numbers
set.seed(12345)
sim_population = rbinom(n = 1000000, size = 20, prob = 0.1)

hist(sim_population, main = "random dist pop", xlab = "x", ylab = "Density")
max(sim_population)
#max value is 12 which means even tho it goes from 0 to 20, prob of observing counts over 12 is low

#Samples
#as sample size grows, sampling error shrinks
#sampling 20 observations with replacement

set.seed(5431213)
sim_sample = sample(sim_population, size = 20, replace = T)
hist(sim_sample + 0.00001, main = "sample size = 20", xlab = "x")
#it would be hard to tell that that sample came from a binom dist pop

#trying different sample sizes

set.seed(5431213)
sim_sample_40 = sample(sim_population, size = 40, replace = T)
hist(sim_sample + 0.00001, main = "sample size = 40", xlab = "x")

set.seed(5431213)
sim_sample_200 = sample(sim_population, size = 200, replace = T)
hist(sim_sample + 0.00001, main = "sample size = 200", xlab = "x")

#as you increase sample size, normalize
#sample size has to be low to represent parent dist

sd(sim_sample)
mean(sim_sample)


#Sampling Distrubtion
mean_sampler = function(pop, sample_size, n_means)
{
  # pre-allocate a results vector
  means = vector(mode = "numeric", length = n_means)
  
  # sampling loop
  for (i in 1:n_means)
  {
    samp = sample(pop, size = sample_size, replace = TRUE)
    means[i] = mean(samp)
  }
  
  return(means)
}
#example app with a sample size 30 and 200 iterations

sample_means = mean_sampler(
  pop = sim_population, 
  sample_size = 30,
  n_means = 200)

hist(
  sample_means,
  main = "Distribution of Sample Means\nsample size: 30, number of means: 200",
  xlab = "sample mean")

#with 1000 iterations

sample_means_1000it = mean_sampler(
  pop = sim_population, 
  sample_size = 30,
  n_means = 1000)

hist(
  sample_means_1000it,
  main = "Distribution of Sample Means\nsample size: 30, number of means: 1000",
  xlab = "sample mean")


# use the function rexp() to make an expon dist pop

