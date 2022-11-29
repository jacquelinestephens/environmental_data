# Lab 4: Uncertainty and Error
# Eachc common distribution has a set of 4 R functions:
#dnorm(): the probability density
#pnorm(): the cumulative probability density
#qnorm(): the quantile function
#rnorm(): function to generate random, normally-distributed numbers.

#Plotting a PDF curve

# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0) #horizontal line at y=0

#The random deviates functions start with the letter r, for example:

#rnorm() - normal distribution (mean = 0, sd = 1)
#rbinom() - binomial distribution
#rpois() - poisson distribution

#penguin example
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

#To generate random numbers from a Normal distribution
# you need to know three things:
# of observations to create, the mean, the SD

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)
#there are 344 obs with a mean of 4202 grams and SD of 802 grams
#what did the na.rm=TRUE argument do in mean() and sd()?
mean(penguins$body_mass_g, na.rm = FALSE)
sd(penguins$body_mass_g, na.rm = FALSE)
nrow(penguins)
# the result is NA
#TRUE or FALSE indicating whether NA values should be stripped
#before the computation proceeds
?mean

#Random Penguin Masses
#generate four vectors of random penguin body masses using 
#the quantities I calculated from the observed values

n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)

#now plot the histograms of the simulateed data

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

#Random Uniform Numbers
#runif() function will generate random, uniformly-distributed #

dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

#How could you modify the sampling procedure to get more 
#representative sample?
#Small samples are subject to sampling error
#i.e. non-representative samples
#if you randomly gen more numbers, hist look more uniform

dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

#Randomness and Replication: set.seed()
#random number generators (RNGs)

#here is what happens if set random seed before generating
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)
#the two data sets contain the same numbers

#when you use set.seed() you are specifying a starting position
#in the sequence of random numbers that R will generate
#if you dont set an explicit seed, you're very unlikely
#to get the same sequence twice

#Measuring Error
#what is a residual? the difference between a predicted valuee
#and the observed value

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)


set.seed(110)
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)





#Question 1: create four sections of 

Q1_mean = 10.4
Q1_sd = 2.4

norm_17 = rnorm(n = 17, mean = Q1_mean, sd = Q1_sd)
norm_30 = rnorm(n = 30,  mean = Q1_mean, sd = Q1_sd)
norm_300 = rnorm(n = 300,  mean = Q1_mean, sd = Q1_sd)
norm_3000 = rnorm(n = 3000,  mean = Q1_mean, sd = Q1_sd)


?png()

require(here)

png(
  filename = here("lab_04_hist_01.png"), 
  width = 1500, 
  height = 1600, 
  res = 180, 
  units = "px" )

par(mfrow = c(2, 2))

hist(norm_17, main = "17 Random #s")
hist(norm_30, main = "30 Random #s")
hist(norm_300, main = "300 Random #s")
hist(norm_3000, main = "3000 Random #s")

dev.off()


#Question 7
#created plot of density function of a standard normal distribution
x = seq(-6, 6, length.out = 1000)
y = dnorm(x)

plot(x, y,
     main = "Standard Normal PDF", 
     type = "l", xlim = c(-3, 3))
abline(h = 0)
#what happens when you change xlim?
plot(x, y,
     main = "Standard Normal PDF", 
     type = "l", xlim = c(-10, 10))

Q1_mean = 10.4
Q1_sd = 2.4
nrow(x)

x = seq(-20,20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
plot(x, y,
     main = "Normal PDF: mean = 10.4 sd = 2.4",
     type= "l", xlim = c(2, 20))
abline(h = 0)

?svg()

svg(filename = "norm_1.svg", width = 7, height = 7)
x = seq(-20,20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
plot(x, y,
     main = "Normal PDF: mean = 10.4 sd = 2.4",
     type= "l", xlim = c(2, 20))
abline(h = 0)


dev.off()

#Question 9-10


n_pts = 50
x_min = 1
x_max = 20

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts, min = 0, max = 20)

dat_random = data.frame(x = x_random, y = y_random)

set.seed(1)
dat_unif_1 = runif(n = 150, min = 0, max = 10)
set.seed(1)
random = runif(n = 50, min = 0, max = 8)

svg(filename = "4random_plots.svg", width = 7, height = 7)
par(mfrow = c(2, 2))
plot(y ~ x, data = dat_random, pch = 10, col = "red", main = "Scatterplot Red")
boxplot(x_random, y_random, col = "blue", main = "Boxplot")
hist(dat_unif_1, col = "yellow")
plot(random, col = "green", pch = 4, main = "Scatterplot Green")
dev.off()


#Question 11-12
svg(filename = "random_GreenSP.svg", width = 7, height = 7)

par(mfrow = c(1,1))
set.seed(1)
random = runif(n = 50, min = 0, max = 8)


guess_x = 0
guess_y = 0
guess_slope = 0.15

plot(random, col = "green", pch = 4, main = "Scatterplot Green")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dev.off()

#Question 13-14
set.seed(1)
random = runif(n = 50, min = 0, max = 8)


guess_x = 0
guess_y = 0
guess_slope = 0.15

plot(random, col = "green", pch = 4, main = "Scatterplot Green")

y_predicted = line_point_slope(random, guess_x, guess_y, guess_slope)

random_new = cbind(random,y_predicted)
View(random_new)
resids = cbind(random - y_predicted)
random_final = cbind(random,y_predicted, resids)
View(random_final)
colnames(random_final) = c("Random", "Predicted", "Resids")

sum(random_final[resids])
sum(abs(random_final[resids]))

hist(random[resids], main = "Histogram of Residual Values",
     xlab = "Residuals")
plot(y_predicted,resids, 
     main = "Relationship between Predicted and Residuals",
     xlab = "Predicted", ylab = "Residual")
