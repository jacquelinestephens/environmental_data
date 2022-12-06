dpois(x=7,lambda = 10.4)
#what is the prob I observe a count of exactly 7 if I have
#a poisson-distributed pop with lambda = 10.4
dpois(x=8,lambda = 10.4)


#binomial dis, two parameters : n= number of trials (size)
# and p: prob of succes on each trial (prob)

?dbinom()
#dbinom(x, size, prob, log = FALSE)
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
#rbinom(n, size, prob)

#Using dnorm()

#ask whether you are more likely to observe a value of 0.5 or 1.0
dnorm(0.5, mean = 0, sd = 1)
dnorm(1, mean = 0, sd = 1)
# which was more likely? 0.5 observed was more likely

# using pnorm() calculates the cumulative dens

#observe value of 0.5 or less 
pnorm(0.5, mean = 0, sd = 1)
pnorm(1, mean = 0, sd = 1)
#prob 1 is greater

#Probability Density Plot
# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -4, to = 4, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")
#try a higher number of points
# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l", ylab = "Probability Density")


y_2 = dnorm(x, mean = 0, sd = 2)
y_3 = dnorm(x, mean = -2, sd = 1)

plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)
points(y_3~x, type = "l", lty = 4)



#Cumulative Density Plot
y_cdf_1 = pnorm(x, mean = 0, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density")

#add the CDF curve for the norm dis with sd = 2

y_cdf_2 = pnorm(x, mean = 0, sd = 2)
y_cdf_3 = pnorm(x, mean = -2, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "Cumulative Density")
points(y_cdf_2 ~ x, type = "l", lty = 2)
points(y_cdf_3 ~ x, type = "l", lty = 4)




#Binomial Mass Plot
#height of the curve for a PDF (continuous dis) gives a relative measure of likelihood
#height of the curve for a PMF (discrete dis) gives the true probability.

x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.4)

barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")

n = 6 
p = 4/6 

dbinom(4, size= n ,pro = p) 










n = 6 

p= 4/6 

pbinom(q = 4, size = n , p= p) 

oneorless = pnorm(1, mean =0, sd =1)
twoorless = pnorm(2, mean =0, sd =1)
twoorless - oneorless


#question 9 and 10


png(
  filename = here("inclass3_PDF_CDF.png"), 
  width = 2000, 
  height = 1600, 
  res = 180, 
  units = "px" )

par(mfrow = c(1, 2))

x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)


y_2 = dnorm(x, mean = 0, sd = 2)
y_3 = dnorm(x, mean = -2, sd = 1)

plot(y ~ x, main= "Vamps PDF plot", type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)
points(y_3~x, type = "l", lty = 4)

y_cdf_1 = pnorm(x, mean = 0, sd = 1)

y_cdf_2 = pnorm(x, mean = 0, sd = 2)
y_cdf_3 = pnorm(x, mean = -2, sd = 1)
plot(y_cdf_1 ~ x, main = "vamps CDF plot", type = "l", ylab = "Cumulative Density")
points(y_cdf_2 ~ x, type = "l", lty = 2)
points(y_cdf_3 ~ x, type = "l", lty = 4)

dev.off()




#3 letter words
cat(letters[sample(26, size = 3, replace = T)], sep = "")
W = 26^3
W


cat(letters[sample(26, size = 4, replace = T)], sep = "")
26^4
