#inclass activity likelihoods

install.packages('here')
require(here)


#use read.csv() with here() to read the data files into data.frame objects

dat_birds = read.csv(here("data","bird.sta.csv"))
dat_habitat = read.csv(here("data","hab.sta.csv"))

#likelihood is a measure of the relative prob of observing a set of one or more obs given a proposed model and parameter values
#likelihood of a single obs is proportional to the value of the prob density/mass function


#maximum likelihood is a criterion for selecting a model

#likelihood of two observations
#counts of Wilson's Warblers at study site in Oregon (2 sites counted 2 and 6 birds)
x_observed = c(2, 6)
print(x_observed)

# use the poisson dist which has a sinlge parameter lambda
#the mean and sd of a poisson dist are equal to lambda

#I think the count of 2 is unusually low, so I decide to propose a Poisson dist
#with λ=4.5 as a model of the counts of Wilson’s Warblers on my study sites.

#use the dpois() to calc the prob mass for the two counts given pois dist with lambda = 4.5
dpois(x = 2, lambda = 4.5)

dpois(x=6, lambda = 4.5)

#likelihood of obs those particular counts together is the product of the individual likelihoods

dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)
#store the counts in a single vector

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

# more easily calc the product and sum of log-likelihoods
prod(dpois(x = wiwa_counts, lambda = 4.5))
sum(log(dpois(x= wiwa_counts, lambda = 4.5)))

sum(log(dpois(x = wiwa_counts, lambda = 4.2)))

#likelihood of many observations
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

#numerical data exploration
#start with a 5-number summary, then plot a histogram
summary(dat_all$WIWA)
hist(dat_all$WIWA)
#set breaks arg to 7 to suggest that R should create 7 bins

hist(dat_all$WIWA, breaks = 7)

#NOTE: histogram includes both the obs of zero and one in the first bin
#we requested 7 bins and only got 6!

#to ungroup thezeros and ones, use a vector 0:7

hist(dat_all$WIWA, breaks = 0:7)
#still not exactly what we want, for the first bin R includes both endpoints in its counts
#counting all of the 0- and 1- observations together

#histogram with custom breaks attempt 3: success!
0:7 -0.5
#if we write code that subtracts a single value from a vector
#R will subtract the number from each element in the vector
hist(dat_all$WIWA, breaks = 0:7 - .5)
#this works because the data were discrete counts 

#Histogram with discrete count data
#if we dont know the maximum value ahead of time
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))


#questions 1-2
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

#questions 3-5

summary(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 0:(max(dat_all$WIWR) + 1) - 0.5,
     main = "Histogram for Winter Wrens")

dpois(x = dat_all$WIWR, lambda = 1.456)
sum(log(dpois(x = dat_all$WIWR, lambda = 1.456)))

#Questions 6-9
#dbinom(x,size,prob) - binomial distribution
#looking for a prob that gives the least negative number
#started at 0.5 
dbinom(dat_all$WIWR, 6,0.24)
sum(log(dbinom(x = dat_all$WIWR, size = max(dat_all$WIWR), prob = 0.24))) 
sum(log(dbinom(dat_all$WIWR, 6,0.24)))

?dpois


#questions 12-14
set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)

sum(log(dnorm(x= vec_rnorm, mean = 0, sd = 1)))

summary(vec_rnorm)    
mean(vec_rnorm)
sd(vec_rnorm)
