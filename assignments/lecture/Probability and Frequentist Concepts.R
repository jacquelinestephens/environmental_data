#probability and frequentist concepts: Seeing Theory


#FOR DISCRETE PARAMETRIC DISTRIBUTION
#dbinom: the probability mass function, exact value
#pbinom: the cumulative mass function, value or x or less or more
#qbinom: the quantile function, median or 50th percentile, or 90th

#Remember that discrete distributions use the term mass rather than density.
#each distribution has different parameters

#FOR CONTINUOUS PARAMETRIC DISTRIBUTION
#dnorm: the probability density function, a value of x or y more likely
#pnorm: the cumulative density function, value between
#qnorm: the quantile function, median or 50th percentile, or 90th

#Remember that continuous distributions use the term density rather than mass.



#The law of total probability 
#the sum of all events in the sample space is 1.0

#What is the probability of observing a value less than 
#7.5 in a normal distribution with mean 10 and standard deviation 3?

pnorm(7.5, mean = 10, sd = 3)

#sampling distribution is the distribution of a sample statistic
#Question 1
dbinom(3, 4, 0.75)
?dbinom()

#Question 2

pbinom(3, 4, 0.75)

#Question 3

1 - pbinom(3, 5, 0.75)

#Question 4
dnorm(1.2, mean = 2, sd= 2)

#Question 5

1 - dnorm(1.2, mean = 2, sd= 2)

#Question 6
pnorm(3.2, mean = 2, sd = 2) - pnorm(1.2,mean = 2, sd= 2)


#Question 12 and 13
25 ^3

B = 25^(410*40*80)
(410*40*81) - (410*40*80)
B* B^16400
