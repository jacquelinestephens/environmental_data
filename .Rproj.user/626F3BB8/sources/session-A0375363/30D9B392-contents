qnorm(c(0.05, 0.95))
?qt()

alpha = 0.05
t_lower = qt(alpha/2, df= 10)
t_upper = qt(1-(alpha/2), df = 10)

alpha = 0.05
t_lower = qt(alpha/2, df= 350)
t_upper = qt(1-(alpha/2), df = 350)


n = 50
sd = 3.14
mean = 10

alpha =0.05
z_lower = qnorm(alpha/2, mean = 10, sd = 3.14)
z_upper = qnorm(1-(alpha/2), mean = 10, sd = 3.14)


ex_sample_se = 3.14 / sqrt(50)
ex_sample_se


#Question 5
n = 50
sd = 3.14
mean = 10

alpha = 0.05
crit_lower = qt(alpha/2, df= n-1)
crit_upper = qt(1-(alpha/2), df = n-1)

sse = sd / sqrt(n)

CIrad = sse * t_upper

upper_CI = mean + CIrad
lower_CI = mean - CIrad
