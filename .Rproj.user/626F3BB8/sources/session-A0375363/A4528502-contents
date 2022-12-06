# Lab 8

require(here)
dat_bird = read.csv(here("data","bird.sub.csv"))
dat_habitat = read.csv(here("data","hab.sub.csv"))
veg_dat = read.csv(here("data","vegdata.csv"))

#Bootstrap: Exploring the Alternative Hypothesis
#t-tests are useful when we have categorical predictor with two levels and a contin response variabe

require(palmerpenguins)

#droplevels() to remove unused factor levels from a dataframe
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

#Parametric Two-Sample Test
#alternative hypothesis that A.penguins have shorter flippers than Chinstrap

t.test(flipper_length_mm ~ species, 
       data = dat_pen, alternative = "less")
#take note of the CI and p value and group means

#Bootstrap Two-Sample Test using two.boot()
install.packages("simpleboot")
require(simpleboot)

adelie_FL = subset(dat_pen, species == "Adelie" )
chinstrap_FL = subset(dat_pen, species == "Chinstrap" )


pen_boot = two.boot(adelie_FL$flipper_length_mm,
                    chinstrap_FL$flipper_length_mm,
                    FUN = mean,
                    R = 10000)
str(pen_boot)
BS_diff = pen_boot$t
hist(BS_diff, main = "Histogram of 10000 bootstrap differences in mean penguin FL",
     xlab = "Difference in mean FL (mm)")

#Tree Data
#assess tree seedling response to understory veg treatments
#four treatments including a control were randomly assigned to 32 plots

boxplot(pine ~ treatment, dat = veg_dat)

#create a new data frame that contains only the obs that were "clipped or control"
dat_tree = droplevels(subset(veg_dat, treatment %in% c("control", "clipped")))

boxplot(pine ~ treatment, data = dat_tree)
View(dat_tree)
table(dat_tree$treatment)
?table




#Nonoparametric two-sample test: Wilcoxon ranked sum test
wilcox.test(flipper_length_mm ~ species, 
            data = dat_pen, alternative = "less")
# p-value = 1.15 e-08

#Bootstrap


#Bird Data: standardization of the Oregon bird data
#Z-standardization is a form of normalization.
#There are habitat variables for 30 subbasins.
dat_all = merge( dat_bird,dat_habitat, by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
dat_all
#note that the b.sidi column values are about a 1/10 of the other column values
#process for z-standardization
#1. calculate the sample mean and sd
#2. subtract the mean from each value
#3. divide each value by the sample standard deviation

# for the b.sidi column: Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use subset-by-name ($) to create a new column of z-standardized values.
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

#examine the standardized data to see if the standardization worked:
mean(dat_all$b.sidi.standardized) 
#answer is effectively zero
sd(dat_all$b.sidi.standardized)

# for the s.sidi column: Calculate the sample mean and sd:
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use subset-by-name ($) to create a new column of z-standardized values.
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

#examine the standardized data to see if the standardization worked:
mean(dat_all$s.sidi.standardized) 
#answer is effectively zero
sd(dat_all$s.sidi.standardized)


#in this example the relational fields that link the bird and habitat data sets are
#basin (3 unique basins)
#sub (10 unique sub-basins en each basin)
#we are only going to use two of the many variables in the data
#Simpson’s diversity index for breeding birds: b.sidi
#Simpson’s diversity index for vegetation cover types: s.sidi
#s.sidi index represents the diversity in landscape composition as defined 
#largely by vegetation seral (intermediate stages of succession) stage.

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
#it appears there is a negative relationship between the bird diversity and veg


#Simple Linear Regression using a Least Squares criterion
#s.sidi is the predictor specified in the model
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

#add a regression line from a model fitted by lm() to an existing plot using abline()

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


#The slope Coefficient
#how likely it is to observe a slope coefficient this large and negative if in 
#fact there is no real relationship between bird diversity & vegetation diversity

#use the t statistic and corresponding p-value computed by the lm() function, 
#but this assumes that the errors are normally distributed about the mean, 
#which may or may not be the case here.

#construct our own null hypothesis test using a monte carlo randomization procedure
#first simplify the data set by extracting just the two variables
dat_1 = subset(dat_all,select = c(b.sidi, s.sidi))

#Null Distribution: Monte carlo Simulation breaks up the associations by sampling 
#random values from each column, instead of keeping rows intact

#To create a resampled dataset, create two vectors of rand- generated row indices.
#Then create two new vectors of bird and vegetation diversity indices

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)
#re-create the scatterplot with regression line
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#would have to do this many times to see what the range of outcomes expect to see by chance alone
#because of sampling error, a single permutation could by chance have a slope 
#that is different from nearly zero

#monte carlo randomization loop:repeat the process many times to estimate 
#the sampling distribution of the null hypothesis.
#re-allocate a vector to hold the results using numeric()

m = 10000 
result_mc = numeric(m) 


for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2])
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_mc[i] = coef(fit_resampled_i)[2]
} 


#the output of your loop is a collection of regression slope parameters that were 
#calculated on randomized and resampled data
#The Null Distribution
hist(
  result_mc,
  main = "Mike's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")

#draws a vertical line to show value of slope from the regression model on observed
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = slope_observed, lty = 2, col = "blue", lwd = 2)
#Critical Slope Value

quantile(result_mc, c(.05))

#for the simulation, 18 slopes out of 10000 generated from the null distribution
#that were equal to or less than the observed slope

#if we want an exact p-value for this lower one side tst, compute the percentage of the permuted dis

#Alternative Distribution: Bootstrapping
set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

#now build another linear model and compare the slope coefficients
#to what we calculated with the original data

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)

#coefficients of original and BS are pretty close

#now repeat the process many times

m = 10000 
result_bs = numeric(m) 


for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  
  coef(fit_bs1)
  
  result_bs[i] = coef(fit_bs1)[1]
} 



for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
 
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_1])
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[1]
  result_bs[i] = coef(fit_resampled_i)[1]
} 


for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_boot)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_bs[i] = coef(fit_resampled_i)[2]
} 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_boot)
  slope_resampled_i = coef(fit_resampled_i)
  result_bs[i] = coef(fit_resampled_i)
} 


hist(
  result_bs,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)

mean(result_bs)

#Compare Null and alternative by creating density plots
plot(
  density(result_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient")


plot(
  density(result_bs),
  main = "Mike's Alternative Distribution Density Plot",
  xlab = "Slope Coefficient")


plot(
  density(result_mc),
  main = "Jackie's Null and Alternative Distribution",
  xlab = "Slope Coefficient",
  xlim = c(-0.05,0.05),
  ylim = c(0,65),
  lwd = 2)
lines(density(result_bs), col=2, lwd = 2)
legend(
  'topright', bty = "n",
  legend=c('Null','Alt.'),
  lty=c(1,1),lwd=c(3,3), col=c(1,2), text.col = c(1,2))



#________________________________________________________________________________

#lab Questions 1-4
install.packages("simpleboot")
require(simpleboot)
?two.boot
require(palmerpenguins)

#droplevels() to remove unused factor levels from a dataframe
dat_pen = droplevels(subset(penguins, species != "Gentoo"))

adelie_FL = subset(dat_pen, species == "Adelie" )
chinstrap_FL = subset(dat_pen, species == "Chinstrap" )

#using the na.omit will remove vectors with NA values

pen_boot = two.boot(na.omit(adelie_FL$flipper_length_mm),na.omit
                    (chinstrap_FL$flipper_length_mm),
                    FUN = mean,
                    R = 10000)
str(pen_boot)
#the bootstrapped differences are stored in t
BS_diff = pen_boot$t
hist(BS_diff, main = "10000 Bootstrap Differences in Mean Penguin FL",
     xlab = "Difference in mean FL (mm)")
quantile(pen_boot$t, c(0.025, 0.975))

mean(pen_boot$t)
median(pen_boot$t)

pen_boot$t0
mean(pen_boot$t) - pen_boot$t0
sd(pen_boot$t)

#Questions 5-7
?ecdf()
#empircial cumulative distribution function using the BS differences found in t
pen_ecdf = ecdf(pen_boot$t)
plot(pen_ecdf(sort(pen_boot$t)))
1-pen_ecdf(-4.5)
1-pen_ecdf(0)
pen_ecdf(-8)

#Question 8

#Question 9
veg = data.frame(veg_dat)
colnames(veg)
#create a new data frame that contains only the obs that were "clipped or control"

CCTveg = droplevels(subset(veg_dat,treatment %in% 
                      c("control", "clipped")))
table(CCTveg$treatment)
wilcox.test(CCTveg$pine ~ CCTveg$treatment, alternative = "two.sided")

?wilcox.test
#asks if the medians are different, requires that the distributions be similar
#because the boxplot displays two very different distributions in variation so because breaking the wilicox test


#Question 10 - 11
clipped = subset(CCTveg, treatment == "clipped" )
control = subset(CCTveg, treatment == "control" )

tree_boot = two.boot(clipped$pine,control$pine,
                    FUN = mean,
                    R = 10000)
str(tree_boot)
require("boot.ci")
quantile(tree_boot$t,c(0.025, 0.975))

#Question 12-13
# Use subset-by-name ($) to create a new column of z-standardized values.
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

#examine the standardized data to see if the standardization worked:
mean(dat_all$b.sidi.standardized) 
#answer is effectively zero
sd(dat_all$b.sidi.standardized)

# for the s.sidi column: Calculate the sample mean and sd:
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use subset-by-name ($) to create a new column of z-standardized values.
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

#examine the standardized data to see if the standardization worked:
mean(dat_all$s.sidi.standardized) 
#answer is effectively zero
sd(dat_all$s.sidi.standardized)

#Question 14

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2])
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_mc[i] = coef(fit_resampled_i)[2]
} 

#Question 15
mc_crit_val = quantile(result_mc, c(.05))
mc_crit_val
hist(
  result_mc,
  main = "Null Distribution of Regression Slope",
  xlab = "Slope Parameter")

#draws a vertical line to show value of slope from the regression model on observed
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = mc_crit_val, lty = 2, col = "red", lwd = 2)

legend(
  'topright', bty = "n",
  legend=c('OS','CV'),
  lty=c(1,2),lwd=c(3,3), col=c(4,2), text.col = c(1,1))


#question 18

m = 10000 
result_bs = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_boot)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_bs[i] = coef(fit_resampled_i)[2]
} 

mean(result_bs)

quantile(result_bs, c(.05,0.95))


#Question 19

plot(
  density(result_mc),
  main = "Null and Alternative Distribution",
  xlab = "Slope Coefficient",
  xlim = c(-0.05,0.05),
  ylim = c(0,65),
  lwd = 2)
lines(density(result_bs), col=2, lwd = 2)
legend(
  'topright', bty = "n",
  legend=c('Null','Alt.'),
  lty=c(1,1),lwd=c(3,3), col=c(1,2), text.col = c(1,2))
