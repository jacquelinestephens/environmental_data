#lab 9 Modeling Data 2
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#Binomial Test For Proportions
#suitable for data consisting of a set of single traits in which each
#observation can take on one of two values (succcess or fail, presence absence)

#two sided binomial test of proportions to answer is success is more likely than failure

#How likely is a response of 33/61 if the reproductive success
#and failure are equally likely, i.e., Pr(success)=0.5?
#specify the number of successes (33) and total sample size (61)
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

#Reproductive Catastrophe and Late Filling
#ponds experience late filling in approx 2 out of every 7 years
#define the variables to hold the late- and normal-filling rates

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

#what is the evidence that reproductive success is more or less
#frequent than the normal filling rate

#we expect successful reproduction in approx 5 of every 7 years

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

#default test is a two sided alternative 
#prefer a onesided alternative hypothesis that obs success rate
#is less than the pond normal-filling rate

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

#Comparison with one-sample tests
t.test(catrate$cat.rate, mu = 2/7)

#two sample tests can be used to 
#compare two variances, compare two sample means or medians, check the 
#correlation of two variables, compare two (or more) proportions
#test for independence of two variables in a contingency table

#Comparing two variances: test whether significantly different
#Fisher's F test based on F-statistics: ratio between two variances
#F-distribution has two parameters: numerator degrees of Freedom, denominator degrees of freedom

#if the variances of the two samples are the same, then the ratio of the variances will be close to one

#in order to be significantly different, ratio needs to be smaller or larger than 1

#F-distribution ex: vegetation data: seedling response to understory veg treatments
#4 treatments randomly assigned to 32 plots in a randomized block design
veg = read.csv(here("data", "vegdata.csv"))
head(veg)
boxplot(pine ~ treatment, data = veg)

#from the boxplot, clear that the means are different and also the variances are different

#Variance Test: to test variance between pine seedling count differs between control and clipped
veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)
var.test(
  pine ~ treatment,
  data = veg2)

#the Fisher's test says that the f value is larger than 1 and the pvalue is smaller than 0.05 so the null can be rejected

#F-tests assumes normality for unequal variances
#test for normality using the shapiro.test, have to select one sample because shapiro is one sample test
shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])
#p value is not less than 0.05 so distribution is normal!
#Non normal for clipped treatment

#Non-parametric Variance test: if data is non-normal, use Fligner-Killen test
fligner.test(
  pine ~ treatment,
  data = veg2)
# p value tells us that the homogeneity of variances is not supported

#tests for multiple variances: n-sample parametric test Bartlett's test
#tests for homogeneity of variances amount all four treatment levels
bartlett.test(pine~treatment, data = veg)
#Note that Bartlett’s test, like Fisher’s F test is highly sensitive to 
#non-normality and the presence of outliers.

fligner.test(pine ~ treatment, data = veg)
#the results agree between the two tests, pvalues are sign different between non par and par
#using parametric tests when the assumptions are not met leads to incorrect conclusions about significance

#comparing two sample means
#how likely is it that our sample means were drawn from populations with the same mean?
#highly likely: our two sample means are not sign different
#highly unlikely: sample means are sign different

#work out the probability that the two samples were indeed drawn from populations with the same mean

#student t test and wilcoxons rank sum test to test for comparing two sample means or medians

#T-test Student: appropiate when samples are independent, variances constant, errors norm dist

t.test(
  pine ~ treatment,
  data = veg2)

#confidence intervals that includes 0 indicates that the sample means are not significantly different


#T-test assumptions: validity of the student t test depends on meeting the assumptions
#variances were sign different
#clipped sample was non-normally distributed 
#hence good reason to be suspicious of the test results

#Wilcox Test rank sum is appropriate when the samples are independent, but errors are not normally dist
wilcox.test(
  pine ~ treatment,
  data = veg2)
# I have more confidence in the wilcox test because the p value is bigger, and it took into consideration the errors not being norm dist

#Tests for paired samples
#sometimes two-sampled data comes from paired observations
#In this case, we might expect a correlation between the two measurements, 
#either because they were made on the same individual, or were taken from the same location.

#example data set of mice whose body mass was measured before and after a treatment.

install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)
t.test(mice2$before, mice2$after, paired = TRUE)

#check for normality assumptions to verify t test is appropiate
shapiro.test(mice2$before)
shapiro.test(mice2$after)
#fact that there is only 10 observations in each group is uncomfy for assuming norm

wilcox.test(mice2$before, mice2$after, paired = TRUE)

#now compared to unpaired test
t.test(mice2$before, mice2$after, paired = FALSE)

#when in doubt choose the non-paired option
#paired tests make additional assumptions about data (pairs of observations are related)

#correlation: ith any two continuous variables, x and y, the question
#naturally arises as to whether their values are correlated with each other

#Marbled Salamander: dispeersal of first time breeders (ftb) from natal ponts = juvenile dispersal
#dispersal of experienced breeders (eb) from their established breeding ponds = adult dispersal

#question: whether dispersal rates for first time breeders and experienced are correlated
require(here)
disp = read.csv(here("data", "dispersal.csv"))
disp
plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue")

#test sign of correlations using cor.test()
#need to use='complete.obs' argument to address missing values

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')
#there is no correlation between the rates

#use spearmans rank correlation used to estimate rank based measure of association

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#comparing two distributions with empirical cum dist of samples (ecdf)
#Kolmogorov-Smirnov test: are two samples the same or sign different in one or more 
#unspecificed ways? Does a particular sample distribution arise from a particular 
#hypothesized theoretical distribution?

#two distributions with exactly the same mean could be significantly 
#different if they differed in variance, or in skew or kurtosis, or both.

#empirical cumulative distribution functions (ecdf) - give the probability
#that a randomly selected value of X is less than or equal to x.

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")

#are the two dist different? use ks.test
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#there is not enough evidence to suggest dispersal-distance relationship
#differs between first-time breeders and experienced breeders

#Comparing two ir more proportions: sex-linked killing
#simple binomial proportions test by specifiying two vectors

#the number of mortalities for females and males c(4,16)
#the total number of female and male candidates: c(40,250)

prop.test(
  x = c(4,16),
  n = c(40,250))
#sign pvalue indicates that the proportions are different between samples
#proportions observed are unlikely to have been drawn from the same pop

#insign p value means nsufficient evidence to reject the null hypothesis 
#and we would conclude that the proportions are not statistically different.

prop.test(
  x = c(8,32),
  n = c(80,500))
#doubling sample size (same proportions) decreased the p value by half

#Dependence of variables in a contingency table
#With count data, the number 0 is often the value of the response variable;
#in other words, there are often observations that receive a count of 0

#A contingency table shows the counts of how many times each of the
#contingencies actually happened in a particular sample


#Contingency: Chi-square test

#sign of different between obs and expected: Pearson’s chi-squared test (generalized linear models are an alternative)

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

#the sign p value does not tell us whether barred owls are present more or less frequently than expected

round(chisq_owls$expected,1) #expected values
chisq_owls$observed #observed values
#there were less than expected owls observed in the young forest

#Chi-Square Residuals
#calculate residuals = difference between observed and expected values
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)
#residual is negative so confirms that there are fewer owls present in the young forest than expected

#Fisher's Exact test
#Pearson chi-sqr test expects the expected values to be large, (> 4 or 5)

# when one or more of the expected frequencies is less than 4 (or 5)
#then it is wrong to use Pearson’s chi-squared test for your contingency table

fisher.test(owls)
#pvalue becomes more sign, there is a difference between expected and observed to be supported

#Bird habitat data
#whether the presence/absence of brown creepers varies 
#between the interior and edge of forest stands.

#read in the bird and habitat data and merged them into a single file based on the common fields.
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))
#Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table
#true = presence, false = absence

#_______________________________________________________________________________

#QUestion 1 through 2
BRCR_table = matrix(c(29, 144, 314, 559), nrow=2)
rownames(BRCR_table) = c("present", "absent")
colnames(BRCR_table) = c("Edges", "Interior")
BRCR_table
chisq_BRCR = chisq.test(BRCR_table)
chisq_BRCR

round(chisq_BRCR$expected, 1) #expected values
chisq_BRCR$observed #observed values
round(chisq_BRCR$observed - chisq_BRCR$expected, digits = 1)

#Question 3
require(palmerpenguins)
head(penguins)
fit_species = lm(formula = body_mass_g ~ species,data = penguins)
#Question 4
fit_sex = lm(formula = body_mass_g ~ sex, data = penguins)
#Question 5
fit_both = lm(formula = body_mass_g ~ species*sex,data = penguins)
#Question 6
boxplot(formula = body_mass_g ~ species,data = penguins, 
        main = "Conditional Boxplot for Body Mass by Species",
        xlab = "Species", ylab = "Body Mass (grams)")
#Question 7
boxplot(formula = body_mass_g ~ sex, data = penguins,
        main = "Conditional Boxplot for Body Mass by Sex",
        xlab = "Sex", ylab = "Body Mass (grams)")
#Question 8
boxplot(formula = body_mass_g ~ species*sex,data = penguins,
        main = "Double Conditional Boxplot for \n Body Mass by Sex and Species",
        xlab = "", ylab = "Body Mass (grams)",
        names = c("Adelie\n female", "Adelie\n male", "Chinstrap\n female", "Chinstrap\n male", "Gentoo\n female", "Gentoo\n male"),
        las = 2)

#Question 11
bartlett.test(body_mass_g ~ species,data = penguins)
#Question 12
bartlett.test(body_mass_g ~ sex,data = penguins)
#Question 13
dat_ss = aggregate(
            body_mass_g ~ sex*species,
            data = penguins,
            FUN = c)
str(dat_ss)
?bartlett.test
bartlett.test(dat_ss$body_mass_g)

#Question 15
require(here)
FLtrees = read.csv(here("data", "trees_FL.csv"))
dat_fl = data.frame(FLtrees)
head(dat_fl)

barplot(table(dat_fl$ProbabilityofFailure), 
        main = "Probability of Failure",
        xlab = "Failure Class")
barplot(table(dat_fl$Failure_Standardized), 
        main = "Standardized Failure")
hist(dat_fl$DBH_in, xlab = "DBH (inches)", 
     main = "Histogram of DBH")
plot(dat_fl$DBH_in, dat_fl$HeighttoTop_ft, main = "Scatterplot of Relationship between \n DBH and Height of Trees",
     xlab = "DBH (inches)", ylab = "Height (feet)")

#Question 17

#intact and whole trees
dat_none = droplevels(subset(dat_fl, Failure_Standardized == "none"))
dat_branch = droplevels(subset(dat_fl, Failure_Standardized == "branch"))
dat_whole = droplevels(subset(dat_fl, Failure_Standardized == "whole"))
ks.test(dat_none$DBH_in, dat_whole$DBH_in)

#Question 20
#linear
cor.test(dat_fl$DBH_in, dat_fl$HeighttoTop_ft,use = 'comlete.obs')

#if curved and does not come from a bivariate normal distribution
cor.test(
  dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
  use='complete.obs',
  method='spearman')

#Question 21
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chisq_fl_table_2 = chisq.test(fl_table_2)
chisq_fl_table_2

round(chisq_fl_table_2$expected, 1) #expected values
chisq_fl_table_2$observed #observed values
round(chisq_fl_table_2$observed - chisq_fl_table_2$expected, digits = 0)

