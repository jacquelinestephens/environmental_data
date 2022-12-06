#Lab 10:ANOVA, one way Analysis of Variance
require(here)
rope = read.csv(here("data", "rope.csv"))
#data comes from interest in evaluating the differential resistance of various climbing rope types to accidnetal cutting by different hand-saw blades
#Factorial Experiment with the predictor variables:
#rope = 6 rope types, blade = 4 blade types, fully crossed design with 5 replicates of each rope and blade type

#placed a fixed length of rope under tension, approx load of avg human body

#several response variables: percent of rope cut by the blade, percent of rope strength lost, several other measures of rope damage

#ANOVA with predictor: rope type, response: percent rope cut
View(rope)
summary(rope)
str(rope)
summary(unique(rope$rope.type))
#6 different rope types and 121 sample size
class(rope$rope.type)
#want to convert character string into a categorical factor variable
as.factor(rope$rope.type)
rope$rope.type = as.factor(rope$rope.type)
class(rope$rope.type)
levels(rope$rope.type)
#Number of observations and groups
install.packages("dplyr")
library(dplyr)
rope %>% count(rope$rope.type, sort = TRUE)
table(rope$rope.type)
str(rope)
n_obs= 121
n_groups= 6
#Partitioning Variance: Total
ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
df_tot = n_obs-1
#partitioning variance: within group
#Calcuate the group means within group
agg_groupmean = aggregate(x = rope$p.cut, 
          by = list(rope$rope.type), 
          FUN = mean)
str(agg_groupmean)
agg_resids = aggregate(x = rope$p.cut, 
                       by = list(rope$rope.type), 
                       FUN = function(x){x-mean(x)})
str(agg_resids)
#calculate sums of squared residuals within each group

agg_sum_sq_resids = aggregate(x = rope$p.cut, 
                              by = list(rope$rope.type), 
                              FUN = function(x){sum((x-mean(x))^2)})

str(agg_sum_sq_resids)
ss_within = sum(agg_sum_sq_resids$x)
df_within = (5*19)+(1*20)
#PArtitioning Variance: Amoung Groups
ss_among = ss_tot - ss_within
df_among = n_groups-1
#extent to which ss_within is less than ss_tot is a reflection of 
#the magnitude of the differences between the means.
#if witthin is much smaller than total, then most of the variation in the respons
#is due to differences among groups (or levels of the independent factor)

#Normalizing: Mean Squares

ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)
#The Test Statistic: F
f_ratio = ms_among / ms_within
f_pval = pf(f_ratio,df_among, df_within, lower.tail = FALSE)
#F-ratio = the among-group variance divided by the within-group variance.
#used to test the null hypothesis that treatment means are all the same

#if its a big number we reject and if its not big, we fail to reject the null

?pf()
pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)


#ANOVA in R
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$"Sum Sq"
#ANOVA alternative hypothesis “At least one of the group means is different from the rest.”
# ask which group or groups are different with a post-hoc test
#Tukey Honest Significant Difference (HSD) Test
#works by making pairwise comparisons between each possible pair of groups

#number of pairwise comparisons grows quickly with the number of groups so demo with model of a subset of the data
rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)
fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)
round(rope2_hsd$rope.type, digits = 4)




#Self Check
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check)


________________________________________________________________________-_________
#Question 3
bartlett.test(p.cut ~ rope.type,data = rope)

#Question 5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)



dat_BLAZE= subset(rope, rope.type == "BLAZE")
BLAZE_resids = aggregate(x = dat_BLAZE$p.cut, 
                         FUN = function(x){x-mean(x)})
shapiro.test(BLAZE_resids)

dat_BLAZE= subset(rope, rope.type == "BLAZE")
BLAZE_resids = dat_BLAZE$p.cut - mean(dat_BLAZE$p.cut)
shapiro.test(BLAZE_resids)

dat_BS= subset(rope, rope.type == "BS")
BS_resids = dat_BS$p.cut - mean(dat_BS$p.cut)
shapiro.test(BS_resids)

dat_PI= subset(rope, rope.type == "PI")
PI_resids = dat_PI$p.cut - mean(dat_PI$p.cut)
shapiro.test(PI_resids)

dat_SB= subset(rope, rope.type == "SB")
SB_resids = dat_SB$p.cut - mean(dat_SB$p.cut)
shapiro.test(SB_resids)


dat_VEL= subset(rope, rope.type == "VEL")
VEL_resids = dat_VEL$p.cut - mean(dat_VEL$p.cut)
shapiro.test(VEL_resids)


dat_XTC= subset(rope, rope.type == "XTC")
XTC_resids = dat_XTC$p.cut - mean(dat_XTC$p.cut)
shapiro.test(XTC_resids)


mean(dat_BLAZE, na.rm = FALSE)

shapiro.test(dat_BLAZE))

aggregate(p.cut ~ rope.type, data = rope, FUN = mean)
aggregate(
  p.cut ~ rope.type, data = rope,
  FUN = function(x) shapiro.test(x)$p.value)


agg_groupmean = aggregate(x = rope$p.cut, 
                          by = list(rope$rope.type), 
                          FUN = mean)
str(agg_groupmean)
agg_resids = aggregate(x = rope$p.cut, 
                       by = list(rope$rope.type), 
                       FUN = function(x){x-mean(x)})
str(agg_resids)









#Question 8-11
?residuals()
fitrope1_resid = residuals(fit_rope_1)
shapiro.test(fitrope1_resid)

#Question 12-17
require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")
boxplot(body_mass_g ~ species, data = pen_fem,
        main = "Female Penguins Body Mass by Species",
        xlab = "Species",
        ylab = "Body Mass (grams)")
bartlett.test(body_mass_g ~ species, data = pen_fem)
#Question 15

fit_pen = lm(body_mass_g ~ species, data = pen_fem)
pen_fem
fit_pen_resid = residuals(fit_pen)
shapiro.test(fit_pen_resid)
t.test(fit_pen_resid)
?ks.test

pen_fem_hsd = TukeyHSD(aov(fit_pen))
class(pen_fem_hsd)
round(pen_fem_hsd$species, digits = 4)


