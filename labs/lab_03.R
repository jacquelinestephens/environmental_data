# Lab 3: Data exploration and Deterministic Functions
install.packages("psych")
require(psych)
#use subsetting by name with ex. dataset iris
#names(iris)
#along with the square brackets to pull out three columns of interest:
# pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
head(dat_bird)
dat_habitat = read.csv(here("data", "hab.sta.csv"))
head(dat_habitat)

#combine both data sets into a new data frame but how do we know that there 
#are the same number of ros in both data frames?
# How can we be sure that we assoicate the correct row of dat_habitat and dat_birds?

?merge()
dat_all = merge(dat_bird,dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

#Convert bird data to presence/absence
#counts of Cedar Waxwings at 100 randomly sampled sites
sample(dat_all$CEWA, 100)
max(dat_all$CEWA)
#In fact, a total of only 116 cedar waxwings were observed at 53 (out of 1046) of sites.
#How could I use R to calculate the total number of waxwings?
sum(dat_all$CEWA)
#the number of sites in which they were present????
length(which(dat_all$CEWA >0))

#Presence Absence example with Boolean vectors
my_vec = rep(1:3, 5)
#creates a list of 1 through 3, 5 times (1,2,3,1,2,3,1,2,3,1,2,3, etc)
my_vec == 3
#[1] FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE

#You could also use the greater than to select the elements that are greater than 1
my_vec > 1

#use this as a template to build a boolean vector from the column of count data for CEWA
dat_all$CEWA >0
sum(dat_all$CEWA > 0)

#Data Type Coercion
#to coerce a Boolean vector into a vector of ones and zeroes
cewa_present_absent = as.numeric(dat_all$CEWA >0)
plot(x = dat_all$elev, y = cewa_present_absent)

#Fitting a logistic curve: the shape of the curve

#Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#Logistic Fit 1 using midpoint 400 and try slope 0.1 to start
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#Logisitic Fit 2 trying a negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

#Logistic Fit 3 trying a shallower negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

#A couple of arguments to plot() and curve() might be of interest:
#cex: this controls the size of the plotting character. Default is 1.
#pch: this controls the shape of the plotting character. Default is 1. Code 16 plots a solid point.
#col: this controls the color of the plotting character. Default is 1, corresponding to black.
# adjustcolor() makes points semi-transparent
#For example, I used the col argument and adjustcolor() inside a call to plot() to create the some modifications to one of my plots above:

#Instructions and Deliverables
#1. make pair plot with slope, elevation, aspect, and basal area
colnames(dat_habitat)
pairs.panels(dat_habitat[, c("slope", "aspect", "elev","ba.tot")])

png(filename = here("terrain basal area_pairplot.png"), width = 800, height = 600)
pairs.panels(dat_habitat[, c("slope", "aspect", "elev","ba.tot")])
dev.off()



#2. Choose two bird species and create plots of presence absence (on the y axis) and basal area (on the x axis)
#species 1 PUFI by total basal area
PUFI_present_absent = as.numeric(dat_all$PUFI >0)
plot(x = dat_all$ba.tot, y = PUFI_present_absent, 
     main = "Presence of Purple finch Related to Total Basal Area",
     xlab = "Basal Area (m2 per ha)", 
     ylab = " Bird Absence vs Presence")
curve(logistic_midpoint_slope(x, midpoint = 50, slope = -0.2), add = TRUE)





      
#species 2 SWTH by total basal area
SWTH_present_absent = as.numeric(dat_all$SWTH>0)
plot(x = dat_all$ba.tot, y = SWTH_present_absent, 
     main = "Presence of Swaison's Thrush Related to Total Basal Area",
     xlab = "Total Basal Area (m2 per ha)", 
     ylab = " Bird Absence vs Presence")
#species 2 SWTH by Site Elevation
SWTH_present_absent = as.numeric(dat_all$SWTH>0)
plot(x = dat_all$elev, y = SWTH_present_absent, 
     main = "Presence of Swaison's Thrush Related to Site Elevation",
     xlab = "Elevation (meters)", 
     ylab = " Bird Absence vs Presence")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#species 1 PUFI by elevation
PUFI_present_absent = as.numeric(dat_all$PUFI>0)
plot(x = dat_all$elev, y = PUFI_present_absent, 
     main = "Presence of Purple Finches Related to Site Elevation",
     xlab = "Elevation (meters)", 
     ylab = " Bird Absence vs Presence")
curve(logistic_midpoint_slope(x, midpoint = 500, slope = 0.05), add = TRUE)

sum(dat_all$GRJA)
length(which(dat_all$GRJA >0))
