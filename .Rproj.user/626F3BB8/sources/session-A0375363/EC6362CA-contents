#ricker function = mod of the expon function
#  f(x)=a* x * e ^ -b*x
# parameter a is the initial slope



ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
#from and to arguments tell what range of x-values
#add=FALSE argument value creates a new plot

exp_fun = function(x, a, b)
{
  return(a * exp(-b*x))
}

curve(
  exp_fun(x, 0.1, 1), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
#changing a to = 0.1 instead of 1 in the prior ex
#changed the y axis to range to 0.10


curve(
  exp_fun(x, 2.2, 1/15), 
  add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, 
  ylab = "f(x)"); box()

#simulated data on a line
#choose e50 uniformly-distributed random x values within interval 2 and 10

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, 
     main = "Simulated Data\nNo Errors", 
     xlab = "", ylab = "")
#normal errors 1: to generate "observed" y-values
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", ylab = "")

#Normal Errors 2: make varibility larger with increasing x values
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

#SD was multiplied by x_sim in the second plot to increase x values

#Exponentially-distributed errors

error_mean = o
error_sd = 0

y_observed_3 = 
  y_pred + 
  rexp(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed_3, 
     main = "Exponentially Distributed Errors", 
     xlab = "", ylab = "")

#Fitted Linear Models
fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)

#Marbled salamander dispersal data

require(here)
dat_MSD = read.csv(here("data", "dispersal.csv"))

plot(dat_MSD$disp.rate.ftb, dat_MSD$dist.class,
     main = "Relationship between Juvenile Dispersal and Distance",
     xlab = "Dispersal rate", ylab= "Distance")


#Question 1 - 4
exp_fun = function(x, a, b)
{
  return(a * exp(-b*x))
} 

?curve()

png(
  filename = here("lab_05_EF.png"), 
  width = 1000, 
  height = 800, 
  res = 180, 
  units = "px" )

curve_1 = curve(
  exp_fun(x, 1.9, 0.1), 
  from = 0, to = 20, add = FALSE, ylim = c(-0.1, 2),
  main = "Q2: Exponential function", 
  ylab = "f(x)", xlab = "x", lty  = 1, col= "black");box()
curve_2 = curve( exp_fun(x, 1.9, 0.3), 
       from = 0, to = 20, add = TRUE,
       main = "Exponential function: a = 1, b = 1",
       ylab = "f(x)", xlab = "x", lty  = 2, col= "black")
curve_3 = curve( exp_fun(x, 1.2, 0.2), 
                 from = 0, to = 20, add = TRUE, 
                 main = "Exponential function: a = 1, b = 1",
                 ylab = "f(x)", xlab = "x", lty  = 1, col= "red")
curve_4 = curve( exp_fun(x, 1.2, 0.4), 
                 from = 0, to = 20, add = TRUE, 
                 main = "Exponential function: a = 1, b = 1",
                 ylab = "f(x)", xlab = "x", lty  = 2, col= "red")
dev.off()

#Question 5 - 7 Ricker Function

#ricker function = mod of the expon function
#  f(x)=a* x * e ^ -b*x
# parameter a is the initial slope

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
#from and to arguments tell what range of x-values
#add=FALSE argument value creates a new plot

png(
  filename = here("lab_05_RF.png"), 
  width = 1000, 
  height = 800,
  res = 180, 
  units = "px" )

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#lab plot
RF_1 = curve(
  ricker_fun(x, 25, 0.2), 
  from = 0, to = 50, add = FALSE, ylim = c(0,100),
  main = "Q5 Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 1, col= "black");box()
RF_2 = curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 50, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 2, col= "black")
RF_3 = curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 50, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 2, col= "black")
RF_4 = curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 50, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 1, col= "red")
RF_5 = curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 50, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 2, col= "red")
RF_6 = curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 50, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", 
  lty  = 2, col= "red")

dev.off()


#Question 8 - 13

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

# use locator(1) to find the exact coordinates on a plot
#just put in console and click on location on plot you want coordinates
png(
  filename = here("lab_05_Q9_FLM.png"), 
  width = 1000, 
  height = 800,
  res = 180, 
  units = "px" )

plot(
  dat_MSD$dist.class,
  dat_MSD$disp.rate.ftb,
  xlim = c(0,1500),
  xlab = "Distance Class", 
  ylab = "Standardized Dispersal Rate", 
  main = "Marbled Salamander - first time breeders\n Good Linear model")

curve(line_point_slope(x,0, 0.75, -0.0005), add = TRUE)

dev.off()

png(
  filename = here("lab_05_Q11_EFM.png"), 
  width = 1000, 
  height = 800,
  res = 180, 
  units = "px" )

plot(
  dat_MSD$dist.class,
  dat_MSD$disp.rate.ftb,
  xlim = c(0,1500),
  xlab = "Distance Class", 
  ylab = "Standardized Dispersal Rate", 
  main = "Marbled Salamander - first time breeders\n Exponential Model")

curve(exp_fun(x, 0.8, 0.0020), add = TRUE,
        lty  = 1, col= "red")
dev.off()

png(
  filename = here("lab_05_Q13_RFM.png"), 
  width = 1000, 
  height = 800,
  res = 180, 
  units = "px" )
plot(
  dat_MSD$dist.class,
  dat_MSD$disp.rate.ftb,
  xlim = c(0,1500),
  xlab = "Distance Class", 
  ylab = "Standardized Dispersal Rate", 
  main = "Marbled Salamander - first time breeders\n Ricker Model")
curve( ricker_fun(x, 0.011, 0.0045), add = TRUE, 
       lty  = 1, col= "blue")

dev.off()
#I want the peak to move to the right but I cant figure it out


#Question 14 -15
#calculate residuals for three fitted models 
#and store them in a data.frame

View(dat_MSD)
linear_predicted = line_point_slope(dat_MSD$dist.class,0, 0.75, -0.0005)
resids_linear = c(dat_MSD$disp.rate.ftb - linear_predicted)

expon_predicted = exp_fun(dat_MSD$dist.class, 0.8, 0.0020)
resids_expon = c(dat_MSD$disp.rate.ftb - expon_predicted)

ricker_predicted = ricker_fun(dat_MSD$dist.class,  0.011, 0.0045)
resids_ricker = c(dat_MSD$disp.rate.ftb - ricker_predicted)

dat_MSD_MR = data.frame(dat_MSD$disp.rate.ftb,resids_linear, resids_expon, resids_ricker)
View(dat_MSD_MR)
?hist


png(
  filename = here("lab_05_Q15_histResids.png"), 
  width = 2000, 
  height = 1600,
  res = 180, 
  units = "px" )

par(mfrow = c(1,3))

hist_RL = hist(dat_MSD_MR$resids_linear,
               main = "Histogram of Linear Residuals", 
               xlab = "Resiudals", col = "gray")
hist_ER = hist(dat_MSD_MR$resids_expon,
               main = "Histogram of Exponential Residuals",
               xlab = "Residuals", col = "red")
hist_RF = hist(dat_MSD_MR$resids_ricker,
               main = "Histogram of Ricker\n Function Residuals",
               xlab = "Residuals", col = "blue")

dev.off()


