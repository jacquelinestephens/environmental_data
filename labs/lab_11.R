#Lab 11: Simulation and Power Analysis

require(here)

bird = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))

birdhab = merge(bird,hab, by = c("basin", "sub"))

dim(birdhab)

#working with the standardized Brown creeper abundance (column BRCR) and late successional forest (column ls) data within birdhab
#simulating a static environmental process using a simple linear regression model based on oregon bird data

plot(birdhab$ls, birdhab$BRCR, xlab = "late-successional forest extent",
     ylab = "Brown Creeper abundance")
#Fit a model
fit_1 = lm(BRCR ~ ls, data=birdhab)
abline(fit_1)
summary(fit_1)

#deterministic model: linear function
linear = function(x,y_int,slope)
{return(y_int+(slope*x))}
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

#Stochastic Model: Normal Distribution just rnorm() with appropiate arguments for mean and sd)

linear_simulator = function(x,y_int,slope, st_dev)
{ y = linear(x, y_int,slope)
  z = rnorm(length(x),mean = 0,st_dev)
  return(y+z)}

#Test you simulator function
#runif() add the random number of points you want
x = runif(10)
plot(x, linear_simulator(x, 1.0, 4.5, 0.1))

n = 200

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

x = runif(10)
plot(x, linear_simulator(x, 10, -6.5, 0.1))


#build the simulation: retrieve the model coefficients 
#to simulate new data you need to retrieve the intercept, slope, and sd from the model using coefficients()

fit_1_coefs = coefficients(fit_1) #extracts the intercept and slope values
str(fit_1_coefs) #examines the output of coefficients

#retrieving the sd is slightly more difficult
fit_1_summary = summary(fit_1) #create a model summary object
str(fit_1_summary)
fit_1_summary$sigma #value for sd is stored in the element sigma
int_obs =fit_1_coefs[1]
slope_obs = fit_1_coefs[2]
sd_obs = fit_1_summary$sigma

#choose Predictor values
# the first goal of the simulation is to generate some new "data" using our models

#Simulate Data: now that you know the parameters and have a strat for choosing x-values
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

#alternatively, plot observed data first and then add the simulated data to the existing plot
plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))


#one problem with the use of the norm dis is that it is unbounded on the lower limit
#thus negative values are possible = UNDESIRABLE BEHAVIOR OF THE MODEL because abundance can not be neg

#because the y-int is close to 0, the sim is likely to produce neg values when x is 0

#Power analysis for the linear regression model
#probability of correctly rejecting the null hypothesis when it is false

#Single Simulation: start by finding out whether we can reject the null in a single experiment
#choose crit p value (alpha) as rejection criterion (often 0.05)
#simulate a dataset with given int, slope, and # of obs
#create a simple linear regression model and extract the p-value
#null hypothesis: BRCR abund is independent of late-succ forest (no hab preference)
#compare the p-value from the model of simulated data to the crit p-val you chose above

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)


summary(fit_sim)

#EXTRACTING P-VALUES FROM R ANALYSES CAN BE TRICKY

#coefficients of the summary() of the linear fit are a matrix including the standard error,
#t-statistics, and p-value for each parameter

#use the matrix indexing to pull out the specific values of interest

sum_1 = summary(fit_sim)
sum_1$coefficients

sum_1$coefficients[2, 4] #calls out the matrix cells that contains the p-value of interest

#Repeated Simulations
#To estimate the probability of successfully rejecting the null hypothesis when it is false (the power)

#repeat this procedure many times and calculate the proportion of the time that we reject the null hypothesis

#first specify the # of sim to run and set up a vector to hold the p-value for each simulation

#second repeat what we did above saving the pvalues to the storage vector

#third calculate stat power as # of times we correctly rejected the null divided by the total number of sim

#we known the null hyp is false because were using a slope value different from zero

n_sims = 1000 #specify number of sims to run
p_vals = numeric(n_sims) #set up vector to store p-value for each sim
alpha = 0.05 #crit p-value as reject criterion
for(i in 1:n_sims)#create a loop to run sim
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims
length(p_vals)#all simulation were found to have a sign slope coefficient
#stat power = 1


#function to build sim ulation loop models
linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

#simulating effect sizes
#because we can not know the true population values, we often estimate the stat power over a range of model parameteres (or even different models)


#this example simulation estimates statistical power as a function of the slope (the effect size)
alpha = 0.05
n_sims = 1000

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    effect_size = effect_sizes_1,
    power       = effect_size_powers)
#we created a seq of effect sizes to try 
#nested the orginal code inside an outer loop that iterates over each of the effect sizes
#saved a vector of statistical powers
#p-values calculated in the inner loop for a specific effect size
#stat power calculated in the outer loop and stored in effect_size_powers

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')
#plot the results and add a vertical line to show the slope of our original data set
#the power for our observed effect size of 0.00058 is 1

#Simulating Sample sizes: we can do the same thing for a gradient in sample sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)
plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')


#Bivariate Power Analysis
#would be more interesting to vary combinations of parameters like slope and sample size
#using another loop, saving results in a matrix and using contour() or persp() to plot results

#effect size and sample size
alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )
#note hte only difference in this code is a third outer loop and created a matrix to store the results

#our outer power data are stored in a matrix (instead of a vector) so we need a way to visualize 2-dim gridded data
#raster data


#Plotting a matrix
#image() is a quick way to plot a matrix as if it were raster data 
#plot a grid in which the pixel color is determined by the value of the matrix element

image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

#Plotting 3-Dimensional Data
#to make plots less pixellated you need to run sim many times and with small increments in parameter values

#setting n_sims and n_effect_sizes to smaller values can help you experiment with the code in a time efficient manner

#Contour plotting (similar to topographic maps)
#The [iso]lines show interpolated lines at which the value is the same.

#contour() function expects arguments for x,y, and z
#x and y set up the axes
#for our case, x = effect size and y = sample size
#z coordinates are a matrix in which cells represent the values for which to create the contours

contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")


#3d Plotting: Perspective plots
#static plots using the persp() function 
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')
#Interactive Plot
#rgl() allows you to create similar plots that are interactive
install.packages("rgl")
require(rgl)
persp3d(  x = sim_n_effect_size$effect_size,
          y = sim_n_effect_size$sample_size,
          z = sim_n_effect_size$power,
          xlab = "beta", ylab = "n", zlab = "power",
          col = 'lightblue',
          theta = 30, phi = 30, expand = .75,
          ticktype = 'detailed')
#saving an interactive plot using the rgl function
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here("n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

#saving R data objects
#when you do a simulation study, it is convenient to save your simulation results so that you dont hae to re-run your simulation

#save R objects to files using save()
#.Rdata file = binary files that can only be read by R

#if you have data stored in a data.frame or matrixx, you can write a text file (CSV)

save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))
#when you want to load the data again you can use 
# load(file = here::here("data", "lab_11_n_effect_sizes.Rdata")))



#########################################################################


#Dispersion Simulations: the population standard deviation
#Population Dispersion Analysis

#template

#Question 1
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sds_j = pop_sds[1:j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(x = birdhab$ls,
                            y_int = int_obs,
                            slope = slope_obs,
                            st_dev = pop_sds_j)
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_powers[j] = sum(p_vals<alpha)/n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_powers)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(power ~ sd, data = sim_output_dispersion, type = "l", 
     xlab = "Population SD",
     ylab = "Power Statistic",
     main = "Dispersion vs Statistical Power")

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = slope_obs, lty = 2, col = "red")



#question 3
alpha = 0.05

# Start with a small number
n_sims = 1000
p_vals = numeric(n_sims) 

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)


pop_sd_powers = numeric(length(n_sds))

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha)/n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}


sim_3_dat = list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)

image(sim_output_3)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_100.RData"))

contour(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "Population dispersion",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

#Question 5
#interactive 3D perspective plot using persp3d() in package rgl of the sample size and population dispersion power simulation
install.packages("rgl")
require(rgl)

persp3d(x = sim_3_dat$pop_sd,
        y = sim_3_dat$sample_size,
        z = sim_3_dat$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'darkolivegreen',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')
#saving an interactive plot using the rgl function
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here("lab11_question5.html"),
  selfcontained = TRUE
)
