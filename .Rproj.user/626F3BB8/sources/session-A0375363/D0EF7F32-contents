#lab 12: Beyond the General Linear Model
#Simulating Sample sizes: we can do the same thing for a gradient in sample sizes
require(here)

bird = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))

birdhab = merge(bird,hab, by = c("basin", "sub"))

dim(birdhab)

#Fitting a LOWESS Model
#work from lab 11

#Question 1
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(2, 20)
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
  main = "Sample Size Simulation, reps = 30",
  type = 'l', xlab = 'Sample size', ylab = 'Statistical Power')

#fitting a lowess model
fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, span = 0.3)
newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  main = "Sample Size/Power Simulation \nLOESS Smoother, 30%",
  ylab = "Statistical Power", xlab = "Sample Size")
points(
  x = sim_sample_size$sample_size, 
  y = sim_sample_size$power,
  col = adjustcolor("blue", alpha = 0.3),
  pch = 19)
legend(
  "bottomright",
  legend = c("Smoothed", "Original"),
  lty = c(1, NA), pch = c(NA, 16),
  col = c(1, adjustcolor("blue", alpha = 0.3)),
  inset=c(0.1,0.25))


#Question 2

