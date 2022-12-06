#walkthrough the process of insstalling and loading an R package
#here package is designed to make file import/export easier

# most r packages can be installed with the install.packages() function
# use quotation marks when installing a new package

install.packages("here")

require("here")

#[1] "C:/Users/jbste/OneDrive/Documents/environmental_data"
# read.csv(here("data", "my_data.csv"))

#When you use here(), include the subdirectories (in the correct order) 
#and filename as character values (i.e. with quotations marks)

#to tell whether you are looking in the right spot, use file.exists()
#file.exists(here("data", "data_sets", "my_data.csv")) and will say TRUE or FALSE

dat_birds = read.csv(here("data","bird.sta.csv"))
dat_habitat = read.csv(here("data","hab.sta.csv"))

#focus on terrain variables at sampling locations:elevation, slope, aspect, tree cover (measured by basal area)
?hist()


hist_E = hist(dat_habitat$elev,
     main = "Elevation", 
     xlab = "Elevation (m)", 
     breaks = 10)
#I dont know how to get the x axis values to display the entire range of 85-872

hist_S = hist(dat_habitat$slope, 
              main = "Slope", 
              xlab = "Slope (%)",
              breaks = 10)
hist_A = hist(dat_habitat$aspect, 
              main = "Aspect", 
              xlab = "Aspect (degrees)", breaks = 10)

par(mfrow = c(1,3))
plot(hist_A)
plot(hist_E)
plot(hist_S)


#Question 7 scatterplots
scatterElev = plot(dat_habitat$elev, dat_habitat$ba.tot,
     main = "Total Basal Area related to Elevation",
     xlab= "Elevation (m)",
     ylab= "Total Basal Area", 
     col="gold2", cex=0.5)
scatterSlope = plot(dat_habitat$slope, dat_habitat$ba.tot,
                    main = "Total Basal Area related to Slope",
                    xlab= "Slope (%)",
                    ylab= "Total Basal Area", 
                    col="blue", cex=0.5)
scatterAspect = plot(dat_habitat$aspect, dat_habitat$ba.tot,
                     main = "Total Basal Area related to Aspect",
                     xlab= "Aspect (degrees)",
                     ylab= "Total Basal Area", 
                     col="red", cex=0.5)
#Calculate the values of y for a linear function, given the
#coordinates of a known point (x1, y1) and the slope of the line

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

scatterElev = plot(x = dat_habitat$elev, 
                   y = dat_habitat$ba.tot,
                   main = "Total Basal Area related to Elevation",
                   xlab= "Elevation (m)",
                   ylab= "Total Basal Area", 
                   col="gold2", cex=0.5)

meanElev = mean(dat_habitat$elev)
meanBaTot = mean(dat_habitat$ba.tot)
c(meanElev,meanBaTot)

points(meanElev, meanBaTot, col= "blue")

curve(
  line_point_slope(
    x, 
    meanElev, 
    meanBaTot,
    .08), 
  add = TRUE)

#the linear line i used
curve(
  line_point_slope(
    x, 
    x1 = 300, 
    y1 = 25,
    0.11), 
  add = TRUE)

#for terrain variable slope
scatterSlope = plot(dat_habitat$slope, dat_habitat$ba.tot,
                    main = "Total Basal Area related to Slope",
                    xlab= "Slope (%)",
                    ylab= "Total Basal Area", 
                    col="blue", cex=0.5)
curve(
  line_point_slope(
    x, 
    x1 = 40, 
    y1 = 35,
    0.6), 
  add = TRUE)

#for terrain variable aspect

scatterAspect = plot(dat_habitat$aspect, dat_habitat$ba.tot,
                     main = "Total Basal Area related to Aspect",
                     xlab= "Aspect (degrees)",
                     ylab= "Total Basal Area", 
                     col="red", cex=0.5)
curve(
  line_point_slope(
    x, 
    x1 = 175, 
    y1 = 40,
    0.05), 
  add = TRUE)
