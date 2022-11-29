#In-Class Models 2: Model Coefficients

data("iris")
data(iris)

#practice using model coefficients to make predictions
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)
summary(fit_species)

boxplot(Sepal.Length ~ Species,
        data = iris)
#base case is the setosa species, mean length of base is 5.006

#Model coefficients: continuous predictor
plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")



?residuals()
residuals(fit_species)
shapiro.test(residuals(fit_species))


fit_petals = lm(Petal.Width ~ Petal.Length,
                data = iris)
summary(fit_petals)
