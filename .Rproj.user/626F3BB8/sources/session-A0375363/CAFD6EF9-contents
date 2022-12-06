require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

plot(body_mass_g~sex, data = dat_ade, 
     main = "Body Mass of Adelie Penguins", 
     xlab = "sex", ylab = "Body Mass (grams)")


Fem_adelie = subset(dat_ade, sex == "female")
t.test(Fem_adelie$body_mass_g, y= 0, 
       alternative = c("greater","less"), mu = 0 )

?t.test

#Question 2
Adelie_female <- droplevels(subset(penguins, sex == "female")) 

t.test(Adelie_female$body_mass_g) 


#Question 4
adelie_male = droplevels(subset(penguins, sex == "male"))
t.test(adelie_male$body_mass_g, alternative = "greater", mu = 4000)

?t.test


#Question 6
t.test(adelie_male$body_mass_g,Adelie_female$body_mass_g, 
       alternative = "two.sided")

#Question 8
t.test(adelie_male$body_mass_g,Adelie_female$body_mass_g, 
       alternative = "greater" )
#the alternative argument will relate to x to y so "greater" will test x is greater than y


#Question 9
t.test(adelie_male$body_mass_g,Adelie_female$body_mass_g, 
       alternative = "less" )
