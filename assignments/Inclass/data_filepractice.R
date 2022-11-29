here()
download.file("here")
install.packages('here')
download.file('here')
download.packages('here')
require(here)
here()
getwd()
read.csv(here("data","catrate.csv"))
read.csv(here("data","delomys.csv"))
read.csv(here("data","rope.csv"))
dat_catrate = read.csv(here("data","catrate.csv"))
dat_delomys = read.csv(here("data","delomys.csv"))
dat_rope = read.csv(here("data","rope.csv"))
head(dat_catrate)
head(dat_delomys)
head(dat_rope)
plot(dat_delomys$body_mass, dat_delomys$body_length, 
     main = "The Relationship between Body Mass and Length Jackie Stephens", 
     xlab = "Body Mass", ylab = "Body Length", col=as.numeric(factor(dat_delomys$sex)))
# first factor level is going to be alphabetical so female is black and male is red

png(filename = here("basic_scatterplot.png"), width = 1000, height = 600)
plot(dat_delomys$body_mass, dat_delomys$body_length, 
     main = "The Relationship between Body Mass and Length Jackie Stephens", 
     xlab = "Body Mass", ylab = "Body Length", col=as.numeric(factor(dat_delomys$sex)))
dev.off()
help("plot")

