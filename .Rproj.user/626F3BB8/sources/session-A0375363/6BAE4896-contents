install.packages('here')
require(here)

#use read.csv() with here() to read the data files into data.frame objects

dat_birds = read.csv(here("data","bird.sta.csv"))
dat_habitat = read.csv(here("data","hab.sta.csv"))

colnames(dat_habitat)

hist(x= dat_birds$CBCH, xlab = "Number of birds counted",
     breaks = 0:7 - 0.5)
max(dat_birds$PUFI)
hist(x = dat_birds$WIWA, xlab = "Number of birds counted", breaks=0:7 - 0.5 )
#breaks tell how many bins you are organizing your data into
#and the - 0.5 indicates where the bars will be plotted over

pairs(dat_habitat[,c("elev","slope","ba.con","ba.tot")] )
max(dat_birds$WIWA)
#tells you how many total birds there are
apply(dat_birds[-c(1,2,3)],2,sum)
