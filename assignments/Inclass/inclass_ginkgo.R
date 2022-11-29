#In-class Ginkgo
require(here)
ginkgo = read.csv(here("data", "ginkgo_data_2022.csv"))
head(ginkgo)
summary(ginkgo)
boxplot(petiole_length ~ seeds_present, data = ginkgo)
plot(ginkgo$max_depth, ginkgo$max_width, 
     main = "Relationship Between Max \n Leaf Depth and Width",
     xlab = "Max Leaf Depth (mm)",
     ylab = "Max Leaf Width (mm)")
dat_SP_site = subset(ginkgo, select = c(seeds_present,site_id))
summary(unique(dat_SP_site)) #unique function calls out only unique site iDs
# there are 22 trees present and 4 had seeds

boxplot(petiole_length ~ seeds_present, data = ginkgo)
plot(ginkgo$max_depth, ginkgo$max_width, 
     main = "Relationship Between Max \n Leaf Depth and Width",
     xlab = "Max Leaf Depth (mm)",
     ylab = "Max Leaf Width (mm)")

