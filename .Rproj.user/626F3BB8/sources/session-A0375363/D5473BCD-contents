#Week 6 Reading Questions on Seed Predation

#question 2
rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
pol_predation_rate = 0.124
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = 731
# psd_predation_rate = psd_n_predation / psd_n_total = 25 / 731
psd_predation_rate = 0.034



#following is just a self-test to see if the rates match what is in the book
print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3))) 

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))

#Continued Question 2

table = matrix(c(pol_n_predation, pol_n_no_predation, pol_n_total, pol_predation_rate,
                 psd_n_predation, psd_n_no_predation, psd_n_total, psd_predation_rate),ncol=4, byrow=TRUE)
colnames(table) = c('Any taken', 'None taken', 'N', 'Predation Rate')
row.names(table) = c('Polyscias fulva (pol)', 'Pseudospondias microcarpa (psd)')

require(here)

png(
  filename = here("readingQs_week6_table.png"), 
  width = 1000, 
  height = 800, 
  res = 180, 
  units = "px" )

grid

dev.off()

install.packages("grid")
require(grid)

install.packages("gridExtra")

#Question 4

overall_proportion = (pol_n_predation + psd_n_predation)/ (pol_n_total+psd_n_total)
#ratio of the predation probabilities
ratio_pred = pol_predation_rate / psd_predation_rate


