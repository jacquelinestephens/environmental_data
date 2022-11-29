a = "Jackie Stephens"
b1 = 45.6
b2 = "45.6"
c1 = c(0:3)
typeof(a)
typeof(b1)
class(b1)
class(a)
class(b2)
b1+b2
b1+c1
v1 = c(-2:2)
v2 = (v1)*3
sum(v2) 

vec_4 = c(1:12) 

mat_1 = matrix(vec_4, nrow = 3, ncol = 4, byrow = TRUE) 

View(mat_1) 

mat_2 = matrix(vec_4,nrow = 3, ncol = 4, byrow = FALSE) 

View(mat_2) 

v5 = c(0:5)

my_list_1 = list(5.2, "five point two", v5) 

names(my_list_1) = c("two", "one", "three") 

my_list_1[[3]]

my_list_1$one

#Question 15 and 16
my_vec = rep(1:3, 5)
my_vec

my_bool_vec = my_vec == 3

my_vec[my_bool_vec]
data.frame(my_vec, my_bool_vec)



