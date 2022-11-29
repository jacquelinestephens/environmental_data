int_rnd = sample(100,1)
int_rnd_sentence = paste0("The value of the randomly-generated number is :",
                          int_rnd)
print(int_rnd_sentence)
#you can nest all of those tasks within a single function
print(paste0("The value of the randomly-generated number is:", sample(100,1)))
# here is a simple for-loop in R
for (i in 1:10) {
  print(i)
}
#Question 1 COMPLETED
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_1
#Use a logical test operator to create a Boolean vector (called vec_2) whose entries are TRUE if the corresponding entry in vec_1 is 3 and FALSE otherwise.
vec_2 = vec_1 == 3

vec_1[vec_2]

#Question 2 COMPLETED

#Question 3, 4, and 5 COMPLETED
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)
#12345

#check how many entities have the value 3:
sum(vec_1 == 3)
#959

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

print(vec_1)



#Question 6 COMPLETED
for (i in 1:10) {
  print(
    paste0(
      "This is loop iteration: 1"))
}

#Question 7 COMPLETED
n = 14
for (i in 1:n)
{
  print(i)
}
#Question 8 COMPLETED
n=17
vec_1 = sample(10,n,replace = TRUE)
head(vec_1)
for (i in 1:n) {
  print(
    paste0("The element of vec_1 at index ",i, " is ",vec_1[i]))
}



#Question 9 COMPLETE

create_and_print_vec = function(n, min = 1, max =10)
{
  vec_4 = sample(min:max,n,replace = FALSE) 
  for (i in 1:n) {
    print(
      paste0("The element of vec_4 at index ",i, " is ",vec_4[i]))
  }
}
create_and_print_vec(10, min = 100, max = 2000)

