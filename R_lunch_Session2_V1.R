# Contents
# Loops: For, While
# List Comprehension: a fast way to create a list using for loop
# Def: the function that is similar to SAS macro
# 
# Demo
# Put it all together. Use function, loop and dictionary to create a Chatbot that 
# does small talks!
# 
#############################################################################################
#
# # BEFORE TO START a quick example of the logic of an IF-ELSE statement:
# if(condition) {
#   statement
# } else {
#   statement
# }
#
# 1) Loops: While
# In R programming, while loops are used to loop until a specific condition is met.
# Syntax of while loop
# 
# while (test_expression)
# {
#   statement
# }
#
# Here, test_expression is evaluated and the body of the loop is entered if the result is TRUE.
# The statements inside the loop are executed and the flow returns to evaluate the test_expression again.
# This is repeated each time until test_expression evaluates to FALSE, in which case, the loop exits.

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

# Let's break the loop when x=3:
x <- 1
while(x < 5) {x <- x+1; if (x == 3) break; print(x); }
## [1] 2


# Let's skip one step when x=3:
x <- 1
while(x < 5) {x <- x+1; if (x == 3) next; print(x);}
## [1] 2
## [1] 4
## [1] 5

# 2) Loops: for
# For-loops provide a way to iterate over objects.
# LOGIC:
# for(value in sequence){
#   statement
# }
# the statement is executed for the number of iterations.

numbers <- 1:10
for (i in numbers) {
  print(i)
}
## [1] 1
## [1] 2
## [1] 3
## [1] 4
## [1] 5
## [1] 6
## [1] 7
## [1] 8
## [1] 9
## [1] 10

# You can use a for-loop in the same way with a character vector:
charvec <- c('first element', 'second element', 'third element')
for (i in charvec) {
  print(i)
}
## [1] "first element"
## [1] "second element"
## [1] "third element"

# You can populate objects with for loops
my_list <- list()
for (i in seq_along(charvec)) {
  my_list[i] <- charvec[i]
}
# NOTE:
# First, you create an empty list using my_list <- list(). This is the object that you will populate in the loop.
# the same happens if you want to populate a matrix with a for loop.

# NESTED FOR LOOPS
# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)

# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# 3) Loops: BREAK and NEXT
# It can be used to break out of a for loop completely by using the break statement or skip an iteration
# using the next statement.
#. 
# for (value in sequence) {
#   if(next_condition) {
#     next
#   }
#   code
#   if(breaking_condition) {
#     break
#   }
# }

#matrix
m <- matrix(1:9, ncol = 3, nrow = 3)
# I insert one NA in it
m[2,1]<-NA


for (i in 1:nrow(m)) {
  if(is.na(m[i,1])) {
    print("Skipping NA")
    next
  }
  
  if(m[i,1] >= 3) {
    print("Time to sell!")
    break
  } else {
    print("loop continues")
  }
}

#ANOTHER EXAMPLE:
# Make a lower triangular matrix (zeroes in upper right corner)
m=10 
n=10
# A counter to count the assignment
ctr=0
# Create a 10 x 10 matrix with zeroes 
mymat = matrix(0,m,n)

for(i in 1:m) {
  for(j in 1:n) {   
    if(i==j) { 
      break;
    } else {
      # you assign the values only when i<>j
      mymat[i,j] = i*j
      ctr=ctr+1
    }
  }
  print(i*j) 
}

# 4) Loops in big data tables: APPLY
# For Loops and while can be slow on big datasets.
# for this reasons it can be useful to use APPLY functions.
# They are a sort of hidden loops. It is populated with a number of functions:
# a) apply
# b) lapply
# c) sapply
# d) rapply
# e) tapply
# f) vapply
# g) mapply
# 
# to manipulate slices of data in the form of matrices or arrays in a repetitive way, allowing to cross or
# traverse the data and avoiding explicit use of loop constructs.
# 
# Related functions are sweep(), by() and aggregate() and are occasionally used in conjunction with the elements of the apply() family.
#                                                                   
# 4a) The most frequently used is:
#     apply()
#  You want to apply a given function to the rows (index "1") or columns (index "2") of a matrix.
 # Given a matrix M, the call:
 # - apply(M,1,fun) - apply(M, 2,fun) 
 # will apply the specified function fun to the rows of M, if 1 is specified; 
 # or to the columns of M, when 2, is specified. This numeric argument is called "margin" and it is limited to the values 1 and 2 because 
 # the function operates on a matrix. However, you could have an array with up to 8 dimensions instead.
# EX:
# define matrix `mymat` by replicating the sequence `1:5` for `4` times and transforming into a matrix
mymat<-matrix(rep(seq(5), 4), ncol = 5)
# `mymat` sum on rows
apply(mymat, 1, sum)
# `mymat` sum on columns
apply(mymat, 2, sum)
# With user defined function within the apply that adds any number `y` to the sum of the row 
# `y` is set at `4.5` 
apply(mymat, 1, function(x, y) sum(x) + y, y=4.5)
# Or produce a summary column wise for each column
apply(mymat, 2, function(x, y) summary(mymat))

# 4b)lapply()
#  You want to apply a given function to every element of a list and obtain a list as a result (which explains 
#  the "l" in the function name). Read up on this function here.
#                                                                   
# 4c)sapply()
#  You want to apply a given function to every element of a list but you wish to obtain a vector rather than a list.

#########################################################################################################

# EXERCISE 1
# split Shawbrook DB in training set and test set
# split DB names in categorical and numeric variables
#
#SOL 1
pathData<-"C:\\Users\\AlbertoSerafini\\OneDrive - 4Most\\Machine Learning\\Shawbrook Bank DB\\master_data.csv"
MyData_sample = read.csv(pathData, nrows= 10000)

# split db in TRAINING SET and TEST SET
## 75% of the sample size
smp_size <- floor(0.75 * nrow(MyData_sample))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(MyData_sample)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

cat_var <- names(train)[which(sapply(train, is.character))]
num_var <- names(train)[which(sapply(train, is.numeric))]

# EXERCISE 2
# manually change some statuses to NA
# SOL 2
# ifelse(condition, result if TRUE, result if FALSE)
train_na <- as.data.frame(sapply(train,
                                 function(x){ifelse(x %in%c("","_","__", "M", "NULL", "___","____", "C",
                                                            "_____","______",
                                                            "_______","________","_________",
                                                            "T", "H", "F", "G", "K",
                                                            "P", "E", "I"), NA, x)}))
                                                                  
########################################################################################################

# EXERCISE 3:
# Chat Bot 1.0: Echo Bot¶
# The bot responds what you say to it

#SOL 3
fun <- function(){
  x <- readline("What is the value of x?")  
  y <- readline("What is the value of y?")
  t <- readline("What are the T values?")
  v <- readline("What are the V values?")
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  t <- as.numeric(unlist(strsplit(t, ",")))
  v <- as.numeric(unlist(strsplit(v, ",")))
  
  out1 <- x + y
  out2 <- t + v
  
  return(list(out1, out2))
}

fun()
                                                                  
# EXERCISE 4:
# Chat Bot 2: Add variety
# Lets chat the bot (like 2 people speaking). It can  always give the same answer to the same question. 
# Let's make it more human by adding variety to the responses to the same question.

#SOL 4
fun2 <- function(){
  Q1<-"What is your name?"
  x<-c("Alberto","my name is Alberto", "Mr. Serafini")
  A1<-sample(x,1)
  return(list(Q1, A1))
}

fun2()

# EXERCISE 5
# What if the question is not in the bot's dictionary, can it still return an answer? Let's detect whether 
# the message is a question or statement, and return a random response.

#SOL 5
fun3 <- function(){
  y<-c("What is the weather today?","I love painting")
  Q1<-sample(y,1)
  x1<-c("Really sunny","don't know", "It is sunny")
  A1<-sample(x1,1)
  x2<-c("great! Let me know more about it","I find that extremely interesting")
  A2<-sample(x2,1)
  
  if (substr(Q1,nchar(Q1),nchar(Q1))=="?"){
    return(list(Q1, A1))
  }else{
    return(list(Q1, A2))
  }
}

fun3()