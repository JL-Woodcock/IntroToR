# LunchR workshop - Data types and data structures
#
# Contents: data types and data structures
#
#########################################################################################################
#
# R native data types:
# - Numeric
# - Integer
# - Complex
# - Logical
# - Character
#
# Data structures: 
# 1) atomic vector
# 2) matrix
# 3) array
# 4) list
# 5) data frame
#
# 1) vectors can be:
#    atomic vectors: ALL elements of the same type
dbl_var <- c(1, 2.5, 4.5)
typeof(dbl_var)
## [1] "double"
is.double(dbl_var)
## [1] TRUE
is.atomic(dbl_var)
## [1] TRUE
# NB: is.numeric() is a general test for the "numberliness" of a vector
is.numeric(dbl_var)
## [1] TRUE

db2_var<-c(1, c(2, c(3, 4)))   #Atomic vectors are always flat
# With the L suffix, you get an integer rather than a double
int_var <- c(1L, 6L, 10L)
typeof(int_var)
## [1] "integer"
is.integer(int_var)
## [1] TRUE
is.atomic(int_var)
## [1] TRUE

# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

# 4) vectors can be also LISTs:
#    NB: is.vector(vector_name)   test if is a vector with NO attributes
#                                 otherwise you should use is.atomic(vector_name) or is.list(vector_name)
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)
## List of 4
##  $ : int [1:3] 1 2 3
##  $ : chr "a"
##  $ : logi [1:3] TRUE FALSE TRUE
##  $ : num [1:2] 2.3 5.9

# lists can be recursive:
x <- list(list(list(list())))
str(x)
## List of 1
##  $ :List of 1
##   ..$ :List of 1
##   .. ..$ : list()
is.recursive(x)
## [1] TRUE
is.list(x)
## [1] TRUE

# Lists are used to build up many of the more complicated data structures in R.
# For example, both data frames (described in data frames) and linear models objects (as produced by lm()) are lists:

# 3) ARRAY: it is an atomic vector with a dimensional attribute 
#    created with array() or dim()
# One vector argument to describe all dimensions
b <- array(1:12, c(2, 3, 2))
#    array(...., c(n_row,n_col, n_dimensions))
## , , 1
##
## [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
##
## , , 2
## 
## [,1] [,2] [,3]
## [1,]    7    9   11
## [2,]    8   10   12
##
# you can change names or give the names or check the length or dimension of the matrices:
length(b)
## [1] 12
dim(b)
## [1] 2 3 2

dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
## , , A
## 
##     a b c
## one 1 3 5
## two 2 4 6
## 
## , , B
## 
##     a  b  c
## one 7  9 11
## two 8 10 12


# 2) MATRIX: it is a special case of array with 2 dimensions
#    created with matrix() or dim()
# Two scalar arguments to specify rows and columns
a <- matrix(1:6, ncol = 3, nrow = 2)
# or
c <- 1:6
dim(c) <- c(3, 2)
##      [,1] [,2]
## [1,]    1    4
## [2,]    2    5
## [3,]    3    6

dim(c) <- c(2, 3)
##      [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6

# you can change names or give the names or check the length or dimension of the matrices:
length(a)
## [1] 6
nrow(a)
## [1] 2
ncol(a)
## [1] 3
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
##   a b c
## A 1 3 5
## B 2 4 6

# You can test if an object is a matrix or array using
# is.matrix() and is.array(), or by looking at the length
# of the dim(). as.matrix() and as.array() make it easy
# to turn an existing vector into a matrix or array.

# in the matrix or vectors you can select a value or a set of values in a certain position
a[1,2]<-    #I assign the value 5 to the cell (row 1,col 2) of the matrix a
a[1,] # select row 1
a[,1] # select column 1
a[1,2:3] # select row 1 from column 2 to 3

# 5) DATA FRAME
# it is the most common way to store big data in R and make the analysis easier. 
# It is a 2-dimensional structure and share properties with the matrix. 
# This means that a data frame has names(), colnames(), and rownames(), although
# names() and colnames() are the same thing. The length() of a data frame is the 
# length of the underlying list and so is the same as ncol(); nrow() gives the number
# of rows. 

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
## 'data.frame':    3 obs. of  2 variables:
##  $ x: int  1 2 3
##  $ y: Factor w/ 3 levels "a","b","c": 1 2 3

# A FACTOR is a vector that can contain only predefined values, and is used to store
# categorical data. Factors are built on top of integer vectors using two attributes: 
#   the class, "factor", which makes them behave differently from regular integer vectors, 
# and the levels, which defines the set of allowed values.
sex <- factor(c("M", "F"))

# Beware data.frame()'s default behaviour which turns strings into factors. 
# Use stringsAsFactors = FALSE to suppress this behaviour:
  df <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE)
str(df)
## 'data.frame':    3 obs. of  2 variables:
##  $ x: int  1 2 3
##  $ y: chr  "a" "b" "c"

# You can also COMBINE DATA FRAMES
cbind(df, data.frame(z = 3:1))
##   x y z
## 1 1 a 3
## 2 2 b 2
## 3 3 c 1
rbind(df, data.frame(x = 10, y = "z"))
##    x y
## 1  1 a
## 2  2 b
## 3  3 c
## 4 10 z

# IN GENERAL you can map your variables with commands:
#   - as.numeric()
#   - as.matrix()
#   - ....
df_matr<-as.matrix(df)

#########################################################################################################

# EXERCISE
# EXERCISE 1: DOWNLOAD shawbrook DB and select just the from column 905 to 986 and the first 10k rows:

# SOL1:
pathData<-"C:\\Users\\AlbertoSerafini\\OneDrive - 4Most\\Machine Learning\\Shawbrook Bank DB\\master_data.csv"
MyData_sample = read.csv(pathData, nrows= 10000)
Data_test<-MyData_sample[905:986]


# EXERCISE 2:
# check the class of the dataset and modifiy it in a matrix
# check the names of the columns
# check the class of each column
# download from csv all the values of the columns [905:986]

# SOL2:
class(Data_test)
data_test2<-as.matrix(Data_test)
cols_check = colnames(MyData_sample)

# Since I have to analyse columns from 905 to 986, I remove all the other values
cols_check[1:904] = "NULL"
cols_check[987:ncol(MyData_sample)] = "NULL"
cols_check[905:986] = sapply(MyData_sample[905:986], class)

MyData = read.csv(pathData, colClasses = cols_check)
  



