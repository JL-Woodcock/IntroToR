# LunchPy - Session 4 - Feature Selection
# 
# When you are working with high-dimensional datasets, is there any rule of thumb that can be easily applied to 
# bring the number of variables down?
# This session is walk you through how to reduce the number of variables by 30% on Shawbrook data, and also look 
# at some starter codes to demonstrate a few feature selection techniques.
# 
# Contents
# Rules of Thumb (1,037 -> 705 columns)
# Dive into object columns (829 -> 687 columns)
# Dive into numeric columns (21 -> 18 columns)
# Sklearn feature selection for univariate analysis (Demo on numeric data only)
# VarianceThreshold: Removing features with low variance. Discuss the limitation of this approach
# SelectKBest: Univariate feature selection. 
# Clustering (Demo on numeric data only)
# 
# Results could guide further feature selection within each cluster
# Tree Based Model Feature Importance (Demo uses the entire train data)
# Use Lightgbm as an example. Discuss the pros and cons of this approach.

###################################################################################

# Import file
pathData<-"C:\\Users\\AlbertoSerafini\\OneDrive - 4Most\\Machine Learning\\Shawbrook Bank DB\\master_data.csv"
MyData_sample = read.csv(pathData, nrows= 10000)
# Data_test<-MyData_sample[905:986]

# Identify the column names and values containing a certain string
str_tab_val<-MyData_sample[ , grepl( "app" , names( MyData_sample ) ) ]
## [1] JAN14 JAN14 JAN14 JAN14 ...
str_tab_names<-grep("app", names(MyData_sample), value=TRUE)
## [1] "app_month"
#  grepl returns logical vector like this which is what we use to subset columns
# grepl( "app" , names( MyData_sample ) )
## [1]  FALSE FALSE FALSE FALSE FALSE  TRUE FALSE ....

#Through a function
cols_fun<-function(cols){
assign(paste0(cols,"_val"),MyData_sample[ , grepl( cols , names( MyData_sample ) ) ] )
assign(paste0(cols,"_cols"), grep("app", names(MyData_sample), value=TRUE))
  }
cols_fun("app")

# Data quality and judgement based rules (1,037 -> 850 columns)
# ID/Date/Decision and other variables not suitable for scoring: 'Region', 'Age', etc. (20 columns)
# 100% missing values (54 columns)
# Duplicated columns (109 columns)
# One unique value (4 columns)

assign(paste0("Date","_cols"), grep("Date", names(MyData_sample), value=TRUE))
assign(paste0("id","_cols"), grep("id", names(MyData_sample), value=TRUE))
assign(paste0("ID","_cols"), grep("ID", names(MyData_sample), value=TRUE))
assign(paste0("proposalid","_cols"), grep("proposalid", names(MyData_sample), value=TRUE))
assign(paste0("Decision","_cols"), grep("Decision", names(MyData_sample), value=TRUE))
assign(paste0("Acc","_cols"), grep("Acc", names(MyData_sample), value=TRUE))
assign(paste0("Region","_cols"), grep("Region", names(MyData_sample), value=TRUE))
assign(paste0("PostCodeRegion","_cols"), grep("PostCodeRegion", names(MyData_sample), value=TRUE))
assign(paste0("Unnamed0","_cols"), grep("Unnamed: 0", names(MyData_sample), value=TRUE))
assign(paste0("Age","_cols"), grep("Age", names(MyData_sample), value=TRUE))
assign(paste0("app_month","_cols"), grep("app_month", names(MyData_sample), value=TRUE))
assign(paste0("APR","_cols"), grep("APR", names(MyData_sample), value=TRUE))
assign(paste0("MonthlyRepaymentAmount","_cols"), grep("MonthlyRepaymentAmount", names(MyData_sample), value=TRUE))
assign(paste0("ApplicationScore","_cols"), grep("ApplicationScore", names(MyData_sample), value=TRUE))
assign(paste0("CreatedOn","_cols"), grep("CreatedOn", names(MyData_sample), value=TRUE))

cols_to_drop<-c(Date_cols,id_cols,ID_cols,proposalid_cols,Decision_cols,Acc_cols,
                Region_cols, PostCodeRegion_cols,Unnamed0_cols,Age_cols,
                app_month_cols,APR_cols,MonthlyRepaymentAmount_cols,ApplicationScore_cols,
                CreatedOn_cols)

#drop this columns
drop1<-names.use <- names(MyData_sample)[!(names(MyData_sample) %in% cols_to_drop)]
MyData_sample1 <- MyData_sample[, drop1]

#columns that are completely missing have to be deleted
MyData_sample2 <- MyData_sample1[, colSums(is.na(MyData_sample1)) != nrow(MyData_sample1)]

# Drop duplicated columns
MyData_sample3<-MyData_sample2[!duplicated(as.list(MyData_sample2))]

# Drop columns with one value
MyData_sample4<-Filter(function(x)(length(unique(x))>1), MyData_sample3)

# Select num and char variables
num_var_1 <- sapply(MyData_sample4,function(x){sum(is.na(as.numeric(levels(unique(x)))))==0})
num_var_2 <- sapply(MyData_sample4,function(x){length(levels(x))>2})
num_var <- colnames(MyData_sample4)[num_var_1&num_var_2]
MyData_sample4[,num_var] <- sapply(MyData_sample4[,num_var], function(x)as.numeric(x))
cat_var <- names(MyData_sample4)[which(sapply(MyData_sample4, is.factor))]
num_var <- names(MyData_sample4)[which(sapply(MyData_sample4, is.numeric))]

# Drop values with more than 1 unique non-missing values but high concentration in one value 
# Identify the variables where the most frequent value is 95+%
# select only num var
MyData_sample_num <- MyData_sample4[, num_var]
# check frequency in every column
freq<-lapply(MyData_sample_num[, -1], function(x) as.data.frame(table(x)))

# install.packages("caret")
library(caret)
# get indices of data.frame columns (pixels) with low variance
badCols <- nearZeroVar(MyData_sample_num)
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(MyData_sample_num),4)))
# remove those "bad" columns from the training and cross-validation sets
MyData_sample5 <- MyData_sample4[, -badCols]
highvar_num <- MyData_sample_num[, badCols]

# Drop values with more than 1 unique non-missing values, no high concentration, but high missing rate
## Some sample data
## Remove columns with more than 50% NA
MyData_sample6<-MyData_sample5[, -which(colMeans(is.na(MyData_sample5)) > 0.5)]

# drop columns from data frame with less than 2 unique levels in R
MyData_sample7<-sapply(MyData_sample6, function(col) length(unique(col)))

# Profile the variables with low variance (TAB: highvar_num)
summary(highvar_num)
# 4 figures arranged in 2 rows and 2 columns
library(hexbin)
library(ggplot2)
attach(mtcars)
par(mfrow=c(3,3))
hist(highvar_num$Deposit,breaks=12, col="red", main="Deposit")
hist(highvar_num$MOB_0,breaks=12, col="red",, main="MOB_0")
hist(highvar_num$MOB_1,breaks=12, col="red",, main="MOB_1")
hist(highvar_num$MOB_2,breaks=12, col="red",, main="MOB_2")
hist(highvar_num$MOB_3,breaks=12, col="red",, main="MOB_3")
hist(highvar_num$MOB_4,breaks=12, col="red",, main="MOB_4")
hist(highvar_num$MOB_5,breaks=12, col="red",, main="MOB_5")
hist(highvar_num$MOB_6,breaks=12, col="red",, main="MOB_6")
hist(highvar_num$MOB_7,breaks=12, col="red",, main="MOB_7")
# with these analysis you can have the conclusion that a variable 
# is not to drop these two variables as they are meaningful and intuitive.

##################################################################

# Score:
# In statistics, the score, score function, efficient score[1] or informant[2] 
# indicates how sensitive a likelihood function L ( ?? ; X )
# is to its parameter ??\theta.
# Explicitly, the score  for ??\theta is the gradient of the log-likelihood with 
# respect to ??\theta.

# The score plays an important role in several aspects of inference. For example:
# in formulating a test statistic for a locally most powerful test;[3]
# in approximating the error in a maximum likelihood estimate;[4]
# in demonstrating the asymptotic sufficiency of a maximum likelihood estimate;[4]
# in the formulation of confidence intervals;[5]
# in demonstrations of the Cramér-Rao inequality.[6]

# z-score = (x-??)/??
# where:
# x is a raw score to be standardized; 
# ?? is the mean of the population; 
# ?? is the standard deviation of the population. 

install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")
library(R.basic)
scores<-apply(MyData_sample_num,2,zscore)

##################################################################

# get the p-values (not working)

b<-apply(MyData_sample_num[,c("Deposit")],2,shapiro.test)
do.call(rbind,lapply(b,function(v){v$p.value}))

# Get the raw p-values for each feature, and transform from p-values into scores
scores = -np.log10(selector.pvalues_)

# Plot the scores. 
plt.title('Score of the numeric variables')
pd.Series(scores, index = X_df_num.columns).sort_values(ascending = False).plot.bar()

##################################################################

# f_classif: ANOVA F-value between label/feature for classification tasks.
# Other methods for your reference:
#   mutual_info_regression: Mutual information for a continuous target.
# chi2: Chi-squared stats of non-negative features for classification tasks.
# SelectKBest: Select features based on the k highest scores.
# SelectFpr: Select features based on a false positive rate test.
# SelectFdr: Select features based on an estimated false discovery rate.
# SelectFwe: Select features based on family-wise error rate.
# SelectPercentile: Select features based on percentile of the highest scores.

##################################################################

# Clustering
# Use the same set of numeric variables for demo

# Check the correlation within a cluster

##################################################################

# Tree based Model Feature Importance
# Lightgbm for demo

# try to plot trees


# The importantant variables are mostly numeric columns. 
# It means the label encoding does not extract the information from categorical varibales. 
# Will try other ways to encode the categorical variables to improve it.

##################################################################

# Scorecard variables for reference
# BSC150: Credit Cards Category1 Credit limit last 1 month (live)
# BSC445: All Credit Cards Number of months since 2+
#   BTC435: All Credit Cards Utilisation (%) last 1 month (live) - 2nd Applicant
# FSC304: Fixed term accounts Mortgages and Secured Loans Number of accounts (live)
# LSC251: Age of oldest account (live)
# LSC617: Age in months of most recently opened account (all accounts)
# SSC4: Number of credit searches last 12 months
# BSC468: All Credit Cards Age of oldest account in months (live)
# FSC309: Fixed term accounts Mortgages and Secured Loans Monthly repayment amount (live)

##################################################################


