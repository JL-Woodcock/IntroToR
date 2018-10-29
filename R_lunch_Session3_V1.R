# Exploratory Data Analysis

###################################################################################

# Import file
pathData<-"C:\\Users\\AlbertoSerafini\\OneDrive - 4Most\\Machine Learning\\Shawbrook Bank DB\\master_data.csv"
MyData_sample = read.csv(pathData, nrows= 10000)
Data_test<-MyData_sample[905:986]

smp_size <- floor(0.75 * nrow(MyData_sample))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(MyData_sample)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

train_na <- as.data.frame(sapply(train,
                                 function(x){ifelse(x %in%c("","_","__", "M", "NULL", "___","____", "C",
                                                            "_____","______",
                                                            "_______","________","_________",
                                                            "T", "H", "F", "G", "K",
                                                            "P", "E", "I"), NA, x)}))

# all changed to factors, so numeric needs to be brought back
num_var_1 <- sapply(train_na,function(x){sum(is.na(as.numeric(levels(unique(x)))))==0})
num_var_2 <- sapply(train_na,function(x){length(levels(x))>2})
num_var <- colnames(train_na)[num_var_1&num_var_2]
train_na[,num_var] <- sapply(train_na[,num_var], function(x)as.numeric(x))
cat_var <- names(train_na)[which(sapply(train_na, is.factor))]
num_var <- names(train_na)[which(sapply(train_na, is.numeric))]

###################################################################################

#add one column to the data frame
train_na$newcolumn<-NA

# OR data.table library can be really useful for data frame manipulation
# install.packages("data.table")
library(data.table)
#this step is important in order to use this library
dt <- data.table(train_na)
# count rows, or do the mean, etc.
dt[, .N, by=cyl]
#add a column
dt[,newcolumn2 := NA]
#remove a column
dt[,newcolumn2 := NULL]

# returns all columns except mpg and cyl
test <- dt[, !c("mpg", "cyl")]
#before the "," you can add a condition (ex: col==1)

# Other options
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

###################################################################################

# if-then condition:
# LOGIC:  tab[if,the, group by]
test[is.na(carb),newcolumn2:=1]

#Describe the numeric dataframe columns:
x1<-summary(MyData_sample[,1])
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 182900  214800  312000  281600  331900  339800 
x2<-as.matrix(t(x1))

#get number of rows and columns
dim(MyData_sample)
nrow(MyData_sample)
ncol(MyData_sample)
# list the variables in mydata
names(MyData_sample)
# list the structure of mydata
str(MyData_sample[,1])

###################################################################################

# EDA: Plot a variable
#1) Simple Scatterplot
attach(MyData_sample)
plot(ApplicationScore, Age, main="Scatterplot Example",
     xlab="Application Score ", ylab="Age ")
# Add fit lines
abline(lm(ApplicationScore~Age), col="red") # regression line (y~x)
lines(lowess(ApplicationScore,Age), col="blue") # lowess line (x,y) 

# The scatterplot( ) function in the car package offers many enhanced features, 
# including fit lines, marginal box plots, conditioning on a factor, and interactive 
# point identification. Each of these features is optional.
# Enhanced Scatterplot of ApplicationScore vs. Age
# by LandlineFlag
# install.packages("car")
library(car)
scatterplot(ApplicationScore ~ Age | LandlineFlag, data=MyData_sample,
            xlab="Age", ylab="Application Score",
            main="Enhanced Scatter Plot")

# There are at least 4 useful functions for creating scatterplot matrices. Analysts must
# love scatterplot matrices!
  # Basic Scatterplot Matrix
pairs(~ApplicationScore+Age+LandlineFlag+OriginalTerm,data=MyData_sample,
        main="Simple Scatterplot Matrix")

# High Density Scatterplots
# When there are many data points and significant overlap, scatterplots become less useful. 
# There are several approaches that be used when this occurs. The hexbin(x, y) function in the
# hexbin package provides bivariate binning into hexagonal cells (it looks better than it sounds).

# High Density Scatterplot with Binning
# install.packages("hexbin")
library(hexbin)
bin<-hexbin(MyData_sample$ApplicationScore, MyData_sample$Age, xbins=50)
plot(bin, main="Hexagonal Binning") 

# 2) Histogram
hist(MyData_sample$ApplicationScore,breaks=12, col="red")
# Add a Normal Curve (Thanks to Peter Dalgaard)
# I need to delete the NA
ApplicationScore<-ifelse(is.na(MyData_sample$ApplicationScore),0,MyData_sample$ApplicationScore)
x <- ApplicationScore
h<-hist(x, breaks=10, col="red", xlab="Application Score",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

# Kernel Density Plot
# Kernal density plots are usually a much more effective way to view the distribution of a variable.
# Create the plot using plot(density(x)) where x is a numeric vector.
d <- density(x) # returns the density data
# x does not have to contain missing values (NA)
plot(d, main="Kernel Density") # plots the results
polygon(d, col="red", border="blue") 

# Get the frequency count of a categorical column
# install.packages("plyr")
library(plyr)
count(MyData_sample, 'ApplicationScore')

#CORRELATION;
# select some variables in order to compute the correlation
MyData_sample1<-cbind(MyData_sample$ApplicationScore,MyData_sample$Age,MyData_sample$MonthlyRepaymentAmount)
MyData_sample1<-na.omit(MyData_sample1)
colnames(MyData_sample1)<-c("ApplicationScore","Age","MonthlyRepaymentAmount")
# Compute correlation matrix
res <- cor(MyData_sample1)
round(res, 2)

###################################################################################

# GGPLOT2 library:
# You can use the ggplot in order to do more colorful and beautiful graphs
# http://www.sthda.com/english/wiki/ggplot2-histogram-easy-histogram-graph-with-ggplot2-r-package

# 1) Scatter plot
# install.packages("hexbin")
# install.packages("ggplot2")
library(hexbin)
library(ggplot2)
options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Scatterplot: useful to analyse correlation
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + #select elements you need to analyse
  geom_point(aes(col=state, size=popdensity)) + #group by the "state" and "popdensity" -> it will five different size/colour to the scatters
  geom_smooth(method="loess", se=F) +    #type of graph
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)

# 2) Scatterplot With Encircling:
# useful to analyse correlation AND analyse particular values
options(scipen = 999)
# install.packages("Rttf2pt1")
# install.packages("ggalt")
library(ggplot2)
library(Rttf2pt1)
library(ggalt)

midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]

# Plot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # draw smoothing line
  geom_encircle(aes(x=area, y=poptotal),    #select data
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # encircle
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")

# 3) Animated Scatter plot (Not for R version 3.3.3)
# Source: https://github.com/dgrtwo/gganimate
# devtools::install_github("dgrtwo/gganimate")
# install.packages("cowplot")  # a gganimate dependency
# install.packages("gganimate")
# install.packages("gapminder")
library(ggplot2)
library(gganimate)
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, frame = year)) +   # plot is anymated for the year
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale


# 4) Correlogram
# devtools::install_github("kassambara/ggcorrplot")
# install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)

# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)
gganimate(g, interval=0.2)

# 5) Diverging Bars from the average
library(ggplot2)
theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

# 6) Ordered Bar Charts
# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)
# The X variable is now a factor, let's plot.
library(ggplot2)
theme_set(theme_bw())
# Draw plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Histogram on continuous variables
library(ggplot2)
theme_set(theme_classic())

# 7) Histogram on a Continuous (Numeric) Variable
g <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 

# 8) Density plot
library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")

# Others
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
