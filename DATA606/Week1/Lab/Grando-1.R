#Source cdc data
setwd("~/Documents/Masters/DATA606/Week1/Lab/Lab1")
source("more/cdc.R")
require(ggplot2)
#Question 1 - Make a scatterplot of weight versus desired weight. Describe the relationship between these two variables.
ggplot(cdc, aes(y=wtdesire, x=weight)) + geom_point() + labs(y="desired weight")
#It appears that there is a positive linear correlation between weight and desired weight

#Question 2 - Let’s consider a new variable: the difference between desired weight (wtdesire) and current weight (weight). Create this new variable by subtracting the two columns in the data frame and assigning them to a new object called wdiff.
cdc$wdiff <- cdc$weight - cdc$wtdesire

#Question 3 - What type of data is wdiff? If an observation wdiff is 0, what does this mean about the person’s weight and desired weight. What if wdiff is positive or negative?
#wdiff is numerical data because performing arithmetic operations on this data would still have meaning.
#If an observation of wdiff is 0, then that indicates a person is at their desired weight.
#Since I have subtracted desired weight from weight, a positive value would indicate that a person is heavier than they wish to be.  A negative value would indicate a person is lighter than they wish to be.

#Question 4 - Describe the distribution of wdiff in terms of its center, shape, and spread, including any plots you use. What does this tell us about how people feel about their current weight?
summary(cdc$wdiff)
ggplot(cdc, aes(y=wdiff, x=1)) + geom_boxplot()
hist(cdc$wdiff)
#There are some extreme values (minimum 0f -500, maximum of 300), which hinder us from properly viewing the data shape, so let's check the top and bottom 10 values
require(plyr)
head(arrange(cdc, wdiff))
tail(arrange(cdc, wdiff))
#It looks like the tail of wdiff is pretty valid but the head has a pretty drastic jump from -110 to -311 and -500, so here are plots excluding those two values
ggplot(cdc, aes(y=wdiff, x=1)) + geom_boxplot() + scale_y_continuous(limits=c(-110,300))
ggplot(cdc, aes(x=wdiff)) + geom_histogram(binwidth = 10) + scale_x_continuous(limits=c(-110,300))
#There appears to still be a significant number of outlying entries and it does not appear the data is normally distributed.
#The data appears to tell us that people generally believe their ideal weight to be less than their current weight

#Question 5 - Using numerical summaries and a side-by-side box plot, determine if men tend to view their weight differently than women.
cdcm <- subset(cdc, gender=="m")
cdcf <- subset(cdc, gender=="f")
summary(cdcm$wdiff)
summary(cdcf$wdiff)
ggplot(cdc,aes(y=wdiff,x=gender)) + geom_boxplot()
#It's hard to tell anything from the boxplots without excluding the major outliers, so i'll remove the previously mentioned values
ggplot(cdc,aes(y=wdiff,x=gender)) + geom_boxplot() + scale_y_continuous(limits=c(-110,300))
#The values are pretty close, but it appears that females may generally have a larger difference between their weight and desired weight, with the latter being lower.

#Question 6 - Now it’s time to get creative. Find the mean and standard deviation of weight and determine what proportion of the weights are within one standard deviation of the mean.
mean(cdc$weight)
sd(cdc$weight)
#14,152 people are within one standard deviation of the mean of the data set which is 70.76%

testdata <- subset(cdc, mean(cdc$weight)+sd(cdc$weight) > cdc$weight & mean(cdc$weight)-sd(cdc$weight) < cdc$weight)

sum(mean(cdc$weight)+sd(cdc$weight) > cdc$weight & mean(cdc$weight)-sd(cdc$weight) < cdc$weight)
nrow(cdc)
sum(mean(cdc$weight)+sd(cdc$weight) > cdc$weight & mean(cdc$weight)-sd(cdc$weight) < cdc$weight) / nrow(cdc)


