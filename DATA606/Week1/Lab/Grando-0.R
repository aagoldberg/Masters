#Source baptism count data
setwd("~/Documents/Masters/DATA606/Week1/Lab/Lab0")
source("more/arbuthnot.r")
#Source present day data
source("more/present.R")
require(ggplot2)
require(gridExtra)
#Question 1 - What years are included in this data set? What are the dimensions of the data frame and what are the variable or column names?
#What years are included in this data set?
levels(factor(present$year))
#What are the dimensions of the data frame and what are the variable or column names?
dim(x = present)
names(present)
#Question - 2 How do these counts compare to Arbuthnot’s? Are they on a similar scale?
present$total <- present$boys + present$girls
arbuthnot$total <- arbuthnot$boys + arbuthnot$girls
summary(arbuthnot$total)
summary(present$total)
plot1 <- ggplot(arbuthnot, aes(year)) + geom_line(aes(y=total)) + ggtitle("Arbuthnot data")
plot2 <- ggplot(present, aes(year)) + geom_line(aes(y=total)) + ggtitle("present data")
grid.arrange(plot1,plot2)

#No, huge population dip in Arbuthnot data between 1640 and 1660.  Much larger data set for present data (as can be seen from the data frame summary)

#Question - 3 Make a plot that displays the boy-to-girl ratio for every year in the data set. What do you see? Does Arbuthnot’s observation about boys being born in greater proportion than girls hold up in the U.S.? Include the plot in your response.
present$BGRatio <- present$boys / (present$girls + present$boys)
arbuthnot$BGRatio <- arbuthnot$boys / (arbuthnot$girls + arbuthnot$boys)
summary(arbuthnot$BGRatio)
summary(present$BGRatio)
plot1 <- ggplot(arbuthnot, aes(year)) + geom_line(aes(y=BGRatio)) + ggtitle("Arbuthnot data") + scale_y_continuous(limits=c(0.51, 0.54), breaks = seq(0.51, 0.54, by=.01)) + labs(y="Boy to girl ratio")
plot2 <- ggplot(present, aes(year)) + geom_line(aes(y=BGRatio)) + ggtitle("present data") + scale_y_continuous(limits=c(0.51,0.54), breaks = seq(0.51,0.54, by=.01)) + labs(y="Boy to girl ratio")
grid.arrange(plot1,plot2,ncol=2)

# Question 4 - In what year did we see the most total number of births in the U.S.? You can refer to the help files or the R reference card http://cran.r-project.org/doc/contrib/Short-refcard.pdf to find helpful commands.
present[which.max(present[,"total"]),]