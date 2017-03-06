eedataurl <- "https://github.com/john-grando/Masters/raw/master/Workshop/R/Week3/assignment/ENB2012_data.csv"
eedata <- read.csv(file = eedataurl, 
                   header = TRUE, sep <- ",", 
                   #colClasses = c("numeric","numeric","numeric","numeric", "numeric", "character", "numeric", "character", "numeric", "numeric"),
                   colClasses = c("character","character","character","character", "character", "character", "character", "character", "numeric", "numeric"),
                   col.names = c("relative.compactness","surface.area","wall.area","roof.area","overall.height","orientation","glazing.area","glazing.area.distribution","heating.load","cooling.load"), 
                   stringsAsFactors = FALSE)
aggcheck <- aggregate(cbind(cooling.load,heating.load) ~ glazing.area, data = eedata, mean)
require(ggplot2)
ggplot(eedata, aes(x = cooling.load)) + geom_histogram(binwidth = 1)
ggplot(eedata, aes(x = heating.load)) + geom_histogram(binwidth = 1)

eedatanumeric <- read.csv(file = eedataurl, 
                          header = TRUE, sep <- ",", 
                          colClasses = c("numeric","numeric","numeric","numeric","numeric","character","numeric","character","numeric","numeric"),
                          col.names = c("relative.compactness","surface.area","wall.area","roof.area","overall.height","orientation","glazing.area","glazing.area.distribution","heating.load","cooling.load"), 
                          stringsAsFactors = FALSE)

#From the data provided, it appears that glazing area is actually the glazing ratio.  We can then calculate an extra column which gives the actual glazing area.
eedata$glazing.area.actual <- formatC(eedatanumeric$glazing.area * eedatanumeric$wall.area)
eedatanumeric$glazing.area.actual <- eedatanumeric$glazing.area * eedatanumeric$wall.area

#To start, I take a look at the histograms of the two response variables in this study
require(ggplot2)
ggplot(eedata, aes(x = cooling.load)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
ggplot(eedata, aes(x = heating.load)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
#Upon inspection, it appears both of these response variables are bimodal.  Therefore, I will check whether there are any inputs influencing this shape.

testlm <- lm(cooling.load ~ relative.compactness, data = eedatanumeric)
ggplot(eedata, aes(x = cooling.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ surface.area, data = eedatanumeric)
ggplot(eedata, aes(x = cooling.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ roof.area, data = eedatanumeric)
ggplot(eedata, aes(x = cooling.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ overall.height, data = eedatanumeric)
ggplot(eedata, aes(x = cooling.load, fill = overall.height)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
ggplot(eedata, aes(y = cooling.load, x = roof.area)) + geom_boxplot() + labs(x = "roof area", y = "cooling load")

#Test the heating load
testlm <- lm(heating.load ~ relative.compactness, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ surface.area, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ wall.area, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ glazing.area, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ glazing.area.actual, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = glazing.area.actual)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")


#similaraly, the heating response variable is greatly impacted by the overall.height.
testlm <- lm(heating.load ~ overall.height, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = overall.height)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
ggplot(eedata, aes(y = heating.load, x = overall.height)) + geom_boxplot() + labs(x = "overall height", y = "cooling load")
#However, roof area is also a significant factor as well
testlm <- lm(heating.load ~ roof.area, data = eedatanumeric)
summary(testlm)
ggplot(eedata, aes(x = heating.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
ggplot(eedata, aes(y = heating.load, x = roof.area)) + geom_boxplot() + labs(x = "overall height", y = "heating load")

#However, we see there only one roof area applied to buildings with a 3.50 overall height and it appears to correlate to a much lower heating and cooling load
ggplot(eedata, aes(x = cooling.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~overall.height)
ggplot(eedata, aes(x = heating.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load") + facet_wrap(~overall.height)

#let's split this data into two new tables where one is for 3.50 tall buildings and the other is for 7.0 tall buildings; I'm assuming the unit is meters from here on out.
eedata3.50 <- subset(eedata, eedata$overall.height == "3.50")
eedata3.50numeric <- subset(eedatanumeric, eedata$overall.height < 6)
eedata7.00 <- subset(eedata, eedata$overall.height == "7.00")
eedata7.00numeric <- subset(eedatanumeric, eedata$overall.height > 4)

#Look at the 3.50 histogram
ggplot(eedata3.50, aes(x = cooling.load)) + geom_histogram(binwidth = 1)
ggplot(eedata3.50, aes(x = heating.load)) + geom_histogram(binwidth = 1)

# Perform a cooling analysis on the 3.50m tall buildings
testlm <- lm(cooling.load ~ relative.compactness + surface.area + wall.area + glazing.area, data = eedata3.50numeric)
summary(testlm)
testlm <- lm(cooling.load ~ relative.compactness, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = cooling.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ surface.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = cooling.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ wall.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = cooling.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ glazing.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = cooling.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")



testlm <- lm(cooling.load ~ glazing.area.actual, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = cooling.load, fill = glazing.area.actual)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
#Glazing area appears to have the bigges impact on the cooling response variable in this subset of data, but not by much
ggplot(eedata3.50, aes(x = cooling.load)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~glazing.area)
#When a facet wrap is applied, it can be seen that wall area plays a factor in the cooling load as well.  It appears that higher glazing ratios and larger surface areas are correlated to larger cooling loads; however, not as much as the heating loads which is outlined next.
ggplot(eedata3.50, aes(x = cooling.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~glazing.area)
#reverse wrapping
ggplot(eedata3.50, aes(x = cooling.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~wall.area)

#Test the two parameters with each other
testlm <- lm(cooling.load ~ glazing.area * wall.area, data = eedatanumeric)
summary(testlm)

#Perform a heating analysis on the 3.50m tall buildings.
testlm <- lm(heating.load ~ relative.compactness + surface.area + wall.area + glazing.area, data = eedata3.50numeric)
summary(testlm)
testlm <- lm(heating.load ~ relative.compactness, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = heating.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ surface.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = heating.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ wall.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = heating.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
#Glazing area appears to have the biggest impact on the heating response variable in this subset of data, but not by much
testlm <- lm(heating.load ~ glazing.area, data = eedata3.50numeric)
summary(testlm)
ggplot(eedata3.50, aes(x = heating.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
#When a facet wrap is applied, it can be seen that wall area contributes to the bimodal distribution of this subset.  It appears that higher glazing ratios and larger surface areas are correlated to larger heating loads.
ggplot(eedata3.50, aes(x = heating.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load") + facet_wrap(~glazing.area)
#reverse wrapping
ggplot(eedata3.50, aes(x = heating.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load") + facet_wrap(~wall.area)

#Test the two parameters with each other
testlm <- lm(heating.load ~ glazing.area * wall.area, data = eedatanumeric)
summary(testlm)

#Look at the 7.0 Histogram
ggplot(eedata7.00, aes(x = cooling.load)) + geom_histogram(binwidth = 1)
ggplot(eedata7.00, aes(x = heating.load)) + geom_histogram(binwidth = 1)

#Perform a cooling analysis on the 7.0m buildings
testlm <- lm(cooling.load ~ relative.compactness + surface.area + wall.area + glazing.area, data = eedata7.00numeric)
summary(testlm)
testlm <- lm(cooling.load ~ relative.compactness, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = cooling.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ surface.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = cooling.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ wall.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = cooling.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
testlm <- lm(cooling.load ~ roof.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = cooling.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
#Glazing area doesn't appear to have the same level of impact on the 7.0m buildings but these are separated to be consistent with the 3.50m findings.
testlm <- lm(cooling.load ~ glazing.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = cooling.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
ggplot(eedata7.00, aes(x = cooling.load)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~glazing.area)
#facet wrap
ggplot(eedata7.00, aes(x = cooling.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~glazing.area)
#reverse wrapping
ggplot(eedata7.00, aes(x = cooling.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~wall.area)


#Perform a heating analysis on the 7.0m buildings
testlm <- lm(heating.load ~ relative.compactness + surface.area + wall.area + glazing.area, data = eedata7.00numeric)
summary(testlm)
testlm <- lm(heating.load ~ relative.compactness, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = heating.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ surface.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = heating.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ wall.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = heating.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
testlm <- lm(heating.load ~ roof.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = heating.load, fill = roof.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load")
#Glazing area doesn't appear to have the same level of impact on the 7.0m buildings; however, for the heating response variable, it's a little closer.
testlm <- lm(heating.load ~ glazing.area, data = eedata7.00numeric)
summary(testlm)
ggplot(eedata7.00, aes(x = heating.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load")
#facet wrap
ggplot(eedata7.00, aes(x = heating.load, fill = wall.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load") + facet_wrap(~glazing.area)
#reverse wrapping
ggplot(eedata7.00, aes(x = heating.load, fill = glazing.area)) + geom_histogram(binwidth = 1) + labs(x = "heating load") + facet_wrap(~wall.area)

#Cooling - scatter plot separating 3.50 vs. 7 meter buildings by surface area and glazing ratio... if 7.0 analysis shows the same.
ggplot(eedatanumeric, aes(y = cooling.load, x = wall.area)) + geom_point(aes(color = glazing.area)) + facet_wrap(~overall.height)
ggplot(eedatanumeric, aes(y = cooling.load, x = glazing.area)) + geom_point(aes(color = wall.area)) + facet_wrap(~overall.height)
#Confirmation of glazing ratio and wall area result in higher cooling loads
ggplot(eedatanumeric, aes(y = cooling.load, x = glazing.area)) + geom_point(aes(color = glazing.area.actual)) + facet_wrap(~overall.height)

ggplot(eedata, aes(x = cooling.load, fill = relative.compactness)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~surface.area)
ggplot(eedata, aes(x = cooling.load, fill = surface.area)) + geom_histogram(binwidth = 1) + labs(x = "cooling load") + facet_wrap(~relative.compactness)
