if (Sys.info()["sysname"]=="Windows"){
  setwd("~/Masters/DATA606/DataProject")
} else {
  setwd("~/Documents/Masters/DATA606/DataProject")
}
fileurl = "PublicLEEDProjectDirectory.csv"
project_data <- read.csv(file = fileurl, header = TRUE, sep = ",", stringsAsFactors = FALSE)
svfileurl = "SummaryVersionsMap.csv"
sv_data <- read.csv(file = svfileurl, header = TRUE, sep = ",", stringsAsFactors = FALSE)
project_data$CertDate <- as.Date(project_data$CertDate,"%m/%d/%Y")
project_data$RegistrationDate <- as.Date(project_data$RegistrationDate,"%m/%d/%Y")
project_data$SubmittalTime <- as.numeric(project_data$CertDate - project_data$RegistrationDate)
project_data$SubmittalTimePending <- ifelse(is.na(project_data$CertDate)==TRUE, as.Date("3/2/2017","%m/%d/%Y")-project_data$RegistrationDate,NA)
project_data$RatingSystemFamily <- sapply(project_data$LEEDSystemVersionDisplayName, 
                                          function(x){
                                            x <- sv_data[which(sv_data$RatingSystem == x),2]
                                            x <- ifelse(identical(x, character(0)), character(0), as.character(x))
                                          }
                                          )
project_data$System <- sapply(project_data$LEEDSystemVersionDisplayName, 
                             function(x){
                               x <- sv_data[which(sv_data$RatingSystem == x),3]
                               x <- ifelse(identical(x, character(0)), character(0), as.character(x))
                             }
)
project_data$Version <- sapply(project_data$LEEDSystemVersionDisplayName, 
                              function(x){
                                x <- sv_data[which(sv_data$RatingSystem == x),4]
                                x <- ifelse(identical(x, character(0)), character(0), as.character(x))
                              }
)
project_data$Platform <- sapply(project_data$LEEDSystemVersionDisplayName, 
                              function(x){
                                x <- sv_data[which(sv_data$RatingSystem == x),5]
                                x <- ifelse(identical(x, character(0)), character(0), as.character(x))
                              }
)
project_data$RSLaunch <- sapply(project_data$LEEDSystemVersionDisplayName, 
                                function(x){
                                  x <- sv_data[which(sv_data$RatingSystem == x),6]
                                }
)
project_data$RSLaunch <- as.Date(as.character(project_data$RSLaunch),"%m/%d/%Y")
project_data$RSRegClose <- sapply(project_data$LEEDSystemVersionDisplayName, 
                                function(x){
                                  x <- sv_data[which(sv_data$RatingSystem == x),7]
                                }
)
project_data$RSRegClose <- as.Date(as.character(project_data$RSRegClose),"%m/%d/%Y")
project_data$RSSunset <- sapply(project_data$LEEDSystemVersionDisplayName, 
                                  function(x){
                                    x <- sv_data[which(sv_data$RatingSystem == x),8]
                                  }
)
project_data$RSSunset <- as.Date(as.character(project_data$RSSunset),"%m/%d/%Y")
project_data$RSIteration <- sapply(project_data$LEEDSystemVersionDisplayName, 
                                function(x){
                                  x <- sv_data[which(sv_data$RatingSystem == x),9]
                                }
)
project_data$RSIteration <- as.integer(as.character(project_data$RSIteration))

#Make subsection of 365 day periods within each RS
######maybe do this by Cert Date instead of registration date.
#####Ignore the last few years (about 3?)
project_data$RegAfterLaunch <- as.integer(project_data$RegistrationDate - project_data$RSLaunch)
project_data$CertAfterLaunch <- as.integer(project_data$CertDate - project_data$RSLaunch)
project_data$RegYear <- trunc(project_data$RegAfterLaunch / 365) + 1
project_data$CertYear <- trunc(project_data$CertAfterLaunch / 365) + 1

#date closures - check
#######Doubtful projects are being rushed... sunset date was 2015, maybe just make it 1,000 or so days before the sunset date

project_data$OwnerTypesAdj <- sapply(project_data$OwnerTypes, 
                                     function(x){
                                       x <- ifelse(grepl("government",tolower(x)),"government",
                                            ifelse(grepl("education",tolower(x)),"education",
                                            ifelse(grepl("confidential",tolower(x)),"confidential",
                                            ifelse(grepl("corporate",tolower(x)),"corporate",
                                            ifelse(grepl("^profit",tolower(x)),"profit",
                                            ifelse(grepl("^non-profit",tolower(x)),"non-profit",
                                            ifelse(grepl("individual",tolower(x)),"individual",
                                            ifelse(grepl("investor",tolower(x)),"investor",
                                            ifelse(x=="",NA,
                                            ifelse(is.null(x),NULL,
                                            "other"))))))))))
                                     }
)
table(project_data$OwnerTypesAdj)
table(subset(project_data$OwnerTypes, project_data$OwnerTypesAdj=="other"))

#v3 and v2 projects which were registered a maximum of the max v3 days after RSLaunch
maxv3days <- as.numeric(as.Date("3/8/2017","%m/%d/%Y")- as.Date("4/27/2009","%m/%d/%Y"))
v2cutoff <- as.Date("11/15/2000","%m/%d/%Y") + maxv3days
#v3 day comparison
#project_data$TimeAdj <- ifelse(project_data$Platform=="v2",v2cutoff - project_data$RegistrationDate, NA)
#project_data_reduc <- subset(project_data, project_data$Platform=="v3" | (project_data$Platform=="v2" & project_data$TimeAdj>0))
#project_data_reduc$SubmittalTimeAdj <- ifelse(project_data_reduc$Platform=="v2" & project_data_reduc$CertDate < v2cutoff, 
#                                              project_data_reduc$CertDate - project_data_reduc$RegistrationDate,
#                                              ifelse(project_data_reduc$Platform=="v3",
#                                                     project_data_reduc$SubmittalTime,
#                                                     NA))
#project_data_reduc$SubmittalTimePendingAdj <- ifelse(project_data_reduc$Platform=="v2" & (is.na(project_data_reduc$CertDate)==TRUE | project_data_reduc$CertDate > v2cutoff),
#                                                     v2cutoff-project_data_reduc$RegistrationDate,
#                                                     project_data_reduc$SubmittalTimePending)


####Test
project_data_reduc <- project_data
project_data_reduc$SubmittalTimePending <- ifelse(project_data$Platform=="v2" & project_data$SubmittalTimePending > maxv3days,maxv3days,project_data$SubmittalTimePending)
project_data_reduc$SubmittalTimePending <- ifelse(project_data$Platform=="v2" & project_data$SubmittalTime > maxv3days,maxv3days,project_data$SubmittalTimePending)
project_data_reduc$SubmittalTime <- ifelse(project_data$Platform=="v2" & project_data$SubmittalTime>maxv3days,NA,project_data$SubmittalTime)
summary(project_data_reduc)

completed_projects <- subset(project_data_reduc, project_data_reduc$RegAfterLaunch>0 & project_data_reduc$SubmittalTime>0 & !(is.na(project_data_reduc$CertDate)) & !(identical(project_data_reduc$RatingSystemFamily, character(0))) & !(is.na(project_data_reduc$RatingSystemFamily)) & !(project_data_reduc$RatingSystemFamily=="ND") & !(project_data_reduc$RatingSystemFamily=="Homes") & !(project_data_reduc$Platform=="v4"))

open_projects <- subset(project_data_reduc, project_data_reduc$RegAfterLaunch>0 & project_data_reduc$SubmittalTimePending>0 & is.na(project_data_reduc$CertDate) & !(identical(project_data_reduc$RatingSystemFamily,character(0))) & !(is.na(project_data_reduc$Platform)) & !(project_data_reduc$RatingSystemFamily=="ND") & !(project_data_reduc$RatingSystemFamily=="Homes") & !(project_data_reduc$Platform=="v4"))

table(completed_projects$OwnerTypesAdj)
table(completed_projects$RatingSystemFamily)
table(completed_projects$Platform)

require(ggplot2)

#Density plots of the current data, split among various attributes.
ggplot(completed_projects, aes(x=SubmittalTime, fill=RatingSystemFamily))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))
ggplot(open_projects, aes(x=SubmittalTimePending, fill=RatingSystemFamily))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))

ggplot(completed_projects, aes(x=SubmittalTime))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))+facet_wrap(~OwnerTypesAdj)
ggplot(open_projects, aes(x=SubmittalTimePending))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))+facet_wrap(~OwnerTypesAdj)

ggplot(completed_projects, aes(x=SubmittalTime, fill=Platform))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))
ggplot(open_projects, aes(x=SubmittalTimePendingAdj, fill=Platform))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))

ggplot(completed_projects, aes(x=SubmittalTime))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity",aes(y=..density..))+facet_wrap(~RatingSystemFamily)
ggplot(open_projects, aes(x=SubmittalTimePendingAdj))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity",aes(y=..density..))+facet_wrap(~RatingSystemFamily)

ggplot(completed_projects, aes(y=SubmittalTimeAdj, x=Platform))+geom_boxplot()
ggplot(open_projects, aes(y=SubmittalTimePendingAdj, x=Platform))+geom_boxplot()

ggplot(completed_projects, aes(y=SubmittalTimeAdj, x=RatingSystemFamily))+geom_boxplot()
ggplot(open_projects, aes(y=SubmittalTimePendingAdj, x=RatingSystemFamily))+geom_boxplot()

ggplot(completed_projects, aes(y=SubmittalTimeAdj, x=Platform))+geom_boxplot()+facet_wrap(~RatingSystemFamily)
ggplot(open_projects, aes(y=SubmittalTimePendingAdj, x=Platform))+geom_boxplot()+facet_wrap(~RatingSystemFamily)

ggplot(completed_projects, aes(y=SubmittalTimeAdj, x=Platform))+geom_boxplot()+facet_wrap(~OwnerTypesAdj)
ggplot(open_projects, aes(y=SubmittalTimePendingAdj, x=Platform))+geom_boxplot()+facet_wrap(~OwnerTypesAdj)

#Density plots by Rating System by certification year
testnormv2 <- subset(completed_projects, completed_projects$Platform=="v2" & !(is.na(completed_projects$CertYear)))
ggplot(testnormv2, aes(x=SubmittalTimeAdj))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))+facet_wrap(~RegYear)

testnormv3 <- subset(completed_projects, completed_projects$Platform=="v3" & !(is.na(completed_projects$CertYear)))
ggplot(testnormv3, aes(x=SubmittalTimeAdj))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))+facet_wrap(~RegYear)

#plot histogram with registration year x axis and % certified from that group as y axis

#Density plots by registration year with multiple rating systems
comparison_year1 <- subset(completed_projects, (completed_projects$Platform=="v2" | completed_projects$Platform=="v3") & completed_projects$RegYear == "1" & !(is.na(completed_projects$RegYear)))
ggplot(comparison_year1, aes(x=SubmittalTimeAdj, fill=Platform))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))
v2check <- subset(comparison_year1, comparison_year1$Platform=="v2")
summary(v2check$SubmittalTimeAdj)
v3check <- subset(comparison_year1, comparison_year1$Platform=="v3")
summary(v3check$SubmittalTimeAdj)

comparison_year2 <- subset(completed_projects, (completed_projects$Platform=="v2" | completed_projects$Platform=="v3") & completed_projects$RegYear == "2" & !(is.na(completed_projects$RegYear)))
ggplot(comparison_year2, aes(x=SubmittalTimeAdj, fill=Platform))+geom_histogram(binwidth = 50, alpha=0.5, position = "identity", aes(y=..density..))
v2check <- subset(comparison_year2, comparison_year2$Platform=="v2")
summary(v2check$SubmittalTimeAdj)
v3check <- subset(comparison_year2, comparison_year2$Platform=="v3")
summary(v3check$SubmittalTimeAdj)

comparison_year3 <- subset(completed_projects, (completed_projects$Platform=="v2" | completed_projects$Platform=="v3") & completed_projects$RegYear == "3" & !(is.na(completed_projects$RegYear)))
ggplot(comparison_year3, aes(x=SubmittalTimeAdj, fill=Platform))+geom_histogram(binwidth = 50, alpha=0.5, position = "identity", aes(y=..density..))
v2check <- subset(comparison_year3, comparison_year3$Platform=="v2")
summary(v2check$SubmittalTimeAdj)
v3check <- subset(comparison_year3, comparison_year3$Platform=="v3")
summary(v3check$SubmittalTimeAdj)

v2_days <- rep(NA,maxv3days)
v3_days <- rep(NA,maxv3days)
n <- maxv3days
for(i in 1:maxv3days){
  v2_days[i] <- nrow(subset(project_data_reduc, project_data_reduc$Platform=="v2" & project_data_reduc$SubmittalTimeAdj < i)) / nrow(subset(project_data_reduc, project_data_reduc$Platform=="v2"))
  v3_days[i] <- nrow(subset(project_data_reduc, project_data_reduc$Platform=="v3" & project_data_reduc$SubmittalTimeAdj < i)) / nrow(subset(project_data_reduc, project_data_reduc$Platform=="v3"))
}
certification_ratio_df <- data.frame(c(1:maxv3days))
names(certification_ratio_df) <- c("days")
certification_ratio_df$v2 <- v2_days
certification_ratio_df$v3 <- v3_days
ggplot()+geom_line(data = certification_ratio_df, aes(x=days, y=v2, color="v2"))+geom_line(data = certification_ratio_df, aes(x=days, y=v3, color="v3"))

v2_cert_proj <- nrow(subset(project_data, project_data$Platform=="v2" & project_data$IsCertified=="Yes")) / nrow(subset(project_data, project_data$Platform=="v2"))
v3_cert_proj <- nrow(subset(project_data, project_data$Platform=="v3" & project_data$IsCertified=="Yes")) / nrow(subset(project_data, project_data$Platform=="v3"))

ggplot(subset(completed_projects,completed_projects$RegYear<8), aes(x=SubmittalTime, fill=Platform))+geom_histogram(binwidth = 100, alpha=0.5, position = "identity", aes(y=..density..))+facet_wrap(~RegYear)
