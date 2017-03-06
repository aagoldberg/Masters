scores <- (c(57, 66, 69, 71, 72, 73, 74, 77, 78, 78, 79, 79, 81, 81, 82, 83, 83, 88, 89, 94))
boxplot(scores)

setwd("~/Documents/Masters/DATA606/Week1/Homework")
library(openintro)
data("heartTr")

patient_control_dead <- sum(heartTr$transplant=="control" & heartTr$survived=="dead")
patient_control <- sum(heartTr$transplant=="control")
patient_treatement_dead <- sum(heartTr$transplant=="treatment" & heartTr$survived=="dead")
patient_treatment <- sum(heartTr$transplant=="treatment")
patient_control_dead_ratio <- patient_control_dead / patient_control
patient_treatment_dead_ratio <- patient_treatement_dead / patient_treatment


patient_alive <- sum(heartTr$survived=="alive")
patient_alive
patient_dead <- sum(heartTr$survived=="dead")
patient_dead
patient_treatment
patient_control

patient_treatment_dead_ratio - patient_control_dead_ratio
