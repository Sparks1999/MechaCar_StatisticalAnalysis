#Deliverable 1
library(dplyr)
mpg <- read.csv(file = 'MechaCar_mpg.csv', stringsAsFactors = F, check.names = F)
lmmpg = lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mpg)
summary(lmmpg)
#Vehicle weight would realistically have a big impact on the calculated 

#Deliverable 2
suspension <- read.csv(file = 'Suspension_Coil.csv', stringsAsFactors = F, check.names = F)
suspension
total_summary <- suspension %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

lot_summary <- suspension %>%
  group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

#Deliverable 3
t.test(suspension$PSI, mu = 1500)

Lot1 <- subset(suspension, Manufacturing_Lot == 'Lot1')
t.test(Lot1$PSI, mu = 1500)

Lot2 <- subset(suspension, Manufacturing_Lot == 'Lot2')
t.test(Lot2$PSI, mu = 1500)

Lot3 <- subset(suspension, Manufacturing_Lot == 'Lot3')
t.test(Lot3$PSI, mu = 1500)