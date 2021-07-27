rm(list = ls())#Remove all variable stored previously

data <- read.csv("C:/Users/ASUS/Desktop/Hamza/Python Course/R/COVID19_line_list_data.csv")
describe(data)

data$death_dummy= as.integer(data$death != 0)

#Clean death column
unique(data$death_dummy)
#death rate
sum(data$death_dummy / nrow(data))

#Age
#Claim : people who die are older
dead = subset(data,death_dummy == 1)
alive = subset(data,death_dummy == 0)
mean(dead$age,na.rm = TRUE)
mean(alive$age,na.rm = TRUE)

#Is it statisticly significant?
t.test(alive$age,dead$age,alternative = "two.sided",conf.level = 0.99)

#Result theory : 
#Normally if p-value < 0.05 , we reject null hypothesis
#here p-value ~ 0 , so we reject the null hypothesis and conclude
#that is this is statiscly significant


#Gender
#Claim : gender has no effect in death
men = subset(data,gender == "male")# 8.5 %
women = subset(data,gender == "female")# 3.7 %
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy , na.rm = TRUE)

#Is it statisticly significant?

t.test(men$death_dummy,women$death_dummy,alternative = "two.sided",conf.level = 0.99)

#99% confidence : men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 & < 0.05 so this is statistically signifiant

ggplot(data = data) + geom_point(mapping=aes(x=age,y=death_dummy))
