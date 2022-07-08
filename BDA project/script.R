# import library (Harrell Miscellaneous)
library(Hmisc) 

# import Data set
data <- read.csv("D:/projects/BDA project/COVID19_line_list_data.csv")

# Data definition commands
describe(data) 

# data cleaning 
# hear all the inconsistent values are ignored 
#using $ we can create new column "death_clear" which contain only two value "0" or "1"
data$death_clear <- as.integer (data$death != 0)

#calculate Death rate
sum(data$death_clear) / nrow(data) # 5.8


# hypothesis testing on the basis of gender 
# claim : gender does not matter in death rate (null hypothesis)
man = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(man$death_clear, na.rm = TRUE) 
mean(women$death_clear, na.rm = TRUE)

# is this statistically significant?
t.test(man$death_clear, women$death_clear, alternative="two.sided", conf.level = 0.97)
# 99% confidence: man have from 0.8% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistically significant 
# hence we reject null hypothesis and accept alternative hypothesis 
# higher death rate in man as compare to women 




# hypothesis testing on the basis of age
# claim: people who die are older than young (null hypothesis)
dead = subset(data, death_clear == 1)
alive = subset(data, death_clear == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.97)
# p = 2.2e-16
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant
# hence we accept alternative hypothesis
# higher death rate in older people



