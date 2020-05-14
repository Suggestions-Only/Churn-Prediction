##Churn Prediction Project

#package installation and getting the data
install.packages("C50")
library(C50)
data(churn)

#installing and loading ggplot2 for later
install.packages("ggplot2")
library(ggplot2)

#installing and loading tidyverse for later
install.packages("tidyverse")
library(tidyverse)

#exploring correlation between number of calls for different times of day
cor(churnTrain$total_day_calls, churnTrain$total_eve_calls)
cor(churnTrain$total_day_calls, churnTrain$total_night_calls)
cor(churnTrain$total_eve_calls, churnTrain$total_night_calls)

graph1 <- ggplot(data = churnTrain,
                 aes(x = total_day_calls, y = total_eve_calls)) +
          geom_point()
graph1

graph2 <- ggplot(data = churnTrain,
                 aes(x = total_day_calls, y = total_night_calls)) +
          geom_point()
graph2

graph3 <- ggplot(data = churnTrain,
                 aes(x = total_eve_calls, y = total_night_calls)) +
          geom_point()
graph3

#exploring relationship between call length during different times of day
cor(churnTrain$total_day_minutes, churnTrain$total_eve_minutes)
cor(churnTrain$total_day_minutes, churnTrain$total_night_minutes)
cor(churnTrain$total_eve_minutes, churnTrain$total_night_minutes)

graph4 <- ggplot(data = churnTrain,
                 aes(x = total_day_minutes, y = total_eve_minutes)) +
          geom_point()
graph4

graph5 <- ggplot(data = churnTrain,
                 aes(x = total_day_minutes, y = total_night_minutes)) +
          geom_point()
graph5

graph6 <- ggplot(data = churnTrain,
                 aes(x = total_eve_minutes, y = total_night_minutes)) +
          geom_point()
graph6

#exploring whether length of call and number of calls are relatively interchangeable
cor(churnTrain$total_day_calls, churnTrain$total_day_minutes)
cor(churnTrain$total_eve_calls, churnTrain$total_eve_minutes)
cor(churnTrain$total_night_calls, churnTrain$total_night_minutes)

graph7 <- ggplot(data = churnTrain,
                 aes(x = total_day_calls, y = total_day_minutes)) +
          geom_point()
graph7

graph8 <- ggplot(data = churnTrain,
                 aes(x = total_eve_calls, y = total_eve_minutes)) +
          geom_point()
graph8

graph9 <- ggplot(data = churnTrain,
                 aes(x = total_night_calls, y = total_night_minutes)) +
          geom_point()
graph9

#checking the number of total non-users
sum(churnTrain$total_day_calls == 0)
sum(churnTrain$total_eve_calls == 0)

#checking variation in account length
hist(churnTrain$account_length)
hist.acc.length <- ggplot(data = churnTrain, aes(x = account_length)) +
                    geom_histogram()
hist.acc.length

#checking variation in customer service calls
hist(churnTrain$number_customer_service_calls)
ggplot(data = churnTrain, aes(x = number_customer_service_calls)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8))

#checking how common international plans are
table(churnTrain$international_plan)
prop.table(table(churnTrain$international_plan))
323/(323+3010)

#checking how common voicemail plans are
table(churnTrain$voice_mail_plan)
prop.table(table(churnTrain$voice_mail_plan))
922/(922+2411)

#exploring account length as a possible churn predictor
t.test(account_length~churn, data = churnTrain)
churnTrain %>%
  group_by(churn) %>%
  summarise(med = median(account_length))
ggplot(data = churnTrain, aes(x = churn, y = account_length)) +
  geom_jitter() + 
  coord_flip()

#exploring custoner service calls as a possible churn predictor
t.test(number_customer_service_calls~churn, data = churnTrain)
churnTrain %>%
  group_by(churn) %>%
  summarise(med = median(number_customer_service_calls))
ggplot(data = churnTrain, aes(x = churn, y = number_customer_service_calls)) +
  geom_jitter() + 
  ylim(c(0,7.5)) +
  coord_flip()

#creating new variable - index of problems to account length
churnTrain$problem_ratio <- churnTrain$number_customer_service_calls/churnTrain$account_length
summary(churnTrain$problem_ratio)
ggplot(data = churnTrain, aes(x = churn, y = problem_ratio)) +
  geom_boxplot() +
  coord_flip()
sum(churnTrain$problem_ratio > .2)
t.test(problem_ratio~churn, data = churnTrain)

prop.table(table(churnTrain$churn, churnTrain$area_code))
prop.table(table(churnTrain$churn, churnTrain$international_plan))
prop.table(table(churnTrain$churn, churnTrain$voice_mail_plan))

##test of a first final model
model_1_test <- glm(churn~number_customer_service_calls+area_code+
                      international_plan+voice_mail_plan,
                    family = binomial(link="logit"), data = churnTrain)
summary(model_1_test)

model_2_test <- glm(churn~number_customer_service_calls+international_plan+
                      voice_mail_plan+total_day_charge+total_eve_charge+
                      total_night_charge, family = binomial(link="logit"),
                    data = churnTrain)
summary(model_2_test)

model_3_test <- glm(churn~number_customer_service_calls+international_plan+
                           voice_mail_plan+total_day_calls+total_eve_calls+
                           total_night_calls, family = binomial(link="logit"),
                         data = churnTrain)
summary(model_3_test)

model_4_test <- glm(churn~number_customer_service_calls+international_plan+
                      voice_mail_plan+total_day_minutes+total_eve_minutes+
                      total_night_minutes, family = binomial(link="logit"),
                    data = churnTrain)
summary(model_4_test)

model_5_test <- glm(churn~number_customer_service_calls+international_plan+
                      voice_mail_plan+total_day_minutes+total_eve_minutes+
                      total_night_minutes+total_day_charge+total_eve_charge+
                      total_night_charge, family = binomial(link="logit"),
                    data = churnTrain)
summary(model_5_test)

cor(churnTrain$total_day_charge, churnTrain$total_day_minutes)
cor(churnTrain$total_eve_charge, churnTrain$total_eve_minutes)
cor(churnTrain$total_night_charge, churnTrain$total_night_minutes)

state.model <- glm(churn~state, family = binomial(link = "logit"),
                   data = churnTrain)
summary(state.model)

plan.model <- glm(churn~international_plan+voice_mail_plan,
                  family = binomial(link = "logit"), data = churnTrain)
summary(plan.model)

churnTrain$day_min_ratio <- churnTrain$total_day_minutes/churnTrain$account_length
churnTrain$eve_min_ratio <- churnTrain$total_eve_minutes/churnTrain$account_length
churnTrain$night_min_ratio <- churnTrain$total_night_minutes/churnTrain$account_length

trying.shit.out <- glm(churn~day_min_ratio+eve_min_ratio+night_min_ratio,
                       family = binomial(link="logit"), data = churnTrain)
summary(trying.shit.out)

comparison.model <- glm(churn~total_day_minutes+total_eve_minutes+total_night_minutes,
                        family = binomial(link="logit"), data = churnTrain)
summary(comparison.model)
