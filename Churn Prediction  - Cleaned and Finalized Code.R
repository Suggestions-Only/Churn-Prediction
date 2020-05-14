##Churn Prediction Project - Final Outline

install.packages("C50")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("ModelMetrics")

library(C50)
library(ggplot2)
library(tidyverse)
library(ModelMetrics)

data(churn)
remove(churnTest)

prop.table(table(churnTrain$churn))
churnTrain$churn <- factor(churnTrain$churn,
                           levels(churnTrain$churn)[c(2,1)])

#Step 1 - Checking for Redundant Variables
#day, evening, and night calls are not redundant or compensatory
cor(churnTrain$total_day_calls, churnTrain$total_eve_calls)

ggplot(data = churnTrain,
       aes(x = total_day_calls, y = total_eve_calls)) +
  geom_point()

cor(churnTrain$total_day_calls, churnTrain$total_night_calls)

ggplot(data = churnTrain,
       aes(x = total_day_calls, y = total_night_calls)) +
  geom_point()

cor(churnTrain$total_eve_calls, churnTrain$total_night_calls)

ggplot(data = churnTrain,
       aes(x = total_eve_calls, y = total_night_calls)) +
  geom_point()

#day, evening, and night minutes are not redundant or compensatory
cor(churnTrain$total_day_minutes, churnTrain$total_eve_minutes)

ggplot(data = churnTrain,
       aes(x = total_day_minutes, y = total_eve_minutes)) +
  geom_point()

cor(churnTrain$total_day_minutes, churnTrain$total_night_minutes)

ggplot(data = churnTrain,
       aes(x = total_day_minutes, y = total_night_minutes)) +
  geom_point()

cor(churnTrain$total_eve_minutes, churnTrain$total_night_minutes)

ggplot(data = churnTrain,
       aes(x = total_eve_minutes, y = total_night_minutes)) +
  geom_point()

#checking correlation between calls and call length for each time of day
cor(churnTrain$total_day_calls, churnTrain$total_day_minutes)

ggplot(data = churnTrain,
       aes(x = total_day_calls, y = total_day_minutes)) +
  geom_point()

cor(churnTrain$total_eve_calls, churnTrain$total_eve_minutes)

ggplot(data = churnTrain,
       aes(x = total_eve_calls, y = total_eve_minutes)) +
  geom_point()

cor(churnTrain$total_night_calls, churnTrain$total_night_minutes)

ggplot(data = churnTrain,
       aes(x = total_night_calls, y = total_night_minutes)) +
  geom_point()

#checking the number of total non-users
sum(churnTrain$total_day_calls == 0)
sum(churnTrain$total_eve_calls == 0)
sum(churnTrain$total_night_calls == 0)

#charge and minutes are redundant - the telecom company charges a set rate per
# minute of phone conversation
cor(churnTrain$total_day_charge, churnTrain$total_day_minutes)

ggplot(data = churnTrain,
       aes(x = total_day_charge, y = total_day_minutes)) +
  geom_point()

cor(churnTrain$total_eve_charge, churnTrain$total_eve_minutes)

ggplot(data = churnTrain,
       aes(x = total_eve_charge, y = total_eve_minutes)) +
  geom_point()

cor(churnTrain$total_night_charge, churnTrain$total_night_minutes)

ggplot(data = churnTrain,
       aes(x = total_night_charge, y = total_night_minutes)) +
  geom_point()

day_price <- churnTrain$total_day_charge/churnTrain$total_day_minutes
eve_price <- churnTrain$total_eve_charge/churnTrain$total_eve_minutes
night_price <- churnTrain$total_night_charge/churnTrain$total_night_minutes

#Step 2 - Data Tranformations: create variables that might be useful, in this
# case the ratio of customer service calls over account length
churnTrain$problem_ratio <- churnTrain$number_customer_service_calls/churnTrain$account_length
churnTrain$day_min_ratio <- churnTrain$total_day_minutes/churnTrain$account_length
churnTrain$eve_min_ratio <- churnTrain$total_eve_minutes/churnTrain$account_length
churnTrain$night_min_ratio <- churnTrain$total_night_minutes/churnTrain$account_length

#Step 3 - Explore continuous predictors with t-tests
t.test(account_length~churn, data = churnTrain)

t.test(number_customer_service_calls~churn, data = churnTrain)

t.test(problem_ratio~churn, data = churnTrain)

#Step 4 - Exploring factor predictors with logistic regression
state.model <- glm(churn~state, family = binomial(link = "logit"),
                   data = churnTrain)
summary(state.model)

international.model <- glm(churn~international_plan,
                           family = binomial(link = "logit"),
                           data = churnTrain)
summary(international.model)

voice_mail_model <- glm(churn~voice_mail_plan, family = binomial(link = "logit"),
                        data = churnTrain)
summary(voice_mail_model)

#Step 5 - ratios vs. totals

ratios.model <- glm(churn~day_min_ratio+eve_min_ratio+night_min_ratio,
                       family = binomial(link="logit"), data = churnTrain)
summary(ratios.model)

totals.model <- glm(churn~total_day_minutes+total_eve_minutes+total_night_minutes,
                        family = binomial(link="logit"), data = churnTrain)
summary(totals.model)

#Step 6 - putting the final model together

final_churn_model <- glm(churn~number_customer_service_calls+international_plan+
                           voice_mail_plan+total_day_minutes+total_eve_minutes+
                           total_night_minutes, family = binomial(link="logit"),
                         data = churnTrain)
summary(final_churn_model)

#Step 7 - interpret the model

#Step 8 - in-sample predictions
churn_prob <- predict(final_churn_model, newdata = churnTrain,
                      type = "response")
churnTrain$churn_prob <- churn_prob
churnTrain %>%
  group_by(churn) %>%
  summarise(med = median(churn_prob))
churn_guess <- ifelse(churn_prob > .267, 2, 1)
(sum(as.numeric(churnTrain$churn)==churn_guess))/length(churnTrain$churn)
auc(actual = churnTrain$churn, predicted = churn_guess)

#Step 9 - out-of-sample predictions
churn_predicts <- predict(final_churn_model, newdata = Customers_To_Predict,
                          type = "response")
