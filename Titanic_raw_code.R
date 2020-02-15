# Code Only

library(tidyverse)
train_data <- read_csv('train.csv')
test_data <- read_csv('test.csv')

glimpse(train_data)
glimpse(test_data)

train_data %>%
  group_by(Survived) %>%
  count()

train_data <- train_data %>%
  select(-c(Cabin, Ticket))

test_data <- test_data %>%
  select(-c(Cabin, Ticket))

train_data %>%
  mutate(Ageless = is.na(Age)) %>%
  count(Ageless)

test_data %>%
  mutate(Ageless = is.na(Age)) %>%
  count(Ageless)

train_data <- train_data %>%
  mutate(Famsize = SibSp + Parch)

test_data <- test_data %>%
  mutate(Famsize = SibSp + Parch)


train_data$Survived <- as.factor(train_data$Survived)

train_data$Embarked <- as.factor(train_data$Embarked)
test_data$Embarked <- as.factor(test_data$Embarked)

train_data$Sex <-  as.factor(train_data$Sex)
test_data$Sex <-  as.factor(test_data$Sex)

train_data$Pclass <-  as.factor(train_data$Pclass)
test_data$Pclass <-  as.factor(test_data$Pclass)

train_data$Famsize <- as.factor(train_data$Famsize)
test_data$Famsize <- as.factor(test_data$Famsize)

glimpse(train_data)
glimpse(test_data)

find_age <- group_by(train_data, Pclass, Sex, Famsize) %>%
  summarise(Avg_age = mean(Age, na.rm = T))

test_age <- group_by(test_data, Pclass, Sex, Famsize) %>%
  summarise(Avg_age = mean(Age, na.rm = T))

train_data <- train_data %>%
  inner_join(find_age)

test_data <- test_data %>%
  inner_join(test_age)

train_data %>%
  count(is.na(Avg_age))

test_data %>%
  count(is.na(Avg_age))

for (i in 1:length(train_data$Age)){
  if (is.na(train_data$Age[i])){
    train_data$Age[i] <- train_data$Avg_age[i]
  }
}

for (i in 1:length(test_data$Age)){
  if (is.na(test_data$Age[i])){
    test_data$Age[i] <- test_data$Avg_age[i]
  }
}

train_data <- train_data %>%
  select(-c(SibSp, Parch, Avg_age)) %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 5, 12, 19, 29, 49, 80), labels = c("Infant", "Child", "Teen", "Young Adult", "Adult", "Elderly")))

test_data <- test_data %>%
  select(-c(SibSp, Parch, Avg_age)) %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 5, 12, 19, 29, 49, 80), labels = c("Infant", "Child", "Teen", "Young Adult", "Adult", "Elderly")))

train_data %>%
  count(is.na(Age_Group))

test_data %>%
  count(is.na(Age_Group))

train_data %>%
  count(Age_Group)

train_data$Age_Group <- train_data$Age_Group %>%
  replace_na("Adult")

test_data %>%
  count(Age_Group)

test_data$Age_Group <- test_data$Age_Group %>%
  replace_na("Adult")

train_data %>%
  count(is.na(Fare))

test_data %>%
  count(is.na(Fare))

test_data$Fare <- test_data$Fare %>%
  replace_na(median(train_data$Fare))

train_data %>%
  count(is.na(Embarked))

table(train_data$Embarked)

train_data$Embarked <- train_data$Embarked %>%
  replace_na("S")

test_data %>%
  count(is.na(Embarked))

train_data %>%
  count(is.na(Famsize))
  
test_data %>%
  count(is.na(Famsize))

write_csv(test_data, path = "titanic_tidy_train.csv")
write_csv(test_data, path = "titanic_tidy_test.csv")

log_fit <- glm(Survived ~ Pclass + Sex + Fare + Embarked + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit, correlation = T)

log_fit2 <- glm(Survived ~ Pclass + Sex + Fare + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit2)

log_fit3 <- glm(Survived ~ Pclass + Sex + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit3)



test_probs <- predict(log_fit3, test_data, type = 'response')

test_predict <- rep("0", 418)
test_predict[test_probs > .5] = "1"

log_final <- test_data %>%
  mutate('Survived' = test_predict) %>%
  select(c(PassengerId, Survived))

write_csv(log_final, path = "log_regression_submit.csv")
