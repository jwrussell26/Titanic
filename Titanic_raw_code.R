# Code Only

library(tidyverse)
train_data <- read_csv('train.csv')

glimpse(train_data)

train_data %>%
  group_by(Survived) %>%
  count()

train_data <- train_data %>%
  select(-c(Cabin, Ticket))

train_data %>%
  mutate(Ageless = is.na(Age)) %>%
  count(Ageless)

train_data <- train_data %>%
  mutate(Famsize = SibSp + Parch)

find_age <- group_by(train_data, Pclass, Sex, Famsize) %>%
  summarise(Avg_age = mean(Age, na.rm = T))

train_data <- train_data %>%
  inner_join(find_age)

train_data %>%
  count(is.na(Avg_age))

for (i in 1:length(train_data$Age)){
  if (is.na(train_data$Age[i])){
    train_data$Age[i] <- train_data$Avg_age[i]
  }
}

train_data <- train_data %>%
  select(-c(SibSp, Parch, Avg_age)) %>%
  filter(!is.nan(Age)) %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 5, 12, 19, 29, 49, 80), labels = c("Infant", "Child", "Teen", "Young Adult", "Adult", "Elderly")))

train_data$Embarked <- factor(train_data$Embarked)

cor(train_data[, -c(2, 4:5, 8, 10)])

ggplot(train_data) +
  geom_bar(aes(Age_Group, fill = Age_Group))

by_age <- train_data %>%
  group_by(Age_Group) %>%
  summarise(prop_lived = sum(Survived == 1) / n()) 

ggplot(by_age) +
  geom_col(aes(Age_Group, prop_lived, fill = Age_Group))

by_sex <- train_data %>%
  group_by(Sex) %>%
  summarise(proportion_lived = sum(Survived == 1) / n())

ggplot(by_sex) +
  geom_col(aes(Sex, proportion_lived, fill = Sex))


log_fit <- glm(Survived ~ Pclass + Sex + Fare + Embarked + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit, correlation = T)

log_fit2 <- glm(Survived ~ Pclass + Sex + Fare + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit2)

log_fit3 <- glm(Survived ~ Pclass + Sex + Famsize + Age_Group, data = train_data, family = binomial)

summary(log_fit3)



test_data <- read_csv('test.csv')
glimpse(test_data)

test_data <- test_data %>%
  select(-c(Cabin, Ticket))

test_data %>%
  mutate(Ageless = is.na(Age)) %>%
  count(Ageless)

test_data <- test_data %>%
  mutate(Famsize = SibSp + Parch)

test_age <- group_by(test_data, Pclass, Sex, Famsize) %>%
  summarise(Avg_age = mean(Age, na.rm = T))

test_data <- test_data %>%
  inner_join(test_age)

test_data %>%
  count(is.na(Avg_age))

for (i in 1:length(test_data$Age)){
  if (is.na(test_data$Age[i])){
    test_data$Age[i] <- test_data$Avg_age[i]
  }
}

test_data <- test_data %>%
  select(-c(SibSp, Parch, Avg_age, Name, Age, Fare, Embarked)) %>%
  filter(!is.nan(Age))
  

write_csv(test_data, path = "tidy_test.csv")


test_probs <- predict()
