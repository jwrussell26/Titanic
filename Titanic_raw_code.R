# Code Only

library(tidyverse)
train_data <- read_csv('train.csv')

glimpse(train_data)

train_data <- train_data %>%
  select(-c(Cabin, Ticket))

train_data %>%
  mutate(Ageless = is.na(Age)) %>%
  count(Ageless)

train_data <- train_data %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 5, 12, 19, 29, 49, 80), labels = c("Infant", "Child", "Teen", "Young Adult", "Adult", "Elderly")))

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
