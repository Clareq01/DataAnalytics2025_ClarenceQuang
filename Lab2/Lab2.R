library("ggplot2")
library("readr")
library(dplyr)
## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset

dataset <- dataset %>% 
  select(PRICE, BEDS, BATH, PROPERTYSQFT) %>%
  filter(!is.na(PRICE) & !is.na(BEDS) & !is.na(BATH) & !is.na(PROPERTYSQFT))

dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$PRICE > 0 & 
                     dataset$BEDS > 0 & 
                     dataset$BATH > 0 & 
                     dataset$PROPERTYSQFT > 0, ]
dataset <- dataset %>%
  mutate(
    log_PRICE = log10(PRICE),
    log_PROPERTYSQFT = log10(PROPERTYSQFT),
    log_BEDS = log10(BEDS),
    log_BATH = log10(BATH)
  )

#Model 1
model1 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS + log_BATH, data = dataset)
summary(model1)

ggplot(dataset, aes(x = log_PROPERTYSQFT, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, col = "red") +
  ggtitle("Model 1: log(PROPERTYSQFT) vs log(PRICE)")

plot(model1$residuals, main = "Residuals of Model 1", ylab = "Residuals", xlab = "Index")

#Model 2
model2 <- lm(log_PRICE ~ log_BEDS + log_BATH, data = dataset)
summary(model2)

ggplot(dataset, aes(x = log_BEDS, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, col = "red") +
  ggtitle("Model 2: log(BEDS) vs log(PRICE)")

plot(model2$residuals, main = "Residuals of Model 2", ylab = "Residuals", xlab = "Index")

#Model 3
model3 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS, data = dataset)
summary(model3)

ggplot(dataset, aes(x = log_PROPERTYSQFT, y = log_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, col = "red") +
  ggtitle("Model 3: log(PROPERTYSQFT) vs log(PRICE)")

plot(model3$residuals, main = "Residuals of Model 3", ylab = "Residuals", xlab = "Index")


