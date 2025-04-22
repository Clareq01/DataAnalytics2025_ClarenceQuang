###################
##### Abalone #####
###################
library(class)       
library(caret)       
library(ggplot2)     
library(cluster)     
library(factoextra)  

abalone <- read.csv("Lab3/abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
dataset$sex <- as.numeric(factor(dataset$sex))
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Subset 1
features1 <- dataset[, c("length", "diameter", "height")]
features1 <- as.data.frame(lapply(features1, normalize))
# Subset 2
features2 <- dataset[, c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")]
features2 <- as.data.frame(lapply(features2, normalize))

target <- dataset$age.group
set.seed(100)
index <- createDataPartition(target, p=0.7, list=FALSE)
train1 <- features1[index, ]
test1 <- features1[-index, ]
train2 <- features2[index, ]
test2 <- features2[-index, ]
train_label <- target[index]
test_label <- target[-index]

# Train kNN models
knn1 <- knn(train=train1, test=test1, cl=train_label, k=5)
knn2 <- knn(train=train2, test=test2, cl=train_label, k=5)

#confusion matrix
print(confusionMatrix(knn1, test_label))
print(confusionMatrix(knn2, test_label))

# Choose better model (say it's model2); find optimal k
accuracies <- c()
k_values <- 1:20
for (k in k_values) {
  pred <- knn(train=train2, test=test2, cl=train_label, k=k)
  acc <- sum(pred == test_label) / length(test_label)
  accuracies <- c(accuracies, acc)
}
k_values[which.max(accuracies)]

# Exercise 2
kmeans_data <- features2

set.seed(100)
km_result <- kmeans(kmeans_data, centers=3, nstart=25)

clustered_data <- kmeans_data
clustered_data$cluster <- as.factor(km_result$cluster)

# Plot clusters using 2 features
ggplot(clustered_data, aes(x=whole_weight, y=shucked_wieght, color=cluster)) +
  geom_point(size=2) +
  labs(title="K-Means Clusters", x="Whole Weight", y="Shucked Weight")


