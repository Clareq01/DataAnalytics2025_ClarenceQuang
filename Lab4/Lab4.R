  library(readr)
  library(caret)
  library(ggfortify)
  library(class)
  library(e1071)
  
  wine <- read_csv("Lab4/wine.data", col_names = FALSE)
  colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                      "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                      "Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
  X <- wine[,-1]
  Y <- as.factor(wine$class)
features_scaled <- scale(X)

# PCA
principal_components <- princomp(features_scaled, cor = TRUE, scores = TRUE)

# Identify top contributors to PC1
pc1_loadings <- principal_components$loadings[, 1]
abs_pc1 <- abs(pc1_loadings)
sorted_loadings <- sort(abs_pc1, decreasing = TRUE)
print(sorted_loadings)

# Drop the 3 least contributing variables to PC1
least_contributors <- names(sorted_loadings)[(length(sorted_loadings)-2):length(sorted_loadings)]
X_reduced <- X[, !(names(X) %in% least_contributors)]
features_scaled_reduced <- scale(X_reduced)

# Rerun PCA on reduced features
principal_components_reduced <- princomp(features_scaled_reduced, cor = TRUE, scores = TRUE)

# kNN on original dataset
set.seed(100)
train_index <- createDataPartition(Y, p = 0.7, list = FALSE)
X_train <- features_scaled[train_index, ]
X_test <- features_scaled[-train_index, ]
Y_train <- Y[train_index]
Y_test <- Y[-train_index]

knn_pred_original <- knn(train = X_train, test = X_test, cl = Y_train, k = 5)

# kNN on PCA-transformed data (first 3 PCs)
scores <- principal_components$scores[, 1:3]
scores_train <- scores[train_index, ]
scores_test <- scores[-train_index, ]

knn_pred_pca <- knn(train = scores_train, test = scores_test, cl = Y_train, k = 5)

# Evaluation - original
confusion_original <- confusionMatrix(knn_pred_original, Y_test)
print(confusion_original)

# Evaluation - PCA
confusion_pca <- confusionMatrix(knn_pred_pca, Y_test)
print(confusion_pca)

