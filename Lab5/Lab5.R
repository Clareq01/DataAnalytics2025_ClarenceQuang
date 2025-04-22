library(readr)
library(caret)
library(ggfortify)
library(class)
library(e1071)

# Load data
wine <- read_csv("Lab4/wine.data", col_names = FALSE)
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                    "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                    "Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
X <- wine[,-1]
Y <- as.factor(wine$class)

# Set seed and create train-test split
set.seed(100)
trainIndex <- createDataPartition(Y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
Y_train <- Y[trainIndex]
Y_test <- Y[-trainIndex]

selected_features <- c("Alcohol", "Flavanoids", "Color intensity", "Proline")
X_train_sub <- X_train[, selected_features]
X_test_sub <- X_test[, selected_features]

#Linear Kernel 
tune_linear <- tune.svm(x = X_train_sub, y = Y_train,
                        kernel = "linear",
                        cost = 10^(-1:2))

svm_linear <- tune_linear$best.model
pred_linear <- predict(svm_linear, X_test_sub)

#RBF Kernel
tune_rbf <- tune.svm(x = X_train_sub, y = Y_train,
                     kernel = "radial",
                     cost = 10^(-1:2),
                     gamma = c(0.01, 0.1, 1))

svm_rbf <- tune_rbf$best.model
pred_rbf <- predict(svm_rbf, X_test_sub)

# ------------------ kNN Classifier ------------------
# Standardize data
preproc <- preProcess(X_train_sub, method = c("center", "scale"))
X_train_knn <- predict(preproc, X_train_sub)
X_test_knn <- predict(preproc, X_test_sub)

knn_pred <- knn(train = X_train_knn, test = X_test_knn, cl = Y_train, k = 5)

# ------------------ Evaluation ------------------
eval_metrics <- function(true, pred, model_name) {
  cm <- confusionMatrix(pred, true)
  cat("\n---", model_name, "---\n")
  print(cm$byClass[, c("Precision", "Recall", "F1")])
}

eval_metrics(Y_test, pred_linear, "SVM Linear")
eval_metrics(Y_test, pred_rbf, "SVM RBF")
eval_metrics(Y_test, knn_pred, "kNN (k=5)")

