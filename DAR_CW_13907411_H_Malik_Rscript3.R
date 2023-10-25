install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
install.packages("pROC")
library(pROC)
install.packages("ROCR")
library(ROCR)

#Question 4, part(a)
WineQuality_Training$quality <- as.factor(WineQuality_Training$quality)

set.seed(1)
tune_linear <- tune(svm, quality ~ ., data = WineQuality_Training, kernel = "linear", ranges = list(cost = c(0.1, 1, 10)))
summary(tune_linear$best.parameters)

#Question 4, part(b)
svm_linear <- svm(quality ~ ., data = WineQuality_Training, kernel = "linear", cost = 1, probability = TRUE)
svm_linear_prediction <- predict(svm_linear, WineQuality_Testing, probability = TRUE)
svm_linear_accuracy <- sum(svm_linear_prediction == WineQuality_Testing$quality) / length(svm_linear_prediction)
svm_linear_probability <- predict(svm_linear, WineQuality_Testing, decision.values = TRUE)
print(svm_linear_accuracy)
print(svm_linear_probability)


#Question 4, part(c)
set.seed(1)
tune_radial <- tune(svm, quality ~ ., data = WineQuality_Training, kernel = "radial", ranges = list(cost = c(0.1, 1, 10), gamma = c(0.1, 0.5, 1)))
summary(tune_radial$best.parameters)

#Question 4, part (d)
svm_radial <- svm(quality ~ ., data = WineQuality_Training, kernel = "radial", cost = 1, gamma = 1, probability = TRUE)
svm_radial_prediction <- predict(svm_radial, WineQuality_Testing, probability = TRUE)
svm_radial_accuracy <- sum(svm_radial_prediction == WineQuality_Testing$quality) / length(svm_radial_prediction)
svm_radial_probability <- predict(svm_linear, WineQuality_Testing, decision.values = TRUE)
print(svm_radial_accuracy)
print(svm_radial_probability)


#Question 4, part (e)
WineQuality_Testing$binary_quality <- ifelse(WineQuality_Testing$quality == "Good", 1, 0)
decision_values_linear = attr(svm_linear_probability, "decision.values")
decision_values_radial = attr(svm_radial_probability, "decision.values")

roc_svm_linear <- roc(WineQuality_Testing$binary_quality, decision_values_linear)
roc_svm_radial <- roc(WineQuality_Testing$binary_quality, decision_values_radial)

plot(roc_svm_linear, col = "blue", main = "ROC Curve Comparison")
lines(roc_svm_radial, col = "green")
legend("bottomright", legend = c("Linear Kernel", "Radial Kernel"), col = c("blue", "green"), lwd = 2)

auc_svm_linear <- auc(roc_svm_linear)
auc_svm_radial <- auc(roc_svm_radial)
print(auc_svm_linear)
print(auc_svm_radial)

