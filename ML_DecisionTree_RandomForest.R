#room occupancy data to train a decision tree classifier and random forest classifier to predict

#used libraries tree and randomforest
install.packages("randomForest")
install.packages("tree")
install.packages("readr")
install.packages("dplyr")
library(randomForest)
library(tree)
library(readr)
library(dplyr)

#train a decision tree classifier and evaluate performance 
Room_Occupancy_Training_set$Occupancy <- as.factor(Room_Occupancy_Training_set$Occupancy)

decision_tree_classifier <- tree(Occupancy ~ ., data = Room_Occupancy_Training_set)
decision_tree_prediction <- predict(decision_tree_classifier, Room_Occupancy_Testing_set, type = "class")
decision_tree_accuracy <- sum(decision_tree_prediction == Room_Occupancy_Testing_set$Occupancy) / length(decision_tree_prediction)
print(decision_tree_accuracy)
print(decision_tree_prediction)

#plot the tree structure

#rpart.plot(decision_tree_classifier, type = 4, extra = 104, box.palette = c("blue", "green"), shadow.col = "gray")
plot(decision_tree_classifier)
text(decision_tree_classifier, pretty = 0)
print(decision_tree_classifier)

#train a random forest classifier and evaluate performance 
set.seed(1)
random_forest_classifier <- randomForest(Occupancy ~ ., data = Room_Occupancy_Training_set)
random_forest_prediction <- predict(random_forest_classifier, Room_Occupancy_Testing_set, type = "class")
random_forest_accuracy <- sum(random_forest_prediction == Room_Occupancy_Testing_set$Occupancy) / length(random_forest_prediction)
print(random_forest_accuracy)
print(random_forest_classifier)

#output and analyse the feature importance
feature_importance <- importance(random_forest_classifier)
print(feature_importance)
barplot(sort(feature_importance[,1], decreasing = TRUE) , main = "Feature Importance", xlab = "Features", ylab = "Importance", col = "lightgreen", las = 1)
