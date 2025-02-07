# Load required libraries
library(class)      # for kNN
library(dplyr)      # for data manipulation
library(caret)      # for confusion matrix
library(ggplot2)    # for visualization

# Load the dataset
sleeptime_data <- read.csv("sleeptime_prediction_dataset.csv")

# Add a binary 'SleepCategory' column
sleeptime_data$SleepCategory <- ifelse(sleeptime_data$SleepTime >= 7, "sufficient", "insufficient")

# Normalize features
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

features <- c("WorkoutTime", "ReadingTime", "PhoneTime", "WorkHours", "CaffeineIntake", "RelaxationTime")
sleeptime_data[features] <- as.data.frame(lapply(sleeptime_data[features], normalize))

# Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(sleeptime_data$SleepCategory, p = 0.8, list = FALSE)
train_data <- sleeptime_data[train_index, ]
test_data <- sleeptime_data[-train_index, ]

# Prepare feature matrices and target vectors
X_train <- train_data[, features]
X_test <- test_data[, features]
y_train <- train_data$SleepCategory
y_test <- test_data$SleepCategory

# Train the kNN model
k <- 5
predictions <- knn(train = X_train, test = X_test, cl = y_train, k = k)

# Evaluate the model
conf_matrix <- confusionMatrix(factor(predictions), factor(y_test))
print(conf_matrix)

# Visualize the results
ggplot(test_data, aes(x = WorkoutTime, y = SleepTime, color = predictions)) +
  geom_point(size = 2) +
  labs(title = "kNN Predictions for Sleep Categories",
       x = "Workout Time (Normalized)",
       y = "Sleep Time (Hours)",
       color = "Predicted Category") +
  theme_minimal()
