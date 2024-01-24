install.packages("tree")
install.packages("rpart.plot")
library(tree)
library(rpart)
library(rpart.plot)

olympics = read.csv("C:/Users/jayac/OneDrive/Desktop/McGill/Multivariate/Datasets/Olympics.csv")
attach(olympics)
View(olympics)

# Replacing the NA values in Medal column with 'None'
olympics$Medal[is.na(olympics$Medal)] <- "None"

# Drop the unwanted columns
olympics <- subset(olympics, select = -c(ID, Name, Team, Event, Year, City, Games))

# Getting the count of Missing Values
missing_counts <- colSums(is.na(olympics))

# Display the counts
print(missing_counts)

# Delete the Missing values
olympics_cleaned <- na.omit(olympics)

library(dplyr)

# Getting the count of Duplicate values in the dataset
duplicate_count <- sum(duplicated(olympics_cleaned) | duplicated(olympics_cleaned, fromLast = TRUE))

# Display the count
print(duplicate_count)

# Assuming 'olympics_cleaned' is your cleaned dataframe
olympics_cleaned <- olympics_cleaned[!duplicated(olympics_cleaned), ]

# Calculate BMI and create a new column
olympics_cleaned$BMI <- olympics_cleaned$Weight / ((olympics_cleaned$Height / 100)^2)

install.packages("caret")
library(caret)

# Identify numeric columns
numeric_columns <- sapply(olympics_cleaned, is.numeric)

# Create a new column 'Medal_Group' with binary values
olympics_cleaned$Medal_Group <- ifelse(olympics_cleaned$Medal %in% c("Gold", "Silver", "Bronze"), "Gain", "Loss")

# Drop the unwanted columns
olympics_cleaned <- subset(olympics_cleaned, select = -c(Medal))

# Filter data for female athletes
female_data <- olympics_cleaned[olympics_cleaned$Sex == "F", ]
male_data <- olympics_cleaned[olympics_cleaned$Sex == "M", ]

# Count the occurrences of each sport for 'Gain'
gain_sport_counts <- table(olympics_cleaned$Sport[olympics_cleaned$Medal_Group == "Gain"])

# Order the sports by frequency in descending order
sorted_gain_sports <- sort(gain_sport_counts, decreasing = TRUE)

# Select the top ten sports for 'Gain'
top_ten_gain_sports <- names(head(sorted_gain_sports, 10))

# Filter the data for the top ten sports with 'Gain'
gain_data_top_ten <- olympics_cleaned[olympics_cleaned$Sport %in% top_ten_gain_sports & olympics_cleaned$Medal_Group == "Gain", ]

# Create a bar plot for the top 10 sports with 'Gain'
ggplot(gain_data_top_ten, aes(x = Sport, fill = Medal_Group)) +
  geom_bar() +
  labs(title = "Top 10 Sports with Maximum 'Gain' in Medal_Group",
       x = "Sport",
       y = "Count",
       fill = "Medal_Group") +
  theme_minimal()

# Count the occurrences of each sport for 'Gain'
loss_sport_counts <- table(olympics_cleaned$Sport[olympics_cleaned$Medal_Group == "Loss"])

# Order the sports by frequency in descending order
sorted_loss_sports <- sort(loss_sport_counts, decreasing = TRUE)

# Select the top ten sports for 'Gain'
top_ten_loss_sports <- names(head(sorted_loss_sports, 10))

# Filter the data for the top ten sports with 'Gain'
loss_data_top_ten <- olympics_cleaned[olympics_cleaned$Sport %in% top_ten_loss_sports & olympics_cleaned$Medal_Group == "Loss", ]

# Create a bar plot for the top 10 sports with 'Gain'
ggplot(loss_data_top_ten, aes(x = Sport, fill = Medal_Group)) +
  geom_bar() +
  labs(title = "Top 10 Sports with Maximum 'Loss' in Medal_Group",
       x = "Sport",
       y = "Count",
       fill = "Medal_Group") +
  theme_minimal()

# Count the occurrences of each sport
sport_counts <- table(olympics_cleaned$Sport)

# Order the sports by frequency in descending order
sorted_sports <- sort(sport_counts, decreasing = TRUE)

# Select the top ten sports
top_ten_sports <- head(sorted_sports, 10)

# Plot the bar chart
barplot(top_ten_sports, col = "skyblue",
        main = "Top Ten Popular Sports in Olympics",
        xlab = "Sport", ylab = "Frequency",
        cex.names = 0.7, las = 2)

# Count the occurrences of each sport for females
female_sport_counts <- table(female_data$Sport)

# Order the sports by frequency in descending order
sorted_female_sports <- sort(female_sport_counts, decreasing = TRUE)

# Select the top ten sports for females
top_ten_female_sports <- names(head(sorted_female_sports, 10))

# Filter the data for the top ten sports
female_data_top_ten <- female_data[female_data$Sport %in% top_ten_female_sports, ]

# Create a bar plot for medal distribution among females for the top 10 sports
ggplot(female_data_top_ten, aes(x = Sport)) +
  geom_bar() +
  labs(title = "Distribution of Medals Among Female Athletes (Top 10 Sports)",
       x = "Sport",
       y = "Count") +
  theme_minimal()


# Count the occurrences of each sport for males
male_sport_counts <- table(male_data$Sport)

# Order the sports by frequency in descending order
sorted_male_sports <- sort(male_sport_counts, decreasing = TRUE)

# Select the top ten sports for males
top_ten_male_sports <- names(head(sorted_male_sports, 10))

# Filter the data for the top ten sports
male_data_top_ten <- male_data[male_data$Sport %in% top_ten_male_sports, ]

# Create a bar plot for medal distribution among males for the top 10 sports
ggplot(male_data_top_ten, aes(x = Sport)) +
  geom_bar() +
  labs(title = "Distribution of Medals Among Male Athletes (Top 10 Sports)",
       x = "Sport",
       y = "Count") +
  theme_minimal()

# Identify categorical columns
categorical_columns <- c("NOC", "Season","Sex","Sport")

# One-hot encode categorical columns
dummy_vars <- dummyVars(~ ., data = olympics_cleaned[, categorical_columns])
olympics_encoded <- predict(dummy_vars, newdata = olympics_cleaned)

# Combine the one-hot encoded columns with the original dataframe
olympics_cleaned <- cbind(olympics_cleaned[, -(which(names(olympics_cleaned) %in% categorical_columns))], olympics_encoded)

# Split the data into training and testing sets
set.seed(123)
# Split the data into training and testing sets
sample_indices <- createDataPartition(olympics_cleaned$Medal_Group, p = 0.8, list = FALSE)
train_data <- olympics_cleaned[sample_indices, ]
test_data <- olympics_cleaned[-sample_indices, ]

# Ensure 'Medal_Group' is a factor variable in both training and testing sets
train_data$Medal_Group <- as.factor(train_data$Medal_Group)
test_data$Medal_Group <- as.factor(test_data$Medal_Group)

# Pruning a tree with minimum split and number of nodes parameters
myoverfittedtree <- rpart(
  Medal_Group ~ ., 
  data = train_data, 
  control = rpart.control(
    cp = 0.001,
    minsplit = 300,        # Adjust the minimum number of observations in a node for a split
    minbucket = 200        # Adjust the minimum number of observations in a terminal node
  )
)

# Print the complexity parameter table
printcp(myoverfittedtree)

# Plot the complexity parameter plot
plotcp(myoverfittedtree)

# Find the optimal complexity parameter
opt_cp <- myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]), "CP"]

# Prune the tree with the optimal complexity parameter
mybesttree <- prune(myoverfittedtree, cp = opt_cp)

# Print the complexity parameter table for the pruned tree
printcp(mybesttree)

# Plot the pruned decision tree
rpart.plot(mybesttree)

# Print the optimal complexity parameter
print(opt_cp)

# Predict on the testing set
predicted_classes <- predict(mybesttree, newdata = test_data, type = "class")

# Confusion matrix
conf_matrix <- table(Actual = test_data$Medal_Group, Predicted = predicted_classes)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy on Test Set:", accuracy))

# Calculate precision, recall, and F1-score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))

# Area under the ROC curve (AUC)
library(pROC)
roc_curve <- roc(as.numeric(test_data$Medal_Group == "Gain"), as.numeric(predicted_classes == "Gain"))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

print(mybesttree)

# Convert the classes to numeric (0 and 1)
actual <- as.numeric(test_data$Medal_Group == "Gain")
predicted <- as.numeric(predicted_classes == "Gain")

# Create a ROC curve
roc_curve <- roc(actual, predicted)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add labels
abline(h = 0, v = 1, lty = 2, col = "gray")
legend("bottomright", legend = c("AUC = ", round(auc(roc_curve), 2)), col = "blue", lwd = 2)
summary(mybesttree)
# Display the plot


######Model 2######

olympics = read.csv("C:/Users/jayac/OneDrive/Desktop/McGill/Multivariate/Datasets/Olympics.csv")
attach(olympics)
View(olympics)

# Replacing the NA values in Medal column with 'None'
olympics$Medal[is.na(olympics$Medal)] <- "None"

# Drop the unwanted columns
olympics <- subset(olympics, select = -c(ID, Season,  Name, Team, Event, Year, City, Games))

# Getting Missing value count
missing_counts <- colSums(is.na(olympics))

# Display the counts
print(missing_counts)

# Delete the rows with missing values
olympics_cleaned <- na.omit(olympics)


library(dplyr)

# Getting duplicate value counts in the dataset
duplicate_count <- sum(duplicated(olympics_cleaned) | duplicated(olympics_cleaned, fromLast = TRUE))

# Display the count
print(duplicate_count)

# Drop the duplicate records
olympics_cleaned <- olympics_cleaned[!duplicated(olympics_cleaned), ]

# Calculate BMI and create a new column
olympics_cleaned$BMI <- olympics_cleaned$Weight / ((olympics_cleaned$Height / 100)^2)

#install.packages("caret")
library(caret)

# Create a new column 'Medal_Group' with binary values
olympics_cleaned$Medal_Group <- ifelse(olympics_cleaned$Medal %in% c("Gold", "Silver", "Bronze"), "Gain", "Loss")

# Drop the unwanted columns
olympics_cleaned <- subset(olympics_cleaned, select = -c(Medal))

# Identify categorical columns
categorical_columns <- c("NOC","Sex","Sport")

# One-hot encode categorical columns
dummy_vars <- dummyVars(~ ., data = olympics_cleaned[, categorical_columns])
olympics_encoded <- predict(dummy_vars, newdata = olympics_cleaned)

# Combine the one-hot encoded columns with the original dataframe
olympics_cleaned <- cbind(olympics_cleaned[, -(which(names(olympics_cleaned) %in% categorical_columns))], olympics_encoded)

# Split the data into training and testing sets
set.seed(123)
# Split the data into training and testing sets
sample_indices <- createDataPartition(olympics_cleaned$Medal_Group, p = 0.8, list = FALSE)
train_data <- olympics_cleaned[sample_indices, ]
test_data <- olympics_cleaned[-sample_indices, ]

# Ensure 'Medal_Group' is a factor variable in both training and testing sets
train_data$Medal_Group <- as.factor(train_data$Medal_Group)
test_data$Medal_Group <- as.factor(test_data$Medal_Group)

# Pruning a tree with minimum split and number of nodes parameters
myoverfittedtree1 <- rpart(
  Medal_Group ~ ., 
  data = train_data, 
  control = rpart.control(
    cp = 0.001,
    minsplit = 300,        # Adjust the minimum number of observations in a node for a split
    minbucket = 200        # Adjust the minimum number of observations in a terminal node
  )
)

# Print the complexity parameter table
printcp(myoverfittedtree1)

# Plot the complexity parameter plot
plotcp(myoverfittedtree1)

# Find the optimal complexity parameter
opt_cp <- myoverfittedtree1$cptable[which.min(myoverfittedtree1$cptable[,"xerror"]), "CP"]

# Prune the tree with the optimal complexity parameter
myfinaltree <- prune(myoverfittedtree1, cp = opt_cp)

# Print the complexity parameter table for the pruned tree
printcp(myfinaltree)

# Plot the pruned decision tree
rpart.plot(myfinaltree)

# Print the optimal complexity parameter
print(opt_cp)

# Predict on the testing set
predicted_classes <- predict(myfinaltree, newdata = test_data, type = "class")

# Confusion matrix
conf_matrix <- table(Actual = test_data$Medal_Group, Predicted = predicted_classes)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy on Test Set:", accuracy))

# Calculate precision, recall, and F1-score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))

# Area under the ROC curve (AUC)
library(pROC)
roc_curve <- roc(as.numeric(test_data$Medal_Group == "Gain"), as.numeric(predicted_classes == "Gain"))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

print(myfinaltree)

# Convert the classes to numeric (0 and 1)
actual <- as.numeric(test_data$Medal_Group == "Gain")
predicted <- as.numeric(predicted_classes == "Gain")

# Create a ROC curve
roc_curve <- roc(actual, predicted)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add labels and Display the Plot
abline(h = 0, v = 1, lty = 2, col = "gray")
legend("bottomright", legend = c("AUC = ", round(auc(roc_curve), 2)), col = "blue", lwd = 2)

summary(myfinaltree)
myfinaltree