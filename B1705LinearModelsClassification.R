# ----- B1705 Week 9 | Linear Models for Classification | 12.03.2024 -----
# ----- Pre-Lecture Work -----
# ----- 1. Logistic Regression: Example -----

set.seed(123)  # Setting seed for reproducibility
n <- 100  # Number of games

# Simulating team and opponent points
team_points <- rnorm(n, mean = 50, sd = 10)
opponent_points <- rnorm(n, mean = 50, sd = 10)

# Introducing randomness to the win/loss outcome
# Even if a team has higher points, it doesn't always win
win <- ifelse(team_points > opponent_points, 1, rbinom(n, 1, 0.45))

# Adding some noise to ensure there's no perfect prediction
# Now, even teams with fewer points have a chance of winning
win[team_points < opponent_points] <- ifelse(runif(sum(team_points < opponent_points)) > 0.55, 0, win[team_points < opponent_points])

data <- data.frame(team_points, opponent_points, win)

# Logistic Regression Model
model <- glm(win ~ team_points + opponent_points, data = data, family = "binomial")

# Summary of the model
summary(model)

library(ggplot2)

# Predicting and visualising results
data$predicted_prob <- predict(model, data, type = "response")
ggplot(data, aes(x = team_points - opponent_points, y = predicted_prob, color = factor(win))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Points Difference (Team - Opponent)", y = "Probability of Winning", color = "Actual Outcome") +
  theme_minimal()

# ----- 2. Tree-based Methods: Example -----

set.seed(123)

# Generate synthetic data
n <- 500 # number observations

Age <- sample(18:35, n, replace = TRUE) 
Experience <- sample(1:17, n, replace = TRUE) # Experience in years
SkillLevel <- sample(c("High", "Medium", "Low"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)) 
PlayProfessional <- ifelse(Age > 25 & Experience > 5 & SkillLevel == "High", "Yes", "No")

# Adding some noise
noise_indices <- sample(1:n, size = 0.1 * n) # Introduce 10% noise
PlayProfessional[noise_indices] <- ifelse(PlayProfessional[noise_indices] == "Yes", "No", "Yes")

sports_data <- data.frame(Age, Experience, SkillLevel, PlayProfessional)

library(rpart)
library(rpart.plot)

model <- rpart(PlayProfessional ~ Age + Experience + SkillLevel, data = sports_data, method = "class")
rpart.plot(model, main="Can we predict whether an athlete is a professional?", extra=104)

##### 2.1. Random Forest: Example -----

library(randomForest)

set.seed(123) 
sports_data$PlayProfessional <- as.factor(sports_data$PlayProfessional)
sports_data$SkillLevel <- as.factor(sports_data$SkillLevel)

# randomForest model
rf_model <- randomForest(PlayProfessional ~ Age + Experience + SkillLevel, data=sports_data, ntree=500, importance=TRUE)

# Print model summary
print(rf_model)

# Plot variable importance}
varImpPlot(rf_model)

# Assuming you have a separate test set called sports_data_test
# predictions <- predict(rf_model, newdata=sports_data_test)

# For illustration, we'll use the same dataset for prediction
predictions <- predict(rf_model, newdata=sports_data)

# Evaluating model accuracy
confusionMatrix <- table(sports_data$PlayProfessional, predictions)
print(confusionMatrix)

# Calculate Accuracy
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))

##### 2.2. Gradient Boosting: Example -----
library(gbm)

# Convert 'PlayProfessional' to a numeric format where No = 0 and Yes = 1
sports_data$PlayProfessionalNumeric <- as.numeric(sports_data$PlayProfessional) - 1

# Fit the gradient boosting model
gbm_model <- gbm(PlayProfessionalNumeric ~ Age + Experience + SkillLevel, 
                 data=sports_data, 
                 distribution="bernoulli",
                 n.trees=1000, 
                 interaction.depth=3,
                 shrinkage=0.01,
                 n.minobsinnode=10,
                 cv.folds = 5,
                 verbose = FALSE)


# Plot variable importance
summary(gbm_model, plot = TRUE, main = "Variable Importance")

# Predict on the training set (ideally, we'd use a separate test set)
predictions <- predict(gbm_model, newdata=sports_data, n.trees=1000, type="response")

# Convert probabilities to binary outcome based on a 0.5 threshold
predicted_classes <- ifelse(predictions > 0.5, "Yes", "No")

# Create confusion matrix
confusionMatrix <- table(Actual = sports_data$PlayProfessional, Predicted = predicted_classes)

# Print the confusion matrix
print(confusionMatrix)

# Calculate and print accuracy
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))

# Plot training loss over iterations
plot(gbm_model, overlay = TRUE)

# ----- 3. Support Vector Machines: Example -----
# Load the packages
library(e1071)
library(ggplot2)

# Create a synthetic dataset
set.seed(1)  # For reproducibility
x <- matrix(rnorm(40), ncol = 2)  # 20 data points with 2 features
y <- c(rep(-1, 10), rep(1, 10))  # Binary target variable
x[y == 1, ] <- x[y == 1, ] + 1  # Shift data to make them separable

# Combine into a data frame
data <- data.frame(x = x[, 1], y = x[, 2], class = as.factor(y))


# Train the SVM model
svm_model <- svm(class ~ ., data = data, type = 'C-classification', kernel = 'linear')

# Create a grid to predict over the entire space
grid <- with(data, expand.grid(x = seq(min(x), max(x), length = 100),
                               y = seq(min(y), max(y), length = 100)))

# Predict using the SVM model
grid$class <- predict(svm_model, grid)

ggplot(data, aes(x = x, y = y, color = class)) +
  geom_tile(data = grid, aes(fill = class, alpha = 0.3), interpolate = TRUE) +
  geom_point() +
  scale_fill_manual(values = c('-1' = 'blue', '1' = 'red')) +
  scale_color_manual(values = c('-1' = 'blue', '1' = 'red')) +
  theme_minimal() +
  labs(title = 'SVM Decision Boundary', x = 'Feature 1', y = 'Feature 2')

# ----- Lecture Work -----
# ----- Models for Logistic Regression: Example -----
##### 3.1. Load Data -----
rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/rzu7r7c64dr1o3rr2j444/wk9_1.csv?rlkey=zt3a7hp97no5on5zhdezbkszl&dl=1')

##### 3.2. Create Factor(s) -----

df$all_star <- as.factor(df$all_star)

##### 3.3. Normalise Predictor Variables -----
# Normalise using Min-Max scaling
normalise_min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
predictor_columns <- c('average_points', 'average_assists', 'average_rebounds', 'team_experience')
df[predictor_columns] <- as.data.frame(lapply(df[predictor_columns], normalise_min_max))

##### 3.4. Create Training and Testing Datasets -----
set.seed(123)
sample_size <- floor(0.8 * nrow(df))  # 80% for training
train_indices <- sample(seq_len(nrow(df)), size = sample_size)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

##### 3.5. Fit Mode -----
model <- glm(all_star ~ average_points + average_assists + average_rebounds + team_experience, 
             data = train_data, family = binomial())

##### 3.6. Inspect Model Output -----
summary(model)

##### 3.7. Check Model Fit -----
library(ResourceSelection)
hoslem.test(train_data$all_star, fitted(model))

##### 3.8. Apply to Testing Dataset -----
predicted_probabilities <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

##### 3.9. Examine Predictions -----

confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$all_star)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

library(pROC)

roc_curve <- roc(test_data$all_star, predicted_probabilities)
plot(roc_curve)

auc_value <- auc(roc_curve)
print(paste("AUC value:", auc_value))

# The AUC value helps to interpret the model's ability to discriminate between classes.

# ----- 4. Decision Trees: Demonstration -----
##### 4.1. Load Data, Create Factors -----
rm(list=ls())

# input dataset
df <- read.csv('https://www.dropbox.com/scl/fi/97hrlppmr4z7g3a1opc9o/wk9_3.csv?rlkey=atjzew7u0v9ff0moo4gqrr5iv&dl=1')

# create factor with two levels (0 and 1)
df$top_player <- factor(df$top_player, levels = c(0, 1))

# it can be helpful to create labels with your factors
levels(df$top_player) <- c('no', 'yes')

##### 4.2. Feature Selection -----
# Load necessary libraries
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret) 
library(ggplot2)

# Split the data into training and testing sets
set.seed(101)

# create indices for 80% selection
training_indices <- createDataPartition(df$top_player, p = 0.8, list = FALSE)
train_data <- df[training_indices, ] # create training data
test_data <- df[-training_indices, ] # create testing data

##### 4.3. Fitting and Inspecting Model -----
# Fit the model
single_tree_model <- rpart(top_player ~ ., data = train_data, method = "class")

rpart.plot(single_tree_model, main="Is player a 'top' player?", extra=104)

##### 4.4. Predict and Evaluate -----
# Predict and evaluate
single_tree_predictions <- predict(single_tree_model, test_data, type = "class")
confusionMatrix(single_tree_predictions, test_data$top_player)

##### 4.5. Inspecting with Non-Testing Data -----
# use those criteria to check accuracy of predictions
filtered_data <- subset(df, average_rebounds >= 7 & average_assists >= 5.3 & average_points >= 15)

# Ensure ggplot2 is loaded
library(ggplot2)

# Create a bar plot for the count of top players and non-top players using the filtered data
ggplot(filtered_data, aes(x = top_player, fill = as.factor(top_player))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "blue"), name = "Top Player", 
                    labels = c("Not actually top player", "Top Player")) +
  labs(title = "Predictions vs. Actuality - model got most predictions right",
       x = "Top Player Status",
       y = "Count") +
  theme_minimal()

# ----- 5. Random Forests: Example -----

##### 5.1. Clear and Load Data -----
rm(list=ls())

df <- read.csv('https://www.dropbox.com/scl/fi/97hrlppmr4z7g3a1opc9o/wk9_3.csv?rlkey=atjzew7u0v9ff0moo4gqrr5iv&dl=1')

##### 5.2. Data Preparation -----
# create factor
df$top_player <- factor(df$top_player, levels = c(0, 1))
levels(df$top_player) <- c('no', 'yes')

# Normalise using Min-Max scaling
normalise_min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
predictor_columns <- c('players_age', 'team_experience', 'average_points', 'average_assists', 'average_rebounds', 'player_position', 'injury_history', 'free_throw_accuracy')
df[predictor_columns] <- as.data.frame(lapply(df[predictor_columns], normalise_min_max))

##### 5.3. Split into Training and Testing Sets -----

set.seed(101)
training_indices <- createDataPartition(df$top_player, p = 0.8, list = FALSE)
train_data <- df[training_indices, ]
test_data <- df[-training_indices, ]

##### 5.4. Create Random Forest -----
# Fit model with all IVs
random_forest_model <- randomForest(top_player ~ ., data = train_data)

# Predict and evaluate
rf_predictions <- predict(random_forest_model, test_data)
confusionMatrix(rf_predictions, test_data$top_player)

##### 5.5. Plotting Variable Importance -----
importance <- importance(random_forest_model)
varImpPlot(random_forest_model)

# Method of Plotting Importance No.2
library(ggplot2)

# Transforming the importance data frame for ggplot
importance_df <- data.frame(Feature = rownames(importance), Importance = importance[,1])

# Create the plot
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for horizontal bar chart
  labs(title = "Feature Importance in Predicting 'Top Player'",
       x = "Feature",
       y = "Importance") +
  theme_minimal()

# ----- 6. Support Vector Machines (SVM): Demonstration -----
##### 6.1. Load and Prepare Data -----
library(e1071)
library(caret)

rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/97hrlppmr4z7g3a1opc9o/wk9_3.csv?rlkey=atjzew7u0v9ff0moo4gqrr5iv&dl=1')

df$top_player <- factor(df$top_player, levels = c(0, 1))

##### 6.2. Splitting into Training and Test Data -----
training_indices <- createDataPartition(df$top_player, p = 0.8, list = FALSE)
train_data <- df[training_indices, ]
test_data <- df[-training_indices, ]

##### 6.3. Scale Data -----
# Scale predictor variables in both training and testing sets
# Exclude the target variable 'top_player' from this

train_data_scaled <- as.data.frame(scale(train_data[,-ncol(train_data)]))
train_data_scaled$top_player <- train_data$top_player

test_data_scaled <- as.data.frame(scale(test_data[,-ncol(test_data)]))
test_data_scaled$top_player <- test_data$top_player

##### 6.4. Run SVM Model -----
# SVM model on training data
svm_model <- svm(top_player ~ ., data = train_data_scaled, type = 'C-classification', kernel = "radial", cost = 1)

##### 6.5. Create Predictions -----
# Predict using the SVM model on  test data
svm_predictions <- predict(svm_model, test_data_scaled)

##### 6.6. Make Predictions Factors -----
# Since the prediction outputs are factors, ensure that they have the same levels as the test data outcomes
svm_predictions <- factor(svm_predictions, levels = levels(test_data_scaled$top_player))

##### 6.7. Check Model Fit -----
# use confusion matrix
conf_matrix <- confusionMatrix(svm_predictions, test_data_scaled$top_player)
print(conf_matrix)

# Print overall statistics like accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(accuracy)

# Print class-specific measures
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
print(sensitivity)

print(specificity)

###### 6.8. Visualisations -----
library(ggplot2)
library(e1071)

features_to_visualise <- c("average_points", "average_assists")

# Create a subset of data with the selected features and the outcome
svm_data_vis <- data.frame(train_data_scaled[, features_to_visualise], top_player = train_data_scaled$top_player)

# Train the SVM model on the subset
svm_model_vis <- svm(top_player ~ ., data = svm_data_vis, kernel = "radial", cost = 1)

# Create a grid to predict over the feature space
plot_grid <- expand.grid(average_points = seq(min(svm_data_vis$average_points), max(svm_data_vis$average_points), length = 100),
                         average_assists = seq(min(svm_data_vis$average_assists), max(svm_data_vis$average_assists), length = 100))

# Predict the class membership probabilities
plot_grid$top_player <- predict(svm_model_vis, plot_grid)

# Create the plot
ggplot(svm_data_vis, aes(x = average_points, y = average_assists)) +
  geom_tile(data = plot_grid, aes(fill = top_player, x = average_points, y = average_assists), alpha = 0.5) +
  geom_point(aes(color = top_player)) +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  labs(title = "SVM Decision Boundary", x = "Average Points", y = "Average Assists") +
  theme_minimal()
