# ----- B1705 Week 9 | Linear Models for Regression | 12.03.2024 -----
# ----- Pre-Lecture Work -----
##### 1. Example -----

# create a linear regression model
model <- lm(mpg ~ wt + hp, data = mtcars)
summary(model)

## create a visualisation

# libraries
library(ggplot2)
library(ggpmisc)

# summary of the model to extract coefficients and R-squared
model_summary <- summary(model)
equation <- paste("mpg =",
                  format(round(model_summary$coefficients[1], 2), nsmall = 2), "+",
                  format(round(model_summary$coefficients[2], 2), nsmall = 2), "*wt +",
                  format(round(model_summary$coefficients[3], 2), nsmall = 2), "*hp")
r_squared <- paste("R^2 =", format(round(model_summary$r.squared, 2), nsmall = 2))

# Create plot, including regression equation annotated on plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Linear Regression of MPG on Weight and Horsepower",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon (MPG)") +
  annotate("text", x = min(mtcars$wt), y = min(mtcars$mpg), label = equation, hjust = 0, vjust = 0, size = 3.5) +
  annotate("text", x = min(mtcars$wt), y = min(mtcars$mpg) + 2, label = r_squared, hjust = 0, vjust = 0, size = 3.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Print plot
print(p)


# ----- Lecture Work -----
##### 2. Linear Models: Example -----
##### 2.1. Loading Dataset -----

rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/rzu7r7c64dr1o3rr2j444/wk9_1.csv?rlkey=zt3a7hp97no5on5zhdezbkszl&dl=1')

##### 2.2. Normalising Data ----
# i paste in my normalise function
normalise<- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_norm <- as.data.frame(lapply(df, normalise)) # create new dataframe

##### 2.3. Splitting into Training and Testing Sets -----

# first, calculate what number represents 80% of the dataset
sample_size <- floor(0.8 * nrow(df_norm))
# then create that number of indices
train_indices <- sample(seq_len(nrow(df_norm)), size = sample_size)

# create new dataframe by extracting the observations at those rows
train_data <- df_norm[train_indices, ]

# create another dataframe by extracting the observations that are NOT at those rows.
test_data <- df_norm[-train_indices, ]

##### 2.4. Specify the Model -----

##### 2.5. Run Linear Regression Model -----
# fit model
model <- lm(mvp_votes ~ players_age + team_experience + average_points + average_assists + average_rebounds + player_position + injury_history + free_throw_accuracy, data = train_data[, -ncol(train_data)])

# summarise model
summary(model)

# calculate and print AIC
aic_value <- AIC(model)
print(aic_value)

##### 2.6. Check Model Fit -----

model2 <- lm(mvp_votes ~ average_points + average_assists + injury_history, data = train_data)
summary(model2)

aic_value <- AIC(model2)
print(aic_value)

##### 2.7. Apply to Testing Data -----

predictions <- predict(model2, test_data)

##### 2.8. Examine Predictions -----

library(ggplot2)

plot(test_data$mvp_votes, predictions, xlab = "Actual MVP Votes", ylab = "Predicted MVP Votes", main = "Actual vs. Predicted MVP Votes")
abline(0, 1, col = "red")

residuals <- test_data$mvp_votes - predictions

# Residual Plot
plot(test_data$mvp_votes, residuals, main = "Residuals vs. Actual Values",
     xlab = "Actual Values", ylab = "Residuals", pch = 4)
abline(h = 0, col = "red")  # add horizontal line at 0

test_data$predicted <- predictions

# Scatter plot
ggplot(test_data, aes(x = mvp_votes, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()













