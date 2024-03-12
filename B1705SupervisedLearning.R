# ----- B1705 Week 9 | Supervised Learning | 12.03.2024 -----

# ----- Lecture Work -----

# ----- 1. Standardisation -----
##### 1.1. One Variable: Example -----

# Load necessary library
library(ggplot2)

# Create sample data
set.seed(123)
data <- data.frame(Score = rnorm(100, mean = 50, sd = 10))

# Standardise a vector in the dataset
data$Standardised_Score <- scale(data$Score)

# Plot original vs. standardised data
p1 <- ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  ggtitle("Original Data")

p2 <- ggplot(data, aes(x = Standardised_Score)) +
  geom_histogram(binwidth = 0.1, fill = "green", alpha = 0.7) +
  ggtitle("Standardised Data, mean = 0 and sd = 1")

gridExtra::grid.arrange(p1, p2, ncol = 2)

##### 1.2. All Variables: Example -----

# Create sample dataframe
set.seed(123) 
data <- data.frame(
  Variable1 = rnorm(100, mean = 20, sd = 5),
  Variable2 = runif(100, min = 10, max = 50),
  Variable3 = rnorm(100, mean = 0, sd = 1),
  Variable4 = rbinom(100, size = 10, prob = 0.5),
  Variable5 = runif(100, min = 0, max = 100)  # This variable will not be scaled
)

# Scale first four variables by creating another dataframe
data_scaled <- as.data.frame(lapply(data[1:4], scale))

# Add the unscaled Variable5 back into the scaled dataframe
data_scaled$Variable5 <- data$Variable5

# Display scaled data
head(data_scaled)

# ----- 2. Normalisation -----

# using the same dataset created above
# normalise data using min-max 
data$Normalised_Variable1 <- (data$Variable1 - min(data$Variable1)) / (max(data$Variable1) - min(data$Variable1))

# plot original vs. normalised
p3 <- ggplot(data, aes(x = Normalised_Variable1)) +
  geom_histogram(binwidth = 0.02, fill = "red", alpha = 0.7) +
  ggtitle("Normalised Data")

gridExtra::grid.arrange(p1, p3, ncol = 2)






