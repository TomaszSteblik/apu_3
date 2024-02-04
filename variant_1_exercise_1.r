# Check if the 'neuralnet' package is installed, if not, install it and load the library
if (!require(neuralnet)) {
  install.packages("neuralnet")
  library(neuralnet)
}

# Define a function to calculate the value of y based on the input x
calculate <- function(x){
    (x**3)+(2*x)
}

# Generate a sequence of numbers from 1 to 100 as input values
x <- seq(1, 100)

# Calculate the corresponding y values using the 'calculate' function
y <- calculate(x)

# Create a data frame with the x and y values as training data
training_data <- data.frame(x = x, y = y)

# Find the maximum and minimum values for each column in the training data
maxs <- apply(training_data[, 1:2], 2, max)
mins <- apply(training_data[, 1:2], 2, min)

# Scale the training data using the maximum and minimum values
scaled_training_data <- as.data.frame(scale(training_data, center = mins, scale = maxs - mins))

# Train a neural network model using the scaled training data
net <- neuralnet(y ~ x, data = scaled_training_data, hidden = c(5, 5), linear.output = TRUE, threshold = 0.01)

# Print the trained neural network model
print(net)

# Make predictions using the scaled training data
scaled_predictions <- predict(net, scaled_training_data)

# Rescale the predictions to the original range
rescaled_predictions <- (scaled_predictions * (maxs[2] - mins[2])) + mins[2]

# Plot the original data points and the predicted values
plot(x, y, type = "l", col = "blue", lwd = 2, main = "Predykcja funkcji x^3 + 2x")
lines(x, rescaled_predictions, col = "red", lwd = 2)

# Add a legend to the plot
legend("topleft", legend = c("Trening", "Predykcja"), col = c("blue", "red"), lwd = 2)