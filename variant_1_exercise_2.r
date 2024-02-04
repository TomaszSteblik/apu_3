# Set the working directory
setwd("/workspaces/r-ver-2/task_3")

library(neuralnet)
library(reshape2)
library(ggplot2)

# Load the data
smartfony <- read.csv("smartfony.csv")

# Scale the data
maxs <- apply(smartfony[, c("wyswietlacz", "pamiec_RAM", "pamiec_wbudowana", "aparat_foto", "cena")], 2, max)
mins <- apply(smartfony[, c("wyswietlacz", "pamiec_RAM", "pamiec_wbudowana", "aparat_foto", "cena")], 2, min)
scaled_dane <- as.data.frame(scale(smartfony[, c("wyswietlacz", "pamiec_RAM", "pamiec_wbudowana", "aparat_foto", "cena")], center = mins, scale = maxs - mins))

# Train the neural network
net <- neuralnet(cena ~ wyswietlacz + pamiec_RAM + pamiec_wbudowana + aparat_foto, data = scaled_dane, hidden = c(5, 5), linear.output = TRUE)

# Predict on the same data
predykcja <- predict(net, scaled_dane)

# Check the prediction error
blad <- sum((predykcja - scaled_dane$cena)^2) / nrow(scaled_dane)
print(paste("Błąd predykcji:", blad))

# Plot the neural network
plot(net)

# Convert predictions back to original scale
predykcja_unscaled <- predykcja * (maxs["cena"] - mins["cena"]) + mins["cena"]

# Create a data frame for plotting
plot_data <- data.frame(Actual = smartfony$cena, Predicted = predykcja_unscaled)

# Melt the data for ggplot
plot_data_melt <- melt(plot_data, id.vars = NULL)

# Plot the data
p <- ggplot(data = plot_data_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "Type", y = "Cena", fill = "Type", title = "Trening vs Predykcja") +
  theme_minimal()

print(p)