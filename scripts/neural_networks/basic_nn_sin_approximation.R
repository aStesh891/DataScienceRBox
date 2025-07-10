# Basic Neural Network for Sin(x) Approximation
# Implementation with Tanh activation function

# ---------------------------
# 1. Data Preparation
# ---------------------------
set.seed(123)
x <- seq(0, 2 * pi, length.out = 100)
y <- sin(x)

# Normalization
x_scaled <- (x - min(x)) / (max(x) - min(x))
y_scaled <- y

# Convert to column matrices
X <- matrix(x_scaled, ncol = 1)
Y <- matrix(y_scaled, ncol = 1)

# ---------------------------
# 2. Network Parameters
# ---------------------------
n_input <- 1
n_hidden <- 10
n_output <- 1
epochs <- 5000
lr <- 0.001

# ---------------------------
# 3. Weight Initialization (He)
# ---------------------------
W1 <- matrix(
  rnorm(n_input * n_hidden, sd = sqrt(2 / n_input)),
  nrow = n_input,
  ncol = n_hidden
)

W2 <- matrix(
  rnorm(n_hidden * n_output, sd = sqrt(2 / n_hidden)),
  nrow = n_hidden, 
  ncol = n_output
)

b1 <- matrix(0, nrow = 1, ncol = n_hidden)
b2 <- matrix(0, nrow = 1, ncol = n_output)

# ---------------------------
# 4. Activation Functions
# ---------------------------
tanh_safe <- function(x) tanh(pmin(pmax(x, -20), 20))
dtanh_safe <- function(x) 1 - tanh_safe(x)^2

sigmoid <- tanh_safe
dsigmoid <- dtanh_safe

# ---------------------------
# 5. Training Process
# ---------------------------
loss_history <- numeric(epochs)

for (epoch in 1:epochs) {
  
  # Forward pass
  Z1 <- X %*% W1 + matrix(rep(b1, each = nrow(X)), nrow = nrow(X))
  A1 <- sigmoid(Z1)
  Z2 <- A1 %*% W2 + matrix(rep(b2, each = nrow(A1)), nrow = nrow(A1))
  Y_pred <- Z2
  
  # Loss calculation
  error <- Y_pred - Y
  loss <- mean(error^2)
  loss_history[epoch] <- loss
  
  # Backpropagation
  dZ2 <- 2 * error
  dW2 <- t(A1) %*% dZ2
  db2 <- colSums(dZ2)
  
  dA1 <- dZ2 %*% t(W2)
  dZ1 <- dA1 * dsigmoid(Z1)
  dW1 <- t(X) %*% dZ1
  db1 <- colSums(dZ1)
  
  # Weight update
  W2 <- W2 - lr * dW2
  b2 <- b2 - lr * db2
  W1 <- W1 - lr * dW1
  b1 <- b1 - lr * db1
  
  # Progress tracking
  if (epoch %% 100 == 0) {
    cat("Epoch", epoch, "Loss:", loss, "\n")
  }
}

# ---------------------------
# 6. Results Visualization
# ---------------------------
# Training loss plot
plot(
  1:epochs, 
  loss_history, 
  type = "l", 
  col = "darkgreen", 
  lwd = 2,
  xlab = "Epoch", 
  ylab = "Loss", 
  main = "Training Loss"
)

# Final prediction
Z1 <- X %*% W1 + matrix(rep(b1, each = nrow(X)), nrow = nrow(X))
A1 <- sigmoid(Z1)
Z2 <- A1 %*% W2 + matrix(rep(b2, each = nrow(A1)), nrow = nrow(A1))
Y_pred <- Z2
y_pred_final <- Y_pred

# Comparison plot
require(ggplot2)
require(plotly)

plot_data <- data.frame(
  x = x, 
  y = y, 
  y_pred = y_pred_final
)

ggplotly(
  ggplot(aes(x = x), data = plot_data) +
    geom_line(aes(y = y), col = "darkblue", lwd = 1) +
    geom_line(aes(y = y_pred), col = "darkred", lwd = 1, lty = 2) +
    theme_gray() +
    labs(title = "Sin(x) Approximation with Neural Network")
)

