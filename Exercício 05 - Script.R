library(e1071)
library(MASS)

set.seed(42)

mu <- c(0, 0)
Sigma <- diag(2)
X <- mvrnorm(n = 200, mu = mu, Sigma = Sigma)

X_scaled <- scale(X)

plot_ocsvm_2d <- function(nu_value = 0.05, gamma_value = 0.5) {
  # Treinamento do modelo
  model <- svm(X_scaled, y = rep(1, nrow(X_scaled)),
               type = "one-classification",
               kernel = "radial",
               gamma = gamma_value,
               nu = nu_value)
  
  pred <- predict(model, X_scaled)
  
  x_seq <- seq(min(X_scaled[,1]) - 0.5, max(X_scaled[,1]) + 0.5, length.out = 150)
  y_seq <- seq(min(X_scaled[,2]) - 0.5, max(X_scaled[,2]) + 0.5, length.out = 150)
  grid <- expand.grid(x = x_seq, y = y_seq)
  grid_matrix <- as.matrix(grid)
  
  grid_pred <- predict(model, grid_matrix)
  
  plot(grid, col = ifelse(grid_pred, rgb(0.9, 1, 0.9), rgb(1, 0.9, 0.9)),
       pch = 15, cex = 0.4, xlab = "X1", ylab = "X2",
       main = paste("One-Class SVM – nu =", nu_value, ", gamma =", gamma_value))
  points(X_scaled, col = ifelse(pred, "blue", "red"), pch = 20)
  legend("topright", legend = c("Normal", "Outlier"),
         col = c("blue", "red"), pch = 20)
}

par(mfrow = c(1, 3))
plot_ocsvm_2d(nu_value = 0.01)
plot_ocsvm_2d(nu_value = 0.05)
plot_ocsvm_2d(nu_value = 0.1)
