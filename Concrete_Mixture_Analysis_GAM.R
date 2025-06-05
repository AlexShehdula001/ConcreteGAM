library(glmnet)

library(mgcv)
library(gamair)
library("recipes")
library(caret)
library(ggplot2)
library(car)

library(reshape2)

PLOTTEXTSIZE <- 4

data <- read.csv('/Users/alexshehdula/Downloads/concrete+compressive+strength/Concrete_Data.csv')
data

covariates <- names(data)[-1]

# Set up a multi-panel plot

pdf("linear_rel.pdf",width = 12)

par(mfrow = c(2, length(covariates)/2))

# Loop through each covariate and create a scatter plot
for (covariate in covariates) {
  plot(data[[covariate]], data$response, main = covariate,
       xlab = covariate, ylab = "Response")
}

dev.off()

plot_interaction <- function(df, response, covariate1, covariate2) {
  ggplot(df, aes_string(x = covariate1, y = response, color = covariate2, group = covariate2)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(title = paste("Interaction between", covariate1, "and", covariate2))
}

interaction.plot(x.factor = sample_data$Effort, 
                 trace.factor = sample_data$gender,  
                 response = sample_data$Result, fun = median)

data$Concrete.compressive.strength

int_cement_water <- data$Cement * data$Water
plot(data,int_cement_water)

# Create the plot using ggplot2
ggplot(data_melted, aes(x = value, y = response)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "Covariates", y = "Response")



data_scaled<-data
data_scaled <- data
for (i in 1:(ncol(data) - 1)) data_scaled[ ,i] <- (data_scaled[ ,i] - mean(data_scaled[ ,i])) / sd(data_scaled[ ,i])

data_scaled

nrow(data_scaled)

## Linear Model
linmod <- lm(Concrete.compressive.strength ~ ., data = data_scaled)
summary(linmod)

vif(linmod)
plot(residuals(linmod) ~ fitted(linmod), pch = 12,
     main = "Residual plot, linear model", 
     xlab = "Residuals", ylab = "Fitted values",
     cex.main = PLOTTEXTSIZE, cex.lab = PLOTTEXTSIZE, cex.axis = PLOTTEXTSIZE
)


abline(h = 0, col = "red", lty = "dashed")


par(mfrow = c(1, 1))


# Split data into training and testing sets
train_indices <- sample(1:nrow(data_scaled), size = 0.8 * nrow(data_scaled))
train_data <- data_scaled[train_indices, ]
test_data <- data_scaled[-train_indices, ]

# Create model matrix for train and test data
X_train <- model.matrix(Concrete.compressive.strength ~ ., data = train_data)
X_test <- model.matrix(Concrete.compressive.strength ~ ., data = test_data)
y_train <- train_data$Concrete.compressive.strength
y_test <- test_data$Concrete.compressive.strength

# Fit ridge regression model with cross-validation
glmnetridgecv <- cv.glmnet(X_train, y_train, alpha = 0)
minlambda <- glmnetridgecv$lambda.min

# Fit the final model using the optimal lambda
glmnetridge_withcv <- glmnet(X_train, y_train, alpha = 0, lambda = minlambda)

# Predict on the test set
predictions <- predict(glmnetridge_withcv, s = minlambda, newx = X_test)

# Calculate R-squared
rss <- sum((predictions - y_test)^2)
tss <- sum((y_test - mean(y_test))^2)
r2 <- 1 - rss/tss

# Print R-squared
cat("R-squared: ", r2, "\n")





## Ridge
X <- model.matrix(linmod)
y <- data_scaled$Concrete.compressive.strength
glmnetridgecv <- cv.glmnet(X, y, alpha = 0)
plot(glmnetridgecv)
minlambda <- glmnetridgecv$lambda.min # = 0.8312613
glmnetridge_nocv <- glmnet(X, y, alpha = 0)
plot(glmnetridge_nocv, xvar = "lambda")

glmnetridge_withcv <- glmnet(X, y, alpha = 0, lambda = minlambda)
glmnetridge_withcv$beta # Coefficient estimates
cbind(glmnetridge_withcv$beta, coef(linmod))



# Fit the final model using the optimal lambda
glmnetridge_withcv <- glmnet(X_train, y_train, alpha = 0, lambda = minlambda)

# Predict on the test set
predictions <- predict(glmnetridge_withcv, s = minlambda, newx = X_test)

# Calculate performance metrics
mae <- mean(abs(predictions - y_test))
rmse <- sqrt(mean((predictions - y_test)^2))
rss <- sum((predictions - y_test)^2)
tss <- sum((y_test - mean(y_test))^2)
r2 <- 1 - rss/tss

# Print performance metrics
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("R-squared: ", r2, "\n")
fitted_values <- predict(glmnetridge_withcv, s = minlambda, newx = X_train)


glmnetridge_withcv <- glmnet(X_train, y_train, alpha = 0, lambda = minlambda)

# Predict on the training set to get fitted values
fitted_values <- predict(glmnetridge_withcv, s = minlambda, newx = X_train)

# Calculate residuals using the training set
residuals <- y_train - fitted_values

# Performance metrics (using test set)
predictions <- predict(glmnetridge_withcv, s = minlambda, newx = X_test)
mae <- mean(abs(predictions - y_test))
rmse <- sqrt(mean((predictions - y_test)^2))
rss <- sum((predictions - y_test)^2)
tss <- sum((y_test - mean(y_test))^2)
r2 <- 1 - rss/tss

# Print performance metrics
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("R-squared: ", r2, "\n")

hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# Shapiro-Wilk test for normality
shapiro.test(residuals)

# 2. Homoscedasticity
# Plot residuals vs. fitted values
#hereee

pdf("ridge_lasso_resid.pdf")

par(mfrow = c(1, 2))

plot(fitted_values_ridge, residuals_values_ridge, main = "Ridge Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")


plot(fitted_values, residuals, main = "LASSO Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")




dev.off()






## LASSO
glmnetlassocv <- cv.glmnet(X, y, alpha = 1)
plot(glmnetlassocv)
minlambda <- glmnetlassocv$lambda.min # = 0.007063672
glmnetlasso_nocv <- glmnet(X, y, alpha = 1)
plot(glmnetlasso_nocv, xvar = "lambda")
# Plot them on the same graph
par(mfrow = c(2, 1))
plot(glmnetlassocv)
plot(glmnetlasso_nocv, xvar = "lambda")
dev.off()
# Try it with the min lambda
glmnetlasso_withcv <- glmnet(X, y, alpha = 1, lambda = minlambda)
glmnetlasso_withcv$beta # Coefficient estimates
# Too many!
# "1se"?
lambda1se <- glmnetlassocv$lambda.1se
glmnetlasso_1se <- glmnet(X, y, alpha = 1, lambda = lambda1se)
glmnetlasso_1se$beta # Coefficient estimates


#here
# Fit LASSO regression model with cross-validation
glmnetlassocv <- cv.glmnet(X_train, y_train, alpha = 1)

glmnetlassocv

# Fit the final model using the optimal lambda
glmnetlasso_withcv <- glmnet(X_train, y_train, alpha = 1, lambda = minlambda)

# Predict on the test set
predictions <- predict(glmnetlasso_withcv, s = minlambda, newx = X_test)

# Calculate R-squared
rss <- sum((predictions - y_test)^2)
tss <- sum((y_test - mean(y_test))^2)
r2 <- 1 - rss/tss

# View coefficient values
coefficients <- coef(glmnetlasso_withcv)
print(coefficients)

# Plot cross-validation results
par(mfrow = c(2, 1))
plot(glmnetlassocv, main = "LASSO Cross-Validation")
plot(glmnetlasso_withcv, xvar = "lambda", main = "LASSO Coefficients")




# Print R-squared
cat("R-squared: ", r2, "\n")



# Plot cross-validation results
par(mfrow = c(2, 1))
plot(glmnetlassocv, main = "LASSO Cross-Validation")
plot(glmnetlasso_withcv, xvar = "lambda", main = "LASSO Coefficients")



aa <- .8 # Choose some different alphas

glmnetcv <- cv.glmnet(X, y, alpha = aa)
plot(glmnetcv)
glmnetnocv <- glmnet(X, y, alpha = aa)
plot(glmnetnocv, xvar = "lambda")
# Which variables do you think that is?
round(t(glmnetnocv$beta), 4)
lambda1se <- glmnetcv$lambda.1se
glmnet_1se <- glmnet(X, y, alpha = aa, lambda = lambda1se)
glmnet_1se$beta # Coefficient estimates



#additive
fit_gam <- function(train_data, test_data, knots = 6) {
  mod <- gam(Concrete.compressive.strength ~
               s(Cement, bs = "bs", k = knots) + 
               s(Blast.Furnace.Slag, bs = "bs", k = knots) +
              # s(Fly.Ash, bs = "bs", k = knots) +
               s(Water, bs = "bs", k = knots) +
              # s(Superplasticizer, bs = "bs", k = knots) +
               s(Coarse.Aggregate, bs = "bs", k = knots) +
              # s(Fine.Aggregate, bs = "bs", k = knots) +
               s(Age, bs = "bs", k = knots),
             data = train_data)
  predictions <- predict(mod, newdata = test_data)
  coefficients <- coef(mod)
  return(list(predictions = predictions, coefficients = coefficients))
}


set.seed(123)

nrow(data)

folds <- createFolds(data_scaled$Concrete.compressive.strength, k = 10, list = TRUE)

data_scaled
str(folds)

results <- lapply(folds, function(fold_indices) {
  train_data <- data_scaled[-fold_indices, ]
  test_data <- data_scaled[fold_indices, ]
  true_values <- test_data$Concrete.compressive.strength
  fit <- fit_gam(train_data, test_data)
  predictions <- fit$predictions
  coefficients <- fit$coefficients
  list(predictions = data.frame(True = true_values, Predicted = predictions),
       coefficients = coefficients)
})
3

predictions <- do.call(rbind, lapply(results, function(x) x$predictions))
coefficients_list <- lapply(results, function(x) x$coefficients)




# Combine coefficients into a data frame
coefficients_df <- do.call(cbind, coefficients_list)

# Calculate mean coefficients
mean_coefficients <- apply(coefficients_df, 1, mean)

# Output the average coefficients
print(mean_coefficients)



mae <- mean(abs(predictions$True - predictions$Predicted))
rmse <- sqrt(mean((predictions$True - predictions$Predicted)^2))

rss <- sum((predictions$True - predictions$Predicted)^2)
tss <- sum((predictions$True - mean(predictions$True))^2)
r2 <- 1 - rss/tss

data_scaled$Concrete.compressive.strength
str(predictions)

cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("R-squared: ", r2, "\n")

predictions <- do.call(rbind, lapply(results, function(x) x$predictions$Predicted))
true_values <- do.call(rbind, lapply(results, function(x) x$predictions$True))

mbd <- mean(predictions - data_scaled$Concrete.compressive.strength)
cat("Mean Bias Deviation (MBD):", mbd, "\n")

sd(data_scaled$Concrete.compressive.strength)

cross_validate_gam <- function(knots) {
  results <- lapply(folds, function(fold_indices) {
    train_data <- data_scaled[-fold_indices, ]
    test_data <- data_scaled[fold_indices, ]
    predictions <- fit_gam_with_knots(train_data, test_data, knots)
    true_values <- test_data$Concrete.compressive.strength
    data.frame(True = true_values, Predicted = predictions)
  })
  results <- do.call(rbind, results)
  rmse <- sqrt(mean((results$True - results$Predicted)^2))
  return(rmse)
}

fit_gam_with_knots <- function(train_data, test_data, knots) {
  mod <- gam(Concrete.compressive.strength ~
               s(Cement, bs = "bs", k = knots) + 
               s(Blast.Furnace.Slag, bs = "bs", k = knots) +
              # s(Fly.Ash, bs = "bs", k = knots) +
               s(Water, bs = "bs", k = knots) +
              # s(Superplasticizer, bs = "bs", k = knots) +
               s(Coarse.Aggregate, bs = "bs", k = knots) +
              # s(Fine.Aggregate, bs = "bs", k = knots) +
               s(Age, bs = "bs", k = knots),
             data = train_data)
  predictions <- predict(mod, newdata = test_data)
  return(predictions)
}

knot_values <- seq(5, 15, by = 1)  # Example range of knots
rmse_values <- sapply(knot_values, cross_validate_gam)
3

# Find the number of knots with the lowest RMSE
optimal_knots <- knot_values[which.min(rmse_values)]

optimal_knots

# Print the optimal number of knots
cat("Optimal number of knots:", optimal_knots, "\n")

# Plot RMSE vs. Number of Knots
plot(knot_values, rmse_values, type = "b", xlab = "Number of Knots", ylab = "RMSE",
     main = "Cross-Validation RMSE vs. Number of Knots")

mod <- gam(Concrete.compressive.strength ~
             s(Cement, bs = "bs", k = 5) + 
             s(Blast.Furnace.Slag, bs = "bs", k = 5) +
            # s(Fly.Ash, bs = "bs", k = 6) +
             s(Water, bs = "bs", k = 5) +
             #s(Superplasticizer, bs = "bs", k = 6) +
             s(Coarse.Aggregate, bs = "bs", k = 5) +
            # s(Fine.Aggregate, bs = "bs", k = 6) +
             s(Age, bs = "bs", k = 5),
           data = data_scaled)

mod

coef(mod)
vif(mod)

concurvity(mod,full = TRUE)

sqrt(nrow(data_scaled))

summary(mod)
plot(mod)
vif(mod)
vif(mod)

pdf("add_final.pdf")
# Set up the layout for the plots
layout(matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE), heights = c(0.1, 1, 1))

# Set up margins
par(mar = c(0, 0, 2, 0)) # Top margin for the title plot
plot.new() # Create a blank plot
title("Covariate Smoothing Functions", line = -1, cex.main = 2) # Add the title

# Set margins for the actual plots
par(mar = c(4, 4, 2, 2))

# Plot each smoothing function
plot(smooth1, main = "", xlab = "Cement", ylab = "s(Cement)")
plot(smooth2, main = "", xlab = "Blast.Furnace.Slag", ylab = "s(Blast.Furnace.Slag)")
plot(smooth3, main = "", xlab = "Water", ylab = "s(Water)")
plot(smooth4, main = "", xlab = "Coarse.Aggregate", ylab = "s(Coarse.Aggregate)")
plot(smooth5, main = "", xlab = "Age", ylab = "s(Age)")



pdf("add_final.pdf")

plot(mod, pages = 1, rug = TRUE, main = "Covariate Smoothing Functions")

dev.off()

cor_matrix <- cor(data_scaled)

# Print the correlation matrix
print(cor_matrix)

# Alternatively, visualize the correlation matrix using corrplot
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle")

dev.off()

library(corrplot)

# Calculate the correlation matrix
cor_matrix <- cor(data)

vif(linmod)
cor_matrix
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", tl.cex = 0.8)

cor_matrix
logstrength <- log(data$Concrete.compressive.strength)

plot(mod, pages = 1, rug = TRUE)

par(mfrow = c(2, 2))
acf(residuals(mod), main = "ACF of Residuals")
plot(residuals(mod), type = "l", main = "Residuals over Time")

# 3. Distribution of Residuals


pdf("assumptions.pdf")
par(mfrow = c(1, 2))
hist(residuals(mod), main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)
qqnorm(residuals(mod))

qqline(residuals(mod))

dev.off()






shapiro.test(residuals(mod))

# 4. Homoscedasticity
par(mfrow = c(1, 1))
plot(fitted(mod), residuals(mod), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
bptest(mod)

# 5. Adequacy of Smoothing
summary(mod) # Check EDF for each smooth term

# Residual diagnostics
par(mfrow = c(2, 2))
plot(fitted(mod), residuals(mod), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
hist(residuals(mod), main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)
qqnorm(residuals(mod))
qqline(residuals(mod))
shapiro.test(residuals(mod))

# Check ACF of residuals
acf(residuals(mod), main = "ACF of Residuals")
