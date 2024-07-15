library(ggplot2)
library(dplyr)
library(corrplot)
library(glmnet)

# EDA

summary(mobilephone)
str(mobilephone)
any(is.na(mobilephone))

# Histograms of numeric variables
par(mfrow = c(2, 3))  
hist(mobilephone$ROM, main = "ROM")
hist(mobilephone$RAM, main = "RAM")
hist(mobilephone$display_size, main = "Display Size")
hist(mobilephone$num_rear_camera, main = "Number of Rear Cameras")
hist(mobilephone$num_front_camera, main = "Number of Front Cameras")
hist(mobilephone$battery_capacity, main = "Battery Capacity")

# Density plots for numeric variables
par(mfrow = c(2, 3))  
plot(density(mobilephone$ROM), main = "ROM Density")
plot(density(mobilephone$RAM), main = "RAM Density")
plot(density(mobilephone$display_size), main = "Display Size Density")
plot(density(mobilephone$num_rear_camera), main = "Number of Rear Cameras Density")
plot(density(mobilephone$num_front_camera), main = "Number of Front Cameras Density")
plot(density(mobilephone$battery_capacity), main = "Battery Capacity Density")

# Scatterplot matrix for numeric variables
pairs(mobilephone[, c("ROM", "RAM", "display_size", "num_rear_camera", "num_front_camera", "battery_capacity")])

#correlationn
numeric_cols <- c("ROM", "RAM", "display_size", "num_rear_camera", "num_front_camera", "battery_capacity")
correlation_matrix <- cor(mobilephone[numeric_cols])
corrplot(correlation_matrix, method = "color", type = "upper", diag = FALSE, tl.col = "black", tl.srt = 45, tl.cex = 0.7, number.cex = 0.7)

# Boxplots for categorical variables
par(mfrow = c(1, 2))  # Set up a 1x2 grid for plotting
boxplot(sales_price ~ brand, data = mobilephone, main = "Sales Price by Brand")
boxplot(sales_price ~ base_color, data = mobilephone, main = "Sales Price by Base Color")

par(mfrow = c(1, 2))  # Set up a 1x2 grid for plotting
barplot(table(mobilephone$brand), main = "Brand Distribution", col = rainbow(length(unique(mobilephone$brand))))
barplot(table(mobilephone$base_color), main = "Base Color Distribution", col = rainbow(length(unique(mobilephone$base_color))))



# RIDGE REGRESSION

# Prepare data
x <- model.matrix(sales ~ . - 1, data = mobilephone)
y <- mobilephone$sales

# Ridge Regression
ridge_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- ridge_model$lambda.min
ridge_pred <- predict(ridge_model, s = best_lambda, newx = x)
ridge_r2 <- 1 - sum((y - ridge_pred)^2) / sum((y - mean(y))^2)
print(paste("Ridge Regression R-squared:", round(ridge_r2, 4)))

#LASSO REGRESSION
lasso_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- lasso_model$lambda.min
lasso_pred <- predict(lasso_model, s = best_lambda, newx = x)
lasso_r2 <- 1 - sum((y - lasso_pred)^2) / sum((y - mean(y))^2)
print(paste("Lasso Regression R-squared:", round(lasso_r2, 4)))

#MULTIPLE LINEAR REGRESSSION
multiple_model <- lm(sales ~ RAM + battery_capacity + num_rear_camera, data = mobilephone)
summary(multiple_model)


#AIC
# Full Model with all predictors
full_model <- lm(sales ~ ., data = mobilephone)
stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)


##table for model comparison
model_names <-c("Multiple Linear Regression", "Ridge Regression", "Lasso Regression", "Stepwise AIC Regression")
r_squared <- c(summary(multiple_model)$r.squared, ridge_r2, lasso_r2, cor(mobilephone$sales, predict(stepwise_model)))
model_comparison <- data.frame(Model = model_names, R_squared = r_squared)
print(model_comparison)


