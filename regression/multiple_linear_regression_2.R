test_data = read.csv("data\\kc_house_test_data.csv")
train_data = read.csv("data\\kc_house_train_data.csv")

# add constant column to data frame and convert it to matrix
get_matrix <- function(df, features) {
    df$constant = 1
    features_matrix = as.matrix(df[, c("constant", features)])
    return(features_matrix)
}

# predict outcome with features and their weights
predict_outcome <- function(feature_matrix, weights) {
    predictions = array(feature_matrix %*% weights)
    return(predictions)
}

# calculate derivative with respect to a feature
feature_derivative <- function(errors, feature) {
    derivative = 2 * (errors %*% feature)
    return(derivative)
}

# implement gradient descent algorithm
regression_gradient_descent <- function(feature_matrix, output, initial_weights, step_size, tolerance) {
    converged = FALSE
    weights = initial_weights
    while(!converged) {
        gradient_sum_squares = 0
        for(i in 1:length(weights)) {
            errors = predict_outcome(feature_matrix, weights) - output
            feature = feature_matrix[, i]
            derivative = feature_derivative(errors, feature)
            weights[i] = weights[i] - step_size * derivative
            gradient_sum_squares = gradient_sum_squares + derivative^2
        }
        gradient_magnitude = sqrt(gradient_sum_squares)
        if (gradient_magnitude < tolerance) {
            converged = TRUE
        }
    }
    return(weights)
}

# actual house prices in the training dataset
output = array(train_data[, "price"])

# fit a simple model with just "sqft_living"
simple_feature_matrix = get_matrix(train_data, "sqft_living")
initial_weights = array(c(-47000, 1))
step_size = 7e-12
tolerance = 2.5e7
simple_weights = regression_gradient_descent(simple_feature_matrix, output, initial_weights, step_size, tolerance)

cat("Q1: Weight for sqft_living=", round(simple_weights[2], 1))

# actual house prices in the test dataset
test_output = array(test_data[, "price"])

# predict prices of houses in the test dataset using the fitted model earlier
test_simple_feature_matrix = get_matrix(test_data, "sqft_living")
test_predictions1 = predict_outcome(test_simple_feature_matrix, simple_weights)
first_house1 = round(test_predictions1[1])
cat("Q2: Simple prediction of the first house in the test dataset=", first_house1)

# RSS for the test dataset using the simple model
test_RSS1 = sum((test_output - test_predictions1)^2)

# fit another model with both "sqft_living" and "sqft_living15"
features = c("sqft_living", "sqft_living15")
feature_matrix = get_matrix(train_data, features)
initial_weights = array(c(-100000, 1, 1))
step_size = 4e-12
tolerance = 1e9
weights = regression_gradient_descent(feature_matrix, output, initial_weights, step_size, tolerance)

# predict prices of houses in the test dataset using the second model
test_feature_matrix = get_matrix(test_data, features)
test_predictions2 = predict_outcome(test_feature_matrix, weights)
first_house2 = round(test_predictions2[1])
cat("Q3: Second prediction of the first house in the test dataset=", first_house2)
cat("Q4: Actual=", round(test_data$price[1]),
        "Model1=", first_house1,
        "Model2=", first_house2)

# RSS for the test dataset using the second model
test_RSS2 = sum((test_output - test_predictions2)^2)
cat("Q5: RSS1=", test_RSS1, "RSS2=", test_RSS2)
