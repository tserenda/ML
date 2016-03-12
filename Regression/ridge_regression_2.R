train_data = read.csv('data/kc_house_train_data.csv')
test_data = read.csv('data/kc_house_test_data.csv')

get_matrix <- function(df) {
    df = cbind(constant = 1, df)
    return(as.matrix(df))
}

predict_outcome <- function(feature_matrix, weights) {
    # predictions are matrix multiplications of features & weights
    predictions = array(feature_matrix %*% weights)
    return(predictions)
}
feature_derivative_ridge <- function(errors, feature, weight, l2_penalty, feature_is_constant) {
    derivative = 2 * sum(errors %*% feature)
    if (!feature_is_constant) {
        derivative = derivative + 2 * l2_penalty * weight
    }
    return(derivative)
}
ridge_regression_gradient_descent <- function(feature_matrix, output, initial_weights, step_size, l2_penalty, max_iterations = 100) {
    weights = initial_weights
    iterations = 0
    while (iterations < max_iterations) {
        # compute the predictions using your predict_output() function
        predictions = predict_outcome(feature_matrix, weights)
        # compute the errors as predictions - output
        errors = predictions - output
        for (i in 1:len(weights)) {
            # Feature column associated with weights[i]
            feature = feature_matrix[, i]            
            # Compute the derivative for weight[i]
            if (i == 0) {    # derivative of the constant
                derivative = feature_derivative_ridge(errors, feature, weight, l2_penalty, TRUE)
            }
            else {
                feature_derivative_ridge(errors, feature, weight, l2_penalty, FALSE)
            }
            # subtract the step size times the derivative from the current weight 
            weights = weights - step_size * derivative
        }
        interations = iterations + 1
    }
    return(weights)
}

feature_matrix = get_matrix(train_data)
output = train_data$price
num_of_features = length(train_data)
initial_weights = array(data = 0, dim = num_of_features)
step_size = 1e-12
l2_penalty = 0
max_iterations = 1000
simple_weights_0_penalty = ridge_regression_gradient_descent(feature_matrix, output, initial_weights, step_size, l2_penalty, max_iterations)
