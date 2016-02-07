# read in both train and test data from pre-split files to match quiz answers
train_data = read.csv("data\\kc_house_train_data.csv")
test_data = read.csv("data\\kc_house_test_data.csv")
n = nrow(train)     # number of observations in the training set

simple_linear_regression <- function(x, y) {
    yi = sum(y)
    xi = sum(x)
    yixi = sum(y * x)
    xi_squared = sum(as.numeric(x ^ 2))
    w1 = (yixi - (yi * xi) / n) / (xi_squared - (xi / n) * xi)
    w0 = yi / n - w1 * (xi / n)
    return(list(w0, w1))
}

coefficients = simple_linear_regression(train$sqft_living, train$price)
intercept = coefficients[[1]]
slope = coefficients[[2]]

cat("intercept", intercept)
cat("slope", slope)

get_regression_predictions <- function(input, intercept, slope) {
    return(intercept + slope * input)
}

cat("Q1: ", get_regression_predictions(2650, intercept, slope))

get_residual_sum_of_squares <- function(input, output, intercept, slope) {
    predictions = intercept + slope * input
    return(sum((output - predictions) ^ 2))
}

cat("Q2: ", get_residual_sum_of_squares(train$sqft_living, train$price, intercept, slope))

inverse_regression_predictions <- function(output, intercept, slope) {
    return((output - intercept) / slope)
}

cat("Q3: ", inverse_regression_predictions(800000, intercept, slope))

coefficients = simple_linear_regression(train$bedrooms, train$price)
intercept = coefficients[[1]]
slope = coefficients[[2]]

cat("Q4: ", get_residual_sum_of_squares(test$bedrooms, test$price, intercept, slope))
