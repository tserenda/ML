train_data = read.csv('data/kc_house_train_data.csv')
test_data = read.csv('data/kc_house_test_data.csv')
n = nrow(train_data)

simple_linear_regression <- function(x, y) {
    # calculate intercept & slope for x & y
    X = sum(x)
    Y = sum(y)
    XY = sum(x*y)
    X2 = sum(x^2)
    slope = (XY-X*Y/n) / (X2-X/n*X)
    intercept = (Y-slope*X)/n
    return(list(intercept, slope))
}

# fit a simple linear regression model using sqft_living
weights_sqft = simple_linear_regression(train_data$sqft_living, train_data$price)
intercept_sqft = weights_sqft[[1]]
slope_sqft = weights_sqft[[2]]

get_regression_predictions <- function(input, intercept, slope) {
    # calculate predictions for input using intercept & slope
    predictions = intercept + slope * input
    return(predictions)
}

# predict price for a house with 2650 sqft
price2650 = get_regression_predictions(2650, intercept_sqft, slope_sqft)
cat('Q1:', round(price2650))

get_residual_sum_of_squares <- function(input, output, intercept, slope) {
    predictions = get_regression_predictions(input, intercept, slope)
    RSS = sum((output - predictions)^2)
    return(RSS)
}

# RSS for simple linear regression using sqft to predict prices on TRAINING data
RSS = get_residual_sum_of_squares(train_data$sqft_living, train_data$price, intercept_sqft, slope_sqft)
cat('\nQ2:', RSS)

inverse_regression_predictions <- function(output, intercept, slope) {
    # predict the sqft given price
    predictions = (output-intercept) / slope
    return(predictions)
}

# estimate square-feet for a house costing $800,000
sqft800k = inverse_regression_predictions(800000, intercept_sqft, slope_sqft)
cat('\nQ3:', round(sqft800k))

# fit a simple linear regression model using number of bedrooms
weights_bed = simple_linear_regression(train_data$bedrooms, train_data$price)
intercept_bed = weights_bed[[1]]
slope_bed = weights_bed[[2]]

# compare RSS_bed & RSS_sqft on TEST data
RSS_bed = get_residual_sum_of_squares(test_data$bedrooms, test_data$price, intercept_bed, slope_bed)
RSS_sqft = get_residual_sum_of_squares(test_data$sqft_living, test_data$price, intercept_sqft, slope_sqft)
cat('\nQ4: RSS_bed =', RSS_bed, 'RSS_sqft =', RSS_sqft)
