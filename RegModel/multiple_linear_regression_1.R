train_data = read.csv('data/week2/kc_house_train_data.csv')
test_data = read.csv('data/week2/kc_house_test_data.csv')

# transformations & interactions of existing variables
train_data$bedrooms_squared = train_data$bedrooms^2
train_data$bed_bath_rooms = train_data$bedrooms * train_data$bathrooms
train_data$log_sqft_living = log(train_data$sqft_living)
train_data$lat_plus_long = train_data$lat + train_data$long

test_data$bedrooms_squared = test_data$bedrooms ^ 2
test_data$bed_bath_rooms = test_data$bedrooms * test_data$bathrooms
test_data$log_sqft_living = log(test_data$sqft_living)
test_data$lat_plus_long = test_data$lat + test_data$long

# calculate the means
means = colMeans(test_data[, c('bedrooms_squared', 'bed_bath_rooms', 'log_sqft_living', 'lat_plus_long')])
cat('Q1:', round(means, 2))

# fit different models
mod1 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long, train_data)
mod2 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + bed_bath_rooms, train_data)
mod3 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + bed_bath_rooms + bedrooms_squared + log_sqft_living + lat_plus_long, train_data)

cat('\nQ2: bathroom weight of the 1st model=', round(mod1$coefficients['bathrooms']))
cat('\nQ3: bathroom weight of the 2nd model=', round(mod2$coefficients['bathrooms']))

RSS <- function(data, fit) {
    predictions = predict(fit, data)
    rss <- sum((data$price - predictions)^2)
    return(rss)
}

# calculate the RSS on the training data
train_RSS1 = RSS(train_data, mod1)
train_RSS2 = RSS(train_data, mod2)
train_RSS3 = RSS(train_data, mod3)
cat('\nQ4:', train_RSS1, train_RSS2, train_RSS3)

# calculate the RSS on the test data
test_RSS1 = RSS(test_data, mod1)
test_RSS2 = RSS(test_data, mod2)
test_RSS3 = RSS(test_data, mod3)
cat('\nQ5:', test_RSS1, test_RSS2, test_RSS3)
