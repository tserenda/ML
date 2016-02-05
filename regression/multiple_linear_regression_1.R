test = read.csv("data\\kc_house_test_data.csv")
train = read.csv("data\\kc_house_train_data.csv")

# transformations and interactions of existing variables
train$bedrooms_squared = train$bedrooms ^ 2
train$bed_bath_rooms = train$bedrooms * train$bathrooms
train$log_sqft_living = log(train$sqft_living)
train$lat_plus_long = train$lat + train$long

test$bedrooms_squared = test$bedrooms ^ 2
test$bed_bath_rooms = test$bedrooms * test$bathrooms
test$log_sqft_living = log(test$sqft_living)
test$lat_plus_long = test$lat + test$long

# calculate the means
cat("Q1: ", round(sapply(test[, c("bedrooms_squared", "bed_bath_rooms", "log_sqft_living", "lat_plus_long")], mean), 2))

# fit different models
model1 = lm(price ~ sqft_living+bedrooms+bathrooms+lat+long, data = train)
model2 = lm(price ~ sqft_living+bedrooms+bathrooms+lat+long+bed_bath_rooms, data = train)
model3 = lm(price ~ sqft_living+bedrooms+bathrooms+lat+long+bed_bath_rooms+bedrooms_squared+log_sqft_living+lat_plus_long, data = train)

cat("Q2: 1st coeff for bathrooms", model1$coefficients["bathrooms"])
cat("Q3: 2nd coeff for bathrooms", model2$coefficients["bathrooms"])

# calculate the RSS on the training data
train_RSS1 = sum((train$price - predict(model1, train)) ^ 2)
train_RSS2 = sum((train$price - predict(model2, train)) ^ 2)
train_RSS3 = sum((train$price - predict(model3, train)) ^ 2)
cat("Q4: ")
cat("train1=", train_RSS1)
cat("train2=", train_RSS2)
cat("train3=", train_RSS3)

# calculate the RSS on the test data
test_RSS1 = sum((test$price - predict(model1, test)) ^ 2)
test_RSS2 = sum((test$price - predict(model2, test)) ^ 2)
test_RSS3 = sum((test$price - predict(model3, test)) ^ 2)
cat("Q5: ")
cat("test1=", test_RSS1)
cat("test2=", test_RSS2)
cat("test3=", test_RSS3)

