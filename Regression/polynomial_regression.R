polynomial_df <- function(feature, degree) {
    # construct data frame of powers of feature up to degree
    df = data.frame('power_1' = feature)
    if (degree > 1) {
        for (d in 2:degree) {
            colname = paste('power_', d, sep='')
            df[[colname]] = feature^d
        }
    }
    return(df)
}

sales = read.csv('data/kc_house_data.csv')

# compare polynomial models w/ different degrees
plot(sales$sqft_living, sales$price, xlab='SQFT', ylab='Price', pch='.', main='Diff Degree Polynominal Models')
for (d in c(1, 2, 3, 15)) {
    df = polynomial_df(sales$sqft_living, d)
    df$price = sales$price
    mod = lm(price ~ ., df)
    points(df$power_1, predict(mod, df), col = (d+1) %% 10, pch = 20)
}

# create 4 subsets
set1 = read.csv('data/week3/wk3_kc_house_set_1_data.csv')
set2 = read.csv('data/week3/wk3_kc_house_set_2_data.csv')
set3 = read.csv('data/week3/wk3_kc_house_set_3_data.csv')
set4 = read.csv('data/week3/wk3_kc_house_set_4_data.csv')

fit_model15 <- function(df) {
    # fit the 15th order of polynomial model
    poly_df = polynomial_df(df$sqft_living, 15)
    poly_df$price = df$price
    mod = lm(price ~ ., poly_df)
    plot(poly_df$power_1, poly_df$price, xlab='SQFT', ylab='Price', pch='.', main='15th Order of Polynominal Model')
    lines(poly_df$power_1, predict(mod, poly_df), col=2)
    return(mod$coefficients)
}

# fit 15th degree model to each set
coeffs = sapply(list(set1, set2, set3, set4), fit_model15)

cat('Q1:', coeffs['power_15', ])
cat('\nQ2: The fitted lines are different depending on the dataset')

# create training, validation, & testing sets
train_data = read.csv('data/week3/wk3_kc_house_train_data.csv')
valid_data = read.csv('data/week3/wk3_kc_house_valid_data.csv')
test_data  = read.csv('data/week3/wk3_kc_house_test_data.csv')

# fit polynomial models w/ different degrees
RSS = array(dim = 15)
for (degree in 1:15) {
    train_df = polynomial_df(train_data$sqft_living, degree)
    train_df$price = train_data$price
    fit = lm(price ~ ., train_df)
    valid_df = polynomial_df(valid_data$sqft_living, degree)
    predictions = predict(fit, valid_df)
    residuals = predictions - valid_data$price
    RSS[degree] = sum(residuals ^ 2)
}
best = which.min(RSS)
cat('\nQ3:', best)

# compute RSS on testing data for the model w/ the best degree
poly_train = polynomial_df(train_data$sqft_living, best)
poly_train$price = train_data$price
fit = lm(price ~ ., poly_train)

poly_test = polynomial_df(test_data$sqft_living, best)
predictions = predict(fit, poly_test)
residuals = predictions - test_data$price
RSS = sum(residuals ^ 2)
cat('\nQ4:', RSS)