setwd('./ML/Regression')

polynomial_df <- function(feature, degree) {
    # construct data frame of powers of feature up to degree
    poly_df = data.frame('power_1' = feature)
    if (degree > 1) {
        for (d in 2:degree) {
            colname = paste('power_', d, sep = '')
            poly_df[[colname]] = feature^d
        }
    }
    return(poly_df)
}

sales = read.csv('./data/kc_house_data.csv')

# compare polynomial models w/ different degrees
plot(sales$sqft_living, sales$price, xlab = 'SQFT', ylab = 'Price',
     main = 'Diff Degree Polynominal Models')
for (d in c(1, 2, 3, 15)) {
    poly_df = polynomial_df(sales$sqft_living, d)
    poly_df$price = sales$price
    mod = lm(price ~ ., poly_df)
    points(poly_df$power_1, predict(mod, poly_df), col = (d+1) %% 10)
}

# create 4 subsets
set1 = read.csv('./data/week3/wk3_kc_house_set_1_data.csv')
set2 = read.csv('./data/week3/wk3_kc_house_set_2_data.csv')
set3 = read.csv('./data/week3/wk3_kc_house_set_3_data.csv')
set4 = read.csv('./data/week3/wk3_kc_house_set_4_data.csv')

fit_model15 <- function(df) {
    # fit the 15th order of polynomial model
    poly_df = polynomial_df(df$sqft_living, 15)
    poly_df$price = df$price
    mod = lm(price ~ ., poly_df)
    plot(poly_df$power_1, poly_df$price, xlab = 'SQFT', ylab = 'Price',
         main = '15th Degree Polynominal Model')
    lines(poly_df$power_1, predict(mod, poly_df), col = 2)
    return(mod$coefficients)
}

# fit 15th degree model to each set
coeffs = sapply(list(set1, set2, set3, set4), fit_model15)

cat('Q1: ', coeffs['power_15', ])
cat('Q2: The fitted lines are different')

# create training, validation, & testing sets
training_data = read.csv('./data/week3/wk3_kc_house_train_data.csv')
validation_data = read.csv('./data/week3/wk3_kc_house_valid_data.csv')
testing_data = read.csv('./data/week3/wk3_kc_house_test_data.csv')

# fit polynomial models w/ different degrees
models = array(dim = 15)
for (degree in 1:15) {
    poly_df = polynomial_df(training_data$sqft_living, degree)
    poly_df$price = training_data$price
    models[degree] = lm(price ~ ., poly_df)
}

# calculate RSS on the validation data and find the lowest RSS
RSS = array(dim = 15)
for (degree in 1:15) {
    eval = eval(models[degree], validation_data)
    RSS[degree] = sum(eval$residuals ^ 2)
}
best = which.min(RSS)
cat('Q3: ', best)

# compute RSS on testing data for the model w/ the best degree
eval = eval(models[best], testing_data)
cat('Q4: ', sum(eval$residuals ^ 2))
