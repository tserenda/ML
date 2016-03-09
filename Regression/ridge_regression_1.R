library(MASS)

poly_df <- function(x, i) {
    df = data.frame('power_1' = x)
    if (i > 1) {
        for (d in 2:i) {
            column = paste('power_', d, sep='')
            df[[column]] = x^d
        }
    }
    return(df)
}
get_matrix <- function(df) {
    df = cbind(constant = 1, df)
    return(as.matrix(df))
}
predict_outcome <- function(model, df) {
    # predictions are matrix multiplications of features & weights
    feature_matrix = get_matrix(df)
    weights = coef(model)
    predictions = array(feature_matrix %*% weights)
    return(predictions)
}
fit_model15 <- function(df, l2_penalty) {
    poly15 = poly_df(df$sqft_living, 15)
    model = lm.ridge(df$price ~ ., poly15, lambda = l2_penalty)
    predictions = predict_outcome(model, poly15)
    points(poly15$power_1, df$price, pch='.')
    lines(poly15$power_1, predictions)
    return(coef(model))
}
k_fold_CV <- function(k, l2_penalty, df) {
    ave_error = 0
    for (i in 1:k) {
        start = (i-1)*size+1
        end = start+size-1
        validation_set = df[start:end, ]
        presegment = df[1:(start-1), ]
        postsegment = df[(end+1):n, ]
        training_set = rbind(presegment, postsegment)
        model = lm.ridge(price ~ ., training_set, lambda = l2_penalty)
        predictions = predict_outcome(model, validation_set[, 1:15])
        rmse = sqrt(mean(validation_set$price - predictions)^2)
        ave_error = ave_error + rmse
    }
    return(ave_error/k)
}

sales = read.csv('data/kc_house_data.csv')
sales = sales[order(sales$sqft_living, sales$price), ]

# least squares model (large weights)
small_penalty = 1e-5
poly15 = poly_df(sales$sqft_living, 15)
model = lm.ridge(sales$price ~ ., poly15, lambda = small_penalty)
coeffs = coef(model)
cat('\nQ1:', round(coeffs['power_1']))

set1 = read.csv('data/wk3_kc_house_set_1_data.csv')
set2 = read.csv('data/wk3_kc_house_set_2_data.csv')
set3 = read.csv('data/wk3_kc_house_set_3_data.csv')
set4 = read.csv('data/wk3_kc_house_set_4_data.csv')

# high variance models
plot(set1$sqft_living, set1$price, xlab='SQFT', ylab='Price', pch='.', main='15th Order Polynominal Model')
coeffs = sapply(list(set1, set2, set3, set4), fit_model15, small_penalty)
cat('\nQ2:', round(coeffs['power_1',]))

# ridge regression model with low variance and small weights
l2_penalty = 10e3
plot(set1$sqft_living, set1$price, xlab='SQFT', ylab='Price', pch='.', main='15th Order Polynominal Model')
coeffs = sapply(list(set1, set2, set3, set4), fit_model15, l2_penalty)
cat('\nQ3:', round(coeffs['power_1',]))

# select L2 penalty via cross-validation
train_valid_shuffled = read.csv('data/wk3_kc_house_train_valid_shuffled.csv')
n = nrow(train_valid_shuffled)
k = 10
size = n/k

poly15 = poly_df(train_valid_shuffled$sqft_living, 15)
poly15$price = train_valid_shuffled$price
lowest_error = 10e10
best_l2_penalty = 0
l2_penalty_values = exp(seq(log(10e1), log(10e7), length.out = 13))
for (l2_penalty in l2_penalty_values) {
    ave_error = k_fold_CV(10, l2_penalty, poly15)
    if(ave_error < lowest_error) {
        lowest_error = ave_error
        best_l2_penalty = l2_penalty
    }
}
cat('\nQ4:', best_l2_penalty)

# use the best l2_penalty to retrain model
test = read.csv('data/wk3_kc_house_test_data.csv')
poly15 = poly_df(train_valid_shuffled$sqft_living, 15)
poly15$price = train_valid_shuffled$price
model = lm.ridge(price ~ ., poly15, lambda = best_l2_penalty)
predictions = predict_outcome(model, poly_df(test$sqft_living, 15))
RSS = sum((test$price - predictions)^2)
cat('\nQ5:', RSS)


