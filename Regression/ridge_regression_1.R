library(MASS)

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
sales = sales[order(sales$sqft_living, sales$price), ]

poly15 = polynomial_df(sales$sqft_living, 15)
model = lm.ridge(sales$price ~ ., poly15, lambda = 1e-5)
coeffs = coef(model)
print(coeffs)
cat('Q1:', coeffs['power_1'])

# create 4 subsets
set1 = read.csv('data/wk3_kc_house_set_1_data.csv')
set2 = read.csv('data/wk3_kc_house_set_2_data.csv')
set3 = read.csv('data/wk3_kc_house_set_3_data.csv')
set4 = read.csv('data/wk3_kc_house_set_4_data.csv')

get_matrix <- function(df) {
    # add constant feature to data frame and then convert it to matrix
    df$constant = 1
    features_matrix = as.matrix(df[, c(16, 1:15)])
    return(features_matrix)
}

predict_outcome <- function(model, df) {
    # predictions are matrix multiplications of features & weights
    feature_matrix = get_matrix(df)
    weights = coef(model)
    predictions = array(feature_matrix %*% weights)
    return(predictions)
}

fit_model15 <- function(df, l2_penalty) {
    # fit the 15th order of polynomial model
    poly15 = polynomial_df(df$sqft_living, 15)
    model = lm.ridge(df$price ~ ., poly15, lambda = l2_penalty)
    coeffs = coef(model)
    predictions = predict_outcome(model, poly15)
    
    # plot data and the fit
    plot(poly15$power_1, df$price, xlab='SQFT', ylab='Price', pch='.', main='15th Order of Polynominal Model')
    lines(poly15$power_1, predictions, col=2)
    return(coeffs)
}

# fit 15th degree model to each set
l2_penalty = 1e-5   # it just means No Penalty
coeffs = sapply(list(set1, set2, set3, set4), fit_model15, l2_penalty)
cat('\nQ2:', coeffs['power_1',])

# Ridge regression comes to rescue
l2_penalty = 1e5   # Penalize large weights
coeffs = sapply(list(set1, set2, set3, set4), fit_model15, l2_penalty)
cat('\nQ3:', coeffs['power_1',])

# Selecting an L2 penalty via cross-validation
train_valid_shuffled = read.csv('data/wk3_kc_house_train_valid_shuffled.csv')



