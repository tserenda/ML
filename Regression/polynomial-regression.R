polynomial_df <- function(feature, degree) {
    poly_df = data.frame("power_1" = feature)
    if (degree > 1) {
        for (i in 2:degree) {
            df = data.frame(feature ^ i)
            colnames(df) = paste("power_", i, sep = '')
            poly_df = cbind(poly_df, df)
        }
    }
    return(poly_df)
}

# compare polynomial models with different degrees
sales = read.csv(".\\data\\kc_house_data.csv")

# 1st degree
poly1_data = polynomial_df(sales$sqft_living, 1)
poly1_data$price = sales$price
model1 = lm(price ~ ., data = poly1_data)
plot(poly1_data$power_1, poly1_data$price, col = 3, pch = ".")
points(poly1_data$power_1, predict(model1, poly1_data), pch = '-')

# 2nd degree
poly2_data = polynomial_df(sales$sqft_living, 2)
poly2_data$price = sales$price
model2 = lm(price ~ ., data = poly2_data)
plot(poly2_data$power_1, poly2_data$price, col = 3, pch = ".")
points(poly2_data$power_1, predict(model2, poly2_data), pch = '-')

# 3rd degree
poly3_data = polynomial_df(sales$sqft_living, 3)
poly3_data$price = sales$price
model3 = lm(price ~ ., data = poly3_data)
plot(poly3_data$power_1, poly3_data$price, col = 3, pch = ".")
points(poly3_data$power_1, predict(model3, poly3_data), pch = '-')

# 15th degree
poly15_data = polynomial_df(sales$sqft_living, 15)
poly15_data$price = sales$price
model15 = lm(price ~ ., data = poly15_data)
plot(poly15_data$power_1, poly15_data$price, col = 3, pch = ".")
points(poly15_data$power_1, predict(model15, poly15_data), pch = '-')

# create 4 subsets
set1 = read.csv(".\\data\\week3\\wk3_kc_house_set_1_data.csv")
set2 = read.csv(".\\data\\week3\\wk3_kc_house_set_2_data.csv")
set3 = read.csv(".\\data\\week3\\wk3_kc_house_set_3_data.csv")
set4 = read.csv(".\\data\\week3\\wk3_kc_house_set_4_data.csv")

# fit 15th degree model to each of 4 sets
# set 1
poly15_1_data = polynomial_df(set1$sqft_living, 15)
poly15_1_data$price = set1$price
model15_1 = lm(price ~ ., data = poly15_1_data)
plot(poly15_1_data$power_1, poly15_1_data$price, col = 3, pch = ".")
points(poly15_1_data$power_1, predict(model15_1, poly15_1_data), pch = '-')
(coeff1 = model15_1$coefficients)

# set 2
poly15_2_data = polynomial_df(set2$sqft_living, 15)
poly15_2_data$price = set2$price
model15_2 = lm(price ~ ., data = poly15_2_data)
plot(poly15_2_data$power_1, poly15_2_data$price, col = 3, pch = ".")
points(poly15_2_data$power_1, predict(model15_2, poly15_2_data), pch = '-')
(coeff2 = model15_2$coefficients)

# set 3
poly15_3_data = polynomial_df(set3$sqft_living, 15)
poly15_3_data$price = set3$price
model15_3 = lm(price ~ ., data = poly15_3_data)
plot(poly15_3_data$power_1, poly15_3_data$price, col = 3, pch = ".")
points(poly15_3_data$power_1, predict(model15_3, poly15_3_data), pch = '-')
(coeff3 = model15_3$coefficients)

# set 4
poly15_4_data = polynomial_df(set4$sqft_living, 15)
poly15_4_data$price = set4$price
model15_4 = lm(price ~ ., data = poly15_4_data)
plot(poly15_4_data$power_1, poly15_4_data$price, col = 3, pch = ".")
points(poly15_4_data$power_1, predict(model15_4, poly15_4_data), pch = '-')
(coeff4 = model15_4$coefficients)

cat("Q1: ", coeff1['power_15'], coeff2['power_15'], coeff3['power_15'], coeff4['power_15'])
cat("Q2: The plotted fitted lines don't look the same in all four plots")

# create training, validation, and testing sets
training_data 
