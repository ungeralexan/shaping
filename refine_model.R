# can we improve model accuracy after removing some features
# Identify features to remove based on importance
features_to_remove <- c("latitude","island", "households", "mean_bedrooms","nearOcean","nearBay","lessoneOcean")  # start with the least important features

# Update the dataset by removing these features
cleaned_housing_reduced_3 <- df[, !(names(df) %in% features_to_remove)]

# Create new training and testing datasets without the removed features
train_reduced_3 = cleaned_housing_reduced_3[ix, ]
test_reduced_3 = cleaned_housing_reduced_3[-ix, ]

set.seed(21000)

fit_model_reduced_3 <- ranger(
  formula = median_house_value ~ ., 
  data = train_reduced_3, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)

testPred_reduced_3 = predict(fit_model_reduced_3, data = test_reduced_3)

# Extract the actual and predicted values
actuals_reduced_3 <- test_reduced_3$median_house_value
predictions_reduced_3 <- testPred_reduced_3$predictions

# Calculate the MSE and RMSE for the reduced model
mse_rf_reduced_3 = mean((predictions_reduced_3 - actuals_reduced_3)^2)
rmse_reduced_3 = sqrt(mse_rf_reduced_3)

# Output the RMSE to see if there is an improvement
print(rmse_reduced_3)


######
#Now the second improved model what if we remove a correlated feature such as latitude
# can we improve model accuracy after removing some features
# Identify features to remove based on importance
features_to_remove_long <- c("island", "households", "mean_bedrooms","longitude")  # start with the least important features

# Update the dataset by removing these features
cleaned_housing_reduced_long <- cleaned_housing[, !(names(cleaned_housing) %in% features_to_remove_long)]

# Create new training and testing datasets without the removed features
train_reduced_long = cleaned_housing_reduced_long[ix, ]
test_reduced_long = cleaned_housing_reduced_long[-ix, ]

set.seed(1738)

fit_model_reduced_long <- ranger(
  formula = median_house_value ~ ., 
  data = train_reduced_long, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)

testPred_reduced_long = predict(fit_model_reduced_long, data = test_reduced_long)

# Extract the actual and predicted values
actuals_reduced_long <- test_reduced_long$median_house_value
predictions_reduced_long <- testPred_reduced_long$predictions

# Calculate the MSE and RMSE for the reduced model
mse_rf_reduced_long = mean((predictions_reduced_long - actuals_reduced_long)^2)
rmse_reduced_long = sqrt(mse_rf_reduced_long)

# Output the RMSE to see if there is an improvement
print(rmse_reduced_long)
