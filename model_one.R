#random forest model 2
source("rf_prepare.R")
#second random forest model I am building with the house data set

#load the packages we need 
#load all the packages 
#library(kernelshap) 
library(treeshap) #to apply the tree SHAP algorithm
library(ranger) #for the random forest model
library(shapviz) #for the shap visualisation
library(dplyr) #to do some data engineering
library(pdp) # to visualize partial dependence plots


#this is where our model starts
head(df)


# Set a random seed so that same sample can be reproduced in future runs
set.seed(21000)

# Randomly sample indices for the training set
ix = sample(nrow(df), 0.7 * nrow(df))

# Create training and testing datasets
train = df[ix, ]
test = df[-ix, ]

# Confirm that the total number of rows matches the original dataset
print(nrow(train) + nrow(test) == nrow(df))

# Fit the random forest model
original_model <- ranger(
  formula = median_house_value ~ ., 
  data = train, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)

# Extrahiere die Wichtigkeit der Variablen
importance_ranger = original_model$variable.importance

# Ausgabe der Variablenwichtigkeiten
print(importance_ranger)



####plot the feature importance
library(ggplot2)

# Create a data frame for the variable importance
importance_df <- data.frame(Feature = names(importance_ranger), Importance = importance_ranger)

# Plotting using ggplot
png("Pfi.png", width = 1600, height = 1200, res = 150)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +  # Flip to make it horizontal
  labs(title = "Feature Importance in Random Forest Model",
       x = "Features",
       y = "Importance") +
  theme_minimal()
dev.off()




#What about the performance on unseen data 
# Generiere Vorhersagen für das Testset
testPred = predict(original_model, data = test)

# Extrahiere die tatsächlichen Vorhersagewerte
actuals <- test$median_house_value
predictions <- testPred$predictions

# Berechne den MSE
mse_rf = mean((predictions - actuals)^2)

test_rmse = sqrt(mse_rf)
test_rmse
# Correct calculation of MAE
MAE = mean(abs(predictions - actuals))
MAE



##############################
# Extra analysis to assess how good my model is 
# Calculate and print summary statistics for median house values
summary_stats <- summary(cleaned_housing$median_house_value)
print(summary_stats)


# Calculate mean and median of the house values
mean_value_mae <- mean(cleaned_housing$median_house_value)
median_value_mae <- median(cleaned_housing$median_house_value)

# Calculate MAE as a percentage of mean and median
mae_percentage_mean <- (MAE / mean_value_mae) * 100
mae_percentage_median <- (MAE / median_value_mae) * 100

print(paste("MAE as a percentage of the mean: ", round(mae_percentage_mean, 2), "%"))
print(paste("MAE as a percentage of the median: ", round(mae_percentage_median, 2), "%"))

# Calculate errors
errors <- predictions - actuals

# Plot error distribution
hist(errors, breaks = 50, main = "Distribution of Prediction Errors", xlab = "Prediction Error")
#############

########
#SHAP analysis
#now lets see what tree SHAP detects 

#now lets try the shap analysis to the end
unified_model = ranger.unify(original_model,train[-14])

shaps <- treeshap(unified_model,  train[-14] , interactions = FALSE)
shp = shapviz(shaps, X=train)


#SHAP importance plots
png("importance.png", width = 1600, height = 1200, res = 150)
sv_importance(shp)
dev.off()

png("importance_num.png", width = 1600, height = 1200, res = 150)
sv_importance(shp, show_numbers = TRUE)
dev.off()

png("importance_bee.png", width = 1600, height = 1200, res = 150)
sv_importance(shp, kind="bee")
dev.off()

#SHAP dependece plots
png("dependnece.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "longitude", color_var = "auto")
dev.off()


#What about the dependence plot of irrelevant features?
png("dependnece_households.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "households", color_var = "auto")
dev.off()

png("dependnece_island.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "island", color_var = "auto")
dev.off()

png("dependnece_bedrooms.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "mean_bedrooms", color_var = "auto")
dev.off()


#Local analysis
png("waterfall.png", width = 1600, height = 1200, res = 150)
sv_waterfall(shp,row_id = 3)
dev.off()
sv_force(shp,row_id = 3)


###############

####das ist nez
