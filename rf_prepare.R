#clear the workspace
rm(list=ls())
library(tidyverse)
library(dplyr)
library(reshape2)
#read in the data set from kaggle 
house= read.csv("housing.csv")

head(house)



#lets see how many NAs we encounter
#lets see whether we have some missings, apparently yes
sum(is.na(house))
#there are 207 observations that encounter Nas 

#Lets see which variables are entailed by NAs
#function
countNAs <- function(data) {
  na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
  return(na_count)
}


na_counts <- countNAs(house)
print(na_counts)

#Filling the missing values with median of total_bedrooms (handle the missing values)
house$total_bedrooms[is.na(house$total_bedrooms)] = median(house$total_bedrooms, na.rm = TRUE)
#The code replaces missing values in the total_bedrooms column 
#with the median of the column where the missing values are not considered in the median calculation 


#first glimpse of the values of the data
summary(house)

#show the distributions of the data
ggplot(data = melt(house), mapping = aes(x = value)) + 
  geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x')



#here is where the feature engineering starts

#now lets take a look at the ocean proximity (is categorical and needs special handling different from numeric values)
#Therefore, it's temporarily removed from the main dataset (housing_numeric) to prepare the data for models that require numeric input.
drop_vector = c('ocean_proximity')
house_num =  house[ , !(names(house) %in% drop_vector)]


#now check the numeric values for correlations
# Load the corrplot package
library(corrplot)


corrplot(cor(house_num),
         method = "number",
         type = "lower")


#now build the special data frame for the ocean proximity variable
new_var = unique(house$ocean_proximity)

new_var_house = data.frame(ocean_proximity = house$ocean_proximity)

for(i in new_var){
  new_var_house[,i] = rep(0, times= nrow(new_var_house))
}
head(new_var_house)

#the filled ocean proximity table
for(i in 1:length(new_var_house$ocean_proximity)){
  ct = as.character(new_var_house$ocean_proximity[i])
  new_var_house[,ct][i] = 1
}

head(new_var_house)
#The loop iterates through each row of cat_housing and sets the column corresponding to the category in ocean_proximity to 1. This is a manual way to perform one-hot encoding, 
#which transforms the categorical variable into a format that can be provided to machine learning algorithms, by creating a binary column for each category and marking the presence of the category with a 1

column_names = names(new_var_house) #retrieves the names of all columns in cat_housing.
keep = column_names[column_names != 'ocean_proximity'] #identifies the columns to keep, essentially all columns except the original ocean_proximity column, since it's already been encoded into separate columns.
new_var_house = select(new_var_house,one_of(keep)) #to keep only the specified columns in cat_housing. This effectively removes the original ocean_proximity column, leaving only the one-hot encoded columns.

tail(new_var_house)


#now lets change our variables

#now lets face new bedroom variables 
house$mean_population = house$population/house$households # the average population per household
house$mean_bedrooms = house$total_bedrooms/house$households #average number of bedrooms per household
house$mean_rooms = house$total_rooms/house$households # the average number of rooms per houshold 

#These features are likely created based on the assumption that per-household averages might provide more insight into housing value than total counts, reflecting the density and room distribution more accurately.


drop_again = c('total_bedrooms', 'total_rooms', 'population')

house = house[ , !(names(house) %in% drop_again)] # updates the housing dataframe to keep only the columns not listed in drops.

head(house) 



drops_3 = c('ocean_proximity','median_house_value')
house_num =  house[ , !(names(house) %in% drops_3)] #updates housing_num to include only the columns not listed in drops_1. This leaves you with a dataset of purely numerical features without the target variable.

head(house_num)

#merge the lists together
df = cbind(new_var_house, house_num, median_house_value=house$median_house_value)

head(df)
str(df)



#and as we have a humongous data set lets choose only 0.2 percent of the data
# Determine the size of the subset (10% of the original data)
#subset_size <- floor(0.99 * nrow(cleaned_housing))

# Randomly sample indices
#sampled_indices <- sample(nrow(cleaned_housing), size = subset_size)

# Subset the dataframe
#cleaned_housing <- cleaned_housing[sampled_indices, ]
#house is now the new data set we will use for our prediction model



#change the column names: 
#we need to changes variable names of our columns for the ranger function
# Define your new column names for the first four columns
new_column_names <- c("nearBay", "lessoneOcean", "inland", "nearOcean","island")

# Replace the first four column names
colnames(df)[1:5] <- new_column_names



#some descriptive analysis
summary(df)