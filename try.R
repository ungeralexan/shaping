#open up as a try 
# a training and test model to make predictions
library(mlr3)
library(data.table)

task = tsk("penguins")
split = partition(task)
learner = lrn("classif.rpart")
learner$train(task, row_ids = split$train)
learner$model

# make the prediction
prediction = learner$predict(task, row_ids = split$test)
prediction

#how accurate is my prediction
prediction$score(msr("classif.acc"))


#create a data table
dt = data.table(x = 1:6, y = rep(letters[1:3], each = 2))
dt

#mean of x column in groups given by y
dt[, mean(x), by = "y"]

#or easily adding another column
dt[, z := x * 3]
dt