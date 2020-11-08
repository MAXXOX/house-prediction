#packages
install.packages("caret")
install.packages("ggplot2")
#libraries
library(caret)
library(ggplot2)
#data
dane <- read.csv("https://raw.githubusercontent.com/SalAlba/machine-learning/master/data/housing/2/house.csv")
#
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dane$price_bin, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dane[-validation_index,]
# use the remaining 80% of data to training and testing the models
dane <- dane[validation_index,]
#
# dimensions of dataset
dim(dane)
#[1] 17291 22
#
# list types for each attribute
sapply(dane, class)
#id          date         price     price_bin      bedrooms     bathrooms   sqft_living      sqft_lot 
#"numeric"   "character"     "numeric"     "integer"     "integer"     "numeric"     "integer"     "integer" 
#floors    waterfront          view     condition         grade    sqft_above sqft_basement      yr_built 
#"numeric"     "integer"     "integer"     "integer"     "integer"     "integer"     "integer"     "integer" 
#yr_renovated       zipcode           lat          long sqft_living15    sqft_lot15 
#"integer"     "integer"     "numeric"     "numeric"     "integer"     "integer"
#
# take a peek at the first 5 rows of the data
head(dane)
#
# set price_bin as a factor
dane$price_bin <- as.factor(dane$price_bin)
#delete 'id' and 'date' as we won't use it now
dane <- dane[, 3:16]
#
dim(dane)
# [1] 17291    14
sapply(dane, class)
# price     price_bin      bedrooms     bathrooms   sqft_living      sqft_lot        floors    waterfront 
# "numeric"      "factor"     "integer"     "numeric"     "integer"     "integer"     "numeric"     "integer" 
# view     condition         grade    sqft_above sqft_basement      yr_built 
# "integer"     "integer"     "integer"     "integer"     "integer"     "integer" 
#
# summarize the class distribution
percentage <- prop.table(table(dane$price_bin)) * 100
      # Dwie grupy: 93,22% i 6,78%
#
# summarize attribute distributions
summary(dane)
#
# split input and output
x <- dane[, -2]
y <- dane[, 2]
#
plot(y)
#
#scatterplot
#pairs(dane[c(3, 5:16)], pch = 19)
  # nie wiem czy ma sens a strasznie ciezko to dziala
#
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
#
# b) nonlinear algorithms
# CART
#set.seed(7)
#fit.cart <- train(price_bin~., data=dane, method="rpart", metric=metric, trControl=control)
  #You are trying to do regression and your outcome only has two possible values Are you trying to do classification?
  #If so, use a 2 level factor as your outcome column.
#
# kNN
set.seed(7)
fit.knn <- train(price_bin~., data=dane, method="knn", metric=metric, trControl=control)
# Troche trzeba poczekac
#
# c) advanced algorithms
# SVM
#set.seed(7)
#fit.svm <- train(price_bin~., data=dane, method="svmRadial", metric=metric, trControl=control)
# Random Forest
#set.seed(7)
#fit.rf <- train(price_bin~., data=dane, method="rf", metric=metric, trControl=control)
#
# summarize accuracy of models ###########################################################################
print(fit.knn)
# k-Nearest Neighbors
# 
# 17291 samples
# 21 predictor
# 2 classes: '0', '1'
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold)
# Summary of sample sizes: 15562, 15562, 15562, 15562, 15562, 15561, ...
# Resampling results across tuning parameters:
# 
#   k  Accuracy   Kappa
# 5  0.9543119  0.5238163
# 7  0.9488177  0.4432501
# 9  0.9473716  0.3994311
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.
#
# Poprawiony kod ######################################################################################
#
# k-Nearest Neighbors 
# 
# 17291 samples
# 13 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 15562, 15562, 15562, 15562, 15562, 15561, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.9997108  0.9977027
# 7  0.9995952  0.9967787
# 9  0.9996531  0.9972352
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.
#
##########################################################################################################
#
# estimate skill of kNN on the validation dataset
predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, validation$price_bin)
#
str(predictions)
str(validation)
#
#B³¹d w poleceniu 'table(data, reference, dnn = dnn, ...)':
#all arguments must have the same length
# Troche bez sensu bo maja te sama dlugosc (factor with 2 levels) ###########################################
#