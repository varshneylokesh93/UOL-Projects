#Students' Grades Prediction- Project 2 code


#Importing the datasets
student_math = read.csv("student-mat.csv", sep = ";", header = TRUE)
student_por = read.csv("student-por.csv", sep = ";", header = TRUE)


#Create new variable called 'subjects'
student_math= mutate(student_math, subject= "Mathematics")
student_por= mutate(student_por, subject= "Portuguese")


#Merging both the files
student.comb= rbind(student_math, student_por)


#Dropping G1 and G2 from the datasets
student.comb= student.comb[,-c(31,32)]


#If there any missing value in the dataset
sum(is.na(student.comb))
#Structure of the uploaded dataset
#library(dplyr)
glimpse(student.comb)


#Scaling of data
student.comb$age= scale(student.comb$age)
student.comb$failures= scale(student.comb$failures)
student.comb$absences= scale(student.comb$absences)


#Converting int variables to factors which are categorical in nature
student.comb$Medu = as.factor(student.comb$Medu)
student.comb$Fedu = as.factor(student.comb$Fedu)
student.comb$traveltime = as.factor(student.comb$traveltime)
student.comb$studytime = as.factor(student.comb$studytime)
student.comb$subject= as.factor(student.comb$subject)
#Converting int variables to ordinal which are categorical in nature
student.comb$famrel = factor(student.comb$famrel, order= TRUE, levels = c(1,2,3,4,5))
student.comb$freetime = factor(student.comb$freetime, order= TRUE, levels = c(1,2,3,4,5))
student.comb$goout = factor(student.comb$goout, order= TRUE, levels = c(1,2,3,4,5))
student.comb$Dalc = factor(student.comb$Dalc, order= TRUE, levels = c(1,2,3,4,5))
student.comb$Walc = factor(student.comb$Walc, order= TRUE, levels = c(1,2,3,4,5))
student.comb$health = factor(student.comb$health, order= TRUE, levels = c(1,2,3,4,5))


summary(student.comb)


#Splitting the data into Training & Test set using validation set approach
library(caTools)
set.seed(123)
split = sample.split(student.comb$G3, SplitRatio = 0.75)
trainingData = subset(student.comb, split== TRUE)
testData = subset(student.comb, split== FALSE)


# ----------------------------------------------------------------------------Linear Regression ----------------------------------------------------------------------------

#Fitting the model
ln_model = lm(formula = G3~., data = trainingData)
summary(ln_model)
#Based on the model, predicting G3 on the training dataset
y_train_ln= predict(ln_model, trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(trainingData$G3, y_train_ln)
#Based on the model, predicting G3 on the test dataset
y_test_ln = predict(ln_model, newdata = testData)
#Checking accuracy of the model on the test set using RMSE
rmse(testData$G3, y_test_ln)


# ----------------------------------------------------------------------------Hybrid stepwise regression----------------------------------------------------------------------------

#Fitting the model
subset_model = step(ln_model, direction = "both", trace = 1)
summary(subset_model)
#Based on the model, predicting G3 on the training dataset
y_train_subset= predict(subset_model, trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(trainingData$G3, y_train_subset)
#Based on the model, predicting G3 on the test dataset
y_test_subset= predict(subset_model, newdata = testData)
#Checking accuracy of the model on the test set using RMSE
rmse(testData$G3,y_test_subset)


#----------------------------------------------------------------------------Lasso Regression----------------------------------------------------------------------------

x_train_lasso = data.matrix(trainingData[,-31])
y_train_lasso = trainingData$G3
x_test_lasso = data.matrix(testData[,-31])
y_test_lasso = testData$G3
grid =10^seq(2, -2, by = -.1)
library(glmnet)
#Fitting the model
lasso_model=glmnet(x_train_lasso, y_train_lasso, alpha = 1, lambda = grid)
#The code above runs the glmnet func several times for different values of
#lambda. We can automate this task of finding the optimal value of lambda using the
#cv.glmnet function
cv_lasso <- cv.glmnet(x_train_lasso, y_train_lasso, alpha = 1, lambda = grid)
plot(cv_lasso)
optimal_lambda <- cv_lasso$lambda.min
optimal_lambda
#Based on the model, predicting G3 on the training dataset with optimal lambda value
y_train_lasso= predict(lasso_model, s= optimal_lambda, x_train_lasso)
#Checking accuracy of the model on training dataset using RMSE
rmse(trainingData$G3, y_train_lasso)
#Based on the model, predicting G3 on the test dataset with optimal lambda value
y_test_lasso=predict(lasso_model, s= optimal_lambda, x_test_lasso)
#Checking accuracy of the model on the test set using RMSE
rmse(testData$G3,y_test_lasso)


#----------------------------------------------------------------------------Decision Tree----------------------------------------------------------------------------

library(rpart)
set.seed(123)
#Fitting thee model
tree_model = rpart(G3~., method = 'anova', data = trainingData)
plotcp(tree_model)
#Visualize decision tree created by the algorithm
plot(tree_model, uniform = FALSE, main = "Regression Decision Tree", compress = TRUE)
text(tree_model ,use.n = TRUE)
#Based on the model, predicting G3 on the training dataset
y_train_tree = predict(tree_model, trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(trainingData$G3, y_train_tree)
#Based on the model, predicting G3 on the test dataset
y_test_tree=predict(tree_model ,newdata =testData)
#Checking accuracy of the model on the test set using RMSE
rmse(testData$G3, y_test_tree)
#Pruning of the model
pruned.model=prune(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"])
                                                    ,"CP"])
plot(pruned.model, uniform = FALSE, main = "Regression Tree", compress = TRUE)
text(pruned.model,use.n = TRUE)
# To check which variables have been considered important by the algorithm
varImp(pruned.model)
#Prediction G3 on the training set by pruned decision model
y_train_pruned=predict(pruned.model ,trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(trainingData$G3, y_train_pruned)
#Prediction G3 on the test set by pruned decision model
y_test_pruned=predict(pruned.model ,newdata = testData)
#Checking accuracy of the model on test dataset using RMSE
rmse(testData$G3, y_test_pruned)


#----------------------------------------------------------------------------Random Forest----------------------------------------------------------------------------

library(randomForest)
set.seed(124)
#Fitting the model
forest_model = randomForest(G3~., data = trainingData)
forest_model
plot(forest_model)
#Based on the model, predicting G3 on the training dataset
y_train_forest = predict(forest_model, trainingData)
#Checking accuracy of the model on the training set using RMSE
rmse(trainingData$G3, y_train_forest)
#Based on the model, predicting G3 on the test dataset
y_test_forest = predict(forest_model, newdata = testData)
#Checking accuracy of the model on the test set using RMSE
rmse(testData$G3, y_test_forest)
#The above model chose 10 variables randomly to be considered at 
#each split. We would try 12 predictors one by one at each split to see where the
#error is minimum
oob.err=double(12)
test.err=double(12)
#mtry is no of Variables randomly chosen at each split
set.seed(124)
for(mtry in 1:12) 
{
  rf=randomForest(G3 ~ . , data = trainingData,mtry=mtry,ntree=400, importance=TRUE)
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,testData) #Predictions on Test Set for each Tree
  test.err[mtry]= with(testData, mean( (G3 - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
#Prediction G3 on the training set by tuned random forest model
y_train_rf = predict(rf, trainingData)
#Checking accuracy of the model on the training set using RMSE
rmse(trainingData$G3, y_train_rf)
#Checking accuracy of the model on the test set on different mtry values. To get rmse
#we can take square root of mse
test.err
oob.err
#To get at which mtry value test & out of bag error was minimum
which.min(test.err)
which.min(oob.err)
#Plotting both Test Error and Out of Bag Error
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),
        type="b",ylab="Mean Squared Error",
        xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19,
       col=c("red","blue"))
#Important variables chosen by Random Forest Model
varImp(rf)


#----------------------------------------------------------------------------Gradient Boosting----------------------------------------------------------------------------

install.packages('gbm')
library(gbm)
#Fitting thee model by tuning hyper parameters
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .75, .8), 
  optimal_trees = 0,               # a place to store results
  min_RMSE = 0                     # a place to store results
)

for(i in 1:nrow(hyper_grid)) {
  
  
  set.seed(123)
  
  
  gbm.tune <- gbm(
    formula = G3 ~ .,
    data = trainingData,
    n.trees = 3000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # Below grid will store the minimum training error and trees
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

# Grid created while fitting the model
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#Fitting the final gbm model with optimum number of hyper parameters
gbm.fit.final <- gbm(
  formula = G3 ~ .,
  distribution = "gaussian",
  data = trainingData,
  n.trees = 110,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .80, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

# ----------------------------------------------------------------------------Summary of the fitted model----------------------------------------------------------------------------
summary(gbm.fit.final)
gbm.perf(gbm.fit.final)
#Visualizing important variables considered by GBM
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 1
)

#----------------------------------------------------------------------------Prediction G3 on the training set by GBM model----------------------------------------------------------------------------
library(Metrics)
y_train_boost= predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(y_train_boost, trainingData$G3)
#Prediction G3 on the test set by GBM model
y_test_boost <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, testData)
#Checking accuracy of the model on test dataset using RMSE
rmse(y_test_boost, testData$G3)
