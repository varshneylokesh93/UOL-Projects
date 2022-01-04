#European Working Condition Survey- Project 1 code

#Reading data into R
dataset = read.csv('EWCS_2016.csv', na.strings = '-999')


#library(data.table)
dataset = data.table(dataset)
str(dataset)


#Replacing NA's with Mode
#install.packages('DescTools')
#library(DescTools)
estimated_mode = function(x) {
  Mode(x, na.rm = TRUE)
}
dataset = dataset[, lapply(.SD, function(x){
  ifelse(is.na(x),estimated_mode(x),x)
})]


#Renaming column names
library(dplyr)
dataset = setnames(dataset, old = c('Q2a', 'Q2b', 'Q87a', 'Q87b',
                                    'Q87c', 'Q87d', 'Q87e', 'Q90a',
                                    'Q90b', 'Q90c', 'Q90f'),
                   new = c('Gender', 'Age', 'Cheerful', 'Calm',
                           'Active', 'Fresh', 'InterestingLife',
                           'Energetic', 'WorkEnthusiastic', 'Workaholic',
                           'JobSatisfaction'))


#Converting int data type variables to factors
dataset$Gender= as.factor(dataset$Gender)
dataset$Cheerful= as.factor(dataset$Cheerful)
dataset$Calm = as.factor(dataset$Calm)
dataset$Active = as.factor(dataset$Active)
dataset$Fresh= as.factor(dataset$Fresh)
dataset$InterestingLife= as.factor(dataset$InterestingLife)
dataset$Energetic= as.factor(dataset$Energetic)
dataset$WorkEnthusiastic= as.factor(dataset$WorkEnthusiastic)
dataset$Workaholic= as.factor(dataset$Workaholic)
dataset$JobSatisfaction= as.factor(dataset$JobSatisfaction)




# Labeling factor variables for better interpretability
dataset$Gender = factor(dataset$Gender, levels = c(1, 2),
                        labels = c("Male","Female"))
dataset$Cheerful = factor(dataset$Cheerful, levels = c(1, 2,3,4,5,6),
                        labels = c("All of the time","Most of the time",
                                   "More than half of the time",
                                   "Less than half of the time",
                                   "Some of the time", "At no time"))
dataset$Calm = factor(dataset$Calm, levels = c(1, 2,3,4,5,6),
                          labels = c("All of the time","Most of the time",
                                     "More than half of the time",
                                     "Less than half of the time",
                                     "Some of the time", "At no time"))
dataset$Active = factor(dataset$Active, levels = c(1, 2,3,4,5,6),
                      labels = c("All of the time","Most of the time",
                                 "More than half of the time",
                                 "Less than half of the time",
                                 "Some of the time", "At no time"))
dataset$Fresh = factor(dataset$Fresh, levels = c(1, 2,3,4,5,6),
                        labels = c("All of the time","Most of the time",
                                   "More than half of the time",
                                   "Less than half of the time",
                                   "Some of the time", "At no time"))
dataset$InterestingLife = factor(dataset$InterestingLife, 
                                 levels = c(1, 2,3,4,5,6),
                       labels = c("All of the time","Most of the time",
                                  "More than half of the time",
                                  "Less than half of the time",
                                  "Some of the time", "At no time"))
dataset$Energetic = factor(dataset$Energetic, levels = c(1, 2,3,4,5),
                       labels = c("Always","Most of the time",
                                  "Sometimes", "Rarely", "Never"))
dataset$WorkEnthusiastic = factor(dataset$WorkEnthusiastic, 
                                  levels = c(1, 2,3,4,5),
                           labels = c("Always","Most of the time",
                                      "Sometimes", "Rarely", "Never"))
dataset$Workaholic = factor(dataset$Workaholic, levels = c(1, 2,3,4,5),
                           labels = c("Always","Most of the time",
                                      "Sometimes", "Rarely", "Never"))
dataset$JobSatisfaction = factor(dataset$JobSatisfaction,
                                 levels = c(1, 2,3,4,5),
                            labels = c("Always","Most of the time",
                                       "Sometimes", "Rarely", "Never"))





# Compute Gower distance
library(cluster)
gower_dist <- daisy(dataset, metric = "gower")
gower_mat <- as.matrix(gower_dist)


# Print most similar individuals based on distance
dataset[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]


# Print most dissimilar individuals based on distance
dataset[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


#Using the silhouette Method to find opitmal number of clusters
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)


#Fitting PAM model for clustering
k = 3
pam_fit <- pam(gower_dist, diss = TRUE, k)

pam_results <- dataset %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#Visualization of data in a lower dimensional space
library(Rtsne)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



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


# Linear Regression

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


#Hybrid stepwise regression

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


#Lasso Regression

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


#Decision Tree

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


#Random Forest

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


#Gradient Boosting

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

# Summary of the fitted model
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

#Prediction G3 on the training set by GBM model
library(Metrics)
y_train_boost= predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, trainingData)
#Checking accuracy of the model on training dataset using RMSE
rmse(y_train_boost, trainingData$G3)
#Prediction G3 on the test set by GBM model
y_test_boost <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, testData)
#Checking accuracy of the model on test dataset using RMSE
rmse(y_test_boost, testData$G3)



#Classification Model On Bank Marketing Dataset- Porject 3 code


#Read the file in R
bank = read.csv("bank.csv", sep = ";", header = TRUE)


#Excluding 'duration' variable from the dataset
bank= bank[,-12]
summary(bank)


#If there any missing value in the dataset
sum(is.na(bank))
#Structure of the uploaded dataset
#library(plyr)
glimpse(bank)


#Coverting pdays variable into bins
bank$pdays = cut(bank$pdays, breaks = c(-2,-1,91,182,273,364,455,546,637,728,819,910),
                 labels = c('Not contacted','3months passed','6months passed',
                            '9months passed','1yr passed','1.3yrs passed',
                            '1.6yrs passed','1.9yrs passed','2yrs passed',
                            '2.3yrs passed','More than 2.3yrs passed'))


#Training & Test Datasets
deposit_yes <- bank[which(bank$y == 'yes'), ]  # all yes's of outcome class
deposit_no <- bank[which(bank$y == 'no'), ]  # all no's of outcome class
set.seed(100)
deposit_yes_training_rows <- sample(1:nrow(deposit_yes), 0.7*nrow(deposit_yes))  #randomly choosing 70% observations of yes class
deposit_no_training_rows <- sample(1:nrow(deposit_no), 0.7*nrow(deposit_no))  #randomly choosing 70% observations of yes class
training_yes <- deposit_yes[deposit_yes_training_rows, ]  
training_no <- deposit_no[deposit_no_training_rows, ]
trainingData <- rbind(training_yes, training_no)  #combining chosen observations
glimpse(trainingData)
table(trainingData$y)


# Create Test Data
test_yes <- deposit_yes[-deposit_yes_training_rows, ]
test_no <- deposit_no[-deposit_no_training_rows, ]
testData <- rbind(test_yes, test_no)  #combining chosen observations
glimpse(testData)
table(testData$y)

#Generalized Linear Model 

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)
#Fitting the model
glm.model = train(y~., data = trainingData, method="glm", family=binomial(),
                  trControl= fitControl)
summary(glm.model)
# Variable importance by the GLM model
varImp(glm.model)
#Predicting outcome variable on test set
y.pred= predict(glm.model, testData)
#Confusion Matrix
confusionMatrix(y.pred, testData$y)


#Gradient Boosting Machine

#Tuning the hyper paramters
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*30, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
set.seed(825)
#10-fold cross validation
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
#Fitting the model
gbmFit2 <- train(y ~ ., data = trainingData, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

plot(gbmFit2)
#Variable importance by the model
varImp(gbmFit2)
#Predicting outcome variable on test data
y.gbm.pred = predict(gbmFit2, testData)
#Confusion matrix
confusionMatrix(y.gbm.pred, testData$y)


#K-Nearest Neighbors

#Normalizing continuous variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
bank$age = normalize(bank$age)
bank$balance = normalize(bank$balance)
bank$day = normalize(bank$day)
bank$campaign = normalize(bank$campaign)
bank$previous = normalize(bank$previous)

#Encoding categorical variabled to dummy. KNN uses distance metrics to identify classes 
library(dummies)
dummies <- dummyVars(~ ., data=bank[, -16])
c2 <- predict(dummies, bank[, -16])
bank.upd <- as.data.frame(cbind(bank$y, c2))
glimpse(bank.upd)
bank.upd$V1= as.factor(bank.upd$V1)
names(bank.upd)[which(names(bank.upd) == "V1")] <- "y"
bank.upd$y= factor(bank.upd$y, levels= c(1,2), labels= c("no","yes"))

#Training & Test Data after encoding variables
deposit_yes <- bank.upd[which(bank.upd$y == 'yes'), ]
deposit_no <- bank.upd[which(bank.upd$y == 'no'), ]
set.seed(100)
deposit_yes_training_rows <- sample(1:nrow(deposit_yes), 0.7*nrow(deposit_yes))
deposit_no_training_rows <- sample(1:nrow(deposit_no), 0.7*nrow(deposit_no))
training_yes <- deposit_yes[deposit_yes_training_rows, ]  
training_no <- deposit_no[deposit_no_training_rows, ]
trainingData <- rbind(training_yes, training_no) 
glimpse(trainingData)
table(trainingData$y)

# Create Test Data
test_yes <- deposit_yes[-deposit_yes_training_rows, ]
test_no <- deposit_no[-deposit_no_training_rows, ]
testData <- rbind(test_yes, test_no) 
glimpse(testData)
table(testData$y)

#Fitting the model
set.seed(156)
knn.mod = train(
  y ~ .,
  data = trainingData,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(k = seq(1, 30, by = 2))
)
plot(knn.mod)
#Best k-value chosen by KNN
knn.mod$bestTune
str(knn.mod)
# Variable importance by KNN
plot(varImp(knn.mod, top=15))
#Predicting outcome variable on test set
y.knn.pred = predict(knn.mod, testData)
#Confusion Matrix
confusionMatrix(y.knn.pred, testData$y)


#Support Vector Machine

trctrl <- trainControl(method = "cv", number = 10)
#Tuning hyper parameters
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
#Fitting the model
#Note, here training and test datasets have dummy encoded and normalized variables
set.seed(3233)
svm_Radial_Grid <- train(y ~., data = trainingData, method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial,
                         tuneLength = 10)
svm_Radial_Grid
#Variable importance by SVM model
varImp(svm_Radial_Grid)
#Prediction the outcome variable on test data
y.radial.pred = predict(svm_Radial_Grid, testData)
#Confusion Matrix
confusionMatrix(y.radial.pred, testData$y)





#Naive Bayes

#Since in naive bayes algorithm, all variables have considered as categorical
#I am uploading the dataset again to avoid any confusion
#Read the file in R
bank = read.csv("bank.csv", sep = ";", header = TRUE)

#Excluding 'duration' variable from the dataset
bank= bank[,-12]

#Converting all continuous variables to categorical
bank$pdays = cut(bank$pdays, breaks = c(-2,-1,91,182,273,364,455,546,637,728,819,910),
                 labels = c('Not contacted','3months passed','6months passed',
                            '9months passed','1yr passed','1.3yrs passed',
                            '1.6yrs passed','1.9yrs passed','2yrs passed',
                            '2.3yrs passed','More than 2.3yrs passed'))
bank$balance= cut(bank$balance, breaks = c(-3314,40,332,851,1882,71188),
                  labels = c('balNegToNil','balNilToVLow','balVLowToLow',
                             'balLowToMed','balMedToHigh'))
bank$age= cut(bank$age, breaks = c(18,32,37,44,52,87),
              llabels = c('age19To32','age33To37','age38To44','age45To52',
                          'age53To87'))
bank$day= cut(bank$day, breaks = c(0,7,14,21,28,32), labels = c('Wk1','Wk2',
                                                                'Wk3','Wk4',
                                                                'Wk5'))
bank$campaign= cut(bank$campaign, breaks = c(0,1,2,60),
                   labels = c('curn1Time','curn2Times','curnMoreThan2Times'))
bank$previous= cut(bank$previous, breaks = c(-1, 0,1,3,30),
                   labels = c('preNotContacted','pre1Time','pre2-3Times',
                              'preMoreThan3Times'))
glimpse(bank)
#Training & Test Datasets
deposit_yes <- bank[which(bank$y == 'yes'), ]  # all yes's of outcome class
deposit_no <- bank[which(bank$y == 'no'), ]  # all no's of outcome class
set.seed(100)
deposit_yes_training_rows <- sample(1:nrow(deposit_yes), 0.7*nrow(deposit_yes))  #randomly choosing 70% observations of yes class
deposit_no_training_rows <- sample(1:nrow(deposit_no), 0.7*nrow(deposit_no))  #randomly choosing 70% observations of yes class
training_yes <- deposit_yes[deposit_yes_training_rows, ]  
training_no <- deposit_no[deposit_no_training_rows, ]
trainingData <- rbind(training_yes, training_no)  #combining chosen observations
glimpse(trainingData)
table(trainingData$y)

# Create Test Data
test_yes <- deposit_yes[-deposit_yes_training_rows, ]
test_no <- deposit_no[-deposit_no_training_rows, ]
testData <- rbind(test_yes, test_no)  #combining chosen observations

#Fitting the model
set.seed(128)
ctrl <- trainControl(method="cv", 10)
grid <- expand.grid(fL=c(0,0.5,1.0), usekernel = c(TRUE,FALSE), adjust=c(0,0.5,1.0))
x = trainingData[,-16] #matrix without outcome variable
y= trainingData$y
naive.model <- train(x, y, method="nb",
                     trControl=ctrl, tuneGrid = grid)
#Predicting outcome variable on test set
y.naive.pred = predict(naive.model, testData)
#Confusion matrix
confusionMatrix(y.naive.pred, testData$y)
#Variable importance by the naive model
plot(varImp(naive.model))
