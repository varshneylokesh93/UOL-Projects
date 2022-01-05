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
