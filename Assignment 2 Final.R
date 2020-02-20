################################

library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(tictoc)
library(class)
library(cluster)
library(HSAUR)
library(fpc)
library(ggplot2)
library(pROC)

# 1. (15) Preprocessing  
##############

# 1.1 (0) Set a working directory, load the data file and name it 'data'.
################################
# cat("\014")  
rm(list=ls())
# setwd("C:/Users/eyalri/OneDrive - Mellanox/Desktop/Personal/Studies/Introduction to big data/Assignment 2/Assignment2")

setwd("C:/Users/dvirn/Documents/R/Assignment2")

data <- read.csv("census income.csv")

# For knowing the data a little bit:
head(data)
summary(data)
sapply(data,class)


# 1.2 (1) Set your random seed to 1 so that models comparisons will not be affected by randomness.
#######################

set.seed(1)


# 1.3 (2) Replace all '?' values with NA's, then delete rows from the current dataset, 
# in order to receive a full dataset (no NA's).
#######################

data[data == "?"] <- NA
before_removal_nrow = nrow(data)
data <- data[complete.cases(data),]
after_removal_nrow = nrow(data)
cat("Number of records before removal:",(before_removal_nrow),
    "\nNumber of records removed:",(before_removal_nrow - after_removal_nrow))


# 1.4 (3) Create a binary 'label' column, representing the 'income' column. '>50k' will receive a positive label.
# Make sure to remove / replace the original 'income' column.
#######################

data$label_income = data$income == '>50K'
data$income = NULL


# 1.5 (2) Remove the columns 'capital.gain' and 'fnlwgt' from the dataset.
#######################

data[c("capital.gain","fnlwgt")] = NULL


# 1.6 (4) Scale and center the numeric features (standardization - mean of 0 and a standard deviation of 1).
# Afterwards, examine the correlation between them. 
# Explain in your words the main idea behind removing highly correlated features 
# (it doesn't necessarily mean you need to get rid of features in our case). 
#######################

data[sapply(data, is.numeric)] <- scale(data[sapply(data, is.numeric)])

# Just for verification:
summary(data[sapply(data, is.numeric)])
apply(data[sapply(data, is.numeric)],2,sd)

cor(data[sapply(data, is.numeric)])
corr_matrix = cor(data[sapply(data, is.numeric)])
corrplot(corr_matrix, type="upper", order="hclust")

# Answer:
# Correlated data are often redundant.
# A highly correlated data can be inferred from different data using linear interpolation.
# It is wise to remove the data as it will remove the computation complexity of our analysis
# and also data reduction will yield better estimation (due to curse of dimensonality).


# 1.7 (3) Split the data into a test set (30%) and a train set (70%) with respect to the label.
################################

dataset_with_positive_label = data[data$label_income==1,]
dataset_with_negative_label = data[data$label_income==0,]
cat("Number of records with negative label are:",nrow(dataset_with_negative_label),
    "\nnumber of records with positive label are:",nrow(dataset_with_positive_label))

test_index_positive     = sample(nrow(dataset_with_positive_label), round(nrow(dataset_with_positive_label)*0.3))
test_set_data.positive  = dataset_with_positive_label[test_index_positive,]
train_set_data.positive = dataset_with_positive_label[setdiff(1:nrow(dataset_with_positive_label), test_index_positive),]

test_index_negative     = sample(nrow(dataset_with_negative_label), round(nrow(dataset_with_negative_label)*0.3))
test_set_data.negative  = dataset_with_negative_label[test_index_negative,]
train_set_data.negative = dataset_with_negative_label[setdiff(1:nrow(dataset_with_negative_label), test_index_negative),]

test_set_data  = rbind(test_set_data.positive, test_set_data.negative)
train_set_data = rbind(train_set_data.positive, train_set_data.negative)

# We shuffle so it won't be all the positive and then all the negative:
test_set_data  = test_set_data[sample(nrow(test_set_data)),]
train_set_data = train_set_data[sample(nrow(train_set_data)),]


# 2. (20) Decision Tree and Random Forest
#######################

# 2.1 (6) Grow a decision tree using all the features. Predict the labels for the test set with the tree you've built.
# Generate the confusion matrix for the prediction, and plot it. Explain your findings. 
#######################

my_tree <- rpart(factor(label_income) ~ .,
                 data = train_set_data,
                 method ="class")
rpart.plot(my_tree)

# Predict the label for the training set (Sanity check):
pred <- (predict (my_tree, train_set_data) )
confusion_matrix = confusionMatrix(pred[,2]>0.5,train_set_data$label_income) 
confusion_matrix$table
plot(confusion_matrix$table, main = "DT Confusion Matrix - Train Set")
train_set_accuracy = as.numeric(confusion_matrix$overall["Accuracy"])

# Predict the label for the test set:
pred <- (predict (my_tree, test_set_data) )
confusion_matrix = confusionMatrix(pred[,2]>0.5,test_set_data$label_income) 
confusion_matrix$table
plot(confusion_matrix$table, main = "DT Confusion Matrix - Test Set")
test_set_accuracy = as.numeric(confusion_matrix$overall["Accuracy"])
cat("For a single tree:\nThe training set accuracy is:",train_set_accuracy,", While the test set accuracy is:",test_set_accuracy,".\n");

# Answer:
# We can see that most of the predictions are FALSE.
# The model manages to predict the big majority of the FALSE cases and a bit more than a half of the TRUE cases.
# When the model predicts FALSE it's right about 85% of the time, while when it predicts TRUE it's right about 70% of the time.
# Thanks to the calculated split to train and test sets we can see the accuracy almost havn't changed.


# 2.2 (6) Train a simple random forest of 101 trees over the training data, using all the features.
# Predict the labels for the test set with the forest you've built.
# Generate the confusion matrix for the prediction. Explain your findings (in comparison to the previous model). 
#######################

tic(msg = "Time to complete random forest")
random_forest_model = randomForest(factor(label_income)~., train_set_data, ntree=101)
toc()
# Sanity check:
pred_rf <- (predict (random_forest_model, train_set_data) )
confusion_matrix_train_rf = confusionMatrix(pred_rf,train_set_data$label_income) 
confusion_matrix_train_rf$table
plot(confusion_matrix_train_rf$table, main = "RF Confusion Matrix - Train Set")
rf_train_set_accuracy = as.numeric(confusion_matrix_train_rf$overall["Accuracy"])

# Predict the label for the test set:
pred_rf <- (predict(random_forest_model, test_set_data) )
confusion_matrix_test_rf = confusionMatrix(pred_rf,test_set_data$label_income) 
confusion_matrix_test_rf$table
plot(confusion_matrix_test_rf$table, main = "RF Confusion Matrix - Test Set")
rf_test_set_accuracy = as.numeric(confusion_matrix_test_rf$overall["Accuracy"])
cat("For random forest:\nThe training set accuracy is:",rf_train_set_accuracy,", While the test set accuracy is:",rf_test_set_accuracy)

# Answer:
# Unexpectedly, the prediction rate is lower with random forest.
# We can see that this model improved much the accuracy while predicting TRUE, but it almost doesn't predict it at all.


# 2.3 (5) Build another model using random forest, but this time use a regularization method - 
# choose a built in argument that can help you control the trees depth, and create grid search to find the best
# value among at least 5 different values (in terms of accuracy). 
# Explain your findings (in comparison to the previous models). 
#######################

best_accuracy         =   0
best_max_num_of_nodes =   0
max_nodes_arr         =   c(5,15,40,100,500)
  
for (max_node_iterator in 1:length(max_nodes_arr)) {
    
  cat("Running random forest with max nodes:",max_nodes_arr[max_node_iterator],"\n")
  random_forest_model = randomForest(factor(label_income)~.,
                                    train_set_data, ntree=101,
                                    maxnodes = max_nodes_arr[max_node_iterator])
  pred_rf <- (predict (random_forest_model, test_set_data) )
  confusion_matrix_test_rf = confusionMatrix(pred_rf,test_set_data$label_income) 
  rf_test_set_accuracy = as.numeric(confusion_matrix_test_rf$overall["Accuracy"])
  cat("Random forest accuracy on cross validation set is:",rf_test_set_accuracy,"\n")
    
  if (rf_test_set_accuracy > best_accuracy) {
    best_accuracy         = rf_test_set_accuracy
    best_rf_tree_model    = random_forest_model
    best_max_num_of_nodes = max_nodes_arr[max_node_iterator]
    }
}

cat("Best accuracy [",best_accuracy,"] obtained for max nodes of:",best_max_num_of_nodes)

# Answer:
# We see that as the max node increses, the accuracy improves a bit (~1.6% from 5 nodes to 100)
# and then after 100 the tendency changes and in 500 it's already "much" worst than at 5.
# In addition, in any point until the limit of 500 nodes, this model is better than the one without the max nodes limit.
# So we conclude that setting any limit to this model is worthwhile and around max nodes of 100 is best.


# 2.4 (3) Plot a feature importance chart (or charts) for the best model (from the previous question).
# Explain your findings.
#######################

importance(best_rf_tree_model)
varImpPlot(best_rf_tree_model, main = "Feature Importance for best RF model")

# Conclusions:
# 1) We can see some "surprises" when we look at which features have more affect on the income.
#    We'd assume that age, education and hours per week will have a stronger impact on the income than it seems.
#    Our assumption is that the continuous features has a low feature importance.
# 2) So on the same line of thinking we can see that the features with less catagorial options (as sex or race)
#    has a bigger feature importance than features with more catagorial options (occupation i.e.).
# 3) Of course reality also affects, we know most of the high salaries in the economy are white men's,
#    or that your martial status does not really influences your income.
# 4) All conclusions must be doubted because the distribution of the data isn't a good representation of the population
#    (2/3 are men, 90% are white for examples).


# 3. (5) Transformation - In order to build some other more "numerical" models, we will convert all factorial features
# (besides the label) to be numeric. This is despite the fact that most of the features has no numerical meaning.
# In addition to the numerical transformation, apply standardization on all the columns.
# Explain the importance of scaling before modeling.
#######################

data$workclass      = scale(as.numeric(data$workclass))
data$education      = scale(as.numeric(data$education))
data$marital.status = scale(as.numeric(data$marital.status))
data$occupation     = scale(as.numeric(data$occupation))
data$relationship   = scale(as.numeric(data$relationship))
data$race           = scale(as.numeric(data$race))
data$sex            = scale(as.numeric(data$sex))
data$native.country = scale(as.numeric(data$native.country))

# Just for verification:
summary(data)
apply(data,2,sd)

# Answer:
# Scaling is important when running L2 based operations as Eucledian metrics that penalized large distances.
# It is crucial to performe scaling in order to reduce the equalization effects of outliers and anomalies on the final model.


# 4(10) KNN
#######################

# 4.1 (10) Build a KNN model using the training data, and predict the test labels, using 3 significantly different K's.
# Compare and discuss the performances of the models. 
#######################

# Let's set new train and test sets from the scaled dataset (we leave aside the label feature but use the same indexes):
scaled_dataset_with_positive_label = data[data$label_income==1,]
scaled_dataset_with_negative_label = data[data$label_income==0,]

test_index_positive = sample(nrow(dataset_with_positive_label),round(nrow(dataset_with_positive_label)*0.3))
scaled_test_set_data.positive  = scaled_dataset_with_positive_label[test_index_positive,]
scaled_train_set_data.positive = scaled_dataset_with_positive_label[setdiff(1:nrow(scaled_dataset_with_positive_label),test_index_positive),]

test_index_negative = sample(nrow(dataset_with_negative_label),round(nrow(dataset_with_negative_label)*0.3))
scaled_test_set_data.negative  = scaled_dataset_with_negative_label[test_index_negative,]
scaled_train_set_data.negative = scaled_dataset_with_negative_label[setdiff(1:nrow(scaled_dataset_with_negative_label),test_index_negative),]

test_set_data_n  = rbind(scaled_test_set_data.positive,scaled_test_set_data.negative)
train_set_data_n = rbind(scaled_train_set_data.positive,scaled_train_set_data.negative)
test_set_data_n  = test_set_data_n[sample(nrow(test_set_data_n)),]
train_set_data_n = train_set_data_n[sample(nrow(train_set_data_n)),]
label_test_data  = test_set_data_n[, 13]
label_train_data = train_set_data_n[, 13]
test_set_data_n[,13]  = NULL
train_set_data_n[,13] = NULL

# A good K to start is the square root of the number of observations:
sqrt(nrow(data))
# As we can see we got 173.67, we going to round it down because we prefer an odd number for K:
tic(msg = "Time to complete KNN with K=173")
m1 <- knn(train_set_data_n, test_set_data_n, label_train_data, 173)
m1_time = toc()
confusion_matrix_m1 = confusionMatrix(m1,label_test_data)
confusion_matrix_m1$table
plot(confusion_matrix_m1$table, main = "Confusion Matrix m1: K=173")
m1_accuracy = as.numeric(confusion_matrix_m1$overall["Accuracy"])

# Now we'll choose a small number for K:
tic(msg = "Time to complete KNN with K=11")
m2 <- knn(train_set_data_n, test_set_data_n, label_train_data, 11)
m2_time = toc()
confusion_matrix_m2 = confusionMatrix(m2,label_test_data)
confusion_matrix_m2$table
plot(confusion_matrix_m2$table, main = "Confusion Matrix m2: K=11")
m2_accuracy = as.numeric(confusion_matrix_m2$overall["Accuracy"])

# And for the third model we'll pick a big number:
tic(msg = "Time to complete KNN with K=401")
m3 <- knn(train_set_data_n, test_set_data_n, label_train_data, 401)
m3_time = toc()
confusion_matrix_m3 = confusionMatrix(m3,label_test_data)
confusion_matrix_m3$table
plot(confusion_matrix_m3$table, main = "Confusion Matrix m3: K=401")
m3_accuracy = as.numeric(confusion_matrix_m3$overall["Accuracy"])

cat("With K=173 we got an accuracy of:",m1_accuracy,"in",m1_time$toc-m1_time$tic,"seconds
    \nWith K=11 we got an accuracy of:",m2_accuracy,"in",m2_time$toc-m2_time$tic,"seconds
    \nWith K=401 we got an accuracy of:",m3_accuracy,"in",m3_time$toc-m3_time$tic,"seconds\n")

# We can see that the smaller K is the more accurate is the model
# (of course it's in our choices of K's and not always correct, althogh we know that historicly 3-10 is usually a good range).
# The diffrences are not that big in accuracy but they are quite big in running time,
# So it's definitely the best (at least in this case) to choose a small K.


# 5 (30) PCA and ROC
#######################

# 5.1 (10) Find the principal components of the train set.
# Display the explained variance of each PC. 
#######################

principal_components <- princomp(train_set_data_n, cor=TRUE) 
summ_princp = summary(principal_components)
summ_princp

plot(principal_components,"Explained Variance (eigenvalues)")

# The proportion of variance may explain more intuatively:
vars <- summ_princp$sdev^2
vars <- vars/sum(vars)
barplot(vars, main="Proportion of Variance")


# 5.2 (10)
# Using the PC's that explain 70% of the variance, create (by projection of the PC's) a new trainset and testset.
# Fit a simple knn model with k=5, and display its confusion matrix.
#######################

# Based on the PC analysis, we can take only seven componenets (the cumulative proportion is 71.8%).

PC_train_set = principal_components$scores[,1:7]
principal_components_test_set <- princomp(test_set_data_n, cor=TRUE) 
PC_test_set = principal_components_test_set$scores[,1:7]

knn_PCA <- knn(PC_train_set, PC_test_set, label_train_data, 5)
confusion_matrix_pca = confusionMatrix(knn_PCA, label_test_data)
confusion_matrix_pca$table
plot(confusion_matrix_pca$table, main = "PCA Confusion Matrix")
pca_accuracy = as.numeric(confusion_matrix_pca$overall["Accuracy"])
cat("The accuracy of KNN based PCA model (K=5) is:",pca_accuracy)


# 5.3 (10)
# Display the ROC curve of the PCA-KNN model. 
# In general, what can we learn from a ROC chart? 
#######################

knn5_without_PCA <- knn(train_set_data_n, test_set_data_n, label_train_data, 5)
m2_knn_roc_model = roc(label_test_data,2*(as.numeric(knn5_without_PCA)-1.5))
knn_auc = m2_knn_roc_model$auc
pca_knn_roc_model = roc(label_test_data,2*(as.numeric(knn_PCA)-1.5))
plot(pca_knn_roc_model)
pca_auc = pca_knn_roc_model$auc

cat("Using 5-PCA_KNN model yields AUC of:",pca_auc,", while using 5-KNN model yields AUC of:",knn_auc)

# A ROC chart is a useful visual tool for comparing few classification models
# We can see for a classifier the false positive rate (X axis) vs true positive rate (Y axis).
# This way it describes relative trade-offs that the classifier makes between
# benefits/gains (true positives) and costs/risks (false positives).
# So from the ROC chart we can understand about the performance of the model.

# In this case we see that the margins from the random classifier are quite narrow. 

# We saw a major performance drop when moving from just 5-KNN to 5-PCA-KNN model. which can be explained as follows:
# 1. We didnt take enought components, therefore we delivered only 70% of the variance. We should have take more components.
# 2. The large amount of components needed (7) tells us that there aren't much redundent components.


# 6 (20) K-means
#######################

# 6.1 (10) Using the 4 most important features according to question 2.4, run k-means on the complete dataset.
# Generate only 4 clusters. Make sure number of iterations is higher than 10.
#######################

most_improtant_4_features = sort.list(importance(best_rf_tree_model))[1:4]
k_means = kmeans(data[,most_improtant_4_features], 4)
k_means$iter
# It semms the model mange to get to the solution in only few iterations.
# We will try to give it a harder starting point with distinct centers and worse algorithm:
k_means = kmeans(data[,most_improtant_4_features],
                 centers =  matrix(c(-4,-6,-3,-2,1,1,4,1,-4,1,-3,1,1,-6,4,-2), ncol=4, byrow=TRUE),
                 algorithm = "Forgy")
# Let's make sure the number of the iterations is higher than 10:
k_means$iter


# 6.2 (10) plot the clusters and the centers of the clusters.
# What can we learn from the chart about the clusters and the chosen number of clusters?
#######################

plot(data[,most_improtant_4_features], col = k_means$cluster)

# Now we will use only 2 features as an example to see the clusters with their centers:
plot(data[c("race","native.country")], col = k_means$cluster)
points(k_means$centers, col = 1:4, pch=8, cex=1)

# Answer:
# It's look like there's one main cluster and the other three can be concidered to be merged to one other cluster,
# while the "native.country" feature distinguishes the most between those.
# It's hard to learn from these charts about the best number of clusters
# (especialy when we use transformed catagorial features),
# it's better to calculate the total within-cluster variance for each k
# and choose the k where the biggest drop is.
