getwd()
setwd("C:/Users/user/Desktop/Projects/AI Project Final")

# Working with Iris Dataset

# Importing the data
data_1 = read.csv("iris_1.csv", header = FALSE)
attach(data_1)

# Changing the names of the variables
names(data_1) <- c('sepal_length', 'sepal_width', 'petal_length', 'petal_width','class')

# Printing the first ten rows
head(data_1,n = 10)

# Collecting evidence for the question 'should the data be scaled?'
summary(data_1)

# Seperating columns from the original data to work with
data_2 = data_1[,1:4]
Y_iris = data_2

# Printing the first ten rows
head(Y_iris,n = 10)

# Setting the seed so that the results are reproducible
seed_val  <- 10
set.seed(seed_val)

# Selecting a number of clusters
k=5

# Running the k-means algorithm
first_clust_iris = kmeans(Y_iris, centers = 5, nstart = 1)

# How many flowers are in each cluster?
first_clust_iris$size

# Setting the seed
seed_val <- 38
set.seed(seed_val)

# Selecting a number of clusters and run the k-means algorithm
second_clust_iris = kmeans(Y_iris, centers = 5, nstart = 1)

# How many flowers are in each cluster?
second_clust_iris$size

# Adding cluster assignments to the data
Y_iris["first_clust_iris"] <- first_clust_iris$cluster
Y_iris["second_clust_iris"] <- second_clust_iris$cluster

# Printing the first ten rows
head(Y_iris,n = 10)

# Checking correlation 
cor(Y_iris)

# Observing first characteristic of flowers
p_iris = cor(Y_iris)[,'sepal_length']
p_iris[order(-p_iris),drop = FALSE]

# Loading ggplot2
library(ggplot2)

# Creating the plot of sepal length and sepal width for the first clustering algorithm
plot_one_iris  <- ggplot(Y_iris, aes(x = sepal_length, y = sepal_width, color = as.factor(first_clust_iris))) + geom_point()

# Creating the plot of sepal length and sepal width for the second clustering algorithm
plot_two_iris  <- ggplot(Y_iris, aes(x = sepal_length, y = sepal_width, color = as.factor(second_clust_iris))) + geom_point()

# Installing and loading gridExtra package
install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot_one_iris, plot_two_iris, ncol = 2)

# Executing hierarchical clustering with complete linkage
hier_clust_1_iris <- hclust(dist(Y_iris), method = "complete")

# Printing the dendrogram
plot(hier_clust_1_iris)


# Getting cluster assignments based on number of selected clusters
hc_1_assign_iris <- cutree(hier_clust_1_iris, k = 5)

# Executing hierarchical clustering with single linkage
hier_clust_2_iris <- hclust(dist(Y_iris), method = "single")

# Printing the dendrogram
plot(hier_clust_2_iris)

# Getting cluster assignments based on number of selected clusters
hc_2_assign_iris <- cutree(hier_clust_2_iris, k = 5)

# Adding assignment of chosen hierarchical linkage
Y_iris["hc_clust_1_iris"] <- hc_1_assign_iris

# Checking how complete and single hierarchical clustering differs
hc_1_assign_iris == hc_2_assign_iris

# Removing the first_clust_iris, and second_clust_iris variables
hd_simple_iris <- Y_iris[,!(names(Y_iris) %in% c("first_clust_iris","second_clust_iris"))]

# Printing first 10 rows
head(hd_simple_iris, n = 10)

# Getting the mean and standard deviation summary statistics
clust_summary_iris <- do.call(data.frame, aggregate(. ~ hc_clust_1_iris, data = hd_simple_iris, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary_iris

# Adding assignment of chosen hierarchical linkage
Y_iris["hc_clust_2_iris"] <- hc_2_assign_iris

# Removing the first_clust_iris, second_clust_iris and hc_clust_1_iris variables
hd_simple_iris <- Y_iris[,!(names(Y_iris) %in% c("first_clust_iris","second_clust_iris","hc_clust_1_iris"))]

# Printing first 10 rows
head(hd_simple_iris, n = 10)

# Getting the mean and standard deviation summary statistics
clust_summary_iris <- do.call(data.frame, aggregate(. ~ hc_clust_2_iris, data = hd_simple_iris, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary_iris

hd_simple_iris["hc_clust_1_iris"] <- hc_1_assign_iris

# Here we will look at several plots 

# Plotting sepal_length and sepal_width
plot_one_iris_sp.le_sp.wi <- ggplot(hd_simple_iris,aes(x = sepal_length, y = sepal_width, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting sepal_length and sepal_width
plot_two_iris_sp.le_sp.wi <- ggplot(hd_simple_iris,aes(x = sepal_length, y = sepal_width, color = as.factor(hc_clust_2_iris))) + geom_point()

# Plotting sepal_length and petal_length
plot_one_iris_sp.le_pe.le <- ggplot(hd_simple_iris,aes(x = sepal_length, y = petal_length, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting sepal_length and sepal_width
plot_two_iris_sp.le_pe.le <- ggplot(hd_simple_iris,aes(x = sepal_length, y = petal_length, color = as.factor(hc_clust_2_iris))) + geom_point()

# Plotting sepal_length and petal_width
plot_one_iris_sp.le_pe.wi <- ggplot(hd_simple_iris,aes(x = sepal_length, y = petal_width, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting sepal_length and sepal_width
plot_two_iris_sp.le_pe.wi <- ggplot(hd_simple_iris,aes(x = sepal_length, y = petal_width, color = as.factor(hc_clust_2_iris))) + geom_point()

# Plotting sepal_width and petal_length
plot_one_iris_sp.wi_pe.le <- ggplot(hd_simple_iris,aes(x = sepal_width, y = petal_length, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting sepal_width and petal_length
plot_two_iris_sp.wi_pe.le <- ggplot(hd_simple_iris,aes(x = sepal_width, y = petal_length, color = as.factor(hc_clust_2_iris))) + geom_point()

# Plotting sepal_width and petal_width
plot_one_iris_sp.wi_pe.wi <- ggplot(hd_simple_iris,aes(x = sepal_width, y = petal_width, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting sepal_width and petal_width
plot_two_iris_sp.wi_pe.wi <- ggplot(hd_simple_iris,aes(x = sepal_width, y = petal_width, color = as.factor(hc_clust_2_iris))) + geom_point()

# Plotting petal_length and petal_width
plot_one_iris_pe.le_pe.wi <- ggplot(hd_simple_iris,aes(x = petal_length, y = petal_width, color = as.factor(hc_clust_1_iris))) + geom_point()

# Plotting petal_length and petal_width
plot_two_iris_pe.le_pe.wi <- ggplot(hd_simple_iris,aes(x = petal_length, y = petal_width, color = as.factor(hc_clust_2_iris))) + geom_point()

# Printing the plots
grid.arrange(plot_one_iris_sp.le_sp.wi, plot_two_iris_sp.le_sp.wi, plot_one_iris_sp.le_pe.le, plot_two_iris_sp.le_pe.le, plot_one_iris_sp.le_pe.wi, plot_two_iris_sp.le_pe.wi, plot_one_iris_sp.wi_pe.le, plot_two_iris_sp.wi_pe.le, plot_one_iris_sp.wi_pe.wi, plot_two_iris_sp.wi_pe.wi, plot_one_iris_pe.le_pe.wi, plot_two_iris_pe.le_pe.wi, ncol=2)

# Adding TRUE if the algorithm shows promise, adding FALSE if it does not
explore_kmeans_iris <- FALSE
explore_hierarch_complete_iris <- TRUE
explore_hierarch_single_iris <- TRUE

# PCA for iris dataset

Y_iris = as.matrix(data_2)

# Sample variance covariance matrix of iris dataset with working columns
Z = cov(Y_iris)

# Computing the eigenvalues and corresponding eigenvectors of Z
eigen(Z)

# Eigen-values of Z are
lambda = eigen(Z)$value
lambda

# Orthonormal eigen-vectors are (in columns)
v = eigen(Z)$vector
v

# Ordering of eigenvalues in descending order
order(lambda)


# Proportion of total variation explained by the principal components
for (i in lambda) {
  print(i / sum(lambda))
}

# Scree Plot
plot(lambda, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Plot')
lines(lambda)

# Calculating sample PCAs from observed data matrix
X_pca_iris = rep(0,nrow(Y_iris))
for (i in 1:nrow(Y_iris)) {
  X_pca_iris[i] = crossprod(v[,1],Y_iris[i,])
}
X_pca_iris

# Working with Breast Cancer Dataset

# Importing the data
data_3 = read.csv("wdbc_1.csv", header = FALSE)
attach(data_3)

# Changing the names of the variables
names(data_3) <- c('ID number', 'Diagnosis', 'radius',	'texture',	'perimeter',	'area',	'smoothness',	'compactness',	'concavity',	'concave',	'symmetry',	'fractal',	'feature_11',	'feature_12',	'feature_13',	'feature_14',	'feature_15',	'feature_16',	'feature_17',	'feature_18',	'feature_19',	'feature_20',	'feature_21',	'feature_22',	'feature_23',	'feature_24',	'feature_25',	'feature_26',	'feature_27',	'feature_28',	'feature_29',	'feature_30')

# Print the first ten rows
head(data_3,n = 10)

# Collecting evidence for the question 'should the data be scaled?'
summary(data_3)

# Seperating columns from the original data to work with
data_4 = data_3[,3:32]
Y_wdbc = data_4

# Standardizing Breast Cancer Dataset
for (i in 1:30) {
  Y_wdbc[,i] = (Y_wdbc[,i]-mean(Y_wdbc[,i]))/sd(Y_wdbc[,i])
}

# Printing the first ten rows
head(Y_wdbc,n = 10)

# Printing summary after scaling
summary(Y_wdbc)

# Setting the seed so that results are reproducible
seed_val  <- 10
set.seed(seed_val)

# Selecting a number of clusters
k=5

# Running the k-means algorithm
first_clust_wdbc = kmeans(Y_wdbc, centers = 5, nstart = 1)

# How many patients are in each cluster?
first_clust_wdbc$size

# Setting the seed
seed_val <- 38
set.seed(seed_val)

# Selecting a number of clusters and run the k-means algorithm
second_clust_wdbc = kmeans(Y_wdbc, centers = 5, nstart = 1)

# How many patients are in each cluster?
second_clust_wdbc$size

# Adding cluster assignments to the data
Y_wdbc["first_clust_wdbc"] <- first_clust_wdbc$cluster
Y_wdbc["second_clust_wdbc"] <- second_clust_wdbc$cluster

# Printing the first ten rows
head(Y_wdbc,n = 10)

# Checking correlation 
cor(Y_wdbc)

# Observing first characteristic of flowers
p_wdbc = cor(Y_wdbc)[,'radius']
p_wdbc[order(-p_wdbc),drop = FALSE]

# Creating the plot of radius and feature_30 for the first clustering algorithm
plot_one_wdbc  <- ggplot(Y_wdbc, aes(x = radius, y = feature_30, color = as.factor(first_clust_wdbc))) + geom_point()

# Creating the plot of radius and feature_30 for the second clustering algorithm
plot_two_wdbc  <- ggplot(Y_wdbc, aes(x = radius, y = feature_30, color = as.factor(second_clust_wdbc))) + geom_point()

grid.arrange(plot_one_wdbc, plot_two_wdbc, ncol = 2)

# Executing hierarchical clustering with complete linkage
hier_clust_1_wdbc <- hclust(dist(Y_wdbc), method = "complete")

# Printing the dendrogram
plot(hier_clust_1_wdbc)


# Getting cluster assignments based on number of selected clusters
hc_1_assign_wdbc <- cutree(hier_clust_1_wdbc, k = 5)

# Executing hierarchical clustering with single linkage
hier_clust_2_wdbc <- hclust(dist(Y_wdbc), method = "single")

# Printing the dendrogram
plot(hier_clust_2_wdbc)

# Getting cluster assignments based on number of selected clusters
hc_2_assign_wdbc <- cutree(hier_clust_2_wdbc, k = 5)

# Adding assignment of chosen hierarchical linkage
Y_wdbc["hc_clust_wdbc"] <- hc_1_assign_wdbc

# Removing the first_clust_wdbc and second_clust_wdbc variables
hd_simple_wdbc <- Y_wdbc[,!(names(Y_wdbc) %in% c("first_clust_wdbc","second_clust_wdbc"))]

# Printing first 10 rows
head(hd_simple_wdbc, n = 10)

# Get the mean and standard deviation summary statistics
clust_summary_wdbc <- do.call(data.frame, aggregate(. ~ hc_clust_wdbc, data = hd_simple_wdbc, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary_wdbc

# Label Encoding
combined <- hd_simple_wdbc

encoded <- rep(0,length(data_3$Diagnosis))
for (i in 1:length(data_3$Diagnosis)) {
  if(data_3$Diagnosis[i] == 'B'){
    encoded[i] = 0
  }else{
    encoded[i] = 1
  }
}
encoded

combined["encoded"] <- encoded

# Printing first 10 rows
head(combined,n = 10)

# Fitting logistic regression
logistic_model <- glm(encoded ~.,family = binomial(link = 'logit'),data = combined)
summary(logistic_model)

# Ploting radius and feature_21
plot_one_wdbc_re_fe.21 <- ggplot(hd_simple_wdbc,aes(x = radius, y = feature_21, color = as.factor(hc_clust_wdbc))) + geom_point()

# Ploting concavity and smoothness
plot_two_wdbc_con_sm <- ggplot(hd_simple_wdbc,aes(x = concavity, y = smoothness, color = as.factor(hc_clust_wdbc))) + geom_point()

grid.arrange(plot_one_wdbc_re_fe.21, plot_two_wdbc_con_sm, ncol=2)

# Adding TRUE if the algorithm shows promise, adding FALSE if it does not
explore_kmeans_wdbc <- FALSE
explore_hierarch_complete_wdbc <- TRUE
explore_hierarch_single_wdbc <- FALSE

# PCA for Breast Cancer Dataset

# Removing the first_clust_wdbc, second_clust_wdbc and hc_clust_wdbc variables from Y_wdbc
Y_wdbc <- Y_wdbc[,!(names(Y_wdbc) %in% c("first_clust_wdbc","second_clust_wdbc","hc_clust_wdbc"))]

# Printing first 10 rows
head(Y_wdbc,n = 10)

Y_wdbc = as.matrix(Y_wdbc)

# Executing this line to print all the rows of the dataset and printing sample PCAs
options(max.print = 1000000)

# Sample variance covariance matrix of iris dataset with working columns
Z = cov(Y_wdbc)

# Computing the eigenvalues and corresponding eigenvectors of Z
eigen(Z)

# Eigen-values of Z are
lambda = eigen(Z)$value
lambda

# Orthonormal eigen-vectors are (in columns)
v = eigen(Z)$vector
v

# Ordering of eigenvalues in descending order
order(lambda)


# Proportion of total variation explained by the principal components
for (i in lambda) {
  print(i / sum(lambda))
}

# Scree Plot
plot(lambda, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Plot')
lines(lambda)

# Calculating sample PCAs from observed data matrix

for(i in 1:nrow(Y_wdbc)){
  X_pca_wdbc = crossprod(v[,1],Y_wdbc[i,])
  for (j in 2:5) {                                            # Since 5 PCs explain a significant amount of variation 
    X_pca_wdbc = cbind(X_pca_wdbc,crossprod(v[,j],Y_wdbc[i,]))
  }
  print(X_pca_wdbc)
}
