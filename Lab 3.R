# Load data
df <- read.csv('http://bit.ly/BudapestBI-R-csv')

# Convert to metric and fit
df$height <- df$heightIn * 2.54
df$weight <- df$weightLb * 0.45
str(df)
fit <- lm(weight ~ height, data = df)
summary(fit)
plot(df$height, df$weight)
points(df$height, predict(fit), col = "deepskyblue3")
abline(fit, col = 'darkorange2')

plot(df$height, df$weight)
abline(fit, col = 'red')
segments(df$height, df$weight, # original points
        df$height, predict(fit), # predictions, residuals
        col = 'green')

# 104
fit
predict(fit, newdata = data.frame(height = 104))
plot(df$height, df$weight, xlim = c(0, 200), ylim = c(-100, 100))
abline(fit, col ='red')

# Poly model
fit <- lm(weight ~ height + I(height ^2), data = df)
fit <- lm(weight ~ poly(height, 2, raw = TRUE), data = df)
predict(fit, newdata = data.frame(height = 56))

# Ggplot
library(ggplot2)
ggplot(df, aes(x = height, y = weight)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 5))



# Building a correlation model on a small dataset

df <- read.csv('http://bit.ly/math_and_shoes')
# or a method with additional arguments
download.file('http://bit.ly/math_and_shoes',
              'shoes.csv',
              method = 'curl', extra = '-L')
df <- read.csv('shoes.csv')

str(df)

fit <- lm(math ~ size, data = df)
summary(fit)
plot(df$size, df$math)

hist(df$math)
hist(df$size)

points(df$size, predict(fit), col = "deepskyblue3")
abline(fit, col = 'darkorange2')

plot(df$size, df$math)
abline(fit, col = 'red')
segments(df$size, df$math, # original points
         df$size, predict(fit), # predictions, residuals
         col = 'green')

library(ggplot2)
ggplot(df, aes(x = size, y = math)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 5))

library(scatterplot3d)
scatterplot3d(df[,c('size', 'math', 'x')])
fit <- lm(math ~ size + x, df)
p$plane3d(fit)
library(rgl)
plot3d(df$x, df$size, df$math, col = 'red')

residuals(lm(math ~ x, df))
residuals(lm(size ~ x, df))

cor(residuals(lm(math ~ x, df)),
    residuals(lm(size ~ x, df)))

library(psych)
partial.r(df, 2:3, 4)

# Iris

str(iris)

# Sepal width based on the length

fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
summary(fit)
pairs(iris, col = 'deepskyblue3')

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)

plot(iris$Sepal.Length, iris$Sepal.Width)
points(iris$Sepal.Width, predict(fit), col = "deepskyblue3")
abline(fit, col = 'darkorange2')

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + geom_smooth(method = 'lm', 
  formula = y ~ poly(x, 2))

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 3))

# Controlling for another variable (Species)
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)
lm(Sepal.Width ~ Sepal.Length + Species, iris)
summary(lm(Sepal.Width ~ Sepal.Length + Species, iris))

# In three tables
ggplot(iris, aes(Sepal.Width, Sepal.Length)) + geom_smooth(method = 'lm', se = F) + facet_wrap(~ Species)

# In three separate linear models
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() + geom_smooth(method = 'lm')

# In three separate linear models but one trendline
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = Species)) + geom_smooth(method = 'lm')

# In three separate linear models with a trendline not controlling for species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = Species)) + 
  geom_smooth(aes(col = Species), method = 'lm') + 
    geom_smooth(method = 'lm', col = 'black')



# CLUSTERING

# First build a distance matrix
dm <- dist(iris[,1:4]) # refer only to the first 4 variables
str(dm)
summary(dm)

# Plotting dendogram and determining cluster memberships
hc <- hclust(dm) # dendogram
plot(hc)
rect.hclust(hc, k = 3, border = 'red') # make 3 clusters
cutree(hc, k = 3)
rect.hclust(hc, k = 10, border = 'green') # make 10 clusters
cutree(hc, k = 10) # returns cluster membership

# Cluster membership
cn <- cutree(hc, k = 3)
table(iris$Species, cn) # matrix for species and clusters

# Clustering with NbClust finding the optimum number of clusters
library(NbClust)
NbClust(iris[, 1:4], method = 'complete')

# K-Means clustering
kmeans(iris[,1:4], 3)
kc <- kmeans(iris[,1:4], 3)
kc$cluster
table(cn, kc$cluster)

# Supervised learning for species

# Train and test datasets
iris
iris$rnd <- runif(150) # random numbers added
irisrnd <- iris[order(iris$rnd),]
irisrnd$rnd <- NULL
str(irisrnd)
train <- irisrnd[1:100,]
test <- irisrnd[101:150,]

# KNN
library(class)
fit <- knn(train[,1:4], test[,1:4], train$Species) # we estimate by sepal and petal length, width
fit # returns the predictive labels
test[1, 1:4]
table(test$Species, fit) # Compare original species with the estimates labels

# Decision tree
library(rpart)
ct <- rpart(Species ~ ., data = train)
plot(ct)
text(ct)
predict(ct)
str(predict(ct))
predict(ct, newdata = test, type = 'class')
table(
  test$Species,
  predict(ct, newdata = test, type = 'class'))
library(party)
library(partykit)
plot(as.party(ct))

