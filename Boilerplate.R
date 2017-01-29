# LIBRARIES
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(DataCombine)
library(descr)
library(fBasics)
library(stargazer)
library(sandwich)
library(lmtest)
library(splines)
library(readr)
library(gmodels)
library(sjPlot)
library(sjmisc)
library(mfx)
library(descr)
library(rstan)
library(pander) # nice table summaries
library(beanplot)
library(ggthemes)
library(GGally) # smart pair function
library(pairsD3) # smart pai function with interactive Javascript
library(XML)

# CLEAR MEMORY
rm(list = ls())

# SET WORKING DIRECTORY
setwd("C:\\Users\\Documents")
getwd()

# LOAD  DATA
hotels <- read.csv("hotels.csv")
price <- hotels$price_eur

# CLEAN VARIABLES
gdppc$gdppc <- as.numeric(as.character(gdppc$gdppc))
mutate(
  guest_r = as.numeric(substr(guest_r, 1, 3)),
  price_huf = as.numeric(gsub("Ft", "", gsub(" ", "", price))),
  price_eur = as.numeric(price_huf) / 311.00,
  dist_c = as.numeric(gsub(",", ".", dist_c))
) %>%
  rename(
    rating = guest_r,
    stars = star_r,
    distance = dist_c
  )

# DATA TRANSFORMATIONS
names(dataset)[names(dataset) == "X2015..YR2015."] <- "2015"
dataset[dataset == ".."] = NA
data$Leads <- as.numeric(as.character(data$Leads))

# SHOW DATA
hotels
price
pairs(df) # shows all scatterplots

# DESCRIPTIVES
summary(hotels)
basicStats(hotels)
summary(price)
basicStats(price)

# SUBSETTING
hotels <- subset(hotels, star_r >= 3 & star_r <= 4 & distance < 10)
salaries_educ <- salaries[salaries$degree == 3,]
newdata <- subset(mydata, age >= 20 | age < 10, select = c(ID, Weight))
newdata <- subset(mydata, sex == "m" & age > 25,
                  select = weight:income)

# PLOT COLORS
# deepskyblue3
# seagreen3
# darkorange1
# orangered1
# lightblue4
# aquamarine4
# lightsalmon3
# indianred3

# FORMULAE
lm(x ~ y, data = df) # linear model between x and y
lm(x ~ y + z, data = df) # linear model between x and y + z features
lm(x ~ ., data = df) # use all features
lm(x ~ . - z, data = df) # use all features except z
lm(x ~ 0 + y + z, data = df) # drop intercept term
lm(x ~ y + I(y ^ 2), data = df) # use functions

# HISTOGRAM PLOTS
ggplot(hotels, aes(x = price_eur)) +
  geom_histogram(stat = "bin", binwidth = 10) + labs(x = "X Axis, binwidth: 1", y = "Density")
ggplot(hotels, aes(x = price_eur)) +
  geom_histogram(stat = "bin", binwidth = 100) + labs(x = "X Axis, binwidth: 100", y = "Density")
qplot(hotels$price_huf, geom = "histogram", binwidth = 10000)
qplot(hotels$distance, geom = "histogram", binwidth = 1)

# KERNEL DENSITY PLOT
ggplot(hotels, aes(x = price_eur)) + geom_density(stat = "bin", binwidth = 10) + labs(x = "X Axis, binwidth: 1", y = "Density")
ggplot(diamonds, aes(x = price, fill = cut)) + geom_density(alpha = 0.3) # overlapping kernel densities

# SCATTERPLOT
ggplot(data = hotels, aes(x = distance, y = price_huf)) + geom_point(size = 1.5, colour = "orange")

# HEATMAP
ggplot(diamonds, aes(x = cut, y = color, fill = price)) + geom_tile()

# INTEGRATED GRAPHICS
plot(movies$genre)
plot(movies$genre, movies$rating)
plot(x = movies$rating, y = movies$runtime)
hist(movies$rating)
hist(movies$rating, breaks = 20)
boxplot(movies$runtime)
plot(movies[, c("rating", "votes", "runtime")])
pie(table(movies$genre))
plot(mercury$temperature, mercury$pressure,
     xlab = "Temperature",
     ylab = "Pressure",
     main = "T vs P for Mercury",
     type = "o",
     col = "orange",
     col.main = "darkgray",
     cex.axis = 0.6,
     lty = 5,
     pch = 4)

# ADD LINEAR REGRESSION TO A SCATTERPLOT
movies_lm <- lm(movies$rating ~ movies$votes)
plot(movies$votes, movies$rating)
abline(coef(movies_lm))

# LOWESS NONPARAMETRIC REGRESSION
ggplot(data = hotels, aes(x = distance, y = price)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "loess", colour = "darkgreen", se = F)

# LEVEL-LEVEL LINEAR REGRESSION
reg1 <- lm(price ~ distance, data = hotels)
summary(reg1)

# LEVEL-LEVEL LINEAR REGRESSION VISUALIZED WIYTH SCATTERPLOT
ggplot(data = hotels, aes(x = distance, y = price)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "lm", colour = "navy", se = F)

# LOG-LEVEL LINEAR REGRESSION
reg2 <- lm(lnprice ~ distance, data = hotels)
summary(reg2)

# LOG-LEVEL LINEAR REGRESSION VISUALIZED WITH SCATTERPLOT
ggplot(data = hotels, aes(x = distance, y = lnprice)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "lm", colour = "navy", se = F)

# RESIDUALS OF REGRESSIONS
hotels$e1 <- resid(reg1)
hotels$e2 <- resid(reg2)
bestdeals1 <- subset(hotels, e1 < (-100))
bestdeals2 <- subset(hotels, e2 < (-0.5))

# PIECEWISE LINEAR SPLINE IN LEVEL-LOG REGRESSION
ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 1, knots = c(4)), colour = "navy", se = F)

# PIECEWISE LINEAR SPLINE IN LEVEL-LOG REGRESSION WITH SUMMARY
reg3 <- lm(lifeexp ~ lngdppc + I(pmax(lngdppc - 4, 0)), data = xc)
summary(reg3)
reg3_predict <- predict(reg3)
ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_point(size = 1.5, colour = "navy", aes(x = lngdppc, y = reg3_predict))
xc$e3 <- resid(reg3)

# QUADRATIC IN LEVEL-LOG REGRESSION
xc$lngdppc_sq <- xc$lngdppc ^ 2
reg4norm <- lm(lifeexp ~ lngdppc + lngdppc_sq, data = xc)
reg4poly <- lm(lifeexp ~ poly(lngdppc, 2, raw = TRUE), data = xc)
summary(reg4norm)
summary(reg4poly)
xc$e4norm <- resid(reg4norm)
xc$e4poly <- resid(reg4poly)
ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), colour = "navy", se = F)

# CUBIC IN LEVEL-LOG REGRESSION
xc$lngdppc_cu <- xc$lngdppc ^ 3
reg5 <- lm(lifeexp ~ lngdppc + lngdppc_sq + lngdppc_cu, data = xc)
summary(reg5)
xc$e5 <- resid(reg5)
ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1.5, colour = "orange") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3, raw = TRUE), colour = "navy", se = F)

# NICE TABLE
corruption$audit <- factor(corruption$audit, levels = c(0, 1), labels = c("Control", "Audit"))
corruption$invitation_categ <- factor(corruption$invitation_categ, levels = c(0, 1, 2),
                                      labels = c("Control", "Invitations",
                                                 "Invitations Plus Comment Forms"))
sjt.xtab(corruption$audit, corruption$invitation_categ,
         title = "Table 1 - Number of Villages in Each Treatment Category",
         var.labels = c("The group", "Type of the intervention"),
         show.row.prc = TRUE, show.col.prc = TRUE, file = "Table 1.doc")

# TWO REGRESSION IN THE SAME PLOT
ggplot(cps) +
  geom_smooth(aes(ihigrdc_female, lnw), method = "lm", colour = "deepskyblue3") +
  annotate("text", x = 5.1, y = 2.95, label = "ihigrdc_female", colour = "deepskyblue3") +
  geom_smooth(aes(age_female, lnw), method = "lm", colour = "lightsalmon2") +
  annotate("text", x = 40, y = 3, label = "age_female", colour = "lightsalmon2")

# MAKING NEW MEASURE FROM ONE COLUMN TO THE OTHER WITH DUMMY VARABLES
share$sports_regular[share$sports == "1. More than once a week"] <- 1
share$sports_regular[share$sports == "2. Once a week"] <- 1
share$sports_regular[share$sports == "3. One to three times a month"] <- 0
share$sports_regular[share$sports == "4. Hardly ever, or never"] <- 0
share$sports_irregular[share$sports == "1. More than once a week"] <- 0
share$sports_irregular[share$sports == "2. Once a week"] <- 0
share$sports_irregular[share$sports == "3. One to three times a month"] <- 1
share$sports_irregular[share$sports == "4. Hardly ever, or never"] <- 1

# SPLIT COLUMNS INTO BINARY VARIABLES
lst <- strsplit(as.character(finaldataset$education),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$education <- NULL

# MACHINE LEARNING PACKAGES
library(rpart) # Decision tree
library(randomForest) # Random forest
library(gbm) # Gradient boosting machine
library(nnet) # Old neural network package
library(caret) # Hundred models
library(rocr) # Receiver operating characteristic curve
library(glmnet) # Lasso

# DECISION TREE
library(rpart)
set.seed(123)
dim(d_train)
dim(d_test)
md <- rpart(spam ~ ., data = d_train)
md
plot(md, uniform = TRUE, compress = TRUE)
text(md, use.n = TRUE, cex = 0.7)
phat <- predict(md, d_test)[, "1"]
table(ifelse(phat > 0.5, 1, 0), d_test$spam)
args(rpart.control)
md <- rpart(spam ~ ., data = d_train, control = rpart.control(cp = 0))
plot(md, uniform = TRUE, compress = TRUE)
printcp(md)
plotcp(md)
prune(md, cp = 0.01)
prune(md, cp = 0.0014995) # min
prune(md, cp = 0.0051546) # min+1se
args(rpart.control)
md <- rpart(spam ~ ., data = d_train, control = rpart.control(maxdepth = 2))
plot(md, uniform = TRUE, compress = TRUE)
text(md, use.n = TRUE, cex = 0.7)

# RANDOM FOREST
library(randomForest)
set.seed(123)
dim(d_train)
dim(d_test)
md <- randomForest(spam ~ ., data = d_train, ntree = 100)
md
plot(md)
phat <- predict(md, d_test, type = "prob")[, "1"]
table(ifelse(phat > 0.5, 1, 0), d_test$spam)
md <- randomForest(spam ~ ., data = d_train, ntree = 100, importance = TRUE)
varImpPlot(md, type = 2)

# GBM
library(gbm)
set.seed(123)
dim(d_train)
dim(d_test)
d_train_ynum <- d_train
d_train_ynum$spam <- ifelse(d_train_ynum$spam == "1", 1, 0) # gbm needs numeric y
md <- gbm(spam ~ ., data = d_train_ynum, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01)
md
yhat <- predict(md, d_test, n.trees = 100)
table(ifelse(yhat > 0, 1, 0), d_test$spam)
args(gbm)
md <- gbm(spam ~ ., data = d_train_ynum, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01,
          cv.folds = 5)
gbm.perf(md, plot.it = TRUE)
md <- gbm(spam ~ ., data = d_train_ynum, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.3,
          cv.folds = 5)
gbm.perf(md, plot.it = TRUE)
md
yhat <- predict(md, d_test, n.trees = 100)
table(ifelse(yhat > 0, 1, 0), d_test$spam)
yhat <- predict(md, d_test, n.trees = gbm.perf(md, plot.it = FALSE))
table(ifelse(yhat > 0, 1, 0), d_test$spam)

# GIT PUSH
# git remote add origin https://github.com/szavuly/R-Boilerplate.git
# git config remote.origin.url git@github.com:szavuly/R-Boilerplate.git
# git pull -u origin master
# git push -u origin master 
# git push -f origin master

# HALT AND CATCH FIRE 