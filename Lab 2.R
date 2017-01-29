# Load dataset
df <- read.csv('http://bit.ly/BudapestBI-R-csv')
str(df)

# 5th respondent's weight
df[5,5]
# or
df$weightLb[5]

# Minimum weight
min(df$weightLb)

# Minimum weight among males
males <- subset(df, sex == "m")
min(males$weightLb)
# or
min(subset(df, sex == 'm')$weightLb)

# Weight of the tallest man
max(males$heightIn)
subset(males, heightIn == 72)
# or
subset(males, heightIn == 72)$weightLb
# or
mh <- max(males$heightIn)
subset(males, heightIn == mh)$weightLb

# Look for row index
which(df$heightIn ==mh)
which(df$sex == 'm')

# Calculate BMI into a new column
df$height <- df$heightIn * 2.54
df$weight <- df$weightLb * 0.45
df$bmi <- df$weight / (df$height/100)^2

# Difference between age
plot(df$bmi ~ df$ageYear, col = "deepskyblue3")
abline(lm(bmi ~ ageYear, data = df), col = 'darkorange1')
fit <- lm(bmi ~ ageYear, data = df)
abline(fit, col = 'red')
summary(fit)
summary(df)

# Nicer summary tables can be done with the pander package
library(pander)
pander(summary(df$ageYear), style = 'grid')

# Average BMI per age
aggregate(bmi ~ ageYear, FUN = mean, data = df)
round(df$ageYear)
df$year <- round(df$ageYear)
aggregate(bmi ~ year, FUN = mean, data = df)

# Simple plots
hist(df$ageYear)
hist(df$bmi)
abline(v = c(18.5, 25), col = "orangered1")
boxplot(df$bmi)
boxplot(bmi ~ sex, df)
library(beanplot)
beanplot(df$bmi)
str(df)
table(df$sex)
pie(table(df$sex))
barplot(table(df$sex))
pairs(df) # shows all scatterplot within the dataset

# Using different plots next to each other
par(mfrow = c(1, 2))
boxplot(df$bmi)
beanplot(df$bmi)
