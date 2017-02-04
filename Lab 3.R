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
