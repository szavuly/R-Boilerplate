# Create a pie chart
# c() means creating a vector (numerical)
pie(c(80, 20), col = c('deepskyblue3', 'lightsalmon2'),
    labels = c(
      'Cleaning data',
      "Complaning about cleaning data"),
    init.angle = 310,
    main = "Data Science")

# Packages: click Packages on the right to see loadable packages

# Help: Write ?pie in the console to see its description in help
?pie
?sqrt

# Vector
1+3
# Answer: [1] 4 # one element in the vector

# Assign values to variables
x <- 4
x * 4
sqrt(x)
pi
pi * 3

# Random number generation
?runif
runif(5, max=10)
runif(5) * 10 

# Setting seed for same random numbers in every environment
set.seed(123)
runif(2)

# Functions
f <- function(x) 2 * x + 1
f(x)
f(pi)

# Sequence
x <- seq(1, 5)
f(x)
plot(x, f(x))

# Line
x <- seq(1, 5, by = 0.1)
f(x)
plot(x, f(x), type = 'l')
?plot

# Sinus
x <- seq(1, 5, by = 0.1)
s <- sin(x)
plot(s, type = 'l')
?plot

# Creating vectors and simple linear models
h <- c(174, 170, 160) # c: combining values into a vector
h[1] # first element of the vector
w <- c(90, 80, 70)
plot(h, w)
cor(h, w) # correlation coefficient
lm(w ~ h)
plot (h, w)
abline(lm(w ~ h), col = 'red')

# Working with larger dataframes
df <- read.csv("http://bit.ly/BudapestBI-R-csv")
str(df) # see observations
df[1, 1]
df [1, ] # first row
df [ ,1] # first column

# Estimating a linear model
str(df)
lm(df [, 5] ~ df [, 4])
summary(lm(df [, 5] ~ df [, 4]))

# Scatterplot
h <- df[, 5]
w <- df[, 4]
plot(h, w, col = 'deepskyblue3')
abline(lm(w ~ h), col = "orange")
str(df)
lm(df$weightLb ~ df$heightIn)
lm(weightLb ~ heightIn, data = df)

# Adding a new column
df$height <- df$heightIn * 2.54
str(df)
df$weight <- df$weightLb * 0.45
df$bmi <- df$weightLb / (df$height/100)^2

# Checking min and max for variables
min(df$bmi)
max(df$bmi)
sum(df$bmi) # sum of the data, which doesn't make sense in this case
length(df$bmi)
nrow(df)
ncol(df)
dim(df) # dimensions of the data frame

# Filtering
str(df)
?subset()
females <- subset (df, sex == "f")
males <- subset (df, sex == "m")
plot(males$height, males$weight, col = "deepskyblue3")
abline(lm(weight ~ height, data = males), col = 'orange')
aggregate(height ~ sex, FUN = mean, data = df)
