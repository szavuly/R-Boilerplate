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
par(mfrow = c(1, 1)) # puts one plot per picture back



### ADVANCED PLOTTING WITH GGPLOT ###

# Loading library and dataset
library(ggplot2)
?diamonds # this will be our test dataset

# Ggplot bar plot
ggplot(diamonds, aes(x = cut)) + geom_bar() # aes means aesthetics
p <- ggplot(diamonds, aes(x = cut)) + geom_bar()
str(p)
p <- p + geom_bar()
p
p + theme_bw()
ggplot(diamonds, aes(x = cut)) + 
  geom_bar(colour = "seagreen3", fill = "white") # using different formattings
ggplot(diamonds, aes(x = cut)) + geom_bar() + scale_y_log10() # using log y axis
ggplot(diamonds, aes(x = cut)) + geom_bar() + scale_y_reverse()
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()
ggplot(diamonds, aes(x = carat, y = price, 
  color = cut)) +
  geom_point()
ggplot(diamonds, aes(x = carat, y = price, 
  color = color, shape = cut)) +
  geom_point()
ggplot(diamonds, aes(x = carat, y = price, 
  color = color, shape = cut)) +
  geom_point() + facet_wrap(~ clarity)

# Histogram on price
hist(diamonds$price)
# or
ggplot(diamonds, aes(x = price)) + geom_histogram(binwidth = 1000)

# Kernel density and cut
ggplot(diamonds, aes(x = price)) + geom_histogram(binwidth = 1000) + facet_wrap(~ cut)
# or
ggplot(diamonds, aes(x = price, fill = cut)) + geom_density(alpha = 0.3)

# Heatmap on cut and color
ggplot(diamonds, aes(x = cut, y = color, fill = price)) + geom_tile()

# Scatterplot on x, y, z
ggplot(diamonds, aes(x = x, y = y)) + geom_point() # slow
ggplot(diamonds, aes(x = x, y = y)) + geom_hex() # fast

# Fit model carat and price
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_smooth()
?geom_smooth
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_smooth(method = 'lm', se = F)
ggplot(diamonds, aes(carat, price, color = cut)) + geom_point() + geom_smooth(method = 'lm', se = F)
ggplot(diamonds, aes(carat, price)) + geom_point(aes(color = cut)) + geom_smooth(method = 'lm', se = F)
ggplot(diamonds, aes(carat, price)) + geom_point(aes(color = cut)) + geom_smooth(method = 'lm', se = F) + geom_smooth()
ggplot(diamonds, aes(carat, price)) + geom_point(aes(color = cut)) + geom_smooth(method = 'lm', se = F) + geom_smooth() + facet_wrap(~ color)

# Themes
p <- ggplot(diamonds, aes(x, y, color = cut)) + geom_point()
p
library(ggthemes)
p + theme_economist() + scale_color_economist()

# Smart pair plotting
library(GGally)
pairs(df)
ggpairs(df)
library(pairsD3)
pairsD3(df)

# Loading page from an online source
library(XML)
ftse <- readHTMLTable(readLines('https://en.wikipedia.org/wiki/FTSE_100_Index'),
              which = 2, header = TRUE, stringsAsFactors = FALSE) # which refers to the second table of the page
str(ftse)
names(ftse)[4]
names(ftse)[4] <- 'cap'
names(ftse)
str(ftse)

# Data transformation
str(ftse)
ftse$cap <- as.numeric(ftse$cap)
str(ftse)
ftse$Employees<- sub(",", "", ftse$Employees) # substitute comma with nothing
ftse$Employees <- as.numeric(ftse$Employees)
str(ftse)

# Minimum and maximum
range(ftse$Employees)

# Minimum employees per sector
aggregate(Employees ~ Sector, FUN = min, data = ftse)

# Visualize the employees and cap per sector
ggplot(ftse, aes(x = Employees, y = cap)) + geom_point() + facet_wrap(~ Sector)
# or better
ggplot(ftse, aes(x = Employees, y = cap, color = Sector)) + geom_point()

# Banks on a scatterplot with names instead of points
banks <- subset(ftse, Sector == 'Banking')
ggplot(banks, aes(x = Employees, y = cap)) + geom_text(aes(label = Company))
                          

                            
## DATA TABLES ##
library(hflights) # Flights dataframe
library(data.table) # Better performance than data frames
str(hflights)
dt <- data.table(hflights)
str(hflights)
dt # Printing out just the first rows by default

dt[DayOfWeek == 5] # Subset, where day of the week equals 5
dt[DayOfWeek == 5 & Month == 3]
dt[DayOfWeek == 5 & Month == 3, ActualElapsedTime]
dt[DayOfWeek == 5 & Month == 3, ActualElapsedTime, AirTime]
dt[DayOfWeek == 5 & Month == 3, mean(ActualElapsedTime, na.rm = TRUE)] # Removing NAs, causing mean argument not to work
dt[is.na(ActualElapsedTime) & Cancelled == 0 & Diverted == 0]

# Average flight time to LA
dt[Dest == 'LAX', mean(AirTime, na.rm = TRUE)]

# Average flight time per destination
dt[, mean(AirTime, na.rm = TRUE), by = Dest]
dt[, list(
  minAvgTime = min(AirTime, na.rm = TRUE),
  avgAirtime = mean(AirTime, na.rm = TRUE),
  maxAirtime = max(AirTime, na.rm = TRUE)),
  by = Dest]

# Number of cancelled flights to LAX
str(dt)
dt[Cancelled == 1 & Dest == 'LAX', sum(Cancelled, na.rm = TRUE)]
#or simpler
dt[Dest == 'LAX', sum(Cancelled)]
# or
dt[Dest == 'LAX' & Cancelled == 1, length(Year)] # Looking at the length of any vector will count the rows
# or
dt[Dest == 'LAX' & Cancelled == 1, .N] # .N means count

# Number of cancelled flights by destination
dt[Cancelled == 1, .N, by = Dest]

# Sum of miles travelled to LAX
dt[Dest == 'LAX', sum(Distance)]

# Calculate km to a new column
dt$DistanceKm <- dt$Distance * 1.6
# or
dt[, DistanceKm := Distance * 1.6] # Uses less memory

# Percentage of flights cancelled per destination
dt[, sum(Cancelled) * 100 / .N, by = Dest]

# Plot the departure and arrival delays
ggplot(dt, aes(x = ArrDelay, y = DepDelay)) + geom_point()
library(hexbin)
ggplot(dt, aes(x = ArrDelay, y = DepDelay)) + geom_hex()

# Plot the average departure and arrival delay by destination
dta <- dt[, list(
  arrival = mean(ArrDelay, na.rm = TRUE),
  departure = mean(DepDelay, na.rm = TRUE)),
  by = Dest]
ggplot(dta, aes(x = departure, y = arrival)) + geom_point()

# Visualize the average flight time per destination
dta <- dt[, list(airtime = mean(AirTime, na.rm = TRUE)), by = Dest]
ggplot(dta, aes(x = Dest, y = airtime)) + geom_bar(stat = 'identity')

