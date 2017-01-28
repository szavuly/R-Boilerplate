# Create a pie chart
# c() is create a vector (numerical)

pie(c(80, 20), col = c('deepskyblue3', 'lightsalmon2'),
    labels = c(
      'Cleaning data',
      "Complaning about cleaning data"),
    init.angle = 310,
    main = "Data Science")

