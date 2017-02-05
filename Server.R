# Building our backend
library(shiny)
library(ggplot2)

# Function
shinyServer(
  function(input, output) {
    output$ggplot <- renderPlot({
      ggplot(mtcars, aes(x = input$var1,
                         y = input$var2)) +
        geom_point()
    })
  }
  
)