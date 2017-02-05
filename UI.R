# Boilerplate
library(shiny)
shinyUI(
  fluidPage(
    titlePanel('The great mtcars analysis engine'),
    sidebarLayout(
      sidebarPanel(
        selectInput('var1', 'X variable',
                    names(mtcars)),
        selectInput('var2', 'Y variable',
                    names(mtcars))
      ),
      mainPanel(
        plotOutput('ggplot')
      )
    )
  )
)