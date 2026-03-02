library(shiny)

## first code created

## I will like to add some few things to it

## lsets do this
ui <- fluidPage(
  
  titlePanel("Simple Shiny Demo"),
  
  textInput(
    "name",
    "Enter your name:"
  ),
  
  h3(textOutput("greeting"))
  
)

server <- function(input, output) {
  
  output$greeting <- renderText({
    
    paste("Hello", input$name)
    
  })
  
}

shinyApp(ui, server)