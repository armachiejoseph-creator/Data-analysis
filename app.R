library(shiny)

## first code created

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