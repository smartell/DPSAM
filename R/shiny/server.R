library(shiny)

shinyServer(function(input, output, session) 
{

	output$plotSRA <- renderPlot({
      hist(rnorm(1000),density=TRUE)
    })

})