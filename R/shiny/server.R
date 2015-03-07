library(shiny)

shinyServer(function(input, output, session) 
{

	output$plotSRA <- renderPlot({
	  input$actionRunSRA
      hist(rnorm(1000),prob=TRUE)
    })

})