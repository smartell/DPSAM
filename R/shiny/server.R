

shinyServer(function(input, output, session) 
{

	output$mDataTable <- renderTable({
		mData
	})


	output$plotSRA <- renderPlot({
		plotSSBprior(input)
    })


	runSRA <- eventReactive(input$actionRunSRA,{
		# call code for running the SRA
		plotSSBposterior(input)
	})
  
  	output$plotPosterior <- renderPlot({
  		runSRA()
  	})

})