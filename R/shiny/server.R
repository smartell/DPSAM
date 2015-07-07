

shinyServer(function(input, output, session) 
{

	output$mDataTable <- renderTable({
		mData
	})


	# 
	# STOCK REDUCTION ANALYSIS
	# 
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


  	observeEvent(input$actionRunSRA,{
  		updateTabsetPanel(session,inputId="tabPlots",selected="Posteriors")
  	})
  	observeEvent(input$sldr_fmsy,{
  		updateTabsetPanel(session,inputId="tabPlots",selected="Priors")
  	})
  	observeEvent(input$sldr_cmsy,{
  		updateTabsetPanel(session,inputId="tabPlots",selected="Priors")
  	})
  	observeEvent(input$sldr_natm,{
  		updateTabsetPanel(session,inputId="tabPlots",selected="Priors")
  	})


})