

shinyServer(function(input, output, session) 
{

	## 
	# getParams <- function(prefix) {
      
 #      # input[[paste0(prefix, "_recalc")]]

 #      params <- lapply(paramNames, function(p) {
 #        input[[paste0(prefix, "_", p)]]
 #      })
 #      names(params) <- paramNames
 #      params <- c(params,prefix=prefix)
 #      # print(params)
 #      params
 #    }

	

	M <- reactive(do.call(sraModel,getData(input)))

	output$mDataTable <- renderTable({
		mData
	})
	# scnA <- reactive(do.call(equilibrium_model_cpp, getParams("A")))

	output$plotSRA <- renderPlot({
		M()
		input$actionRunSRA
		hist(rnorm(1000),prob=TRUE)
    })

})