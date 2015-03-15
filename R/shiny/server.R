library(shiny)

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

	getData <- function()
	{
		data <- lapply(dataDF[,1],function(p)
		{
			input[[p]]
		})
		names(data) <- dataDF[,1]
		data$age = 0:data[["age"]]
		data$a = data[["winf"]]/(data[["linf"]]^data[["b"]])
		return(list(data))
	}

	M <- reactive(do.call(sraModel,getData()))

	output$mData <- renderTable({
		mData
	})
	# scnA <- reactive(do.call(equilibrium_model_cpp, getParams("A")))

	output$plotSRA <- renderPlot({
		M()
		input$actionRunSRA
		hist(rnorm(1000),prob=TRUE)
    })

})