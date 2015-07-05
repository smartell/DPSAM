

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
	# N <- reactive(do.call(sraModelPrior,getData(input)))

	output$mDataTable <- renderTable({
		mData
	})
	# scnA <- reactive(do.call(equilibrium_model_cpp, getParams("A")))

	output$plotSRA <- renderPlot({
		plotSSBprior(input)

		# df <- N();
		# if(is.na(min(df[["Spawning.Biomass"]]))) clr <- "red" else clr = "green"
		# p  <- ggplot(df,aes(y=Spawning.Biomass,x=Year))
		# p  <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max",alpha=0.3,fill=clr)
		# p  <- p + ylim(c(0,max(df$Spawning.Biomass)))
		# print(p + .THEME)

		# df <- M();
		# if(is.null(.CDF))
		# 	.CDF <<- df
		# else
		# 	.CDF <<- cbind(.CDF,df["Spawning.Biomass"])

		# print(head(.CDF))
		# matplot(.CDF[,1],.CDF[,-1])
		# hist(rnorm(1000),prob=TRUE)
		# print(df)
		# plot(df)
		# p <- ggplot(df,aes(Year,Spawning.Biomass)) + geom_line()
		# p <- p + ylim(c(0,max(df$Spawning.Biomass)))
		# print(p + .THEME)

		# 
		# Press Button to run SRA
		# 
		# input$actionRunSRA
		# df <- isolate(M());
    })

})