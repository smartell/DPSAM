# svrSRA.R
#' R-scripts for running Stock Reduction Analysis
#' @author Steven Martell

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
#' Read input data from guiSRA
#' @author Steven Martell
getData <- function(input)
{
	# Get input data
	data <- lapply(dataDF[,1],function(p)
	{
		input[[p]]
	})
	names(data) <- dataDF[,1]
	data$age = 0:data[["age"]]
	data$a = data[["winf"]]/(data[["linf"]]^data[["b"]])
	
	# Get input parameters
	parm <- lapply(parmDF[,1],function(p)
	{
		input[[p]]
	})
	names(parm) <- parmDF[,1]
	
	# Get prior densities for MSY, Fmsy, M


	out <- c(data,parm)
	out$m    <- mean(input$sldr_natm)
	out$fmsy <- mean(input$sldr_fmsy)
	out$cmsy <- mean(input$sldr_cmsy)
	out$no   <- 1 # sample number

	out$f_range <- input$sldr_fmsy
	out$c_range <- input$sldr_cmsy
	out$m_range <- input$sldr_natm

	print(out)
	return(list(out))

}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
sraModel <- function(mdata)
{
	on.exit(rm(mod))
	mdata$data <- mData
	

	mod <- new(sra,mdata)
	df  <- mod$runModel();

	cat("Model running\n")


	return(df)
}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
#' Generate a prior distribution based on the parameter ranges.
#' @param  mdata a list object with the necessary inputs to run the SRA model.
#' @author STEVEN MARTELL
sraModelPrior <- function(mdata)
{
	on.exit(rm(mod))
	mdata$data <- mData  # Temporary.  year Catch and CPUE data

	# Create a grid of parameter range combindations
	xgrid <- with(mdata,expand.grid(f_range,c_range,m_range))
	

	# Run all possible combinations.
	cdf <- NULL
	for (i in 1:dim(xgrid)[1])	
	{
		mdata$fmsy <- xgrid[i,1]
		mdata$cmsy <- xgrid[i,2]
		mdata$m    <- xgrid[i,3]
		mod <- new(sra,mdata)
		df  <- mod$runModel();
		df$i<- i
		cdf <- rbind(cdf,df)
	}

	return(cdf)
}




# -- ———————————————————————————————————————————————————————————————————————————————— -- #
plotSSBprior <- function(input)
{

	N <- reactive(do.call(sraModelPrior,getData(input)))
	df <- N();
	if(is.na(min(df[["Spawning.Biomass"]]))) clr <- "red" else clr = "green"
	p  <- ggplot(df,aes(y=Spawning.Biomass,x=Year))
	p  <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max",alpha=0.3,fill=clr)
	p  <- p + ylim(c(0,max(df$Spawning.Biomass)))
	print(p + .THEME)
}
