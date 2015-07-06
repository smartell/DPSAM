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

	# print(out)
	return(list(out))

}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
sraModel <- function(mdata)
{
	cat("sraModel running\n")
	on.exit(rm(mod))
	mdata$data <- mData

	mod <- new(sra,mdata)
	df  <- mod$runModel();
	df  <- data.frame(Year = df[["Year"]],Spawning.Biomass = df[["Spawning.Biomass"]])


	return(df)
}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
#' Generate posterior distribution 
#' @param mdata a list object with necessary inputs to run the SRA model.
#' @param prior.df A data frame containing the priors for model parameters.
#' @return a dataframe with posterior samples from the biomass distribution.
sraModelPosterior <- function(mdata)
{
	cat("sraModelPosterior running\n")
	on.exit(rm(mod))
	mdata$data <- mData
	
	# mean and sd for normal priors on parameters
	# P  <- reactive(do.call(getPriorSamples,mdata))
	prior.df <- getPriorSamples(mdata)
	
	print(head(prior.df))
	# run model once
	fn <- function(prior)
	{
		# print(prior)
		mdata$cmsy <- prior[["cmsy"]]
		mdata$fmsy <- prior[["fmsy"]]
		mdata$m    <- prior[["natm"]]
		
		mod <- new(sra,mdata);
		df  <- mod$runModel();
		
		return(df)
		
	}

	# create the list of data frames
	lodf <- apply(prior.df,1,FUN="fn")
	
	return(lodf)
}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
#' Generate a prior distribution based on the parameter ranges.
#' @param  mdata a list object with the necessary inputs to run the SRA model.
#' @author STEVEN MARTELL
sraModelPrior <- function(mdata)
{
	cat("sraModelPrior running\n")
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
		ml  <- mod$runModel();
		df  <- data.frame(Year=ml[["Year"]],Spawning.Biomass=ml[["Spawning.Biomass"]])
		df$i<- i
		cdf <- rbind(cdf,df)
	}

	return(cdf)
}




# -- ———————————————————————————————————————————————————————————————————————————————— -- #
plotSSBprior <- function(input)
{

	gD <- getData(input)

	M <- reactive(do.call(sraModel,gD))
	N <- reactive(do.call(sraModelPrior,gD))
	ndf <- N();
	mdf <- M();
	print(head(mdf))
	if(is.na(min(ndf[["Spawning.Biomass"]]))) clr <- "red" else clr = "green"
	p  <- ggplot(ndf,aes(y=Spawning.Biomass,x=Year))
	p  <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max",alpha=0.3,fill=clr)
	p  <- p + geom_line(aes(Year,y=Spawning.Biomass),data=mdf)
	p  <- p + ylim(c(0,max(ndf$Spawning.Biomass)))
	return(p + .THEME)
	
}

# -- ———————————————————————————————————————————————————————————————————————————————— -- #
plotSSBposterior <- function(input) 
{
	mdata <- getData(input)
	
	M    <- reactive(do.call(sraModelPosterior,mdata))
	lodf <- M();

	bdf  <- ldply(lodf,function(x) x[["Spawning.Biomass"]])
	matplot(t(bdf),type="l")
	

}


# -- ———————————————————————————————————————————————————————————————————————————————— -- #
get.sd <- function(lu,ci=0.95)
{
	mu = mean(lu)
	qt = c(0.5*(1-ci),1+0.5*(ci-1))
	sd = optimize(function(sd)sum((lu-qnorm(qt,mu,(sd)))^2),interval=c(0.01,mu))$min
}


getPriorSamples <- function(mdata)
{
	n   <- 100
	df  <- NULL
	obj <- c("f_range","c_range","m_range")
	for( i in obj )
	{
		lu  <- mdata[[i]]
		mu  <- mean(lu)
		sd  <- get.sd(lu)
		x   <- rnorm(n,mu,sd)
		df  <- cbind(df,x)
	}
	colnames(df) <- c("fmsy","cmsy","natm")
	return(as.data.frame(df))
}




