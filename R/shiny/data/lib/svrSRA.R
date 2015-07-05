# svrSRA.R


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
