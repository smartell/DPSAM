# plogis595.R

plogis595 <- function(x,s50,s95)
{
	return (1.0/(1.0+(exp(-log(19)*((x-s50)/(s95-s50))))));
}