#' DATA POOR STOCK ASSESSMENT MODEL (DPSAM)
#' @name dpsam package
#' @docType package
#' @author STEVEN MARTELL

library(roxygen2)

# STOCK CLASS
stock 		<- new.env()
stock$age 	<- 1:25
# growth parameters
stock$linf	<- 111
stock$k 	<- 0.23
stock$winf  <- 5.0
stock$b     <- 3.0
stock$a 	<- stock$winf/(stock$linf^stock$b)
# maturity parameters
stock$ah 	<- 8.0
stock$gh	<- 0.5
# selectivity parameters
stock$sel50 <- 4.0
stock$sel95 <- 7.0
# population parameters
stock$m  	<- 0.20
stock$fmsy 	<- 0.12
stock$msy 	<- 1.00

# class(stock) = "dpsam"




calcAgeSchedules <- function(stock)
{
	with(stock,{
	     la <- linf*(1.0-exp(-k*age))
	     wa <- a*stock$la^b
	     ma <- plogis(age,ah,gh)
	     fa <- wa*ma
	     va <- 1.0/(1.0+(exp(-log(19)*((age-sel50)/(sel95-sel50)))));
	     lx <- exp(-m*(age-min(age)))
	     lx[max(age)] <- lx[max(age)]/(1.0-exp(-m))
	     phie <- sum(lx*fa)
		return(stock)
	})
}

calcSteepnessBo <- function(stock)
{
	with(stock,{
		lz	<- vector("numeric",length=length(age))
		za  <- m + fmsy*va
		sa  <- exp(-za)
		oa  <- (1-sa)
		qa  <- va*oa/za
		t2  <- wa*va^2/za
		t3  <- exp(-za)-oa/za

		lz[1]    <- 1.0
		dlz.df	 <- 0.0
		dphie.df <- 0.0
		dphiq.df <- t2[1]*t3[1]
		for(i in age)
		{
			if(i > min(age))
			{
				lz[i]  <- lz[i-1] * sa[i-1]
				dlz.df <- dlz.df  * sa[i-1] - lz[i-1]*va[i-1]*sa[i-1]
				if(i==max(age))
				{
					lz[i]  <- lz[i]/oa[i]
					dlz.df <- dlz.df/sa[i] - lz[i-1]*sa[i-1]*va[i]*sa[i]/oa[i]^2 
				}
			}
			dphie.df <- dphie.df + fa[i]*dlz.df
			dphiq.df <- dphiq.df + wa[i]*qa[i]*dlz.df + lz[i]*t2[i]*t3[i]
		}
		phif  <- sum(lz*fa)
		phiq  <- sum(lz*qa*wa)
		reck  <- phie/phif - (fmsy*phiq*phie/phif^2*dphie.df) / (phiq+fmsy*dphiq.df)

		re 	   <- msy / (fmsy*phiq)
		ro     <- re*(reck-1.0)/(reck-phie/phif)
		bo     <- ro * phie
		spr    <- phif/phie
		dre.df <- ro/(reck-1.0)*phie/phif^2*dphie.df
		return(stock)
	})
}

# MAIN
stock <- calcAgeSchedules(stock)
stock <- calcSteepnessBo(stock)

