#' DATA POOR STOCK ASSESSMENT MODEL (DPSAM)
#' @name dpsam package
#' @docType package
#' @author STEVEN MARTELL
if(!require(foreach)) install.packages("foreach")
if(!require(doParallel)) install.packages("doParallel")
library(tools)
library(plyr)
library(roxygen2)
library(Rcpp)
# library(RcppParallel)
# library(foreach)
# library(doParallel)
# install.packages("RcppParallel")

# sourceCpp("src/sra.cpp")

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
stock$sel50 <- 3.0
stock$sel95 <- 5.0
# population parameters
stock$m  	<- 0.20
stock$fmsy 	<- 0.12
stock$msy 	<- 250.

A<-stock
sourceCpp("src/dpsam.cpp")
mod<-new(stock,A)

# MODEL_DATA CLASS
dfile <- "NamibianHake.dat"
data <- read.table(dfile,header=TRUE)
save(data,file=paste0(file_path_sans_ext(dfile),".Rd"))
stock$chat <- data$catch
stock$year <- data$year
stock$cpue <- data$cpue
stock$data <- data

# class(stock) = "stock"

# mod  <- new(Sra,stock)

n <- 500
prior_msy  <- rnorm(n,mean=stock$msy,sd =0.2*stock$msy )
prior_fmsy <- runif(n,0.01,0.5)
prior_m    <- rnorm(n,mean=stock$m,sd=0.05*stock$m)
prior      <- data.frame(msy=prior_msy,
                         fmsy=prior_fmsy,
                         m=prior_m,
                         no=1:n)



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

ageStructuredModel <- function(stock)
{
	
	with(stock,{
		# Extract data elements.
		# year <- data$year
		# chat <- data$catch
		# cpue <- data$cpue

		so   <- reck/phie
		beta <- (reck-1.0)/(bo)
		N    <- matrix(nrow=length(year),ncol=length(age))
		N[1,]<- ro*lx
		ft   <- vector("numeric",length=length(year))
		apo  <- age[-min(age)]
		amo  <- age[-max(age)]
		nage <- max(age)
		for (i in 1:length(year)) 
		{
			ft[i]	   <- getFt(chat[i],m,va,wa,N[i,])
			st         <- exp(-m-ft[i]*va)
			ssb        <- sum(N[i,]*fa)
			if(i < length(year))
			{
				N[i+1,1]   <- so*ssb/(1+beta*ssb)
				N[i+1,apo] <- N[i,amo] * st[amo]
				N[i+1,nage]<- N[i+1,nage]+N[i,nage] * st[nage]
			}
		}
		
		bt  <- as.vector(N %*% wa)
		return(stock)
	})
	
}

getFt <- function(ct,m,va,wa,na)
{	#use newtons root finding method to get ft
	ft 	<- ct/(sum(na*exp(-m/2)*wa*va))	
	for(iter in 1:9)
	{
		T1	<- wa*na
		T2	<- exp(-m-ft*va)
		T3	<- (1-T2)
		T4	<- m+ft*va
		c1	<- sum(ft*va*T1*T3/T4)
		c2	<- sum(va*T1*T3/T4 - ft*va^2*T1*T3/T4^2 + ft*va^2*T1*T2/T4)
		ft	<- ft - (c1-ct)/c2	#newton step.
	}
	# cat("ft = ",ft,"\n")
	return (ft)
}

runModel <- function(pars,stock)
{
	
	stock$msy  <- pars[1]
	stock$fmsy <- pars[2]
	stock$m    <- pars[3]
	stock$no   <- pars[4]

	stock <- calcAgeSchedules(stock)
	stock <- calcSteepnessBo(stock)
	stock <- ageStructuredModel(stock)

	# Bernoulli likelihood for extant stock.
	ret   <- 1
	# return 0 if biomass went extinct.
	if( any(is.na(stock$bt)) || any(stock$bt<0))
	{
		ret = 0
	}
	if( any(is.na(stock$ft)) || any(stock$ft>10))
	{
		ret = 0
	}

	
	
	stock$ret<-ret
	return(as.list(stock))
}


# MAIN

ell <- apply(X=prior,MARGIN=1,FUN="runModel",stock=stock)
Bt  <- ldply(ell,function(x) c(x[["ret"]],x[["bt"]]),.parallel=FALSE)
Ft  <- ldply(ell,function(x) c(x[["ret"]],x[["ft"]]),.parallel=FALSE)
colnames(Bt) <- colnames(Ft) <- c("Ret",data$year)

Theta <- ldply(ell,function(x) c(x[["ret"]],x[["msy"]],x[["fmsy"]],x[["m"]]),.parallel=FALSE)




