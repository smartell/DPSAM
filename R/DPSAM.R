#' DATA POOR STOCK ASSESSMENT MODEL (DPSAM)
#' @name dpsam package
#' @docType package
#' @author STEVEN MARTELL


# if(!require(foreach)) install.packages("foreach")
# if(!require(doParallel)) install.packages("doParallel")
library(tools)
library(plyr)
library(roxygen2)
library(Rcpp)
library(microbenchmark)
# library(RcppParallel)
# library(foreach)
# library(doParallel)
# install.packages("RcppParallel")

# sourceCpp("src/sra.cpp")

# STOCK CLASS
stockId 		<- new.env()
stockId$age 	<- 1:15
# growth parameters
stockId$linf	<- 111
stockId$k 		<- 0.23
stockId$winf  	<- 5.0
stockId$b     	<- 3.0
stockId$a 		<- stockId$winf/(stockId$linf^stockId$b)
# maturity parameters
stockId$ah 		<- log(3)/stockId$k
stockId$gh		<- 0.5
# selectivity parameters
stockId$sel50 	<- 3.0
stockId$sel95 	<- 5.0
# population parameters
stockId$m  		<- 0.20
stockId$fmsy 	<- 0.15
stockId$msy 	<- 250.


# MODEL_DATA CLASS
dfile <- "NamibianHake.dat"
data  <- read.table(dfile,header=TRUE)
save(data,file=paste0(file_path_sans_ext(dfile),".Rd"))
stockId$chat <- data$catch
stockId$year <- data$year
stockId$cpue <- data$cpue
stockId$data <- data

A<-stockId
sourceCpp("shiny/data/src/dpsam.cpp");
# mod <- new(stock,A);
# mod<-new(sra,A);
# mod$initializeModel();
# mod$ageStructuredModel()

runCppModel <- function(prior,L)
{
	on.exit(rm(mod))

	L$cmsy <- prior[1]
	L$fmsy <- prior[2]
	L$m    <- prior[3]
	L$no   <- prior[4]


	mod <- new(sra,L)
	mod$initializeModel();
	mod$ageStructuredModel();
	bt  <- mod$getBt();
	print(bt)
	return(bt)
}

# 
# Sample from prior distribution.
# 
n <- 500
# dfPriorInfo <- data.frame(id   =1:3,
#                           plbl = c("msy","fmsy","m"),
#                           dist = c("rnorm","runif","rnorm"),
#                           par1 = c(stockId$msy,0.01,stockId$m),
#                           par2 = c(0.2*stockId$msy,0.50,0.05*stockId$m),
#                           stringsAsFactors=FALSE)

# X <- dfPriorInfo
# # dfn <- do.call(paste(X$dist),list(n,X$par1,X$par2))
# dfn <- (function(X) data.frame(draws=(do.call(paste(X$dist),list(n,X$par1,X$par2)))))
# # dfPriors <- ddply(dfPriorInfo,"id",.fun = (function(X) data.frame(draws=(do.call(paste(X$dist),list(n,X$par1,X$par2))))))
# dfPriors <- ddply(dfPriorInfo,"id",.fun = dfn )

prior_msy  <- rnorm(n,mean=stockId$msy,sd =0.2*stockId$msy )
prior_fmsy <- runif(n,0.01,0.5)
prior_m    <- rnorm(n,mean=stockId$m,sd=0.05*stockId$m)
prior      <- data.frame(msy=prior_msy,
                         fmsy=prior_fmsy,
                         m=prior_m,
                         no=1:n)
fn <- function()
{
	ell <- apply(X=prior,MARGIN=1,FUN="runCppModel",L=A)
	return(ell)
}





