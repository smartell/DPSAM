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
stockId$ah 		<- 8.0
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
data <- read.table(dfile,header=TRUE)
save(data,file=paste0(file_path_sans_ext(dfile),".Rd"))
stockId$chat <- data$catch
stockId$year <- data$year
stockId$cpue <- data$cpue
stockId$data <- data

A<-stockId
sourceCpp("src/dpsam.cpp");
# mod <- new(stock,A);
# mod<-new(sra,A);
# mod$initializeModel();
# mod$ageStructuredModel()

runCModel <- function(prior,L)
{
	L$msy  <- prior[1]
	L$fmsy <- prior[2]
	L$m    <- prior[3]
	L$no   <- prior[4]


	mod <- new(sra,L)
	mod$initializeModel();
	mod$ageStructuredModel();

	return(1)
}

# class(stockId) = "stockId"

# mod  <- new(Sra,stockId)
n <- 500
prior_msy  <- rnorm(n,mean=stockId$msy,sd =0.2*stockId$msy )
prior_fmsy <- runif(n,0.01,0.5)
prior_m    <- rnorm(n,mean=stockId$m,sd=0.05*stockId$m)
prior      <- data.frame(msy=prior_msy,
                         fmsy=prior_fmsy,
                         m=prior_m,
                         no=1:n)
fn <- function()
{
ell <- apply(X=prior,MARGIN=1,FUN="runCModel",L=A)
	
}



