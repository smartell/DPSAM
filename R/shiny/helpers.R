# 
# Load Libraries
# 
if(!require("shiny"))         install.packages("shiny")
if(!require("shinythemes"))   install.packages("shinythemes")
if(!require("ggplot2"))       install.packages("ggplot2")
if(!require("reshape2"))      install.packages("reshape2")
if(!require("Rcpp"))          install.packages("Rcpp")
if(!require("markdown"))      install.packages("markdown")
if(!require("plyr"))          install.packages("plyr")
if(!require("dplyr"))         install.packages("dplyr")
if(!require("grid"))          install.packages("grid")
if(!require("shinydashboard"))install.packages("shinydashboard")
if(!require("dygraphs"))      install.packages("dygraphs")
sourceCpp("data/src/dpsam.cpp");


.LIB        <- "data/lib/"
.RFILES     <- list.files(.LIB,pattern="\\.[Rr]$")
for(nm in .RFILES) source(file.path(.LIB, nm), echo=FALSE)

# Link to data file
dfile <- "data/NamibianHake.dat"
mData <- read.table(dfile,header=TRUE)


# Data frame for data objects.
dataName  <- c("age","linf","winf","k","b","ah","gh")
dataLabel <- c("Age of plus group","Asymptotic length","Asymptotic weight (Winf)","Growth coefficient (k)","Weight allometry exponent (b)","50% age-at-maturity","95% age-at-maturity")
dataValue <- c(15,100,5.0,0.2,3.0,4.7,5.5)
dataMin   <- c(0,0,0,0,2.0,0,0)
dataMax   <- c(50,250,2500,2.5,4.0,20,15)
dataStep  <- c(1,1,1,0.01,0.01,0.5,0.1)

dataDF    <- cbind(dataName,dataLabel,dataValue,dataMin,dataMax,dataStep)


# Data frame for parameter objects.

parmName  <- c("sel50","sel95")
parmLabel <- c("Age @ 50% selectivity","Age @ 95% selectivity")
parmValue <- c(3.0,5.0)
parmMin   <- c(0.0,0.0)
parmMax   <- c(15.,15.)
parmStep  <- c(0.5,0.5)

parmDF    <- cbind(parmName,parmLabel,parmValue,parmMin,parmMax,parmStep)