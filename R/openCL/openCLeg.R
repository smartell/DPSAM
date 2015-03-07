# openCLeg.R
library(rbenchmark)
library(ggplot2)
library(reshape2)
library(OpenCL)
library(microbenchmark)
p = oclPlatforms()		# platform "apple"
d = oclDevices(p[[1]])  # devices HD Graphics 4000 & GeForce GT 650M

code = c("
__kernel void dnorm(
  __global float* output,const unsigned int count,
  __global float* input,
  const float mu, const float sigma)
  {
    int i = get_global_id(0);
    if(i < count)
        output[i] = exp(-0.5f * ((input[i] - mu) / sigma) * ((input[i] - mu) / sigma))
        / (sigma * sqrt( 2 * 3.14159265358979323846264338327950288 ) );
  };"
 )

k1.dnorm <- oclSimpleKernel(d[[1]], "dnorm", code, precision="best")
k2.dnorm <- oclSimpleKernel(d[[2]], "dnorm", code, precision="best")

f1 <- function(x, mu=0, sigma=1, ...)
{
  oclRun(k1.dnorm, length(x), x, mu, sigma, ...)
}
f2 <- function(x, mu=0, sigma=1, ...)
{
  oclRun(k2.dnorm, length(x), x, mu, sigma, ...)
}

fun <-function(n=10)
{
	x <- seq(0.5,5,length=n)
	# bm <- within(benchmark("CPU"=dnorm(x),
	#              "Iris Pro"=f1(x),
	#              "GeForce GT 750M"=f2(x),
	#              replications=10),
	# {average=user.self/replications})
  bm <- microbenchmark("CPU"=dnorm(x),
               "Iris Pro"=f1(x),
               "GeForce GT 750M"=f2(x),
               times=100L)
	return(bm)
}

n = seq(1,10,by=1)*1e5
runtime =lapply(n,fun)
mdf = melt(runtime,id.var="expr",measure.vars=c("mean","lq","uq"))
df <- dcast(mdf,L1 + test  ~ variable )
limits <- aes(ymax=average+se,ymin=average-se)
p <- ggplot(df,aes(L1,mean,fill=test))
p <- p+geom_bar(stat="identity",position="dodge")
p <- p+labs(x="Sample size (millions)",y="Runtime (milliseconds)",fill="Processor")
print(p + theme_bw())


