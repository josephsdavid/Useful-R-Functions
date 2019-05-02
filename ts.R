# Base R acf and plot
# requires: nothing
# code:
tplot  <- function(vec, begin, end){
	Time  <- seq(begin, end, length = length(vec))
	plot(Time,vec, main = "Regression of Y on Time",  cex.axis = 1)
	acf(vec)
	acf(vec, plot = F)
}

# usage:

# 1Realization1 = gen.arma.wge(n = 250)
#
# tplot(Realization1, 1, 250)
