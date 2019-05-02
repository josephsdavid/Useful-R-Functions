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

# Realization1 = gen.arma.wge(n = 250)
#
# tplot(Realization1, 1, 250)

# Split ACF plot to check for independence of correlation (requirement 3)

#requires(nothing)

splitacf = function(vec){
	par(mfrow = c(1,3))
	acf(vec)
	acf(vec[1:(length(vec)/2)])
	acf(vec[(length(vec)/2):length(vec)])
}

# Realize = gen.arma.wge(500,0.95,0,plot = T, sn = 784)
# splitacf(Realize)
