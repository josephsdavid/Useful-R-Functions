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
	par(mfrow = c(1,1))
}

# Realize = gen.arma.wge(500,0.95,0,plot = T, sn = 784)
# splitacf(Realize)

# vxbar: variance of a stationary time series where autocorrelation approaches zero (from section 1.9) 
# requires:
# NUMERIC variables
library(purrr)
library(dplyr)

# Code
vxbar  <- function(vec){
	vec %>%
		discard(is.na) -> vec
	n  <-  length(vec)
	nlag  <-  n-1
	m  <- mean(vec)
	v  <-  var(vec)
	g0  <- v*nlag/n
	aut  <-  acf(vec,lag.max = nlag)
	sum  <- 0
	for (k in 1:nlag){
		sum  <- sum + (1-k/n)*aut$acf[k+1]*g0
	}
	 return(2*sum/n + g0/n)
}

# example
# brate = read.csv("../../Data/10_year_bond_rate_2010-2015.csv")
# x <- brate$Adj.Close
# vxbar(x)

# vxbarci 
# returns a lovely confidence interval on vxbar
# requires vxbar and all its dependencies

vxbarci  <- function(vec){
	rhs  <- 1.96*sqrt(vxbar(vec))
	lhs  <- mean(x)
	return(c(lhs-rhs, lhs+rhs))
}

# Example
# vxbarci(x)
