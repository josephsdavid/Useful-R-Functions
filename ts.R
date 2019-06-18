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

# difference1
# takes the first difference of an arima model

# requires:
library(tidyverse)
# library magrittr
library(tswge)

diff1 <- . %>% artrans.wge(phi.tr = 1) 

# Superdiff

library(tswge)
library(tidyverse)

differ <- function(x, n){
	f  <- . %>% artrans.wge(phi.tr = c(rep(0,n-1),1))
	f(x)
}


## Tswge generator wrapper
## generates tswge data with a convenient wrapper


library(tswge)

tswgen <- function(n,sn=0){
	sig <- function(...){
		gen.sigplusnoise.wge(n=n,...,sn=sn)
	}
	ari <- function(...,dif){
		gen.arima.wge(n=n,...,sn=sn)
	}
	aru <- function(...){
		gen.aruma.wge(n=n,..., sn=sn)
	}
	list("sig"=sig,"ari"=ari,"aru"=aru)
}

# ts200 <- tswgen(200)
# ts200$sig(phi = c (0.2,0.4,-0.2))
# ts200$ari(d = 4)
# ts200$aru(s=1)
# 
# ts200_37 <- tswgen(200,sn=2)
# ts200_37$sig(phi = c (0.2,0.4,-0.2))
# ts200_37$ari(d = 4)
# ts200_37$aru(s=1)


# AR1 Forecast generator:
# requires: nothing
# makes a forecasting function given mu and sigma

ar1forcastgen <- function(phi,mu){
	fun <- function(xprev,l=1){
		if (l==1)	return(phi*xprev + mu*(1-phi))
		else		return(phi*fun(xprev,l-1) + mu*(1-phi))
	}
	fun
}


# usage
# 
# ex74 <- ar1forcastgen(phi=.8, mu = 24.17)
# times <- c(1,2,3,4,5)
# lapply(times, ex74, xprev = 22.93) %>% as.data.frame
# ex74(22.93,n=2)
#
#
#


# AR(p) forecasting function
# requires: nothing
# returns a vector of forcasted (AR(p)) values

arpgen <- function(phi,vec,l,mu){
	v <- vec[length(vec):(length(vec)-(length(phi)-1))]
	f <- sum(phi*(v)) + mu*(1-sum((phi)))
		if (l==1)	return(f)
	 		return(arpgen(phi,append(vec,f),l-1,mu))

}

arp <- function(phi,vec,l,mu){
	as.numeric(lapply(1:l, arpgen, phi = phi, vec = vec, mu = mu))
}
## usage
# x <- c(27.7,23.4,21.2,21.1,22.7)
# p <- c(1.6,-.8)
# avg <- 29.4
# arp(p,x,5,avg)
