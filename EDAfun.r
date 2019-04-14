# Useful R functions for EDA
# To gain access to these,
# source(/path/to/EDAfun.r) in your file

# Plot all numeric histograms
# This function takes in a data frame, and makes a histogram of all numeric variables
# Requirements: 
library(purrr)
library(tidyr)
library(ggplot2)

# Code
plotAllNumeric <- function(df){
    df%>%keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram() 
}

### Usage
# plotAllNumeric(mtcars)


### Data frame cleaner (use for large data frames)

# requirements:
library(dplyr)

# Code
colDestroyer  <-  function(df, disco){
	df %>%
	dplyr::select(-disco) 
}

### usage
# toss  <- c("colname","colname")
# colDestroyer(df, toss)

# Single responsee variable EDA
	# Two fun's
	# 1. First one creates response + explanatory split as a list
	# 2. second one plots them

# requires:
library(purrr)
library(ggplot2)
library(dplyr)
# or library(plyr)
# Requires your response variable to be the last variable in the df
# To change that, switch the commenting, then it will work with the first variable
# you can also use this as an example to deal with multiple response variables, but you cannot use
# singleResponse then

singleResponse  <-  function(df){
	response  <- names(df)[ncol(df):ncol(df)] 
	# response  <- names(df)[1:1]
	expl  <-  names(df)[1:(ncol(df)-1)]
	# expl  <- names(df)[2:ncol(df)]
	varList <- list("response" = response, "expl" = expl)
	return(varList)



scatterfun  <- function(df,x,y){
	ggplot(df, aes_string( x = x, y = y )) +
		geom_point() +
		stat_smooth(method="loess",  size=.5)+
		theme_bw()
}

# usage:
#	singleResponse(df) -> a
#	allplots  <-  map(a$response, ~map(a$expl, scatterfun, df , y = .x))
#	response_plots  <-  map(allplots, ~cowplot::plot_grid(plotlist = .x))
#	response_plots

# advanced example if you want less plots:
# useless  <- c("colname","colname","colname")
# df %>%
#	keep(is.numeric) %>%
#	colDestroyer(useless) %>%
#	singleResponse -> a
# allplots  <-  map(a$response, ~map(a$expl, scatterfun, df , y = .x))
# response_plots  <-  map(allplots, ~cowplot::plot_grid(plotlist = .x))
# response_plots

# execution function
# basically a souped up version of the advanced example seen above


exec  <-  function(df, x, funk){
	df %>%
		colDestroyer(x) %>%
		funk
}

# it takes a dataframe, removes the variables you dont want, and then runs it through funk(df)

# example seen in the following functions:

# pair plotter (scatterplot matrix)
# note that we are using the response variable as the last variable in the dataframe, you can alter this


pairfun <- function(df){
	df %>% 
		keep(is.numeric) %>%
		pairs(col=df[,ncol(df)])
		# pairs(col=df[,1])
}

# usage
# exec(df, useless, pairfun)

# heatmap
# you can tweak the heat map if you like!
# requirements
library(RColorBrewer)
library(gplots)


heatmapper <- function(df){
	df %>%
		keep(is.numeric) %>%
		tidyr::drop_na() %>%
		cor %>%
		heatmap.2(col = my_palette ,
		density.info = "none", trace = "none",
		dendogram = c("both"), symm = F,
		symkey = T, symbreaks = T, scale = "none",
		key = T)
}

# usage
# exec(df, useless, heatmapper)

# correlation
# requires:
library(corrplot) 


correlator  <-  function(df){
	df %>%
		keep(is.numeric) %>%
		tidyr::drop_na() %>%
		cor %>%
		corrplot("upper", addCoef.col = "white", number.digits = 2,
			 number.cex = 0.5, method="square",
			 order="hclust", title="Variable Corr Heatmap",
			 tl.srt=45, tl.cex = 0.8)
}

# usage
# exec(df, useless, correlator)

# covariance


covariator <- function(df){
	df %>%
		keep(is.numeric) %>%
		tidyr::drop_na() %>%
		cov %>% round(2)
}


# usage
# exec(f1bin.post, useless, covariator) %>% pander
# pander is unneccessary

# total variance


variator  <- function(df) {
	df %>%
		keep(is.numeric) %>%
		tidyr::drop_na() %>%
		cov %>% diag %>% sum
} 

# usage
# exec(df, useless, variator) 




