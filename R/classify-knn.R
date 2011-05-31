# 
# Author: Marek Skrajnowski
###############################################################################

# TODO: Move library loading to package depandancies
library(knnflex)

# TODO: Move source file dependancies to the package index
source("load-mail.R")

classify.knn = function(x, ...) UseMethod("classify.knn", ...)
	
classify.knn.default = function(trainingSet, classes, k) {
	
}

classify.knn.formula = function(formula, data, ...) {
	# parse the formula
	mf = model.frame(formula=formula, data=data)
	trainingSet = model.matrix(attr(mf, "terms"), data=mf)
	classes = model.response(mf)
	
	# classify
	result <- classify.knn.default(trainingSet, classes, ...)
	result$call <- match.call()
	result$formula <- formula
	return(result)
	
}