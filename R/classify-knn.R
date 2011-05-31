# 
# Author: Marek Skrajnowski
###############################################################################

# TODO: Move library loading to package depandancies
library(knnflex)
library(lsa)

# TODO: Move source file dependancies to the package index
source("load-mail.R")

# distance functions
cosine.dist = function(x,y) {
	x.length = sqrt(rowSums(x^2))
	
	d = apply(y,1, function(y.row) {
				y.rep = matrix(rep(y.row,dim(x)[1]), dim(x), byrow = TRUE)
				y.length = sqrt(sum(y.row^2))
				y.length.rep = rep(y.length, dim(x)[1])
				return(rowSums(y.rep*x) / x.length / y.length.rep)
			})
	
	return(1-t(d))
}

euclidean.dist = function(x,y) {
	x.length = sqrt(rowSums(x^2))
	
	d = apply(y,1, function(y.row) {
				y.rep = matrix(rep(y.row,dim(x)[1]), dim(x), byrow = TRUE)
				return( sqrt(rowSums((y.rep-x)^2)) )
			})
	
	return(t(d))
}

classify.knn = function(x, ...) UseMethod("classify.knn", ...)
	
classify.knn.default = function(training.set, classes, 
												k=3, dist.function="cosine", minkowski.power=2, 
												agg.method = "majority", ties.method = "first", ...) 
{
	result = list( training.set = training.set, classes = classes )
				
	result$parameters = list(	k=k, dist.function = dist.function, minkowski.power = minkowski.power, 
										agg.method = agg.method, ties.method = ties.method )
	class(result) = "knn"
	attr(result,'call') <- match.call()
	return(result)
}

classify.knn.formula = function(formula, data, 
												k=3, dist.function=, minkowski.power=2, 
												agg.method = if (is.factor(y)) "majority" else "mean", ties.method = "min", ...) 
{
	# parse the formula
	mf = model.frame(formula=formula, data=data)
	trainingSet = model.matrix(attr(mf, "terms"), data=mf)
	classes = model.response(mf)
	
	# pass to the default function
	result <- classify.knn.default(trainingSet, classes, k, dist.function, minkowski.power, agg.method, ties.method, ...)
	attr(result,'call') <- match.call()
	attr(result,'formula') <- formula
	return(result)
	
}

predict.knn = function(object, newdata = NULL, ...) {
	params = object$parameters
	train = object$training.set
	test = newdata
	classes = object$classes
	
	# cosine distance not supported by knn.dist
	distance = params$distance.function(object$training.set, newdata)
	
	ranks = t(apply(distance, 1,    function(x) rank(x, ties.method = ties.meth)))
	new.classes = apply(ranks, 1, function(x) apply(data.frame(classes[x <= k]),	2, agg.meth))
		
	return(new.classes)
			
}


(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged


