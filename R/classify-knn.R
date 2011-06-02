# 
# Author: Marek Skrajnowski
###############################################################################

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

classify.knn = function(x, ...) UseMethod("classify.knn")
	
classify.knn.default = function(training.set, classes, 
												k=3, dist.function=cosine.dist,  
												agg.method = "majority", ties.method = "first", ...) 
{
	result = list( training.set = training.set, classes = classes )
				
	result$parameters = list(	k=k, dist.function = dist.function,  
										agg.method = agg.method, ties.method = ties.method, formula = NULL )
	class(result) = "knn"
	attr(result,'call') <- match.call()
	return(result)
}

classify.knn.formula = function(formula, data, 
												k=3, dist.function=cosine.dist,  
												agg.method = "majority", ties.method = "first", ...) 
{
	# parse the formula
	mf = model.frame(formula=formula, data=data)
	trainingSet = model.matrix(attr(mf, "terms"), data=mf)[,-1]
	classes = model.response(mf)
	
	# pass to the default function
	result <- classify.knn.default(trainingSet, classes, k, dist.function, agg.method, ties.method, ...)
	result$parameters$formula = formula
	attr(result,'call') <- match.call()
	attr(result,'formula') <- formula
	return(result)
	
}

predict.knn = function(object, newdata = NULL, ...) {
	
	params = object$parameters
	train = object$training.set
	
	if(is.null(params$formula)) {
		test = newdata
	}
	else {
		# rbind makes sure the result is a matrix in case of a single test sample
		test = rbind(model.matrix(params$formula, newdata)[,-1])
	}
	
	test.samples = dim(test)[1]
	classes = object$classes
	
	distance = params$dist.function(object$training.set, test)
	
	ranks = apply(distance, 1,    function(x) rank(x, ties.method = params$ties.method))
	matches = apply(ranks, 2, function(x) { which(x<=params$k) })
	matching.classes = array(classes[matches], dim(matches))
	
	classes = aggregate(matching.classes, list(rep(TRUE,params$k)), params$agg.method)[,-1]
	
	i = cbind(rep(1:test.samples, each=params$k), c(matches))
	matches.dist = array(distance[i], dim(matches))
	
	res = data.frame(Classes = t(classes), Matches = t(matches), 
								Matching.Classes = t(matching.classes), Match.Distance = t(matches.dist))
						
	dimnames(res)[1] = dimnames(test)[1]
		
	class(res) = c("predict.knn", class(res))
	return(res)
			
}


test.predict.knn = function(test.size = 0.2, k=5) {
	data(iris)
	shuffle = sample(dim(iris)[1])
	s.iris = iris[shuffle,]
	split = (1-test.size)*length(shuffle)
	train = s.iris[1:(split-1),]
	test = s.iris[split:length(shuffle),]
	classes = test$Species
	test$Species = factor("ham", levels = c("spam", "ham"))
	
	knn.obj = classify.knn(Species ~ ., data = train, k=k, dist.function = cosine.dist)
	p = predict(knn.obj, newdata = test)
	
	test$Species = classes
	
	#print(p)
	cat("\n--------------------------------------\n\n")
	error.indices = which(test$Species != p$Classes)
	
	print(paste("Errors:", length(error.indices)))
	print(paste("Effectiveness:", round(1-length(error.indices)/dim(test)[1], digits = 2) ))
	
	if(length(error.indices) > 0) {
		print("Bad Samples:")
		report = cbind(test$Species[error.indices],p[error.indices,])
		dimnames(report)[2][[1]][1] = "Real Class"
		dimnames(report)[2][[1]][2] = "Predicted Class"
		print(t(report))
	}
	
}


