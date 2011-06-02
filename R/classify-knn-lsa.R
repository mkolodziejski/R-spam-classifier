# 
# Author: Marek Skrajnowski
###############################################################################

library(lsa)

# TODO: Move source file dependancies to the package index
source("classify-knn.R")
source("load-mail.R")

classify.knn.lsa = function(x, ...) UseMethod("classify.knn.lsa")

classify.knn.lsa.default = function(training.set, classes, 
		k=3, lsa.dim = dimcalc_share(), dist.function=cosine.dist, 
		agg.method = "majority", ties.method = "first", ...) 
{
	lsa.space = lsa(t(training.set), lsa.dim)
	lsa.training.set = t(diag(lsa.space$sk) %*% t(lsa.space$dk))
	lsa.space$dk = NULL # free excess memory
	
	knn.obj = classify.knn(lsa.training.set, classes, k, dist.function, agg.method, ties.method, ...)
	
	result = list(knn = knn.obj, lsa.space = lsa.space, formula = NULL)
	
	class(result) = "knn.lsa"
	attr(result,'call') <- match.call()
	return(result)
}

classify.knn.lsa.formula = function(formula, data, 
		k=3, lsa.dim = dimcalc_share(), dist.function=cosine.dist, 
		agg.method = "majority", ties.method = "first", ...) 
{
	# parse the formula
	mf = model.frame(formula=formula, data=data)
	trainingSet = model.matrix(attr(mf, "terms"), data=mf)[,-1]
	classes = model.response(mf)
	
	# pass to the default function
	result <- classify.knn.lsa.default(trainingSet, classes, k, lsa.dim, dist.function, agg.method, ties.method, ...)
	result$formula = formula
	attr(result,'call') <- match.call()
	attr(result,'formula') <- formula
	return(result)
	
}

predict.knn.lsa = function(object, newdata = NULL, ...) {
	
	if(is.null(object$formula)) {
		test = newdata
	}
	else {
		# rbind makes sure the result is a matrix in case of a single test sample
		test = rbind(model.matrix(object$formula, newdata)[,-1])
	}
	
	tk = object$lsa.space$tk
	lsa.test = t(diag(1,dim(tk)[2]) %*% t(tk) %*% t(test))
	
	res = predict(object$knn, newdata = lsa.test, ...)
	class(res) = c("predict.knn.lsa", class(res))
			
	return(res)
	
}


test.predict.knn.lsa = function(test.size = 0.2, k=5) {
	data(iris)
	shuffle = sample(dim(iris)[1])
	s.iris = iris[shuffle,]
	split = (1-test.size)*length(shuffle)
	train = s.iris[1:(split-1),]
	test = s.iris[split:length(shuffle),]
	classes = test$Species
	test$Species = factor("ham", levels = c("spam", "ham"))
	
	knn.obj = classify.knn.lsa(Species ~ ., data = train, k=k, dist.function = cosine.dist)
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


