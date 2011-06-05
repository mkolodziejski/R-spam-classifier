data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/processed"
samples = 1000
max.words = 750
cv.sets = 5

library(e1071)

source("load-mail.R")
source("classify-knn.R")
source("classify-knn-lsa.R")

bayes.classes = function(res) res;
knn.classes = function(res) res$Classes;
knn.tresholded.classes = function(tresh = 0.51) {
	function(res) {
		classes = res$Classes
		majority = res$Majority
		classes[classes == "spam" & majority < tresh] = "ham"
		classes[classes == "ham"  & (1-majority) >= tresh] = "spam"
		return(classes)
	}
}

batch = list(
		
			#list(name = "naive.bayes", classify = "naiveBayes", get.classes = bayes.classes, args = list() ),
			
			#list(name = "knn.k=3", 		classify = "classify.knn", get.classes = knn.classes, args = list(k=3) ),
			#list(name = "knn.k=7", 		classify = "classify.knn", get.classes = knn.classes, args = list(k=7) ),
			
			list(name = "knn.euclidean.k=3", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=3, dist.function = euclidean.dist) ),
			list(name = "knn.euclidean.k=5", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=5, dist.function = euclidean.dist) ),
			list(name = "knn.euclidean.k=7", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=7, dist.function = euclidean.dist) ),
			list(name = "knn.euclidean.k=11", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=11, dist.function = euclidean.dist) ),
			
			list(name = "knn.cosine.k=3", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=3, dist.function = cosine.dist) ),
			list(name = "knn.cosine.k=5", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=5, dist.function = cosine.dist) ),
			list(name = "knn.cosine.k=7", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=7, dist.function = cosine.dist) ),
			list(name = "knn.cosine.k=11", 	classify = "classify.knn", get.classes = knn.classes, args = list(k=11, dist.function = cosine.dist) )
			
		
			#list(name = "knn.euclidean.lsa.k=3.lsa.share=0.1", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.1), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=7.lsa.share=0.1", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.1), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=3.lsa.share=0.3", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.3), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=7.lsa.share=0.3", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.3), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=3.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.5), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=7.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.5), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=3.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.7), dist.function = euclidean.dist) ),
			#list(name = "knn.euclidean.lsa.k=7.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.7), dist.function = euclidean.dist) ),
			
			#list(name = "knn.cosine.lsa.k=3.lsa.share=0.1", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.1), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=7.lsa.share=0.1", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.1), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=3.lsa.share=0.3", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=7.lsa.share=0.3", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=3.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.5), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=7.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.5), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=3.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.7), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=7.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.7), dist.function = cosine.dist) )
		
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=4/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(4/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=5/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(5/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=6/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(6/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=7/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(7/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=8/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(8/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=9/11", classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(9/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=10/11",classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(10/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) ),
			#list(name = "knn.cosine.lsa.k=11.lsa.share=0.3.majority=11/11",classify = "classify.knn.lsa", get.classes = knn.tresholded.classes(11/11), args = list(k=11, lsa.dim = dimcalc_share(share=0.3), dist.function = cosine.dist) )
			
		)

cat(paste("\n", date(), "Started batch test\n\n")); 
data = load.mail(data.path, samples, shuffle.samples = TRUE, min.glob.word.freq = 0.01, max.words = max.words)
cat(paste(date(), "Data loaded, summary:\n\n"));
print(summary(data))

data = normalize.mail.data(data)

sample.count = dim(data)[1]
cv.set.count = sample.count/cv.sets

begin = seq(1,sample.count, by = cv.set.count)
end = begin + cv.set.count - 1
data.sets = cbind(round(begin), round(end)) 


errors = matrix(0,length(batch),cv.sets)
dimnames(errors)[[1]] = lapply(batch, function(x) x$name)

true.negatives = errors
true.positives = errors
false.negatives = errors
false.positives = errors
model.time = errors
pred.time = errors

cpu.time = function() sum(proc.time()[c(1,2)])

for(si in 1:cv.sets) {
	
	cat(paste("\n",date(), "Started set", si, "\n\n"));
	
	test.indices = data.sets[si,1]:data.sets[si,2]
	test = data[test.indices,]
	train = data[-test.indices,]
	
	for(bi in 1:length(batch)) {
		params = batch[[bi]]
		
		current.time = cpu.time()
		if(params$classify == "naiveBayes") {
			#bin.train = binary.mail.data(train)
			#bin.test = binary.mail.data(test)
			#model = naiveBayes( bin.train[,-1], bin.train[,1] )
			#pred = predict(model, bin.test[,-1])
			model = naiveBayes( train[,-1], train[,1] )
		}
		else {
			model = do.call(params$classify, append( list(train[,-1], train[,1]), params$args ) )
		}
		model.time[bi,si] = cpu.time() - current.time
		
		current.time = cpu.time()
		pred = predict(model, test[,-1])
		pred.time[bi,si] = cpu.time() - current.time
				
		pred.classes = factor(params$get.classes(pred), levels = c("spam","ham"))
		true.positives[bi,si]  = sum(pred.classes==test[,1] & pred.classes == 'spam')
		true.negatives[bi,si]  = sum(pred.classes==test[,1] & pred.classes == 'ham')
		false.positives[bi,si] = sum(pred.classes!=test[,1] & pred.classes == 'spam')
		false.negatives[bi,si] = sum(pred.classes!=test[,1] & pred.classes == 'ham')
		
		progress = ((si-1)*length(batch)+bi-1)/cv.sets/length(batch)
		cat(paste(date(), " Finished ", params$name," for set ", si," (",round(100*progress),"%)\n", sep = ""));
		
	}
	
}

cat(paste("\n", date(), "Finished batch test\n\n"));

errors = false.negatives + false.positives
tp.rate = rowSums(true.positives)  / rowSums(true.positives+false.negatives)
fp.rate = rowSums(false.positives) / rowSums(false.positives+true.negatives)

performance = 1-rowSums(errors)/sample.count
total.model.time = rowSums(model.time)
total.pred.time = rowSums(pred.time)
total.time = total.model.time + total.pred.time

cat("\nErrors per set:\n")
print(errors)

cat("\nPerformance:\n")
print(cbind(performance))
cat("\nTP Rate:\n")
print(cbind(tp.rate))
cat("\nFP Rate:\n")
print(cbind(fp.rate))
cat("\nCPU Time:\n")
print(cbind(total.time))




