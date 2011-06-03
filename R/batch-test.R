data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/processed"
samples = 2000
max.words = 1000
cv.sets = 5

library(e1071)

source("load-mail.R")
source("classify-knn.R")
source("classify-knn-lsa.R")

bayes.classes = function(res) res;
knn.classes = function(res) res$Classes;

batch = list(
		
			#list(name = "naive.bayes", classify = "naiveBayes", get.classes = bayes.classes, args = list() ),
			
			#list(name = "knn.k=3", 		classify = "classify.knn", get.classes = knn.classes, args = list(k=3) ),
			#list(name = "knn.k=7", 		classify = "classify.knn", get.classes = knn.classes, args = list(k=7) ),
			
			list(name = "knn.lsa.k=3.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.5)) ),
			list(name = "knn.lsa.k=7.lsa.share=0.5", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.5)) ),
			list(name = "knn.lsa.k=3.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.7)) ),
			list(name = "knn.lsa.k=7.lsa.share=0.7", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.7)) ),
			list(name = "knn.lsa.k=3.lsa.share=0.9", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=3, lsa.dim = dimcalc_share(share=0.9)) ),
			list(name = "knn.lsa.k=7.lsa.share=0.9", 	classify = "classify.knn.lsa", get.classes = knn.classes, args = list(k=7, lsa.dim = dimcalc_share(share=0.9)) )
			
		)

cat(paste(date(), "Started batch test\n\n")); 
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

model.time = errors
pred.time = errors

cpu.time = function() sum(proc.time()[c(1,2)])

for(si in 1:cv.sets) {
	
	cat(paste(date(), "Started set", si, "\n\n"));
	
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
		errors[bi,si] = sum(pred.classes!=test[,1])
		
		cat(paste(date(), " Finished ", params$name," for set", si,", errors so far:\n", sep = ""));
		print(errors)
		
	}
	
}

cat(paste(date(), "\nFinished batch test\n\n"));

performance = 1-rowSums(errors)/sample.count
total.model.time = rowSums(model.time)
total.pred.time = rowSums(pred.time)
total.time = total.model.time + total.pred.time

cat("Errors per set:\n")
print(errors)
cat("\nPerformance:\n")
print(cbind(performance))
cat("\nCPU Time:\n")
print(cbind(total.time))




