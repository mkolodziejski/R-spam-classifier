# Author: Marek Skrajnowski
###############################################################################

# TODO: Move library loading to package depandancies
library(lsa)


is.spam = function(class) {
	return(class == 'spam')
}

is.ham = function(class) {
	return(class == 'ham')
}

# Mail loading function
# Arguments:
#		paths 					- vector of paths to files or directories which should be scanned for mail documents
#		samples 				- the number of random samples that should be loaded from paths
#		detect.classes		- should the function try to classify mail by occurence of "spam" or "ham" in file names
#		spam.probability 	- the fraction of samples that should be spam, works only with detect.classes = TRUE
# 		shuffle 				- should spam and ham samples be mixed, if FALSE, all ham messages will be before spam, 
#									  works only with detect.classes = TRUE 
#		min.word.length, max.word.length 				- min/max length of words to load
#		min.doc.word.count, max.doc.word.count 	- min/max number of times a loaded word must appear in a document
#		min.glob.word.freq, max.glob.word.freq 		- min/max global frequency of loaded words
#
# Returns a list of:
# 		$matrix  				- word count matrix, columns represent loaded documents, rows represent found words
# 		$classes 				- vector of classes corresponding with the word count matrix

load.mail = function(paths, samples, 
								detect.classes = TRUE, spam.probability = 0.5,
								shuffle.samples = FALSE, 
								min.word.length = 3, max.word.length = FALSE, 
								min.doc.word.count = 1, max.doc.word.count = FALSE, 
								min.glob.word.freq = 0.01, max.glob.word.freq = 0.65) {
	
	# create a list of files in given paths
	files = c()	
	for(path in paths) {
		
		if(!file.exists(path)) {
			warning(paste("Path doesnt exist:", path))
			next
		}
		
		path.info = file.info(path)
		# if path is a directory add all files from that directory
		if(path.info$isdir) {
			files = c(files, dir(path, recursive = TRUE, full.names = TRUE))
		}
		# otherwise add a single file
		else {
			files = c(files, path)
		}
		
	}
	
	# create a sample of files to load	
	files.sample = NULL
	classes = NULL
	
	if(detect.classes) {
		# split files into ham, spam and unknown by occurence of class name in the file name
		spam.indices = grep("spam", basename(files))
		ham.indices = grep("ham", basename(files))
		spam.files = files[spam.indices]
		ham.files = files[ham.indices]
		# TODO: do something with unknown files
		unknown.files = files[ -c(spam.indices, ham.indices) ] 

		# count the number of spam and ham files, so we dont sample more spam/ham than we have
		files.count = length(files)
		spam.count = length(spam.files)
		ham.count = length(ham.files)
		
		spam.sample.count = min( round(spam.probability*samples), spam.count )
		ham.sample.count = min( samples - spam.sample.count, ham.count )

		# pick sample files of each class
		spam.sample = sample(spam.files, spam.sample.count)
		ham.sample = sample(ham.files, ham.sample.count)
	
		# join sample sets and generate a classes vector
		files.sample = c(ham.sample, spam.sample)
		classes = c(rep('ham',ham.sample.count), rep('spam',spam.sample.count))
		names(classes) = basename(files.sample)
		
		# shuffle the examples so spam and ham are mixed
		if(shuffle.samples) {
			shuffled.indices = sample(length(files.sample))
			files.sample = files.sample[shuffled.indices]
			classes = classes[shuffled.indices]
		}
		
	}
	else {
		files.sample = sample(files, min(samples, length(files)) )
		classes = rep(NA, length(files.sample))
	}	
	
	# perform the actual text loading
		
	# use only words which appear in at least 2% of files
	min.glob.word.count = max(round(samples*min.glob.word.freq), 2)
	# skip files which appear in over 65% of files
	max.glob.word.count = round(samples*max.glob.word.freq)
	
	# load stopwords from lsa package
	data(stopwords_en);
	
	tmatrix = textmatrix(files.sample, 
			stemming = TRUE, 
			language = "english", stopwords = stopwords_en, 
			minWordLength = min.word.length, maxWordLength = max.word.length,
			minDocFreq = min.doc.word.count, maxDocFreq = max.doc.word.count,
			minGlobFreq = min.glob.word.count, maxGlobFreq = max.glob.word.count,
			removeNumbers = FALSE, removeXML = FALSE
			);
			
	result =  list()
	class(result) = "load.mail"

	result$matrix = tmatrix
	result$classes = classes
	result$call = match.call()
	
	return(result)

}

summary.load.mail = function(object, ...) {
	
	res = list()
	class(res) = "summary.load.mail"
	
	res$call = object$call
	classes = object$classes
	matrix = object$matrix
	
	res$loaded = data.frame( 	Documents = 	length(classes), 
												Spam = sum(is.spam(classes)  & !is.na(classes)),
												Ham = sum(is.ham(classes) & !is.na(classes)),
												Unknown = sum(is.na(classes)),
												Words = dim(matrix)[1] )
	
	words.per.doc = colSums(matrix)
	unique.words.per.doc = colSums(matrix > 0)
	docs.per.word = rowSums(matrix > 0)

	res$words.per.doc.stats = data.frame( Min = min(words.per.doc), Max = max(words.per.doc), Mean = mean(words.per.doc), Std.Dev = sd(words.per.doc) )
	res$unique.per.doc.stats = data.frame( Min = min(unique.words.per.doc), Max = max(unique.words.per.doc), Mean = mean(unique.words.per.doc), Std.Dev = sd(unique.words.per.doc) )
	res$docs.per.word.stats = data.frame( Min = min(docs.per.word), Max = max(docs.per.word), Mean = mean(docs.per.word), Std.Dev = sd(docs.per.word) )
	
	return(res)
	
}

print.summary.load.mail = function(x, ...) {
	cat("Call:\n")
	print(x$call)
	cat("\nLoaded:\n")
	print(x$loaded)
	cat("\nNumber of words per document:\n")
	print(x$words.per.doc.stats)
	cat("\nNumber of unique words per document:\n")
	print(x$unique.per.doc.stats)
	cat("\nNumber of documents per word:\n")
	print(x$docs.per.word.stats)
}



# testing
load.mail.test = function(samples) {
	
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/tests"
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/test2"
	#data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/small set"
	#data.path = "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\processed"
	data.path = "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\small set"
	#data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/processed"
	
	# load mail from a given path
	data = load.mail(data.path, samples, spam.probability = 0.33);
	
	cat("Summary:\n\n")
	print(summary(data))
	cat("\nEnd Summary\n\n")
	
	# textmatrix
	tmatrix = data$matrix
	print(paste("tmatrix words:", dim(tmatrix)[1], ", docs:",  dim(tmatrix)[2]))
	
	# class vector	
	classes = is.spam(data$classes)
	print(paste("classes length:", length(classes)))
	print(paste("spam count:", sum(classes)))
	print(paste("shuffle:", sum( abs(classes[-1] - classes[-length(classes)]) ) ))
		
	# terms
	terms = dimnames(tmatrix)$terms
	
	# document names
	docs = names(classes)
	docs2 = dimnames(tmatrix)$docs

	write.table(tmatrix, file = paste(data.path, ".matrix.txt", sep=""))
	write.table(terms, file = paste(data.path, ".terms.txt", sep=""))
	write.table(docs, file = paste(data.path, ".docs.txt", sep=""))
	write.table(classes, file = paste(data.path, ".classes.txt", sep=""))
	
	if( sum(docs != docs2) > 0 ) {
		print("Documents in tmatrix and class vector are in different order")
	}	
	
}




