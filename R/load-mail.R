# Author: Marek Skrajnowski
###############################################################################

# TODO: Move library loading to package depandancies
#library(lsa)
source("modified-textmatrix.R")

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
# Returns a data.frame (with added class 'load.mail') holding the class vector (factor) as the first attribute named "Classes"
# and all the loaded words as the following attributes. Documents are treated as samples/examples.
#
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
		
		# shuffle the examples so spam and ham are mixed
		if(shuffle.samples) {
			shuffled.indices = sample(length(files.sample))
			files.sample = files.sample[shuffled.indices]
		}
		
	}
	else {
		files.sample = sample(files, min(samples, length(files)) )
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
	
	tmatrix = tmatrix[,colSums(tmatrix)>0]
			
	# transpose tmatrix so that attributes (words) are in columns and samples (documents) are in rows
	tmatrix = t(tmatrix)
	# change the class of tmatrix to a regular matrix, which it actually is
	class(tmatrix) = "matrix"

	# create the final classes vector
	file.names = basename(dimnames(tmatrix)[[1]])
	classes = rep(NA, length(file.names))
	names(classes) = file.names
	
	if(detect.classes) {
		classes[grep("spam", file.names)] = "spam"
		classes[grep("ham", file.names)] = "ham"
	}
	
	classes = factor(classes)
		
	#result$matrix = tmatrix
	#result$classes = classes
	result = data.frame(Classes = classes, tmatrix)
	attr(result, 'call') = match.call()
	class(result) = c("load.mail", class(result))
	
	return(result)

}

summary.load.mail = function(data, ...) {
	
	res = list()
	class(res) = "summary.load.mail"
	
	res$call = attr(data, 'call')
	classes = data[,'Classes']
	matrix  = data[,-1]
	
	res$loaded = data.frame( 		Documents = length(classes), 
												Spam = sum(is.spam(classes)  & !is.na(classes)),
												Ham = sum(is.ham(classes) & !is.na(classes)),
												Unknown = sum(is.na(classes)),
												Words = dim(matrix)[2],
												row.names = "")
										
	words.per.doc = rowSums(matrix)
	unique.words.per.doc = rowSums(matrix > 0)
	docs.per.word = colSums(matrix > 0)

	res$words.per.doc.stats = data.frame( Min = min(words.per.doc), Max = max(words.per.doc), 
															Mean = mean(words.per.doc), Std.Dev = sd(words.per.doc), 
															row.names = "" )
													
	res$unique.per.doc.stats = data.frame( Min = min(unique.words.per.doc), Max = max(unique.words.per.doc), 
															Mean = mean(unique.words.per.doc), Std.Dev = sd(unique.words.per.doc), 
															row.names = "" )
	
	min.i = which.min(docs.per.word)
	max.i = which.max(docs.per.word)
	
	res$docs.per.word.stats = data.frame( Min = docs.per.word[min.i], MinWord = names(docs.per.word)[min.i],
															Max = docs.per.word[max.i], MaxWord = names(docs.per.word)[max.i],
															Mean = mean(docs.per.word), Std.Dev = sd(docs.per.word), 
															row.names = "" )
	
		
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

# return a normalized (by the number of words per document) textmatrix
normalize.mail.data = function(mail.data) {
		tmatrix = mail.data[-1]
		words.in.docs = rowSums(tmatrix)
		mail.data[-1] = tmatrix / rep(words.in.docs, times=dim(tmatrix)[2])
		return(mail.data)
}

# return a binary textmatrix
binary.mail.data = function(mail.data) {
		mail.data[-1] = as.numeric( mail.data[-1] > 0 )
		return(mail.data)
}

# testing
load.mail.test = function(samples) {
	
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/tests"
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/test2"
	#data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/small set"
	data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/processed"
	#data.path = "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\processed"
	#data.path = "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\small set"
	
	cpu.time = function() sum(proc.time()[c(1,2)])
	elapsed.time = function() proc.time()[3]
	
	# load mail from a given path
	pt = cpu.time()
	et = elapsed.time()
	data = load.mail(data.path, samples, spam.probability = 0.5, min.doc.word.count = 2);
	cat(paste("\nLoading Time: ", round(elapsed.time()-et, digits = 2), " (", round(cpu.time() - pt, digits = 2), " on CPU)\n\n", sep =""))
	
	pt = cpu.time()
	et = elapsed.time()
	print(summary(data))
	cat(paste("\nSummary Time: ", round(elapsed.time()-et, digits = 2), " (", round(cpu.time() - pt, digits = 2), " on CPU)\n\n", sep =""))
	
	#ndata = normalize.mail.data(data)
	#bdata = binary.mail.data(data)
	#print(data)
	#print(ndata)
	#print(bdata)
	
	
			
	
}




