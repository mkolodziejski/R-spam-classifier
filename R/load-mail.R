# Author: Marek Skrajnowski
###############################################################################

# TODO: Move library loading to package depandancies
library(lsa)


# Mail loading function
# Arguments:
#		paths 					- vector of paths to files or directories which should be scanned for mail documents
#		samples 				- the number of random samples that should be loaded from paths
#		spam.probability 	- the fraction of samples that should be spam
# 		shuffle 					- should spam and ham samples be mixed, if FALSE, all ham messages will be before spam 
#		min.word.length, max.word.length 				- min/max length of words to load
#		min.doc.word.count, max.doc.word.count 	- min/max number of times a loaded word must appear in a document
#		min.glob.word.freq, max.glob.word.freq 		- min/max global frequency of loaded words
#
# Returns a list of:
# 		$matrix  				- word count matrix, columns represent loaded documents, rows represent found words
# 		$classes 				- vector of classes corresponding with the word count matrix

load.mail = function(paths, samples, spam.probability = 0.5, shuffle = TRUE, 
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
	
	# split files into ham and spam by occurence of "spam" in the file name
	spam.indices = grep("spam", basename(files))
	spam.files = files[spam.indices]
	ham.files = files[-spam.indices]
	
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
	classes = c(rep(0,ham.sample.count), rep(1,spam.sample.count))
	names(classes) = basename(files.sample)
	
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
	
	# shuffle the examples so spam and ham are mixed
	if(shuffle) {
		shuffle.indices = sample(length(files.sample))
		tmatrix = tmatrix[ ,shuffle.indices]
		classes = classes[shuffle.indices]
	}
			
	return( list(matrix = tmatrix, classes = classes) );

}


load.mail.test = function(samples) {
	
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/tests"
	#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/test2"
	#data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/small set"
	data.path = "/media/Stuff/Marek/studia/MOW/Projekt/Dane/processed"
	
	# load mail from a given path
	data = load.mail(data.path, samples, spam.probability = 0.33);
	
	# textmatrix
	tmatrix = data$matrix
	print(paste("tmatrix words:", dim(tmatrix)[1], ", docs:",  dim(tmatrix)[2]))
	
	# class vector	
	classes = data$classes
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




