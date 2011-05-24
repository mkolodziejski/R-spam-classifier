# TODO: Add comment
# 
# Author: Marek Skrajnowski
###############################################################################

load.data = function(path) {
	data(stopwords_en);
	files.count = length(dir(path, recursive = TRUE))
	
	# use only words which appear in at least 2% of files
	min.global.count = round(files.count*0.01)
	# skip files which appear in over 50% of files
	max.global.count = round(files.count*0.65)
	
	tmatrix = textmatrix(path, 
			stemming = TRUE, 
			language = "english", stopwords = stopwords_en, 
			minWordLength = 3, maxWordLength = FALSE,
			minDocFreq = 1, maxDocFreq = FALSE,
			minGlobFreq = min.global.count, maxGlobFreq = max.global.count,
			removeNumbers = FALSE, removeXML = FALSE
			);
	
	return(tmatrix);

}

data.path = "F:\\Marek\\studia\\MOW\\Projekt\\Dane\\small set"
# create document-term matrix
tmatrix = load.data(data.path);
# terms
terms = dimnames(tmatrix)$terms
# document names
docs = dimnames(tmatrix)$docs

# classes
classes = rep(0, length(docs))
classes[grep("spam", docs)] = 1
names(classes) = docs

write.table(tmatrix, file = paste(data.path, ".matrix.txt", sep=""))
write.table(terms, file = paste(data.path, ".terms.txt", sep=""))
write.table(docs, file = paste(data.path, ".docs.txt", sep=""))
write.table(classes, file = paste(data.path, ".classes.txt", sep=""))



