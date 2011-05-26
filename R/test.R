# TODO: Add comment
# 
# Author: Marek Skrajnowski
###############################################################################

library("lsa")

load.data = function(path) {
	data(stopwords_en);
	files.count = length(dir(path, recursive = TRUE))
	
	# use only words which appear in at least 2% of files
	min.global.count = max(round(files.count*0.01), 2)
	# skip files which appear in over 65% of files
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

#data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/tests"
data.path = "/home/kouodziey/workspace/R-spam-classifier/Data/test2"
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



