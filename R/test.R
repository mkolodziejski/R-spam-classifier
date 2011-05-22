# TODO: Add comment
# 
# Author: Marek Skrajnowski
###############################################################################

load.data = function(path) {
	data(stopwords_en);
	files.count = length(dir(path, recursive = TRUE))
	
	# use only words which appear in at least 2% of files
	min.global.count = round(files.count*0.02)
	# skip files which appear in over 50% of files
	max.global.count = round(files.count*0.5)
	
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

data.path = "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\processed"
tmatrix = load.data("C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\easy_ham_2_out");
write.table(tmatrix, "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\easy_ham_2_out_tmatrix.txt")



