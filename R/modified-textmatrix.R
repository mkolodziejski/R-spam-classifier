# TODO: Add comment
# 
# Author: marek
###############################################################################


textvector = function (file, stemming = FALSE, language = "english", minWordLength = 2, 
		maxWordLength = FALSE, minDocFreq = 1, maxDocFreq = FALSE, 
		stopwords = NULL, vocabulary = NULL, phrases = NULL, removeXML = FALSE, 
		removeNumbers = FALSE) 
{
	txt = scan(file, what = "character", quiet = TRUE, encoding = "UTF-8")
	txt = iconv(txt, to = "UTF-8")
	res = try(tolower(txt), TRUE)
	if (class(res) == "try-error") {
		stop(paste("[lsa] - could not open file ", file, " due to encoding problems of the file.", 
						sep = ""))
	}
	else {
		txt = res
		res = NULL
		gc()
	}
	if (removeXML) {
		txt = gsub("<[^>]*>", " ", paste(txt, collapse = " "), 
				perl = TRUE)
		txt = gsub("<[^>]*>", " ", paste(txt, collapse = " "), 
				perl = TRUE)
		txt = gsub("&gt;", ">", txt, perl = FALSE, fixed = TRUE)
		txt = gsub("&lt;", "<", txt, perl = FALSE, fixed = TRUE)
		txt = gsub("&quot;", "\"", txt, perl = FALSE, fixed = TRUE)
		if (language == "german") {
			txt = gsub("&auml;", "ae", txt, perl = FALSE, fixed = TRUE)
			txt = gsub("&ouml;", "oe", txt, perl = FALSE, fixed = TRUE)
			txt = gsub("&uuml;", "ue", txt, perl = FALSE, fixed = TRUE)
			txt = gsub("&szlig;", "ss", txt, perl = FALSE, fixed = TRUE)
		}
	}
	if (language == "arabic") {
		txt = gsub("[^[:alnum:]'\\_\\~$\\|><&{}*`\\-]", " ", 
				txt)
	}
	else if (!is.null(phrases)) {
		txt = paste(txt, collapse = " ")
		for (p in phrases) {
			repl = gsub("[[:space:]]+", " ", as.character(p))
			repl = gsub("[[:space:]]+", "_", repl)
			txt = gsub(p, repl, txt)
		}
		txt = gsub("[^[:alnum:]\\_]", " ", txt)
		txt = unlist(strsplit(txt, " "))
	}
	else {
		txt = gsub("[^[:alnum:]]", " ", txt)
	}
	txt = gsub("[[:space:]]+", " ", txt)
	txt = unlist(strsplit(txt, " ", fixed = TRUE))
	if (!is.null(stopwords)) 
		txt = txt[!txt %in% stopwords]
	tab = sort(table(txt), decreasing = TRUE)
	if (stemming) 
		names(tab) = SnowballStemmer(names(tab), Weka_control(S = language))
	if (!is.null(vocabulary)) 
		tab = tab[names(tab) %in% vocabulary]
	tab = tab[tab >= minDocFreq]
	if (is.numeric(maxDocFreq)) 
		tab = tab[tab <= maxDocFreq]
	tab = tab[nchar(names(tab), type = "chars") >= minWordLength]
	if (is.numeric(maxWordLength)) 
		tab = tab[nchar(names(tab), type = "chars") <= maxWordLength]
	if (removeNumbers) {
		tab = tab[-grep("(^[0-9]+$)", names(tab), perl = TRUE)]
	}
	if (length(names(tab)) == 0) { 
		warning(paste("[textvector] - the file ", file, " contains no terms after filtering.", 
						sep = ""))
		return(NA)
	}
	else {
		return(data.frame(docs = basename(file), terms = names(tab), 
					Freq = tab, row.names = NULL))
	}
}

textmatrix = function (mydir, stemming = FALSE, language = "english", minWordLength = 2, 
		maxWordLength = FALSE, minDocFreq = 1, maxDocFreq = FALSE, 
		minGlobFreq = FALSE, maxGlobFreq = FALSE, stopwords = NULL, 
		vocabulary = NULL, phrases = NULL, removeXML = FALSE, removeNumbers = FALSE) 
{
	myfiles = NULL
	if (length(mydir) > 1) {
		for (i in 1:length(mydir)) {
			if (file.info(normalizePath(mydir[i]))$isdir == TRUE) {
				myfiles = append(myfiles, dir(mydir[i], full.names = TRUE, 
								recursive = TRUE))
			}
			else if (file.exists(normalizePath(mydir[i]))) {
				myfiles = append(myfiles, normalizePath(mydir[i]))
			}
			else {
				warning(paste("[textmatrix] - WARNING: file ", 
								mydir[i], " does not exist.", sep = ""))
			}
		}
	}
	else if (file.info(normalizePath(mydir))$isdir == TRUE) {
		myfiles = dir(mydir, full.names = TRUE, recursive = TRUE)
	}
	else if (file.exists(normalizePath(mydir)) == TRUE) {
		myfiles = normalizePath(mydir)
	}
	else {
		stop("[textmatrix] - ERROR: specified input file or directory does not exist.")
	}
	dummy = lapply(myfiles, textvector, stemming, language, minWordLength, 
			maxWordLength, minDocFreq, maxDocFreq, stopwords, vocabulary, 
			phrases, removeXML, removeNumbers)
	
	# remove empty docs
	dummy = dummy[!is.na(dummy)]
	
	if (!is.null(vocabulary)) {
		dtm = t(xtabs(Freq ~ ., data = do.call("rbind", dummy)))
		if (is.numeric(minGlobFreq)) {
			dtm = dtm[rowSums(lw_bintf(dtm)) >= minGlobFreq, 
			]
		}
		if (is.numeric(maxGlobFreq)) {
			dtm = dtm[rowSums(lw_bintf(dtm)) <= maxGlobFreq, 
			]
		}
		gc()
		result = matrix(0, nrow = length(vocabulary), ncol = ncol(dtm))
		rownames(result) = vocabulary
		result[rownames(dtm), ] = dtm[rownames(dtm), ]
		colnames(result) = colnames(dtm)
		dtm = result
		gc()
	}
	else {
		dtm = t(xtabs(Freq ~ ., data = do.call("rbind", dummy)))
		if (is.numeric(minGlobFreq)) {
			dtm = dtm[rowSums(lw_bintf(dtm)) >= minGlobFreq, 
			]
		}
		if (is.numeric(maxGlobFreq)) {
			dtm = dtm[rowSums(lw_bintf(dtm)) <= maxGlobFreq, 
			]
		}
		gc()
	}
	environment(dtm) = new.env()
	class(dtm) = "textmatrix"
	return(dtm)
}
