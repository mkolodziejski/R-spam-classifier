# 
# Author: Michal Kolodziejski
###############################################################################

source("classify-knn-lsa.R")

test.knn.lsa = function(dataPath, k=3, samplesNum = 1000, testSetPart = 0.05, dist.function = cosine.dist){
    ts <- load.mail(c(dataPath), samples=samplesNum, shuffle.samples=TRUE, min.glob.word.freq = 0.01)
	load.summary <- summary(ts)
	
	ts = normalize.mail.data(ts)
    tsM <- ts[, -1]
    tsClass <- factor(ts[, 1])

    realSamplesNum <- dim(ts)[1];
    testSetCount <- realSamplesNum * testSetPart;
    trainingSetCount <- realSamplesNum - testSetCount;
    testSetIndex <- trainingSetCount + 1;
    
    knn.classifier <- classify.knn.lsa(tsM[1:trainingSetCount,], tsClass[1:trainingSetCount], k=k)
    knn.pred = predict(knn.classifier, newdata = tsM[testSetIndex:realSamplesNum,])
	
    goodResults <- 0;    
    for(predResult in (knn.pred$Classes[] == tsClass[testSetIndex:realSamplesNum])){
        if(predResult == TRUE)
            goodResults <- goodResults + 1;
    }

    result = list()
    class(result) = "test.knn.lsa"
	result$load.summary <- load.summary
    result$effectiveness <- goodResults / testSetCount;
    return(result);
}

