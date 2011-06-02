# 
# Author: Michal Kolodziejski
###############################################################################

library("e1071")
source("load-mail.R")

test.naiveBayes.e1071 = function(dataPath, samplesNum = 1000, spam.part = 0.5, testSetPart = 0.05){
    ts <- load.mail(c(dataPath), samples=samplesNum, shuffle.samples=TRUE, min.glob.word.freq = 0.01, spam.probability = spam.part)
	load.summary <- summary(ts)
	
    tsM <- ts[, -1]
    tsM[tsM[,]>0]=1
    tsClass <- factor(ts[, 1])

    realSamplesNum <- dim(ts)[1];
    testSetCount <- realSamplesNum * testSetPart;
    trainingSetCount <- realSamplesNum - testSetCount;
    testSetIndex <- trainingSetCount + 1;
    
    enb <- naiveBayes(tsM[1:trainingSetCount,], tsClass[1:trainingSetCount]);
    pred <- predict(enb, tsM[testSetIndex:realSamplesNum,])

    goodResults <- 0;    
    for(predResult in (pred[] == tsClass[testSetIndex:realSamplesNum])){
        if(predResult == TRUE)
            goodResults <- goodResults + 1;
    }

    result = list()
    class(result) = "test.naiveBayes.e1071"
    #result$trainingSet <- tsMD[1:trainingSetCount,];
    #result$treiningSetClasses <- tsClass[1:trainingSetCount];
    #result$testSet <- tsMD[testSetIndex:samplesNum,];
    #result$testSetClasses <- tsClass[testSetIndex:samplesNum];
    #result$classifier <- enb;
    #result$prediction <- pred;
	result$load.summary <- load.summary
    result$effectiveness <- goodResults / testSetCount;
    return(result);
}

