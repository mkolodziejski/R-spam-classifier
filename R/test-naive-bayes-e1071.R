# 
# Author: Michal Kolodziejski
###############################################################################

library("e1071")
source("load-mail.R")


# Creates naive bayes classifier and performs tests on it.
# Arguments:
#        dataPath - path to the directory containing messages to be read
#        samplesNum - total number of messages to be read
#        spam.part - fraction of all messages that should be spam
#        testSetPart - fraction of all messages that should form test-set data
#
# Returns effectiveness of classifier, summary of loaded messages and classification statistics:
# number of false-positive errors, false-negative errors, true-positive and true-negative classifications,
# true-positive rate and false-positive rate.
#
test.naiveBayes.e1071 = function(dataPath, samplesNum = 1000, spam.part = 0.5, testSetPart = 0.05){
    # read data
    ts <- load.mail(c(dataPath), samples=samplesNum, shuffle.samples=TRUE, min.glob.word.freq = 0.01, spam.probability = spam.part)
	load.summary <- summary(ts)
	
    # transform into binary data
    tsM <- ts[, -1]
    tsM[tsM[,]>0]=1
    tsClass <- factor(ts[, 1])

    # distribute messages between training and test set
    realSamplesNum <- dim(ts)[1];
    testSetCount <- realSamplesNum * testSetPart;
    trainingSetCount <- realSamplesNum - testSetCount;
    testSetIndex <- trainingSetCount + 1;
    
    # create classifier and test it
    enb <- naiveBayes(tsM[1:trainingSetCount,], tsClass[1:trainingSetCount]);
    pred <- predict(enb, tsM[testSetIndex:realSamplesNum,])

    # tp rate i fp rate
    goodResults <- 0;
    tp <- 0;
    tn <- 0;
    fp <- 0;
    fn <- 0;
    for(ind in 1:testSetCount){
        if(pred[ind] == tsClass[testSetIndex + ind - 1]){
            goodResults <- goodResults + 1;
            
            if(pred[ind] == "spam"){ tp <- tp + 1; }
            else { tn <- tn + 1; }
        }
        else {
            if(pred[ind] == "spam"){ fp <- fp + 1; }
            else { fn <- fn + 1; }
        }
    }


    result = list()
    class(result) = "test.naiveBayes.e1071"
	result$load.summary <- load.summary
    result$effectiveness <- goodResults / testSetCount;
    result$TP <- tp;
    result$TN <- tn;
    result$FP <- fp;
    result$FN <- fn;
    result$TPrate <- tp / (tp + fn);
    result$FPrate <- fp / (fp + tn);
    return(result);
}

