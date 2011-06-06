# 
# Author: Michal Kolodziejski
###############################################################################

library("klaR")

# DEPRECATED!!! "test.naiveBayes.e1071" should be used instead.
#
# Transforms given training-set matrix into binary matrix and performs classification
# using NaiveBayes function from klaR library.
# Arguments:
#       trainingSet - matrix with training-set data
#       classes - vector with classes corresponding to examples from training-set in the same order.
#
# Returns naive bayes classifier.
#
classify.naiveBayes = function(trainingSet, classes){
    # transformation into binary matrix
    trainingSet[trainingSet[,] > 0] = 1

    # creates data.frame with data from training-set
    tsData <- data.frame(cbind(t(trainingSet)));
    
    # creates naive bayes classifier, usekernel used in case where class variance is equal to zero for attribute
    nb <- NaiveBayes(tsData, factor(tsData$classes), usekernel = TRUE);
    return(nb);
}

