library("klaR")

classify.naiveBayes = function(trainingSet, classes){
    # przeksztalcenie na macierz binarna
    trainingSet[trainingSet[,] > 0] = 1

    # polaczenie transponowanej macierzy slow z klasami i stworzenie data.frame
    tsData <- data.frame(cbind(t(trainingSet)));
    
    # utworzenie klasyfikatora, usekernel uzyte dla przypadkow gdy wariancja klas jest zerowa w ramach rozpatrywanego atrybutu
    nb <- NaiveBayes(tsData, factor(tsData$classes), usekernel = TRUE);
    return(nb);
}

